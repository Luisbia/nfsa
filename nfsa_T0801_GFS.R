#' @title Compare NFSA T0801 data with GFS data
#'
#' @description This function compares NFSA T0801 data with GFS data for a given country and quarter, identifying discrepancies above a specified threshold.
#'
#' @param data Data frame containing T0801 observations with `ref_area`, `id`,
#'   `time_period`, and `obs_value`.
#' @param ref_quarter A character string specifying the quarter in the format "YYYYQQ" (e.g., "2023Q1").
#' @param threshold A numeric value specifying the threshold for the absolute difference between NFSA and GFS values.  Defaults to 5.
#'
#' @return A data frame of discrepancies and validation indicators. Returns
#'   `NULL` with a success message when no discrepancies are found.
#'
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' nfsa_T0801_GFS(data = dat, ref_quarter = "2023Q1", threshold = 5)
#' }
#'
#' @importFrom cli cli_alert_success cli_progress_message
#' @importFrom dplyr distinct filter group_by if_else inner_join left_join mutate pull select slice_max transmute ungroup
#' @importFrom janitor clean_names
#' @importFrom purrr list_rbind map
#' @importFrom readsdmx read_sdmx
#' @importFrom stringr str_detect str_extract str_replace
#' @importFrom tibble enframe
#' @export
nfsa_T0801_GFS <- function(data,
                           ref_quarter,
                           threshold = 5) {

  country <- sort(unique(data$ref_area))

  # Robust date logic
  q_year <- as.numeric(stringr::str_extract(ref_quarter, "^\\d{4}"))
  q_sub  <- stringr::str_extract(ref_quarter, "Q\\d{2}$")
  limit_validation <- paste0(q_year - 4, "-", q_sub)

  # 1. Collect NFSA Data
  nfsa_data <- data |>
    dplyr::select(ref_area, id, time_period, qsa = obs_value)

  # 2. Identify latest GFS XML Files
  cli::cli_progress_message("Locating latest GFS XML files...")
  base_path <- file.path("M:/nas/QSA10/Production", ref_quarter, "(1) QSA/(1_2) Validation in progress/(1_2_5) Consistency checks - QSA vs GFS/Input")
  country <- str_replace(country,"EL", "GR")# EL is GR in nama file names
  gfs_files <- list.files(path = base_path, pattern = "\\.xml$", full.names = TRUE, recursive = TRUE) |>
    tibble::enframe(name = NULL, value = "path") |>
    dplyr::mutate(
      file_name = basename(path),
      # Extract 2-letter country code from filename
      file_ref_area = stringr::str_extract(file_name, "(?<=_)[A-Z]{2}(?=_)"),
      # Extract 12-14 digit timestamp
      update_ts = as.numeric(stringr::str_extract(file_name, "\\d{12,14}"))
    ) |>
    dplyr::filter(file_ref_area %in% country) |>
    dplyr::group_by(file_ref_area) |>
    dplyr::slice_max(update_ts, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::pull(path)

  # 3. Read GFS Data
  read_gfs <- function(file) {
    readsdmx::read_sdmx(file) |>
      janitor::clean_names() |>
      dplyr::filter(adjustment == "N") |>
      dplyr::transmute(
        ref_area, ref_sector, sto, accounting_entry, time_period,
        gfs = as.numeric(obs_value),
        source_date = as.Date(stringr::str_extract(basename(file), "\\d{12,14}"), format = "%Y%m%d")
      ) |>
      dplyr::distinct() |>
      nfsa::nfsa_unite_id()
  }

  gfs_data <- purrr::map(gfs_files, read_gfs,.progress = TRUE) |>
    purrr::list_rbind(names_to = "file")  |>
    dplyr::mutate(ref_area = dplyr::if_else(ref_area == "GR", "EL", ref_area)) |>
    dplyr::mutate(status = dplyr::if_else(stringr::str_detect(file,"non-Validated"),"Non-Validated", "Validated")) |>
    dplyr::select(-file)

  # 4. Join and Logic
  # We join and filter out rows that don't exist in BOTH datasets (consistency check)
  gfs_nfsa <- dplyr::inner_join(gfs_data, nfsa_data,
                                by = c("ref_area", "id", "time_period")) |>
    dplyr::mutate(diff = round(qsa - gfs, 2)) |>
    dplyr::filter(abs(diff) > threshold)

  # GDP Lookup from NFSA (using B1GQ)
  gdp_lookup <- nfsa_data |>
    dplyr::filter(id == "S1.B1GQ.B") |>
    dplyr::select(ref_area, time_period, gdp = qsa)

  gfs_nfsa <- gfs_nfsa |>
    dplyr::left_join(gdp_lookup, by = c("ref_area", "time_period")) |>
    dplyr::mutate(
      as_gdp = round(diff * 100 / gdp, 3),
      high_diff = abs(as_gdp) > 0.3,
      # GFS validation usually focuses on B9 (Net Lending/Borrowing)
      validate = dplyr::if_else(high_diff & id == "S13.B9.B" & time_period >= limit_validation,
                                "NOT VALIDATE","VALIDATE")) |>
    dplyr::select(ref_area,
                  id,
                  time_period,
                  gfs_date = source_date,
                  status,
                  gfs,
                  qsa,
                  Difference = diff,
                  `As % GDP` = as_gdp,
                  `Over 0.3 % GDP?` = high_diff,
                  `Validate?` =validate)


  # 5. Reporting
  if (nrow(gfs_nfsa) == 0) {
    cli::cli_alert_success("T0801 and GFS fully consistent!")
    return(NULL)
  }
  return(gfs_nfsa)
}





