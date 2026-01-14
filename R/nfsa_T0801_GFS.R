#' @title Compare NFSA T0801 data with GFS data
#'
#' @description This function compares NFSA T0801 data with GFS data for a given country and quarter, identifying discrepancies above a specified threshold.
#'
#' @param country A character vector specifying the country code(s) to process.
#' @param quarter A character string specifying the quarter in the format "YYYYQQ" (e.g., "2023Q1").
#' @param threshold A numeric value specifying the threshold for the absolute difference between NFSA and GFS values.  Defaults to 1.
#' @param input_sel A character string specifying the file path for the NFSA input data directory. Defaults to `"M:/nas/Rprod/data/q/new/nsa/"`.
#' @param output_sel A character string specifying the file path for the output directory. Defaults to `here::here("output", "inter_domain")`.
#'
#' @return This function does not return a value. It generates an Excel file containing the comparison results in the specified output directory if discrepancies are found. If no discrepancies are found, a success message is displayed.
#'
#' @details The function performs the following steps:
#'   \enumerate{
#'     \item Collects NFSA T0801 data using `nfsa_get_data`.
#'     \item Collects GFS data from XML files located in a specific directory structure based on the provided quarter.
#'     \item Joins the NFSA and GFS data based on `ref_area`, `ref_sector`, `sto`, `accounting_entry`, and `time_period`.
#'     \item Calculates the absolute difference between NFSA and GFS values.
#'     \item Filters for discrepancies where the absolute difference exceeds the specified threshold.
#'     \item Calculates the difference as a percentage of GDP.
#'     \item Flags observations where the difference as a percentage of GDP exceeds 0.3%.
#'     \item Writes the comparison results to an Excel file in the specified output directory.
#'   }
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' nfsa_T0801_GFS(country = "IE", quarter = "2023-Q1", threshold = 5)
#' }
#'
#' @export
nfsa_T0801_GFS <- function(country,
                           quarter,
                           threshold = 1,
                           input_sel = "M:/nas/Rprod/data/q/new/nsa/",
                           output_sel = here::here("output", "inter_domain")) {

  if (!dir.exists(output_sel)) dir.create(output_sel, recursive = TRUE)

  # Robust date logic
  q_year <- as.numeric(stringr::str_extract(quarter, "^\\d{4}"))
  q_sub  <- stringr::str_extract(quarter, "Q\\d{2}$")
  limit_validation <- paste0(q_year - 4, "-", q_sub)

  # 1. Collect NFSA Data
  cli::cli_progress_message("Collecting NFSA (T0801)...")
  nfsa_data <- nfsa_get_data(country = country, table = "T0801", type = "new") |>
    dplyr::select(ref_area, id, time_period, nfsa = obs_value) |>
    nfsa::nfsa_separate_id()

  # 2. Identify latest GFS XML Files
  cli::cli_progress_message("Locating latest GFS XML files...")
  base_path <- file.path("M:/nas/QSA10/Production", quarter, "(1) QSA/(1_2) Validation in progress/(1_2_5) Consistency checks - QSA vs GFS/Input")

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
        gfs = as.numeric(obs_value)
      ) |>
      dplyr::distinct()
  }

  gfs_data <- purrr::map(gfs_files, read_gfs) |> dplyr::bind_rows()

  # 4. Join and Logic
  # We join and filter out rows that don't exist in BOTH datasets (consistency check)
  gfs_nfsa <- dplyr::inner_join(gfs_data, nfsa_data,
                                by = c("ref_area", "ref_sector", "sto", "accounting_entry", "time_period")) |>
    dplyr::mutate(diff = round(nfsa - gfs, 2)) |>
    dplyr::filter(abs(diff) > threshold)

  # GDP Lookup from NFSA (using B1GQ)
  gdp_lookup <- nfsa_data |>
    dplyr::filter(sto == "B1GQ") |>
    dplyr::select(ref_area, time_period, gdp = nfsa)

  gfs_nfsa <- gfs_nfsa |>
    dplyr::left_join(gdp_lookup, by = c("ref_area", "time_period")) |>
    dplyr::mutate(
      as_gdp = round(diff * 100 / gdp, 3),
      high_diff = abs(as_gdp) > 0.3,
      # GFS validation usually focuses on B9 (Net Lending/Borrowing)
      validate = dplyr::if_else(high_diff & sto == "B9" & time_period >= limit_validation,
                                "NOT VALIDATED", "OK")
    )

  # 5. Reporting
  if (nrow(gfs_nfsa) == 0) {
    cli::cli_alert_success("T0801 and GFS fully consistent!")
    return(NULL)
  }

  # Console Summary Table
  summary_stats <- gfs_nfsa |>
    dplyr::group_by(ref_area) |>
    dplyr::summarise(
      total_flags = n(),
      b9_not_validated = sum(validate == "NOT VALIDATED"),
      .groups = "drop"
    )

  cli::cli_h1("GFS vs QSA Consistency Summary")
  print(summary_stats)

  ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
  unique_areas <- unique(gfs_nfsa$ref_area)
  country_tag <- if(length(unique_areas) == 1) unique_areas else "Multi"
  file_out <- file.path(output_sel, paste0("T0801_GFS_", country_tag, "_", ts, ".xlsx"))

  openxlsx::write.xlsx(gfs_nfsa, file = file_out, overwrite = TRUE, asTable = TRUE)
  cli::cli_alert_success("Validation report created: {.file {file_out}}")

  return(gfs_nfsa)
}






