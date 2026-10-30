#' @title Compare NFSA T0801 Data with Quarterly National Accounts (QNA)
#'
#' @description This function compares the NFSA T0801 data with the Quarterly National Accounts (QNA) data, identifies discrepancies based on a specified threshold, and generates an Excel file containing the comparison results.
#'
#' @param data Data frame containing T0801 observations with `ref_area`, `id`,
#'   `time_period`, and `obs_value`.
#' @param ref_quarter A character string specifying the quarter in the format "YYYYQQ" (e.g., "2023Q1").
#' @param threshold A numeric value indicating the absolute difference threshold for flagging discrepancies between NFSA and QNA data. Default is 1.
#' @param non_validated Logical condition. If `FALSE`, which is the default option, only looks for files in validated. If set to `TRUE` also looks in non-validated.
#'
#' @return A data frame of discrepancies and validation indicators. Returns
#'   `NULL` with a success message when no discrepancies are found.
#'
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' nfsa_T0801_QNA(data = "dat", ref_quarter = "2023Q2", threshold = 2)
#' }
#'
#' @importFrom cli cli_alert_success cli_progress_message
#' @importFrom dplyr bind_rows case_when distinct filter if_else inner_join left_join mutate pull select slice_max transmute
#' @importFrom janitor clean_names
#' @importFrom purrr map
#' @importFrom readsdmx read_sdmx
#' @importFrom stringr str_extract str_replace
#' @importFrom tibble enframe
#' @export
nfsa_T0801_QNA <- function(data,
                           ref_quarter,
                           threshold = 5,
                           non_validated = FALSE) {

country <- sort(unique(data$ref_area))

  # Robust date logic
  q_year <- as.numeric(stringr::str_extract(ref_quarter, "^\\d{4}"))
  q_sub  <- stringr::str_extract(ref_quarter, "Q\\d{2}$")
  limit_validation <- paste0(q_year - 4, "-", q_sub)

  # 1. Collect NFSA Data
  nfsa_data <- data |>
    select(ref_area,time_period,id,qsa = obs_value)

  # 2. Identify latest QNA XML Files
  cli::cli_progress_message("Locating latest QNA XML files...")

  country <- stringr::str_replace(country,"EL", "GR")# EL is GR in nama file names
  base_path <- file.path("M:/nas/QSA10/Production", ref_quarter, "(1) QSA/(1_2) Validation in progress/(1_2_4) Consistency checks - QSA vs QNA/Input")

  # Build country pattern for faster file filtering
  country_pattern <- paste0(".*_(", paste(country, collapse = "|"), ")_.*\\.xml$")

  nama_files <- list.files(path = base_path, pattern = country_pattern,
                           full.names = TRUE, recursive = non_validated) |>
    tibble::enframe(name = NULL, value = "path") |>
    dplyr::mutate(
      file_name = basename(path),
      file_ref_area = stringr::str_extract(file_name, "(?<=_)[A-Z]{2}(?=_)"),
      update_ts = as.numeric(stringr::str_extract(file_name, "\\d{14}"))
    ) |>
    dplyr::filter(file_ref_area %in% country) |>
    dplyr::slice_max(update_ts, n = 1, by = file_ref_area,
                     with_ties = FALSE) |>
    dplyr::pull(path)


  # 3. Read NAMA Data
  read_nama <- function(file) {
    readsdmx::read_sdmx(file) |>
      janitor::clean_names() |>
      dplyr::transmute(
        ref_area, ref_sector, sto, accounting_entry, time_period,
        accounting_entry = dplyr::case_when(
          sto == "EMP" & unit_measure == "PS" & counterpart_area == "W2" ~ "PS",
          sto == "EMP" & unit_measure == "HW" & counterpart_area == "W2" ~ "HW",
          TRUE ~ accounting_entry
        ),
        qna = as.numeric(obs_value),
        source_date = as.Date(stringr::str_extract(basename(file), "\\d{14}"), format = "%Y%m%d%H%M%S")
      ) |>
      nfsa::nfsa_unite_id() |>
      dplyr::distinct()
  }

  # Read XML files with progress feedback
  cli::cli_progress_message("Reading {length(nama_files)} XML file(s)...")
  nama_data <- purrr::map(nama_files, read_nama, .progress = TRUE) |>
    purrr::list_rbind(names_to = "file") |>
    dplyr::mutate(ref_area = if_else(ref_area == "GR", "EL", ref_area)) |>
    dplyr::mutate(status = dplyr::if_else(stringr::str_detect(file,"non-Validated"),"Non-Validated", "Validated")) |>
    dplyr::select(-file)



  # 4. Join and Calculate Consistency
  # Creating a GDP lookup ensures we don't lose rows if GDP is missing for some periods
  gdp_lookup <- nama_data |>
    dplyr::filter(id =="S1.B1GQ.B") |>
    dplyr::select(ref_area, time_period, gdp = qna)

  nama_nfsa <- dplyr::inner_join(nama_data, nfsa_data,
                                 by = c("ref_area", "id", "time_period")) |>
    dplyr::left_join(gdp_lookup, by = c("ref_area", "time_period")) |>
    dplyr::mutate(
      diff = round(qna - qsa, 2),
      as_gdp = if_else(!stringr::str_detect(id,"EMP"),round(diff * 100 / gdp, 3), NA)
    ) |>
    dplyr::filter(abs(diff) > threshold) |>
    dplyr::mutate(
      high_diff = abs(as_gdp) > 0.3,
      validate = dplyr::if_else(high_diff & id == "S1.B1GQ.B" & time_period >= limit_validation,
                                "NOT VALIDATE", "VALIDATE")
    ) |>
    dplyr::select(ref_area,id,time_period,qna_date = source_date,status,
                  qna,qsa,Difference = diff,
                  `As % GDP` = as_gdp,
                  `Over 0.3 % GDP?` = high_diff,
                  `Validate?` =validate)

  # 5. Summary and Export
  if (nrow(nama_nfsa) == 0) {
    cli::cli_alert_success("T0801 and QNA are fully consistent!")
    return(NULL)
  }
  return(nama_nfsa)
}

