#' @title Compare NFSA T0801 Data with Quarterly National Accounts (QNA)
#'
#' @description This function compares the NFSA T0801 data with the Quarterly National Accounts (QNA) data, identifies discrepancies based on a specified threshold, and generates an Excel file containing the comparison results.
#'
#' @param country A character vector specifying the country or countries to process (ISO2 code).
#' @param quarter A character string specifying the quarter in "YYYYQQ" format (e.g., "2023Q1").
#' @param threshold A numeric value indicating the absolute difference threshold for flagging discrepancies between NFSA and QNA data. Default is 1.
#' @param input_sel A character string specifying the directory containing the NFSA data. Default is `"M:/nas/Rprod/data/q/new/nsa/"`.
#' @param output_sel A character string specifying the directory to save the output Excel file. Default is `here::here("output", "inter_domain")`.
#'
#' @return None. The function generates an Excel file in the `output_sel` directory with the comparison results.  It also prints a success message to the console indicating where the file was saved or that the data is consistent.
#'
#' @details This function performs the following steps:
#'   \enumerate{
#'     \item Loads NFSA T0801 data using the `nfsa_get_data` function.
#'     \item Reads relevant QNA data from XML files within a specified directory structure.
#'     \item Merges the NFSA and QNA data based on common keys.
#'     \item Calculates the absolute difference between NFSA and QNA values.
#'     \item Flags discrepancies where the absolute difference exceeds the specified `threshold`.
#'     \item Generates an Excel file containing the comparison results.
#'   }
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' nfsa_T0801_QNA(country = "BE", quarter = "2023Q2", threshold = 2)
#' }
#'
#' @export
nfsa_T0801_QNA <- function(country,
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

  # 2. Identify latest QNA XML Files
  cli::cli_progress_message("Locating latest QNA XML files...")
  base_path <- file.path("M:/nas/QSA10/Production", quarter, "(1) QSA/(1_2) Validation in progress/(1_2_4) Consistency checks - QSA vs QNA/Input")

  # Build country pattern for faster file filtering
  country_pattern <- paste0(".*_(", paste(country, collapse = "|"), ")_.*\\.xml$")

  nama_files <- list.files(path = base_path, pattern = country_pattern,
                           full.names = TRUE, recursive = TRUE) |>
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
        nama = as.numeric(obs_value)
      ) |>
      dplyr::distinct()
  }

  # Read XML files with progress feedback
  cli::cli_progress_message("Reading {length(nama_files)} XML file(s)...")
  nama_data <- purrr::map(nama_files, read_nama, .progress = TRUE) |>
    dplyr::bind_rows()

  # 4. Join and Calculate Consistency
  # Creating a GDP lookup ensures we don't lose rows if GDP is missing for some periods
  gdp_lookup <- nama_data |>
    dplyr::filter(ref_sector == "S1", sto == "B1GQ", accounting_entry == "B") |>
    dplyr::select(ref_area, time_period, gdp = nama)

  nama_nfsa <- dplyr::inner_join(nama_data, nfsa_data,
                                 by = c("ref_area", "ref_sector", "sto", "accounting_entry", "time_period")) |>
    dplyr::left_join(gdp_lookup, by = c("ref_area", "time_period")) |>
    dplyr::mutate(
      diff = round(nfsa - nama, 2),
      as_gdp = round(diff * 100 / gdp, 3)
    ) |>
    dplyr::filter(abs(diff) > threshold) |>
    dplyr::mutate(
      high_diff = abs(as_gdp) > 0.3,
      validate = dplyr::if_else(high_diff & sto == "B1GQ" & time_period >= limit_validation,
                                "NOT VALIDATED", "OK")
    )

  # 5. Summary and Export
  if (nrow(nama_nfsa) == 0) {
    cli::cli_alert_success("T0801 and QNA are fully consistent!")
    return(NULL)
  }
  # ---------------------------

  ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
  country_tag <- if(length(unique(nama_nfsa$ref_area)) == 1) unique(nama_nfsa$ref_area) else "Multi"
  file_out <- file.path(output_sel, paste0("T0801_QNA_", country_tag, "_", ts, ".xlsx"))

  openxlsx::write.xlsx(nama_nfsa, file = file_out, overwrite = TRUE, asTable = TRUE)
  cli::cli_alert_success("Validation report created: {.file {file_out}}")
  nfsa::nfsa_to_excel(nama_nfsa)
  return(nama_nfsa)
}

