#' @title Check NFSA Loading Logs
#'
#' @description This function scans NFSA loading logs, extracts relevant information,
#' and saves it to an Excel file.
#'
#' @param time_min A date specifying the minimum time for log files to be considered.
#'   Defaults to the beginning of the current day.
#' @param time_max A character string representing the maximum date and time for filtering received files, defaults to current date).  Files received after this time will be excluded.
#' @param output_sel Path to the directory where the output Excel file will be saved.
#'   Defaults to "output/logs" within the project directory (using `here::here()`).
#' @param recursive Logical. Should the function search for log files recursively within subdirectories?
#'   Defaults to `FALSE`.
#'
#' @return Invisibly returns a tibble containing the processed log data if any files were loaded, otherwise
#'   invisibly returns `NULL`. An Excel file containing the processed data is saved in the
#'   `output_sel` directory.
#'
#' @examples
#' \dontrun{
#' # Check logs loaded since yesterday
#' nfsa_loaded(time_min = Sys.Date() - 1)
#'
#' # Check logs loaded since a specific date
#' nfsa_loaded(time_min = "2025-10-01")
#'
#' # Check logs recursively within subdirectories
#' nfsa_loaded(time_min = "2020-01-01",recursive = TRUE)
#' }
#' @export
nfsa_loaded <- function(time_min,
                        time_max = lubridate::today(),
                        output_sel = here::here("output", "logs"),
                        recursive = FALSE) {

  # 1. Setup Fields & Pattern
  fields <- c("nameproc", "InputFile", "CheckFile", "TypeOfFile", "TypeOfSer",
              "tableL", "freqL", "adjustL", "countryL", "CP_Area",
              "Ref_Sector", "Sto", "instr_assetsL", "unitL", "nb_series", "feedRC")
  search_pattern <- paste0("(", paste(fields, collapse = "|"), ") =")

  # 2. Define a Robust Reader
  # 'safely' ensures that if a file fails, it returns NULL instead of an error
  safe_read <- purrr::safely(function(file) {
    readr::read_lines(file) |>
      tibble::enframe(name = NULL) |>
      dplyr::filter(stringr::str_detect(value, search_pattern))
  })

  cli::cli_inform("Scanning server for log files...")

  # 3. Filter files by date FIRST
  all_files <- list.files("I:/econ/NFSA/LOG/", pattern = ".*txt$",
                          full.names = TRUE, recursive = recursive)

  loaded_files <- tibble::tibble(file = all_files) |>
    dplyr::mutate(file_date = lubridate::ymd_hm(stringr::str_sub(file, -14, -5))) |>
    dplyr::filter(file_date >= time_min, file_date <= time_max)

  if (nrow(loaded_files) == 0) {
    cli::cli_alert_warning("No files found for the selected period!")
    return(NULL)
  }

  cli::cli_inform("Processing {nrow(loaded_files)} files...")

  # 4. Apply the safe reader
  # This creates a 'result' (the data) and an 'error' (the message if it failed)
  processed_list <- purrr::map(loaded_files$file, safe_read)

  # Check for errors and notify user
  errors <- purrr::map(processed_list, "error")
  failed_count <- sum(!purrr::map_lgl(errors, is.null))

  if (failed_count > 0) {
    cli::cli_alert_danger("{failed_count} file(s) could not be read (they may be locked or corrupt).")
  }

  # 5. Extract successful results and clean
  final_data <- loaded_files |>
    dplyr::mutate(data = purrr::map(processed_list, "result")) |>
    tidyr::unnest(data) |>
    dplyr::mutate(
      value = stringr::str_remove_all(value, "0=OK 2=Warning 4=Rejection 5=Error|\\(\\)"),
      value = stringr::str_replace(value, "feedRC=", "feedRC = ")
    ) |>
    tidyr::separate_wider_delim(value, delim = " = ", names = c("field", "result"), too_many = "merge") |>
    dplyr::mutate(result = stringr::str_trim(result)) |>
    dplyr::distinct() |>
    tidyr::pivot_wider(names_from = field, values_from = result) |>
    janitor::clean_names() |>
    dplyr::transmute(
      country = country_l,
      loaded = file_date,
      input_file,
      table = table_l,
      type_of_series = type_of_ser,
      check_file,
      type_of_file,
      freq = freq_l,
      adjustment = adjust_l,
      sto,
      cp_area,
      ref_sector,
      instr_asset = instr_assets_l,
      unit = unit_l,
      nb_series,
      result = dplyr::case_match(feed_rc,
                                 "0" ~ "OK", "2" ~ "Warning",
                                 "4" ~ "Rejection", "5" ~ "Error",
                                 .default = feed_rc)
    )

  # 6. Export
  if (!dir.exists(output_sel)) dir.create(output_sel, recursive = TRUE)

  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  file_out <- file.path(output_sel, paste0("loaded_", timestamp, ".xlsx"))

  openxlsx::write.xlsx(final_data, file = file_out, overwrite = TRUE, asTable = TRUE)
  cli::cli_alert_success("Log report created: {.file {file_out}}")

  return(final_data)
}
