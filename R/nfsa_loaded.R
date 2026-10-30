#' @title Check NFSA Loading Logs
#'
#' @description This function scans NFSA loading logs and extracts relevant information.
#'
#' @param country Country(ies) to look for.
#' @param time_min A date specifying the minimum time for log files to be considered.
#'   Required; log files dated before this value are excluded.
#' @param time_max Date or date-time upper bound. Defaults to the current date
#'   at midnight; files received after this time are excluded.
#' @param recursive Logical. Should the function search for log files recursively within subdirectories?
#'   Defaults to `FALSE`.
#'
#' @return Invisibly returns a tibble containing the processed log data if any files were loaded, otherwise
#'   invisibly returns `NULL`.
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
#' @importFrom cli cli_alert_danger cli_alert_warning cli_inform
#' @importFrom dplyr case_match distinct filter group_by mutate slice_tail transmute ungroup
#' @importFrom janitor clean_names
#' @importFrom lubridate today ymd_hm
#' @importFrom purrr map map_lgl safely
#' @importFrom readr read_lines
#' @importFrom stringr str_detect str_remove_all str_replace str_sub str_trim
#' @importFrom tibble enframe tibble
#' @importFrom tidyr pivot_wider separate_wider_delim unnest
#' @export
nfsa_loaded <- function(country
                        time_min
                        time_max = lubridate::today(),
                        recursive = FALSE) {

  base_path <- file.path("I:/econ/NFSA/LOG")

  # Build country pattern for faster file filtering
  country_pattern <- paste0(".*_(", paste(country, collapse = "|"), ")_.*\\.txt$")

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
  all_files <- list.files(base_path, pattern = country_pattern,
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
    dplyr::group_by(file,file_date,field) |>
    dplyr::slice_tail() |>
    dplyr::ungroup() |>
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
      result = dplyr::recode_values(feed_rc,
                                    "0" ~ "OK", "2" ~ "Warning",
                                    "4" ~ "Rejection", "5" ~ "Error",
                                    default = feed_rc)
    )

  return(final_data)
}
