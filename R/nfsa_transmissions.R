#' @title Generate NFSA Transmission Report
#'
#' @description This function generates a report of NFSA data transmissions received, filtering by a minimum timestamp and outputting the results to an Excel file.
#'
#' @param time_min A character string representing the minimum date and time for filtering received files (e.g., "2025-10-20").  Files received before this time will be excluded.
#' @param time_max A character string representing the maximum date and time for filtering received files, defaults to current date).  Files received after this time will be excluded.
#' @param output_sel A character string specifying the directory where the output Excel file should be saved. Defaults to `"output/logs"`.
#' @param recursive A logical value. If `TRUE`, the function will recursively search for files in subdirectories within the specified path. Defaults to `FALSE`.
#'
#' @return This function does not return a value, instead outputs an Excel file with the transmission report.
#'
#'
#' @examples
#' \dontrun{
#' # Generate a transmission report for files received since "2025-11-01" and save to default location
#' nfsa_transmissions(time_min = "2025-11-01")
#'
#' # Generate a report, save to alternative location and search in subdirectories
#' nfsa_transmissions(time_min = "2025-10-25", output_sel = "my_output_folder", recursive = TRUE)
#' }
#' @export
nfsa_transmissions <- function(time_min,
                               time_max = lubridate::now(),
                               input_sel = "M:/nas/incoming_SDMX_files/",
                               output_sel = here::here("output", "logs"),
                               recursive = FALSE) {

  # Ensure output directory exists
  if (!dir.exists(output_sel)) dir.create(output_sel, recursive = TRUE)

  cli::cli_inform("Scanning server for NASEC files...")

  # 1. Get files and metadata together
  files <- list.files(path = input_sel, pattern = "NASEC_",
                      full.names = TRUE, recursive = recursive)

  if (length(files) == 0) {
    cli::cli_alert_warning("No NASEC files found in the source folder.")
    return(NULL)
  }

  # 2. Process Metadata
  received_df <- tibble::tibble(full_path = files) |>
    # Get modification time once for all files
    dplyr::mutate(received = lubridate::as_datetime(file.mtime(full_path))) |>
    # Filter by date BEFORE parsing strings to save memory
    dplyr::filter(received >= time_min, received <= time_max)

  if (nrow(received_df) == 0) {
    cli::cli_alert_info("No files found within the specified time range.")
    return(NULL)
  }

  cli::cli_inform("Processing information for {nrow(received_df)} files...")

  # 3. Parse Filenames
  received_df <- received_df |>
    dplyr::mutate(file_name = basename(full_path)) |>
    tidyr::separate_wider_delim(
      cols = file_name,
      delim = "_",
      names = c("nasec", "table", "freq", "ref_area", "year", "quarter", "version"),
      cols_remove = FALSE
    ) |>
    dplyr::mutate(
      version = stringr::str_remove(version, "\\.xml$"),
      # Clean logic for Period (Annual vs Quarterly)
      q_num = stringr::str_extract(quarter, "\\d+"),
      period = dplyr::if_else(q_num == "0",
                              as.character(year),
                              paste0(year, "-Q", q_num))
    ) |>
    dplyr::select(ref_area, file = file_name, table, period, version, received) |>
    dplyr::arrange(desc(received))

  # 4. Export to Excel
  ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
  file_out <- file.path(output_sel, paste0("received_", ts, ".xlsx"))

  openxlsx::write.xlsx(received_df, file = file_out, overwrite = TRUE, asTable = TRUE)

  cli::cli_alert_success("Transmission log created: {.file {file_out}}")
  nfsa::nfsa_to_excel(received_df)
  return(received_df)
}


