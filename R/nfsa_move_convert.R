#' Convert SDMX data to Parquet format for NASEC data tables
#'
#' This function reads SDMX files, converts them to Parquet format, and saves them to a specified directory.
#' It handles T0800 (annual), T0801 (quarterly NSA), and T0801SA (quarterly seasonally adjusted) tables.
#'
#' @param country A character string specifying the country code.
#' @param table A character string specifying the table type. Must be one of "T0800", "T0801", or "T0801SA".
#' @param period A character string specifying the time period:
#'   - For T0800: year in format "2024_0000"
#'   - For T0801/T0801SA: year and quarter in format "2025_0002" (for 2025Q2)
#' @param input_sel A character string specifying the input directory where the SDMX files are located.
#'   If NULL, uses default path based on table type. Defaults to NULL.
#' @param output_sel A character string specifying the output directory where the Parquet files should be saved.
#'   If NULL, uses default path based on table type. Defaults to NULL.
#' @param process_all Logical. If TRUE, processes all matching files (ASA/QSA workflow). If FALSE, only
#'   processes new files compared to existing output. Defaults to FALSE.
#'
#' @return This function primarily operates for side effects (writing Parquet files). It prints messages
#'   to the console indicating whether a new file was processed, if no file was found, or if no new file exists.
#'
#' @details The function searches for SDMX files matching the pattern based on the table type.
#'   It identifies the file(s) with the latest version number. If process_all is FALSE, it compares with
#'   existing files in the output directory and only processes new files. The SDMX data is read, cleaned,
#'   timestamped, and converted to Parquet format.
#'
#' @examples
#' \dontrun{
#' # Convert T0800 annual data
#' nfsa_move_convert(country = "BE", table = "T0800", period = "2024_0000")
#'
#' # Convert T0801 quarterly data
#' nfsa_move_convert(country = "BE", table = "T0801", period = "2025_0002")
#'
#' # Convert T0801SA with custom paths
#' nfsa_move_convert(
#'   country = "IT",
#'   table = "T0801SA",
#'   period = "2025_0002",
#'   input_sel = "path/to/input",
#'   output_sel = "path/to/output"
#' )
#'
#' # Process all files (ASA/QSA workflow)
#' nfsa_move_convert(country = "BE", table = "T0800", process_all = TRUE)
#' }
#'
#' @export
nfsa_move_convert <- function(country,
                               table = c("T0800", "T0801", "T0801SA"),
                               period = NULL,
                               input_sel = NULL,
                               output_sel = NULL,
                               process_all = FALSE) {

  table <- match.arg(table)

  # Table-specific configuration
  table_config <- list(
    "T0800" = list(
      freq_code = "A",
      pattern_suffix = paste0("NASEC_T0800_A_", country),
      version_regex = "(?<=_A_..............).{4}",
      version_regex_extract = "(?<=_A_...).{15}",
      filename_length = 35,
      default_input = "M:/nas/Incoming_SDMX_files/",
      default_output = "M:/nas/Rprod/data/a/new/"
    ),
    "T0801" = list(
      freq_code = "Q",
      pattern_suffix = paste0("NASEC_T0801_Q_", country),
      version_regex = "(?<=_Q_..............).{4}",
      version_regex_extract = "(?<=_Q_...).{15}",
      filename_length = 35,
      default_input = "M:/nas/Incoming_SDMX_files/",
      default_output = "M:/nas/Rprod/data/q/new/nsa/"
    ),
    "T0801SA" = list(
      freq_code = "Q",
      pattern_suffix = paste0("NASEC_T0801SA_Q_", country),
      version_regex = "(?<=_Q_..............).{4}",
      version_regex_extract = "(?<=_Q_...).{15}",
      filename_length = 37,
      default_input = "M:/nas/Incoming_SDMX_files/",
      default_output = "M:/nas/Rprod/data/q/new/sca/"
    )
  )

  config <- table_config[[table]]

  # Use default paths if not specified
  if (is.null(input_sel)) input_sel <- config$default_input
  if (is.null(output_sel)) output_sel <- config$default_output

  # Build file pattern
  if (!is.null(period)) {
    file_pattern <- paste0(config$pattern_suffix, "_", period)
  } else {
    file_pattern <- config$pattern_suffix
  }

  cli::cli_inform("Looking for files in the server...")

  # Find files matching pattern
  files <- list.files(
    path = input_sel,
    pattern = file_pattern,
    full.names = TRUE
  )

  if (length(files) == 0) {
    cli::cli_alert_warning("No {table} file for {country}")
    return(invisible(NULL))
  }

  # Extract versions and get latest
  files_df <- dplyr::as_tibble(value = files) |>
    dplyr::mutate(version = as.numeric(stringr::str_extract(value, config$version_regex)))

  if (process_all) {
    # Process all files (ASA/QSA workflow)
    files_to_process <- files_df |> dplyr::pull(value)
  } else {
    # Get only the latest version file
    files_to_process <- files_df |>
      dplyr::filter(version == max(version)) |>
      dplyr::pull(value)
  }

  # Helper function to convert a single file
  convert_file <- function(file, config, output_sel) {
    file_name <- stringr::str_sub(file, nchar(file) - config$filename_length, nchar(file))
    file_name <- stringr::str_replace(file_name, ".xml", ".parquet")

    dat <- file |>
      readsdmx::read_sdmx() |>
      janitor::clean_names() |>
      dplyr::mutate(
        received = file.mtime(file),
        version = stringr::str_extract(file, config$version_regex_extract)
      )

    # Handle embargo_date presence/absence
    if ("embargo_date" %in% names(dat)) {
      dat <- dat |>
        dplyr::select(
          received, version, table_identifier, freq, adjustment, ref_area, counterpart_area,
          ref_sector, counterpart_sector, consolidation, accounting_entry, sto,
          instr_asset, maturity, expenditure, unit_measure, currency_denom,
          valuation, prices, transformation, cust_breakdown, time_period,
          obs_value, obs_status, conf_status, embargo_date
        ) |>
        dplyr::mutate(
          obs_value = as.numeric(obs_value),
          embargo_date = as.character(embargo_date)
        )
    } else {
      dat <- dat |>
        dplyr::select(
          received, version, table_identifier, freq, adjustment, ref_area, counterpart_area,
          ref_sector, counterpart_sector, consolidation, accounting_entry, sto,
          instr_asset, maturity, expenditure, unit_measure, currency_denom,
          valuation, prices, transformation, cust_breakdown, time_period,
          obs_value, obs_status, conf_status
        ) |>
        dplyr::mutate(
          obs_value = as.numeric(obs_value),
          embargo_date = "NA"
        )
    }

    arrow::write_parquet(dat, paste0(output_sel, "/", file_name))
    cli::cli_alert_success("Converted {file_name}")
  }

  if (!process_all && length(files_to_process) == 1) {
    # Check if file already exists (vintage comparison workflow)
    files_have <- list.files(
      path = output_sel,
      pattern = file_pattern,
      recursive = FALSE
    )

    if (length(files_have) == 0) {
      files_have <- "no file"
    } else {
      files_have_df <- dplyr::as_tibble(value = files_have) |>
        dplyr::mutate(version = as.numeric(stringr::str_extract(value, config$version_regex))) |>
        dplyr::filter(version == max(version)) |>
        dplyr::mutate(
          value = stringr::str_replace_all(value, ".parquet", ".xml"),
          value = paste0(input_sel, value)
        ) |>
        dplyr::pull(value)
      files_have <- files_have_df[1]
    }

    if (files_to_process[1] == files_have) {
      cli::cli_alert_info("No new {table} file for {country}")
      return(invisible(NULL))
    }
  }

  # Convert files
  cli::cli_inform("Converting files...")

  if (process_all) {
    purrr::walk(files_to_process, ~ convert_file(.x, config, output_sel))
  } else {
    purrr::walk(files_to_process, ~ convert_file(.x, config, output_sel))
  }

  invisible(NULL)
}
