#' Convert SDMX data to Parquet format for NASEC data tables
#'
#' This function reads SDMX files, converts them to Parquet format, and saves them to a specified directory.
#' It handles T0800 (annual), T0801 (quarterly NSA), and T0801SA (quarterly seasonally adjusted) tables.
#'
#' @param country A character string specifying the country code.
#' @param table A character string specifying the table type. Must be one of "T0800", "T0801", or "T0801SA".
#' @param period Optional character vector of reference periods. Accepts
#'   `YYYY_0000` or `YYYY` for annual tables, and `YYYY_0001` through
#'   `YYYY_0004` or `YYYYQ1` through `YYYYQ4` for quarterly tables.
#'   `NULL` searches all reference periods. Formats can be mixed.
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
#'   With `process_all = FALSE`, the highest V-version is selected for
#'   each matching reference period, and files already present in the output
#'   directory are skipped. With `process_all = TRUE`, all matching versions
#'   are processed (and existing output files may be overwritten). The SDMX data is read, cleaned,
#'   timestamped, and converted to Parquet format.
#'
#' @examples
#' \dontrun{
#' # Convert T0800 annual data
#' nfsa_move_convert(country = "BE", table = "T0800", period = "2024")
#'
#' # Convert T0801 quarterly data
#' nfsa_move_convert(country = "BE", table = "T0801", period = "2025Q2")
#'
#' # Convert T0801SA with custom paths
#' nfsa_move_convert(
#'   country = "IT",
#'   table = "T0801SA",
#'   period = "2025Q2",
#'   input_sel = "path/to/input",
#'   output_sel = "path/to/output"
#' )
#'
#' # Multiple reference periods, including the original format
#' nfsa_move_convert(country = "BE", table = "T0801",
#'                   period = c("2025Q1", "2025_0002"))
#'
#' # Process all files (ASA/QSA workflow)
#' nfsa_move_convert(country = "BE", table = "T0800", process_all = TRUE)
#' }
#'
#' @importFrom arrow write_parquet
#' @importFrom cli cli_abort cli_alert_info cli_alert_success cli_alert_warning cli_inform
#' @importFrom dplyr filter group_by mutate select slice_max ungroup
#' @importFrom janitor clean_names
#' @importFrom purrr walk
#' @importFrom readsdmx read_sdmx
#' @importFrom stringr str_extract str_match str_replace
#' @importFrom tibble tibble
#' @importFrom tools file_path_sans_ext
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
      default_output = "M:/nas/Rprod/db/a/"
    ),
    "T0801" = list(
      freq_code = "Q",
      pattern_suffix = paste0("NASEC_T0801_Q_", country),
      version_regex = "(?<=_Q_..............).{4}",
      version_regex_extract = "(?<=_Q_...).{15}",
      filename_length = 35,
      default_input = "M:/nas/Incoming_SDMX_files/",
      default_output = "M:/nas/Rprod/db/q/nsa/"
    ),
    "T0801SA" = list(
      freq_code = "Q",
      pattern_suffix = paste0("NASEC_T0801SA_Q_", country),
      version_regex = "(?<=_Q_..............).{4}",
      version_regex_extract = "(?<=_Q_...).{15}",
      filename_length = 37,
      default_input = "M:/nas/Incoming_SDMX_files/",
      default_output = "M:/nas/Rprod/db/q/sca/"
    )
  )

  config <- table_config[[table]]

  # Use default paths if not specified
  if (is.null(input_sel)) input_sel <- config$default_input
  if (is.null(output_sel)) output_sel <- config$default_output

  # Accept both human-readable and original filename reference periods.
  # Keep YYYY_NNNN internally because this is what appears in XML filenames.
  if (!is.null(period)) {
    if (!is.character(period) || !length(period) ||
        anyNA(period) || any(!nzchar(period))) {
      cli::cli_abort("`period` must be NULL or a non-empty character vector.")
    }

    period <- period |>
      stringr::str_replace("^([0-9]{4})Q([1-4])$", "\\1_000\\2") |>
      stringr::str_replace("^([0-9]{4})$", "\\1_0000")

    valid_format <- grepl("^[0-9]{4}_000[0-4]$", period)
    valid_frequency <- if (table == "T0800") {
      grepl("_0000$", period)
    } else {
      grepl("_000[1-4]$", period)
    }
    if (any(!valid_format | !valid_frequency)) {
      cli::cli_abort(
        "Invalid `period` for {table}. Use YYYY or YYYY_0000 for T0800, or YYYYQ1-YYYYQ4 or YYYY_0001-YYYY_0004 for quarterly tables."
      )
    }
    period <- unique(period)
  }

  cli::cli_inform("Looking for files in the server...")

  # Match XML filenames exactly and extract their reference period and V-version.
  filename_pattern <- paste0(
    "^", config$pattern_suffix,
    "_([0-9]{4}_000[0-4])_V([0-9]+)\\.xml$"
  )
  files <- list.files(
    path = input_sel,
    pattern = paste0("^", config$pattern_suffix, "_.*\\.xml$"),
    full.names = TRUE
  )

  matches <- stringr::str_match(basename(files), filename_pattern)
  files_df <- tibble::tibble(
    value = files,
    ref_period_file = matches[, 2],
    version_number = as.integer(matches[, 3])
  ) |>
    dplyr::filter(!is.na(ref_period_file), !is.na(version_number))

  if (!is.null(period)) {
    files_df <- files_df |>
      dplyr::filter(ref_period_file %in% period)
  }

  if (nrow(files_df) == 0L) {
    cli::cli_alert_warning("No {table} file for {country} and requested period(s).")
    return(invisible(NULL))
  }

  if (!process_all) {
    # Choose the latest version independently for each reference period.
    files_df <- files_df |>
      dplyr::group_by(ref_period_file) |>
      dplyr::slice_max(version_number, n = 1, with_ties = FALSE) |>
      dplyr::ungroup()
  }

  files_to_process <- files_df$value

  # Helper function to convert a single file
  convert_file <- function(file, config, output_sel) {
    file_name <- paste0(tools::file_path_sans_ext(basename(file)), ".parquet")

    dat <- file |>
      readsdmx::read_sdmx() |>
      janitor::clean_names() |>
      dplyr::mutate(
        received = as.character(file.mtime(file)),
        version = stringr::str_extract(
          basename(file), "[0-9]{4}_000[0-4]_V[0-9]+(?=\\.xml$)"
        )
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
          embargo_date = "None"
        )
    }

    arrow::write_parquet(dat, paste0(output_sel, "/", file_name))
    cli::cli_alert_success("Converted {file_name}")
  }

  if (!process_all) {
    # Skip a version only if the corresponding Parquet file already exists.
    # This also works when multiple reference periods were requested.
    output_names <- paste0(
      tools::file_path_sans_ext(basename(files_to_process)), ".parquet"
    )
    already_converted <- file.exists(file.path(output_sel, output_names))
    if (any(already_converted)) {
      cli::cli_alert_info("Skipping {sum(already_converted)} existing Parquet file(s).")
    }
    files_to_process <- files_to_process[!already_converted]
  }

  if (length(files_to_process) == 0L) {
    cli::cli_alert_info("No new {table} file for {country}")
    return(invisible(NULL))
  }

  if (!dir.exists(output_sel)) dir.create(output_sel, recursive = TRUE)

  # Convert files
  cli::cli_inform("Converting files...")
  purrr::walk(files_to_process, ~ convert_file(.x, config, output_sel))

  invisible(NULL)
}
