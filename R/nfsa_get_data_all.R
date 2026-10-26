
#' @title Get NFSA Data (All Versions)
#'
#' @description
#' Retrieves NFSA data from Parquet files, optionally filtering
#' by country, table, reference period, and data conditions.
#'
#' @details
#' Files are selected based on the naming convention:
#' `NASEC_T0801_Q_PL_2026_0002_V0002.parquet`.
#'
#' The reference period is extracted from the filename.
#' When `ref_period` is NULL, all reference periods are included.
#' All available versions are retained.
#'
#' @param input_sel Character string specifying the data directory.
#' @param country Character vector specifying countries to retrieve.
#' @param table Character string specifying the table.
#'   One of "T0800", "T0801", or "T0801SA".
#' @param filters Optional character vector of filter expressions
#'   applied using Arrow before collecting data.
#' @param complete Logical. If FALSE, returns selected columns.
#'   If TRUE, returns all available columns after joining the lookup.
#' @param ref_period Optional character vector of reference periods,
#'   formatted as "YYYY_NNNN", e.g. "2026_0002".
#'   Default NULL includes all reference periods.
#'
#' @return A tibble containing the selected NFSA data,
#'   or NULL when no matching files are found.
#'
#' @examples
#' \dontrun{
#' nfsa_get_data_all(country = "PL")
#'
#' nfsa_get_data_all(
#'   country = "PL",
#'   ref_period = "2026_0002"
#' )
#'
#' nfsa_get_data_all(
#'   country = c("PL", "DE"),
#'   ref_period = c("2026_0001", "2026_0002"),
#'   filters = "ref_sector == 'S13'"
#' )
#' }
#'
#' @importFrom dplyr filter pull collect left_join select mutate arrange
#' @importFrom tibble as_tibble
#' @importFrom stringr str_match
#' @importFrom tidyr drop_na
#' @importFrom rlang parse_exprs
#' @export
nfsa_get_data_all <- function(
    input_sel = "M:/nas/Rprod/db/",
    country,
    table = "T0801",
    filters = NULL,
    complete = FALSE,
    ref_period = NULL
) {

  # 1. Setup Configuration ----------------------------------------
  cfg_map <- list(
    "T0800"   = list(freq_code = "_A_", pattern = "NASEC_T0800_A"),
    "T0801"   = list(freq_code = "_Q_", pattern = "NASEC_T0801_Q"),
    "T0801SA" = list(freq_code = "_Q_", pattern = "NASEC_T0801SA_Q")
  )

  if (!table %in% names(cfg_map)) {
    cli::cli_abort(
      "Invalid table: {table}. Choose T0800, T0801, or T0801SA."
    )
  }

  cfg <- cfg_map[[table]]

  # 2. File Discovery ---------------------------------------------
  files <- list.files(
    path = input_sel,
    pattern = paste0("^", cfg$pattern, "_.*\\.parquet$"),
    recursive = TRUE,
    full.names = TRUE
  )

  # Extract country, reference period and version from filenames
  file_info <- tibble::tibble(
    value = files
  ) |>
    dplyr::mutate(
      file_name = basename(value),
      countries = stringr::str_match(
        file_name,
        paste0("^", cfg$pattern, "_([A-Z]{2})_")
      )[, 2],
      ref_period_file = stringr::str_match(
        file_name,
        paste0(
          "^", cfg$pattern,
          "_[A-Z]{2}_([0-9]{4}_[0-9]{4})_V[0-9]+\\.parquet$"
        )
      )[, 2]
    ) |>
    dplyr::filter(
      countries %in% country,
      !is.na(ref_period_file)
    )

  # 3. Optional Reference Period Filter ---------------------------
  if (!is.null(ref_period)) {
    file_info <- file_info |>
      dplyr::filter(ref_period_file %in% ref_period)
  }

  target_files <- file_info |>
    dplyr::pull(value)

  if (length(target_files) == 0) {
    cli::cli_alert_danger(
      "No files found for table {table}, country {paste(country, collapse = ', ')} and reference period {paste(ref_period %||% 'ALL', collapse = ', ')}"
    )
    return(NULL)
  }

  # 4. Data Processing Pipeline -----------------------------------
  lookup <- nfsa::nfsa_sto_lookup

  ds <- arrow::open_dataset(target_files)

  # Apply Arrow filters (push-down)
  if (!is.null(filters)) {
    parsed_filters <- rlang::parse_exprs(filters)
    ds <- ds |>
      dplyr::filter(!!!parsed_filters)
  }

  res <- ds |>
    dplyr::collect()

  # 5. Join Lookup and Format Output -------------------------------
  if (complete == FALSE) {
    res <- res |>
      dplyr::left_join(
        lookup,
        by = c(
          "counterpart_area",
          "ref_sector",
          "counterpart_sector",
          "consolidation",
          "accounting_entry",
          "sto",
          "instr_asset",
          "unit_measure",
          "prices"
        )
      ) |>
      tidyr::drop_na() |>
      dplyr::select(
        ref_area, version, id, time_period, obs_value
      ) |>
      dplyr::mutate(
        ref_area = dplyr::if_else(
          ref_area == "GR", "EL", ref_area
        )
      )
  }

  if (complete == TRUE) {
    res <- res |>
      dplyr::left_join(
        lookup,
        by = c(
          "counterpart_area",
          "ref_sector",
          "counterpart_sector",
          "consolidation",
          "accounting_entry",
          "sto",
          "instr_asset",
          "unit_measure",
          "prices"
        )
      ) |>
      dplyr::arrange(time_period, ref_area, id) |>
      dplyr::mutate(
        ref_area = dplyr::if_else(
          ref_area == "GR", "EL", ref_area
        )
      )
  }

  return(res)
}

