#' @title Get NFSA Data
#'
#' @description
#' Retrieves NFSA data from Parquet files, optionally filtering by country,
#' table, reference period, and data conditions.
#'
#' @details
#' Filenames follow a convention such as
#' `NASEC_T0801_Q_PL_2026_0002_V0002.parquet`.
#' When `last = TRUE`, the highest numeric V-version is selected separately
#' for each country and reference period. When `last = FALSE`, all versions
#' are retrieved.
#'
#' The `version` column is included if `last = FALSE`, if `ref_period = NULL`,
#' or if more than one distinct reference period is supplied. It is omitted
#' only when `last = TRUE` and exactly one reference period is requested.
#' Note that keeping `version` does not automatically deduplicate observations.
#'
#' @param input_sel Character string specifying the Parquet data directory.
#' @param country Character vector specifying countries to retrieve.
#' @param table Character scalar. One of "T0800", "T0801", or "T0801SA".
#' @param filters Optional character vector of filter expressions applied
#'   using Arrow before collecting the data.
#' @param complete Logical. If `FALSE`, returns selected columns; if `TRUE`,
#'   returns all columns after joining the lookup, subject to version removal.
#' @param ref_period Optional character vector of reference periods in the
#'   format "YYYY_NNNN". `NULL` includes all reference periods.
#' @param last Logical. If `TRUE` (default), retrieves only the highest
#'   version for each country and reference period. If `FALSE`, retrieves
#'   all available versions. The `version` column is retained whenever
#'   `last = FALSE`, `ref_period = NULL`, or multiple reference periods are
#'   requested. It is excluded only when `last = TRUE` and exactly one
#'   reference period is specified.
#'
#' @return A tibble containing the selected NFSA observations, or `NULL` if
#'   no matching files are found.
#'
#' @examples
#' \dontrun{
#' nfsa_get_data(country = "PL")
#' nfsa_get_data(country = "PL", ref_period = "2026_0002")
#' nfsa_get_data(country = "PL", ref_period = "2026_0002", last = FALSE)
#' nfsa_get_data(
#'   country = c("PL", "DE"),
#'   ref_period = c("2026_0001", "2026_0002"),
#'   last = TRUE
#' )
#' }
#'
#' @importFrom dplyr filter pull collect left_join select mutate arrange group_by slice_max ungroup any_of
#' @importFrom tibble tibble
#' @importFrom stringr str_match
#' @importFrom tidyr drop_na
#' @importFrom rlang parse_exprs
#' @export
nfsa_get_data <- function(
    input_sel = "M:/nas/Rprod/db/",
    country ,
    table = "T0801",
    filters = NULL,
    complete = FALSE,
    ref_period = NULL,
    last = TRUE
) {
  # 1. Setup configuration ------------------------------------------
  cfg_map <- list(
    "T0800" = list(pattern = "NASEC_T0800_A"),
    "T0801" = list(pattern = "NASEC_T0801_Q"),
    "T0801SA" = list(pattern = "NASEC_T0801SA_Q")
  )

  if (length(table) != 1L || is.na(table) || !table %in% names(cfg_map)) {
    cli::cli_abort("Invalid table. Choose T0800, T0801, or T0801SA.")
  }
  if (!is.logical(last) || length(last) != 1L || is.na(last)) {
    cli::cli_abort("`last` must be TRUE or FALSE.")
  }
  if (!is.logical(complete) || length(complete) != 1L || is.na(complete)) {
    cli::cli_abort("`complete` must be TRUE or FALSE.")
  }

  keep_version <- !last || is.null(ref_period) ||
    length(unique(ref_period)) != 1L
  cfg <- cfg_map[[table]]

  # 2. Discover files and extract filename metadata ------------------
  files <- list.files(
    path = input_sel,
    pattern = paste0("^", cfg$pattern, "_.*\\.parquet$"),
    recursive = TRUE,
    full.names = TRUE
  )

  filename_pattern <- paste0(
    "^", cfg$pattern,
    "_([A-Z]{2})_([0-9]{4}_[0-9]{4})_V([0-9]+)\\.parquet$"
  )
  matches <- stringr::str_match(basename(files), filename_pattern)

  file_info <- tibble::tibble(
    value = files,
    countries = matches[, 2],
    ref_period_file = matches[, 3],
    file_version = as.integer(matches[, 4])
  ) |>
    dplyr::filter(
      countries %in% country,
      !is.na(ref_period_file),
      !is.na(file_version)
    )

  # 3. Filter reference periods --------------------------------------
  if (!is.null(ref_period)) {
    file_info <- file_info |>
      dplyr::filter(ref_period_file %in% ref_period)
  }

  # 4. Keep the highest version per country and reference period -----
  if (last) {
    file_info <- file_info |>
      dplyr::group_by(countries, ref_period_file) |>
      dplyr::slice_max(order_by = file_version, n = 1, with_ties = FALSE) |>
      dplyr::ungroup()
  }

  target_files <- dplyr::pull(file_info, value)
  if (length(target_files) == 0L) {
    period_label <- if (is.null(ref_period)) "ALL" else
      paste(ref_period, collapse = ", ")
    cli::cli_alert_danger(
      "No files found for table {table}, country {paste(country, collapse = ', ')} and reference period {period_label}"
    )
    return(NULL)
  }

  # 5. Open dataset and push filters down ----------------------------
  lookup <- nfsa::nfsa_sto_lookup
  ds <- arrow::open_dataset(target_files)
  if (!is.null(filters)) {
    parsed_filters <- rlang::parse_exprs(filters)
    ds <- dplyr::filter(ds, !!!parsed_filters)
  }
  res <- dplyr::collect(ds)

  # 6. Join lookup and normalize country codes -----------------------
  res <- res |>
    dplyr::left_join(
      lookup,
      by = c(
        "counterpart_area", "ref_sector", "counterpart_sector",
        "consolidation", "accounting_entry", "sto",
        "instr_asset", "unit_measure", "prices"
      )
    ) |>
    dplyr::mutate(
      ref_area = dplyr::if_else(ref_area == "GR", "EL", ref_area)
    )

  # 7. Format output -------------------------------------------------
  if (!complete) {
    res <- res |>
      tidyr::drop_na(obs_value)
    if (keep_version) {
      res <- dplyr::select(res, ref_area, version, id, time_period, obs_value)
    } else {
      res <- dplyr::select(res, ref_area, id, time_period, obs_value)
    }
  } else {
    res <- dplyr::arrange(res, time_period, ref_area, id)
    if (!keep_version) {
      res <- dplyr::select(res, -dplyr::any_of("version"))
    }
  }

  return(res)
}

