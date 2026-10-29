#' @title Get NFSA Data
#'
#' @description
#' Retrieves NFSA data from Parquet files, optionally filtering by country,
#' table, reference period, and data conditions.
#'
#' @details
#' Filenames follow a convention such as
#' `NASEC_T0801_Q_PL_2026_0002_V0002.parquet`.
#' Reference periods may be supplied as `"2026_0002"`, `"2026Q2"`, or
#' (for annual reference periods) `"2026"` / `"2026_0000"`.
#' They are normalized to the filename format before file matching.
#'
#' When `last = TRUE`, the highest numeric V-version is selected separately
#' for each country and reference period. When `last = FALSE`, all versions
#' are retrieved.
#'
#' The `version` column is included if `last = FALSE`, if `ref_period = NULL`,
#' or if more than one distinct reference period is supplied. It is omitted
#' only when `last = TRUE` and exactly one reference period is requested.
#' Keeping `version` does not automatically deduplicate observations.
#'
#' @param input_sel Character string specifying the Parquet data directory.
#' @param country Character vector specifying countries to retrieve.
#' @param table Character scalar. One of `"T0800"`, `"T0801"`, or `"T0801SA"`.
#' @param filters Optional character vector of filter expressions applied
#'   using Arrow before collecting the data.
#' @param complete Logical. If `FALSE`, return selected columns; if `TRUE`,
#'   return all columns after joining the lookup, subject to version removal.
#' @param ref_period Optional character vector of reference periods in
#'   `"YYYY_NNNN"`, `"YYYYQn"`, or `"YYYY"` format. `NULL` includes all
#'   reference periods.
#' @param last Logical. If `TRUE` (default), retrieve only the highest version
#'   for each country and reference period. Otherwise retrieve all versions.
#' @param fill_M Logical. If `TRUE` (default) and `complete = FALSE`, convert
#'   observations with `obs_status == "M"` to zero.
#'
#' @return A tibble containing selected NFSA observations, or `NULL` if no
#'   matching files are found.
#'
#' @examples
#' \dontrun{
#' nfsa_get_data(country = "PL")
#' nfsa_get_data(country = "PL", ref_period = "2026_0002")
#' nfsa_get_data(country = "PL", ref_period = "2026Q2")
#' nfsa_get_data(country = "PL", table = "T0800", ref_period = "2026")
#' nfsa_get_data(country = c("PL", "DE"),
#'               ref_period = c("2025Q1", "2025_0002"), last = FALSE)
#' }
#'
#' @importFrom arrow open_dataset
#' @importFrom cli cli_abort cli_alert_danger
#' @importFrom dplyr any_of arrange collect filter group_by if_else left_join mutate pull select slice_max ungroup
#' @importFrom rlang parse_exprs
#' @importFrom stringr str_match str_replace
#' @importFrom tibble tibble
#' @importFrom tidyr drop_na
#' @export
nfsa_get_data <- function(
    input_sel = "M:/nas/Rprod/db/",
    country,
    table = "T0801",
    filters = NULL,
    complete = FALSE,
    ref_period = NULL,
    last = TRUE,
    fill_M = TRUE
) {

  # Normalize user input to the YYYY_NNNN format used in filenames.
  normalize_ref_period <- function(x) {
    if (is.null(x)) return(NULL)
    if (!is.character(x) || anyNA(x) || !length(x)) {
      cli::cli_abort("`ref_period` must be NULL or a nonempty character vector without NA values.")
    }
    valid <- grepl("^[0-9]{4}_[0-9]{4}$|^[0-9]{4}Q[1-4]$|^[0-9]{4}$", x)
    if (!all(valid)) {
      cli::cli_abort("Invalid `ref_period`: {paste(x[!valid], collapse = ', ')}. Use YYYY_NNNN, YYYYQn, or YYYY.")
    }
    x <- stringr::str_replace(x, "^([0-9]{4})Q([1-4])$", "\\1_000\\2")
    x <- stringr::str_replace(x, "^([0-9]{4})$", "\\1_0000")
    unique(x)
  }

  ref_period <- normalize_ref_period(ref_period)

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
  if (!is.logical(fill_M) || length(fill_M) != 1L || is.na(fill_M)) {
    cli::cli_abort("`fill_M` must be TRUE or FALSE.")
  }

  keep_version <- !last || is.null(ref_period) || length(ref_period) != 1L
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

  # 3. Filter reference periods (both sides use YYYY_NNNN) ----------
  if (!is.null(ref_period)) {
    file_info <- file_info |>
      dplyr::filter(ref_period_file %in% ref_period)
  }

  # 4. Keep highest numeric version per country and reference period -
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
    if (fill_M) {
      res <- res |>
        dplyr::mutate(
          obs_value = dplyr::if_else(obs_status %in% "M", 0, obs_value)
        )
    }
    # Only filter on obs_value, not on other possibly empty columns.
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
