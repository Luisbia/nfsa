#' Check Completeness of NFSA Data
#'
#' @description
#' Checks whether NFSA data fulfil regulatory or aggregation
#' completeness requirements defined in external Excel files.
#'
#' @details
#' Two completeness checks are supported:
#'
#' - `regulation`: T0801SA regulatory requirements.
#' - `aggregation`: T0801 aggregation requirements.
#'
#' Regulatory requirements:
#' - Small countries require series marked "ALL".
#' - Big countries require all series in the requirements file.
#'
#' Aggregation requirements:
#' - Requirements depend on the smallEA, bigEA and bigEU
#'   indicators in the Excel file.
#'
#' A required observation is considered missing if its
#' country-series-quarter combination is absent from the
#' input data or its `obs_value` is NA.
#'
#' Zero values are considered valid observations.
#'
#' Duplicate observations are consolidated: a quarter is
#' considered available if at least one non-missing
#' observation exists.
#'
#' @param df A data frame containing `ref_area`, `id`,
#'   `time_period` and `obs_value`.
#' @param type Completeness check: "regulation" or "aggregation".
#' @param requirements Path to the Excel requirements file.
#'   If NULL, the default file for the selected test is used.
#' @param time_min First quarter to check, in YYYY-Qn format.
#'   Defaults to "1999-Q1".
#' @param time_max Last quarter to check, in YYYY-Qn format.
#'   If NULL, the latest quarter in `df` is used.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{ref_area}{Reporting country.}
#'   \item{missing_series}{Identifier of the missing series.}
#'   \item{from}{First missing quarter.}
#'   \item{to}{Last missing quarter.}
#'   \item{n_missing}{Number of missing quarters.}
#' }
#' Returns an empty tibble with the same columns if all
#' requirements are fulfilled.
#'
#' @importFrom readxl read_xlsx
#' @importFrom dplyr filter select mutate distinct
#'   bind_rows left_join group_by summarise arrange
#' @importFrom tidyr crossing
#' @importFrom tibble tibble
#' @importFrom cli cli_abort
#' @export
#'
#' @examples
#' \dontrun{
#' nfsa_completeness(
#'   df = dat,
#'   type = "regulation",
#'   time_min = "2020-Q1"
#' )
#'
#' nfsa_completeness_test(
#'   df = dat,
#'   type = "aggregation",
#'   time_min = "2020-Q1",
#'   time_max = "2026-Q2"
#' )
#' }
nfsa_completeness <- function(
    df,
    type = c("regulation", "aggregation"),
    requirements = NULL,
    time_min = "1999-Q1",
    time_max = NULL
) {

  type <- match.arg(type)

  # -------------------------------------------------------
  # 1. Validate input
  # -------------------------------------------------------

  required_cols <- c(
    "ref_area", "id", "time_period", "obs_value"
  )

  missing_cols <- setdiff(required_cols, names(df))

  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "Missing columns: {paste(missing_cols, collapse = ', ')}"
    )
  }

  # -------------------------------------------------------
  # 2. Select requirements file
  # -------------------------------------------------------

  if (is.null(requirements)) {
    requirements <- switch(
      type,
      regulation =
        "M:/nas/Rprod/assets/completeness_T0801SA.xlsx",
      aggregation =
        "M:/nas/Rprod/assets/completeness_aggregation.xlsx"
    )
  }

  if (!file.exists(requirements)) {
    cli::cli_abort(
      "Requirements file not found: {.file {requirements}}"
    )
  }

  req <- readxl::read_xlsx(requirements)

  # -------------------------------------------------------
  # 3. Define country groups
  # -------------------------------------------------------

  small <- c(
    "BG", "EE", "HR", "CY", "LT",
    "LV", "LU", "MT", "SI", "SK"
  )

  bigEA <- c(
    "AT", "BE", "DE", "EL", "ES", "FI",
    "FR", "IE", "IT", "NL", "PT"
  )

  bigEU <- c(
    "CZ", "DK", "HU", "PL", "RO", "SE"
  )

  countries <- sort(unique(
    stats::na.omit(as.character(df$ref_area))
  ))

  # -------------------------------------------------------
  # 4. Generate quarterly periods
  # -------------------------------------------------------

  quarter_index <- function(x) {

    if (anyNA(x) ||
        any(!grepl("^[0-9]{4}-Q[1-4]$", x))) {
      cli::cli_abort(
        "Invalid quarter format. Expected YYYY-Qn."
      )
    }

    as.integer(substr(x, 1, 4)) * 4L +
      as.integer(substr(x, 7, 7)) - 1L
  }

  if (is.null(time_max)) {

    available_time <- df$time_period[
      !is.na(df$time_period)
    ]

    if (length(available_time) == 0L) {
      cli::cli_abort(
        "Cannot determine time_max from df."
      )
    }

    time_max <- max(available_time)
  }

  if (length(time_min) != 1L ||
      length(time_max) != 1L) {
    cli::cli_abort(
      "time_min and time_max must be single quarters."
    )
  }

  start_index <- quarter_index(time_min)
  end_index <- quarter_index(time_max)

  if (start_index > end_index) {
    cli::cli_abort(
      "time_min must not be later than time_max."
    )
  }

  indices <- seq.int(start_index, end_index)

  quarters <- tibble::tibble(
    time_period = sprintf(
      "%04d-Q%d",
      indices %/% 4L,
      indices %% 4L + 1L
    )
  )

  # -------------------------------------------------------
  # 5. Build requirements by country
  # -------------------------------------------------------

  if (type == "regulation") {

    expected_cols <- c("id", "countries")

    if (!all(expected_cols %in% names(req))) {
      cli::cli_abort(
        "Regulation requirements must contain id and countries."
      )
    }

    req_small <- tidyr::crossing(
      ref_area = intersect(countries, small),
      id = req$id[!is.na(req$countries) &
                    req$countries == "ALL"]
    )

    req_big <- tidyr::crossing(
      ref_area = intersect(countries, c(bigEA, bigEU)),
      id = unique(req$id)
    )

    req_countries <- dplyr::bind_rows(
      req_small,
      req_big
    )

  } else {

    expected_cols <- c(
      "id", "smallEA", "bigEA", "bigEU"
    )

    if (!all(expected_cols %in% names(req))) {
      cli::cli_abort(
        paste(
          "Aggregation requirements must contain",
          "id, smallEA, bigEA and bigEU."
        )
      )
    }

    is_required <- function(x) {
      !is.na(x) &
        toupper(trimws(as.character(x))) %in%
        c("TRUE", "1")
    }

    req_small <- tidyr::crossing(
      ref_area = intersect(countries, small),
      id = req$id[is_required(req$smallEA)]
    )

    req_bigEA <- tidyr::crossing(
      ref_area = intersect(countries, bigEA),
      id = req$id[is_required(req$bigEA)]
    )

    req_bigEU <- tidyr::crossing(
      ref_area = intersect(countries, bigEU),
      id = req$id[is_required(req$bigEU)]
    )

    req_countries <- dplyr::bind_rows(
      req_small,
      req_bigEA,
      req_bigEU
    )
  }

  # Expand country-series requirements across quarters
  req_all <- req_countries |>
    dplyr::distinct() |>
    tidyr::crossing(quarters)

  # -------------------------------------------------------
  # 6. Prepare observations
  # -------------------------------------------------------

  observed <- df |>
    dplyr::select(
      ref_area, id, time_period, obs_value
    ) |>
    nfsa::nfsa_separate_id() |>
    dplyr::filter(
      ref_sector %in% c(
        "S1", "S1N", "S11", "S12",
        "S13", "S1M", "S2"
      ),
      time_period >= time_min,
      time_period <= time_max
    ) |>
    nfsa::nfsa_unite_id() |>
    dplyr::select(
      ref_area, id, time_period, obs_value
    )

  # -------------------------------------------------------
  # 7. Consolidate duplicate observations
  # -------------------------------------------------------

  observed <- observed |>
    dplyr::group_by(ref_area, id, time_period) |>
    dplyr::summarise(
      available = any(!is.na(obs_value)),
      .groups = "drop"
    )

  # -------------------------------------------------------
  # 8. Identify missing observations
  # -------------------------------------------------------

  missing <- req_all |>
    dplyr::left_join(
      observed,
      by = c("ref_area", "id", "time_period")
    ) |>
    dplyr::filter(
      is.na(available) | !available
    )

  # -------------------------------------------------------
  # 9. Summarise missing series
  # -------------------------------------------------------

  result <- missing |>
    dplyr::group_by(ref_area, id) |>
    dplyr::summarise(
      from = min(time_period),
      to = max(time_period),
      n_missing = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::rename(
      missing_series = id
    ) |>
    dplyr::select(
      ref_area,
      missing_series,
      from,
      to,
      n_missing
    ) |>
    dplyr::arrange(
      ref_area,
      missing_series
    )

  return(result)
}

