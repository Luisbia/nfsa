#' Detect and Report Revisions in NFSA Data Between Versions
#'
#' This function compares new and previous versions of NFSA data (T0800, T0801, or T0801SA),
#' identifies revisions based on absolute and relative thresholds, and exports the revisions
#' to an Excel file. It utilizes `arrow` for efficient data handling and `openxlsx` for Excel output.
#'
#' @param data A data frame with the columns `ref_area`, `version`, `id`,`time_period` and `obs_value`.
#' @param abs_threshold A numeric value specifying the absolute threshold for revision detection.
#'   Revisions with an absolute difference greater than this threshold are flagged.
#'   Default is 100
#' @param rel_threshold A numeric value specifying the relative threshold for revision detection.
#' Revisions with a relative difference greater than this threshold are flagged. Defaults to 5%.
#' @param gdp_threshold A numeric value specifying the gdp threshold (percentage of GDP) for
#'   revision detection. Revisions with a relative difference greater than this threshold are flagged.
#'   Defaults to 1%.
#' @return A data frame containing the identified revisions with columns for reference area, sector,
#'   STO, accounting entry, time period, new value, previous value, revision, revision percentage,
#'   and revision as percentage of GDP. If no revisions are found, returns an empty data frame.
#'
#' @examples
#' \dontrun{
#' nfsa_revision(data = dat)
#' nfsa_revision(data = dat, abs_threshold = 20, rel_threshold = 0.5)
#' }
#' @importFrom cli cli_alert_success
#' @importFrom dplyr arrange filter group_by join_by lag left_join mutate select ungroup
#' @export
nfsa_revision <- function(data = data,
                          abs_threshold = 100,
                          rel_threshold = 5,
                          gdp_threshold = 1) {

  # Calculate revisions
  data_rev <- data |>
    dplyr::group_by(ref_area,id,time_period) |>
    dplyr::arrange(version, .by_group = TRUE) |>
    dplyr::mutate(revision = obs_value - dplyr::lag(obs_value)) |>
    dplyr::mutate(`In %` = round(revision * 100 / dplyr::lag(obs_value), 1)) |>
    dplyr::ungroup()

  # Calculate GDP for relative comparisons
  tmp <- data |>
    dplyr::filter(id == "S1.B1GQ.B") |>
    dplyr::select(ref_area,version, time_period, GDP = obs_value)

  data_rev <- dplyr::left_join(data_rev, tmp,
                                  by = dplyr::join_by(ref_area,version,time_period)) |>
    dplyr::mutate(`as % GDP` = round(revision * 100 / GDP, 2)) |>
    dplyr::select(-GDP) |>
    dplyr::filter(abs(revision) > abs_threshold) |>
    dplyr::filter(abs(`In %`) > rel_threshold) |>
    dplyr::filter(abs(`as % GDP`) > gdp_threshold)

  # Handle output
  if (nrow(data_rev) == 0) {
    cli::cli_alert_success("No revisions found according to the provided thresholds")
  } else {
    return(data_rev)

  }
}
