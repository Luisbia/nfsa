
#' Check Consistency Between Quarterly and Annual NFSA Data
#'
#' @description
#' Compares quarterly NFSA data (T0801) with annual NFSA data
#' (T0800) and identifies discrepancies exceeding a specified
#' threshold.
#'
#' @details
#' Quarterly observations are aggregated by calendar year,
#' country, institutional sector, transaction, and accounting
#' entry.
#'
#' Only years containing exactly four quarterly observations
#' are considered. The annual totals are then compared with
#' the corresponding annual observations.
#'
#' The comparison uses a left join, meaning that only series
#' present in the quarterly dataset are considered.
#'
#'
#' The function returns only observations for which the
#' absolute difference exceeds the specified threshold.
#' If no discrepancies are found, an empty tibble is returned.
#'
#' @param quarterly_data Data frame containing quarterly NFSA
#'   observations, typically from T0801.
#'   Must contain `ref_area`, `id`, `time_period`, and `obs_value`.
#'   Time periods must begin with a four-digit year.
#' @param annual_data Data frame containing annual NFSA
#'   observations, typically from T0800.
#'   Must contain `ref_area`, `id`, `time_period`, and `obs_value`.
#'   Annual time periods must be expressed as "YYYY".
#' @param threshold Numeric. Maximum acceptable absolute
#'   difference between the sum of quarterly observations and
#'   the annual observation. Defaults to 5.
#'
#' @return
#' A tibble containing discrepancies exceeding `threshold`,
#' with the following columns:
#' \describe{
#'   \item{ref_area}{Country code.}
#'   \item{ref_sector}{Institutional sector.}
#'   \item{sto}{Transaction code.}
#'   \item{accounting_entry}{Accounting entry.}
#'   \item{time_period}{Reference year.}
#'   \item{qnfsa}{Sum of four quarterly observations.}
#'   \item{anfsa}{Corresponding annual observation.}
#'   \item{diff}{Difference between quarterly and annual values,
#'     rounded to two decimal places.}
#' }
#'
#' @examples
#' \dontrun{
#' quarterly <- nfsa_get_data(
#'   country = "BE",
#'   table = "T0801"
#' )
#'
#' annual <- nfsa_get_data(
#'   country = "BE",
#'   table = "T0800"
#' )
#'
#' nfsa_T0801_T0800(
#'   quarterly_data = quarterly,
#'   annual_data = annual,
#'   threshold = 2
#' )
#' }
#'
#' @importFrom dplyr mutate group_by ungroup filter
#' @importFrom dplyr select rename summarise left_join
#' @importFrom dplyr join_by n
#' @importFrom stringr str_sub
#'
#' @export
nfsa_T0801_T0800 <- function(
    quarterly_data = q,
    annual_data = a,
    threshold = 5
) {

  # 1. Aggregate quarterly data to annual frequency
  q_nfsa_data <- quarterly_data |>
    dplyr::mutate(
      year = stringr::str_sub(time_period, 1, 4)
    ) |>
    dplyr::group_by(
      ref_area, id, year
    ) |>
    dplyr::filter(dplyr::n() == 4) |>
    dplyr::ungroup() |>
    dplyr::select(
      ref_area, id, year, obs_value
    ) |>
    dplyr::rename(time_period = year) |>
    dplyr::group_by(ref_area, id, time_period) |>
    dplyr::mutate(
      T0801 = dplyr::if_else(!stringr::str_detect(id,".PS"), sum(obs_value),mean(obs_value)
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-obs_value) |>
    dplyr::distinct()

  # 2. Prepare annual data
  a_nfsa_data <- annual_data |>
    dplyr::select(
      ref_area, id, time_period, obs_value
    ) |>
    dplyr::rename(T0800 = obs_value)

  # 3. Compare quarterly and annual observations
  qnfsa_anfsa <- q_nfsa_data |>
    dplyr::left_join(
      a_nfsa_data,
      by = dplyr::join_by(
        ref_area, id, time_period
      )
    ) |>
    dplyr::mutate(
      Difference = round(T0801 - T0800, 1)
    ) |>
    dplyr::filter(
      !is.na(Difference),
      abs(Difference) > threshold,
    ) |>
    dplyr::mutate(
      `In %` = round(Difference * 100/T0800, 1)
    )

  # 4. Return results
  return(qnfsa_anfsa)
}

