
#' Check Consistency Between Quarterly and Annual Data
#'
#' @description
#' Compares quarterly data (T0801) with annual data
#' and identifies discrepancies exceeding a specified
#' threshold.
#'
#' @details
#' Quarterly observations are aggregated by calendar year.
#'
#' Only years containing exactly four quarterly observations
#' are considered. The annual totals are then compared with
#' the corresponding annual observations.
#'
#' The comparison uses a left join, meaning that only series
#' present in the quarterly dataset are considered.
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
#' @param abs_threshold Numeric. Maximum acceptable absolute
#'   difference between the sum of quarterly observations and
#'   the annual observation. Defaults to 5.
#' @param rel_threshold Numeric. Maximum acceptable relative
#'   difference between the sum of quarterly observations and
#'   the annual observation. Defaults to 1%.
#'
#' @return
#' A tibble containing discrepancies exceeding the thresholds,
#' with the following columns:
#'
#' @examples
#' \dontrun{
#'quarterly_nsa <- nfsa_get_data(country = "EE", ref_period = "2026_0002")
#'quarterly_sca <- nfsa_get_data(country = "EE", table = "T0801SA",ref_period = "2026_0002")
#'annual <- nfsa_get_data(country = "EE", table = "T0800",ref_period = "2025_0000")

#'nfsa_quartely_annual(quarterly_nsa,
#'                     annual) |>
#'  dplyr::filter(!stringr::str_detect(id,"B9X9F"))
#'
#'nfsa_quartely_annual(quarterly_sca,
#'                     annual)
#' }
#' @import dplyr
#' @import tidyr
#' @import stringr
#'
#' @export
nfsa_quartely_annual <- function(df_q,
                                 df_a,
                                 abs_threshold =5,
                                 rel_threshold = 1){

  # 1. Aggregate quarterly data to annual frequency
  q_data <- df_q |>
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
      `sum of quarters` = dplyr::if_else(!stringr::str_detect(id,".PS"), sum(obs_value),mean(obs_value)
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-obs_value) |>
    dplyr::distinct()


# 2. Prepare annual data
a_data <- df_a |>
  dplyr::select(
    ref_area, id, time_period, obs_value
  ) |>
  dplyr::rename(annual = obs_value)


# 3. Compare quarterly and annual observations
q_data_a_data <- q_data |>
  dplyr::left_join(
    a_data,
    by = dplyr::join_by(
      ref_area, id, time_period
    )
  ) |>
  dplyr::mutate(
    Difference = round(`sum of quarters` - annual, 1)
  ) |>
  dplyr::filter(
    !is.na(Difference),
    abs(Difference) > abs_threshold,
  ) |>
  dplyr::mutate(
    `In %` = round(Difference * 100/annual, 1)
  )  |>
  dplyr::filter(
    !is.na(`In %`),
    abs(`In %`) > rel_threshold,
  )

# 4. Return results

if(nrow(q_data_a_data) == 0) {
  cli::cli_alert_success("Quarterly an annual data are consistent within the chosen thresholds")

    }

return(q_data_a_data)

}


