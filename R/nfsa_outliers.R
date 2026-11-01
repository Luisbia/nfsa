
#' Detect Outliers in National Non Financial Sector Accounts (NFSA)
#'
#' @description
#' Identifies potentially anomalous observations in NFSA time series
#' using two sequential criteria: growth rates and changes relative
#' to GDP.
#'
#' Observations meeting these criteria are returned
#'
#' @details
#' The outlier detection procedure consists of two stages:
#'
#' 1. **Growth-rate criterion:** Calculates the percentage change
#'    in each series relative to the observation `type` periods
#'    earlier. Observations are retained when the absolute
#'    percentage change exceeds `rate_limit`.
#'
#' 2. **GDP criterion:** Calculates the absolute change in the
#'    observation as a percentage of GDP (identified by
#'    `"S1.B1GQ.B"`). Observations are retained when the absolute
#'    percentage exceeds `gdp_limit`.
#'
#'
#' All criteria use strict inequalities (`>`).
#'
#' Before applying these criteria, selected balancing items
#' and transactions are excluded using predefined ID patterns.
#' These include B101, B9, D43, P5M, D8, B11, B12, B6G,
#' B8G, B7G, and B5G.
#'
#'
#' @param data a dataframe with T0801/T0801SA ot T0800 data with 4 columns.
#'
#' @param type Integer. Number of periods used to calculate
#'   percentage changes. The default is `4L`, corresponding
#'   to year-on-year changes for quarterly data. Use `1L`
#'   for quarter-on-quarter changes or year-on-year changes.
#'
#' @param time_min Character scalar. Earliest reporting period
#'   to include, expressed in `"YYYY-Qn"` or `YYYY`. Default is `NULL`.
#'
#' @param rate_limit Numeric scalar. Minimum absolute percentage
#'   growth rate required for an observation to be considered
#'   a potential outlier. Default is `30` (30 percent).
#'
#' @param gdp_limit Numeric scalar. Minimum absolute change,
#'   expressed as a percentage of GDP, required for an
#'   observation to be retained. Default is `0.3`
#'   (0.3 percent of GDP).
#'
#'
#' @return
#' If outliers are identified, a tibble containing the observations
#' satisfying all detection criteria, including:
#'
#' - Country, transaction identifier, and reporting period.
#' - Current observation value (`obs_value`).
#' - Absolute change relative to the selected lag (`change`).
#' - Percentage growth rate (`growth`).
#' - GDP value (`gdp`).
#' - Change expressed as a percentage of GDP (`as_gdp`).
#'
#' If no outliers are identified, the function prints
#' `"No outliers according to this criteria"` and returns
#' `NULL` invisibly.
#'
#' @section Output files:
#' When outliers are found, a PDF file is saved in `output_sel`
#' using the following naming convention:
#'
#' `YYYYMMDD_HHMMSS_outliers.pdf`
#'
#' Each page contains one chart comparing the current and
#' previous vintages. A success message displays the file path.
#'
#' @note
#' The function relies on [nfsa::nfsa_get_data()] to retrieve
#' current and previous NFSA vintages. The previous vintage
#' is requested using `type = "prev"`.
#'
#' The initial time filter is applied before calculating growth
#' rates, so the first `type` observations of each time series
#' generally cannot be evaluated.
#'
#' @examples
#' \dontrun{
#' # Detect outliers for Luxembourg using default thresholds
#' outliers <- nfsa_outliers(country_sel = "LU")
#'
#'
#' # Use quarter-on-quarter changes and custom thresholds
#' outliers <- nfsa_outliers(
#'   data = dat_q,
#'   type = 4L,
#'   time_min = "2010-Q1",
#'   rate_limit = 20,
#'   gdp_limit = 0.2
#' )
#'
#' # Useing annual data
#' outliers <- nfsa_outliers(
#'   data = dat_a",
#'   type = 1L)
#' )
#' }
#'
#' @seealso
#' [nfsa::nfsa_get_data()],
#' [nfsa::nfsa_separate_id()],
#' [nfsa::nfsa_theme()]
#'
#' @importFrom dplyr arrange filter full_join group_by join_by lag left_join mutate rename select transmute
#' @importFrom stringr str_detect

#' @export
nfsa_outliers <- function(data,
                          type = 4L,
                          time_min = NULL,
                          rate_limit = 30,
                          gdp_limit = 0.3) {

  dat_o <- data |>
    dplyr::filter(!stringr::str_detect(id,".B101.|.B9.|.D43.|.P5M.|.D8.|.B11.|.B12.|.B6G.|.B8G.|.B7G.|.B5G."))

  if (is.null(time_min)){
    dat_o <- dat_o
  } else {
    dat_o <- dat_o |>
      filter(time_period >= time_min)
  }

  gdp_lookup <- dat_o |>
    dplyr::filter(id == "S1.B1GQ.B") |>
    dplyr::select(ref_area,time_period, gdp = obs_value)

  dat <- dat_o |>
    dplyr::group_by(ref_area,id) |>
    dplyr::arrange(time_period,.by_group = TRUE) |>
    dplyr::mutate(change = obs_value - dplyr::lag(obs_value,type)) |>
    dplyr::mutate(growth = round(change*100/dplyr::lag(obs_value,type),1)) |>
    dplyr::filter(abs(growth)> rate_limit) |>
    dplyr::left_join(y= gdp_lookup,by = dplyr::join_by(ref_area, time_period)) |>
    dplyr::mutate(as_gdp = round(change*100/gdp,1)) |>
    dplyr::filter(abs(as_gdp) > gdp_limit)

  if (nrow(dat) == 0) {
    cli::cli_alert_info("No outliers according to these criteria.")
    return(invisible(NULL))
  } else {

  return(dat)
  }
}
