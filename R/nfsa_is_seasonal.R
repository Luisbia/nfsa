
#' Test Seasonality in Quarterly T0801SA Series
#'
#' @description
#' Tests whether quarterly T0801SA time series exhibit
#' statistically significant seasonality using the
#' `seastests::isSeasonal()` function.
#'
#' @details
#' Data are provided by the user with the standard columns `ref_area`, `id`, `time_period`, `obs_value` and filtered
#' from `time_min` onwards.
#'
#' Each series is identified by `ref_area` and `id`.
#' Observations are sorted chronologically before creating
#' a quarterly time-series object.
#'
#' Series with fewer than `min_obs` observations, missing
#' quarters, duplicate periods, or invalid values are
#' assigned NA for the seasonality result.
#'
#' Zero observations are retained.
#'
#' The default seasonality test is the combined test
#' implemented in the `seastests` package.
#'
#' @param data a data.frame with the standard nfsa columns.
#' @param time_min Character. First quarter to consider,
#'   in "YYYY-Qn" format. Default is "1999-Q1".
#' @param min_obs Integer. Minimum number of quarterly
#'   observations required. Default is 12.
#' @param test Character. Seasonality test passed to
#'   `seastests::isSeasonal()`. Default is "combined".
#'
#' @return
#' A tibble containing:
#' \describe{
#'   \item{ref_area}{Country code.}
#'   \item{id}{Series identifier.}
#'   \item{n_obs}{Number of observations.}
#'   \item{is_seasonal}{TRUE if seasonal, FALSE if not,
#'     or NA if the test could not be performed.}
#' }
#'
#' @examples
#' \dontrun{
#' dat <- nfsa::nfsa_get_data(country = "PL",table = "T0801SA")
#' results_sa <-nfsa_is_seasonal(data =  dat) |>
#'  dplyr::filter(is_seasonal == TRUE)
#' nfsa_is_seasonal(data = dat)
#'
#' nfsa_is_seasonal(
#'   time_min = "2010-Q1"
#' )
#' }
#'
#' @seealso [seastests::isSeasonal()]
#' @import seastests
#' @import dplyr
#' @import tidyr
#' @import purrr
#'
#' @export
nfsa_is_seasonal <- function(
    data = NULL,
    time_min = "1999-Q1",
    min_obs = 12L,
    test = "combined"
) {

  # 1. Validate arguments
  if (!grepl("^\\d{4}-Q[1-4]$", time_min)) {
    stop("time_min must have the format YYYY-Qn.")
  }

  if (length(min_obs) != 1L ||
      is.na(min_obs) ||
      !is.numeric(min_obs) ||
      !is.finite(min_obs) ||
      min_obs < 2L ||
      min_obs != as.integer(min_obs)) {
    stop("min_obs must be an integer >= 2.")
  }

  # 2. Retrieve data
  data <- data |>
    dplyr::filter(.data$time_period >= time_min) |>
    dplyr::select(
      ref_area, id, time_period, obs_value
    )

  # 3. Helper to test one series
  test_series <- function(df) {

    df <- df |>
      dplyr::mutate(
        year = as.integer(
          substr(.data$time_period, 1, 4)
        ),
        quarter = as.integer(
          substr(.data$time_period, 7, 7)
        ),
        period_index = .data$year * 4L +
          .data$quarter - 1L
      ) |>
      dplyr::arrange(.data$period_index)

    n <- nrow(df)

    # Insufficient observations
    if (n < min_obs) {
      return(NA)
    }

    # Invalid or missing observations
    if (anyNA(df$period_index) ||
        any(!is.finite(df$obs_value))) {
      return(NA)
    }

    # Duplicate or missing quarters
    if (any(diff(df$period_index) != 1L)) {
      return(NA)
    }

    # Construct quarterly time series
    sca <- stats::ts(
      df$obs_value,
      start = c(df$year[1], df$quarter[1]),
      frequency = 4
    )

    # Seasonality test
    tryCatch(
      seastests::isSeasonal(
        sca,
        test = test
      ),
      error = function(e) NA
    )
  }

  # 4. Apply test by country and series
  result <- data |>
    tidyr::nest(.by = c(ref_area, id)) |>
    dplyr::mutate(
      n_obs = purrr::map_int(data, nrow),
      is_seasonal = purrr::map_lgl(
        data,
        test_series
      )
    ) |>
    dplyr::select(
      ref_area, id, n_obs, is_seasonal
    )

  return(result)
}
