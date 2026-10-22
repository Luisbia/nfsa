
#' Test Residual Seasonality in Quarterly T0801SA Series
#'
#' @description
#' Identifies quarterly T0801SA series with residual seasonality
#' using the Ollech-Webel combined seasonality test.
#'
#' For series identified as seasonal, five additional tests are
#' performed, returning their logical results and p-values.
#'
#' @details
#' Input data must contain `ref_area`, `id`, `time_period`,
#' and `obs_value`.
#'
#' Each series is identified by `ref_area` and `id`.
#' Observations are filtered from `time_min` onwards and sorted
#' chronologically before constructing a quarterly time series.
#'
#' The primary screening criterion is always the combined
#' seasonality test implemented in `seastests::isSeasonal()`.
#'
#' For series identified as seasonal, the following individual
#' tests are performed:
#' \describe{
#'   \item{qs}{QS seasonality test.}
#'   \item{fried}{Friedman rank test.}
#'   \item{kw}{Kruskal-Wallis test.}
#'   \item{seasdum}{F-test on seasonal dummy variables.}
#'   \item{welch}{Welch seasonality test.}
#' }
#'
#' The individual tests use the significance level of 0.01,
#' matching the implementation of `seastests::isSeasonal()`.
#'
#' The combined test and individual tests are not statistically
#' independent. Consequently, the number of positive individual
#' tests is provided as supplementary diagnostic information,
#' not as a separate decision criterion.
#'
#' Series with insufficient observations, missing quarters,
#' duplicate periods, or invalid values receive NA results.
#'
#' Individual test errors are handled independently.
#' Zero observations are retained.
#'
#' @param data A data.frame containing the standard NFSA
#'   columns `ref_area`, `id`, `time_period`, and `obs_value`.
#' @param time_min Character. First quarter to consider,
#'   in "YYYY-Qn" format. Default is "1999-Q1".
#' @param min_obs Integer. Minimum number of quarterly
#'   observations required. Default is 12.
#'
#' @return
#' A tibble with one row per country and series containing:
#' \describe{
#'   \item{ref_area}{Country code.}
#'   \item{id}{Series identifier.}
#'   \item{n_obs}{Number of observations.}
#'   \item{is_seasonal}{Combined test result (TRUE/FALSE/NA).}
#'   \item{test_qs, test_fried, test_kw, test_seasdum,
#'     test_welch}{Logical results of individual tests.}
#'   \item{p_value_qs, p_value_fried, p_value_kw,
#'     p_value_seasdum, p_value_welch}{
#'     P-values from the individual tests.}
#'   \item{n_tests_positive}{Number of individual tests
#'     detecting seasonality.}
#'   \item{n_tests_completed}{Number of individual tests
#'     successfully completed.}
#' }
#'
#' Individual test results are NA when the combined test
#' does not identify seasonality or cannot be performed.
#'
#' @examples
#' \dontrun{
#' dat <- nfsa::nfsa_get_data(
#'   country = "PL",
#'   table = "T0801SA"
#' )
#'
#' results <- nfsa_is_seasonal(data = dat)
#'
#' # Series with residual seasonality
#' results |>
#'   dplyr::filter(is_seasonal == TRUE)
#'
#' # Inspect individual test p-values
#' results |>
#'   dplyr::filter(is_seasonal == TRUE) |>
#'   dplyr::select(
#'     ref_area, id, dplyr::starts_with("p_value_")
#'   )
#' }
#'
#' @seealso [seastests::isSeasonal()],
#'   [seastests::combined_test()]
#'
#' @importFrom dplyr filter select mutate arrange
#' @importFrom tidyr nest unnest_wider
#' @importFrom purrr map map_int
#' @importFrom tibble tibble
#' @importFrom seastests isSeasonal qs fried kw seasdum welch
#' @importFrom stats ts
#'
#' @export
nfsa_is_seasonal <- function(
    data = NULL,
    time_min = "1999-Q1",
    min_obs = 12L
) {
  
  # 1. Validate arguments ---------------------------------------
  
  if (is.null(data) || !is.data.frame(data)) {
    stop("data must be a data.frame.")
  }
  
  required_cols <- c(
    "ref_area", "id", "time_period", "obs_value"
  )
  
  missing_cols <- setdiff(required_cols, names(data))
  
  if (length(missing_cols) > 0L) {
    stop(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  if (!is.character(time_min) ||
      length(time_min) != 1L ||
      is.na(time_min) ||
      !grepl("^[0-9]{4}-Q[1-4]$", time_min)) {
    stop("time_min must have the format YYYY-Qn.")
  }
  
  if (!is.numeric(min_obs) ||
      length(min_obs) != 1L ||
      is.na(min_obs) ||
      !is.finite(min_obs) ||
      min_obs < 2L ||
      min_obs != floor(min_obs)) {
    stop("min_obs must be an integer >= 2.")
  }
  
  if (!is.numeric(data$obs_value)) {
    stop("obs_value must be numeric.")
  }
  
  if (!is.character(data$time_period)) {
    stop("time_period must be character.")
  }
  
  # 2. Configuration --------------------------------------------
  
  alpha <- 0.01
  
  individual_tests <- c(
    "qs", "fried", "kw", "seasdum", "welch"
  )
  
  test_functions <- list(
    qs = seastests::qs,
    fried = seastests::fried,
    kw = seastests::kw,
    seasdum = seastests::seasdum,
    welch = seastests::welch
  )
  
  # 3. Filter data ----------------------------------------------
  
  data <- data |>
    dplyr::filter(
      !is.na(.data$time_period),
      .data$time_period >= time_min
    ) |>
    dplyr::select(
      ref_area, id, time_period, obs_value
    )
  
  # 4. Safe individual test -------------------------------------
  
  safe_test <- function(x, test_name) {
    
    tryCatch({
      
      result <- test_functions[[test_name]](x)
      
      p_value <- as.numeric(result$Pval)
      
      if (length(p_value) != 1L ||
          !is.finite(p_value) ||
          p_value < 0 ||
          p_value > 1) {
        return(list(
          seasonal = NA,
          p_value = NA_real_
        ))
      }
      
      list(
        seasonal = p_value < alpha,
        p_value = p_value
      )
      
    }, error = function(e) {
      list(
        seasonal = NA,
        p_value = NA_real_
      )
    })
  }
  
  # 5. Default output for a single series -----------------------
  
  empty_result <- function() {
    
    result <- list(is_seasonal = NA)
    
    for (test_name in individual_tests) {
      result[[paste0("test_", test_name)]] <- NA
      result[[paste0("p_value_", test_name)]] <- NA_real_
    }
    
    result$n_tests_positive <- NA_integer_
    result$n_tests_completed <- NA_integer_
    
    result
  }
  
  # 6. Test a single series -------------------------------------
  
  test_series <- function(df) {
    
    output <- empty_result()
    
    n <- nrow(df)
    
    # Minimum observations
    if (n < min_obs) {
      return(output)
    }
    
    # Validate quarterly periods
    if (anyNA(df$time_period) ||
        !all(grepl(
          "^[0-9]{4}-Q[1-4]$",
          df$time_period
        ))) {
      return(output)
    }
    
    # Validate observation values
    if (any(!is.finite(df$obs_value))) {
      return(output)
    }
    
    # Construct time index
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
    
    # Check missing or duplicated quarters
    if (any(diff(df$period_index) != 1L)) {
      return(output)
    }
    
    # Quarterly time series
    sca <- stats::ts(
      df$obs_value,
      start = c(df$year[1], df$quarter[1]),
      frequency = 4
    )
    
    # 6.1 Combined test -----------------------------------------
    
    combined_result <- tryCatch(
      seastests::isSeasonal(
        sca,
        test = "combined"
      ),
      error = function(e) NA
    )
    
    if (!is.logical(combined_result) ||
        length(combined_result) != 1L ||
        is.na(combined_result)) {
      return(output)
    }
    
    output$is_seasonal <- combined_result
    
    # Only run individual tests for seasonal series
    if (!combined_result) {
      return(output)
    }
    
    # 6.2 Five individual tests ---------------------------------
    
    test_results <- purrr::map(
      individual_tests,
      ~ safe_test(sca, .x)
    )
    
    names(test_results) <- individual_tests
    
    for (test_name in individual_tests) {
      
      result <- test_results[[test_name]]
      
      output[[paste0("test_", test_name)]] <-
        result$seasonal
      
      output[[paste0("p_value_", test_name)]] <-
        result$p_value
    }
    
    # 6.3 Diagnostic counts -------------------------------------
    
    test_flags <- vapply(
      test_results,
      function(x) x$seasonal,
      logical(1)
    )
    
    output$n_tests_completed <-
      as.integer(sum(!is.na(test_flags)))
    
    output$n_tests_positive <-
      if (output$n_tests_completed == 0L) {
        NA_integer_
      } else {
        as.integer(sum(test_flags, na.rm = TRUE))
      }
    
    output
  }
  
  # 7. Apply to all series --------------------------------------
  
  result <- data |>
    tidyr::nest(.by = c(ref_area, id)) |>
    dplyr::mutate(
      n_obs = purrr::map_int(.data$data, nrow),
      test_result = purrr::map(
        .data$data,
        test_series
      )
    ) |>
    dplyr::select(
      -data
    ) |>
    tidyr::unnest_wider(
      test_result
    ) |> 
    dplyr::filter(is_seasonal != FALSE)
  
  return(result)
}

