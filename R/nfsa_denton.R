solve_denton <- function(annual, indicator, freq = 4,
                             criterion = c("proportional", "additive")) {
  criterion <- match.arg(criterion)
  n_annual <- length(annual)
  n_high <- length(indicator)

  if (n_high != n_annual * freq) {
    stop("High-frequency indicator length must equal annual length * freq.")
  }
  if (anyNA(annual) || anyNA(indicator)) {
    stop("Annual and indicator series must not contain NA values.")
  }
  if (criterion == "proportional" && any(indicator == 0)) {
    stop("Proportional Denton cannot be used with zero indicator values.")
  }

  diff_rows <- n_high - 1
  D <- matrix(0, nrow = diff_rows, ncol = n_high)

  for (i in seq_len(diff_rows)) {
    if (criterion == "proportional") {
      D[i, i] <- -1 / indicator[i]
      D[i, i + 1] <- 1 / indicator[i + 1]
    } else {
      D[i, i] <- -1
      D[i, i + 1] <- 1
    }
  }

  Q <- crossprod(D)

  C <- matrix(0, nrow = n_annual, ncol = n_high)
  for (yr in seq_len(n_annual)) {
    idx <- ((yr - 1) * freq + 1):(yr * freq)
    C[yr, idx] <- 1
  }

  KKT <- rbind(
    cbind(Q, t(C)),
    cbind(C, matrix(0, nrow = n_annual, ncol = n_annual))
  )
  rhs <- c(rep(0, n_high), annual)

  as.numeric(solve(KKT, rhs)[seq_len(n_high)])
}

parse_annual_year <- function(x) {
  period <- as.character(x)
  ok <- grepl("^\\d{4}$", period)
  if (!all(ok)) {
    stop("Annual time periods must have format YYYY. Invalid values: ",
         paste(unique(period[!ok]), collapse = ", "))
  }
  as.integer(period)
}

parse_quarterly_period <- function(x) {
  period <- as.character(x)
  ok <- grepl("^\\d{4}-Q[1-4]$", period)
  if (!all(ok)) {
    stop("Quarterly time periods must have format YYYY-Q1, ..., YYYY-Q4. Invalid values: ",
         paste(unique(period[!ok]), collapse = ", "))
  }

  data.frame(
    year = as.integer(substr(period, 1, 4)),
    quarter = as.integer(substr(period, 7, 7))
  )
}

nfsa_denton <- function(annual_df = annual_data,
                        quarterly_df = quarterly_data,
                        annual_time_col = "time_period",
                        quarterly_time_col = "time_period",
                        freq = 4,
                        criterion = c("proportional", "additive"),
                        extrapolate = TRUE,
                        extrapolation = c("last_ratio", "last_difference")) {
  criterion <- match.arg(criterion)
  extrapolation <- match.arg(extrapolation)
  if (criterion == "proportional" && extrapolation == "last_difference") {
    stop("Use extrapolation = 'last_ratio' with proportional Denton.")
  }
  if (criterion == "additive" && extrapolation == "last_ratio") {
    stop("Use extrapolation = 'last_difference' with additive Denton.")
  }

  if (!annual_time_col %in% names(annual_df)) {
    stop("annual_df must contain column: ", annual_time_col)
  }
  if (!quarterly_time_col %in% names(quarterly_df)) {
    stop("quarterly_df must contain column: ", quarterly_time_col)
  }
  country <- unique(annual_df$ref_area)
  annual_df$ref_area <- NULL
  quarterly_df$ref_area <- NULL

  annual_year <- parse_annual_year(annual_df[[annual_time_col]])
  quarterly_period <- parse_quarterly_period(quarterly_df[[quarterly_time_col]])

  annual_df[[".denton_year"]] <- annual_year
  quarterly_df[[".denton_year"]] <- quarterly_period$year
  quarterly_df[[".denton_quarter"]] <- quarterly_period$quarter

  annual_df <- annual_df[order(annual_df[[".denton_year"]]), , drop = FALSE]
  quarterly_df <- quarterly_df[
    order(quarterly_df[[".denton_year"]], quarterly_df[[".denton_quarter"]]),
    ,
    drop = FALSE
  ]

  annual_series <- setdiff(names(annual_df), c(annual_time_col, ".denton_year"))
  quarterly_series <- setdiff(
    names(quarterly_df),
    c(quarterly_time_col, ".denton_year", ".denton_quarter")
  )
  matched_series <- intersect(annual_series, quarterly_series)

  if (length(matched_series) == 0) {
    stop("No matching series columns found between annual_df and quarterly_df.")
  }

  years <- annual_df[[".denton_year"]]
  benchmark_quarterly <- quarterly_df[
    quarterly_df[[".denton_year"]] %in% years,
    ,
    drop = FALSE
  ]

  quarters_per_year <- table(benchmark_quarterly[[".denton_year"]])
  missing_years <- setdiff(years, as.numeric(names(quarters_per_year)))
  if (length(missing_years) > 0) {
    stop("Quarterly data missing annual years: ", paste(missing_years, collapse = ", "))
  }
  if (any(quarters_per_year[as.character(years)] != freq)) {
    stop("Each annual year must have exactly ", freq, " quarterly observations.")
  }

  last_benchmark_year <- max(years)
  future_quarterly <- quarterly_df[
    quarterly_df[[".denton_year"]] > last_benchmark_year,
    ,
    drop = FALSE
  ]

  if (!extrapolate) {
    future_quarterly <- future_quarterly[FALSE, , drop = FALSE]
  }

  quarterly_df <- rbind(benchmark_quarterly, future_quarterly)
  quarterly_df <- quarterly_df[
    order(quarterly_df[[".denton_year"]], quarterly_df[[".denton_quarter"]]),
    ,
    drop = FALSE
  ]

  out <- quarterly_df[quarterly_time_col]

  for (series_name in matched_series) {
    annual_values <- as.numeric(annual_df[[series_name]])
    benchmark_indicator <- as.numeric(benchmark_quarterly[[series_name]])

    benchmark_adjusted <- solve_denton(
      annual = annual_values,
      indicator = benchmark_indicator,
      freq = freq,
      criterion = criterion
    )

    adjusted_values <- benchmark_adjusted
    if (nrow(future_quarterly) > 0) {
      future_indicator <- as.numeric(future_quarterly[[series_name]])
      if (extrapolation == "last_ratio") {
        if (tail(benchmark_indicator, 1) == 0) {
          stop("Cannot extrapolate with last_ratio when the last indicator is zero.")
        }
        final_ratio <- tail(benchmark_adjusted, 1) / tail(benchmark_indicator, 1)
        adjusted_values <- c(adjusted_values, future_indicator * final_ratio)
      } else {
        final_difference <- tail(benchmark_adjusted, 1) - tail(benchmark_indicator, 1)
        adjusted_values <- c(adjusted_values, future_indicator + final_difference)
      }
    }

    out[[series_name]] <- adjusted_values
    out$ref_area <- country
  }

  out
}


