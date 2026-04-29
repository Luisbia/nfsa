#' @title Perform Level 1 Seasonality Checks
#'
#' @description Performs level 1 seasonality checks on time series data, comparing
#'   seasonally adjusted (SCA) and non-seasonally adjusted (NSA) data. Supports two
#'   modes: \code{"country"} for individual countries (loaded via \code{nfsa_get_data}),
#'   and \code{"agg"} for aggregates like EA or EU (loaded from a flat CSV file).
#'   Identifies series with potential issues such as residual seasonality,
#'   over-adjustment, or inconsistencies between NSA and SCA data.
#'
#' @param mode A character string specifying the data loading mode. Either
#'   \code{"country"} (default) or \code{"agg"}.
#' @param country A character vector of country codes, used when \code{mode = "country"}
#'   (e.g., \code{c("BE", "IT", "FR")}). Ignored when \code{mode = "agg"}.
#' @param agg A character string specifying the aggregate area, used when
#'   \code{mode = "agg"} (e.g., \code{"EA"} or \code{"EU"}). Ignored when
#'   \code{mode = "country"}.
#' @param time_min The earliest quarter to include in the analysis. Defaults to
#'   \code{"1999-Q1"}. Only used when \code{mode = "country"}.
#' @param input_sel Path to the directory containing input data files. Defaults to
#'   \code{"M:/nas/Rprod/data/"}. Used in both modes.
#' @param output_sel Path to the directory where the output Excel file will be saved.
#'   Defaults to \code{here::here("output", "seas")}.
#'
#' @return A data frame with columns \code{ref_area}, \code{id}, and \code{level1}
#'   (either a \code{"FAIL"} or \code{"WARNING"} message) for all flagged series.
#'   Also writes an Excel file to \code{output_sel} and calls \code{nfsa::nfsa_to_excel()}.
#'   Returns invisibly if no issues are found.
#'
#' @examples
#' \dontrun{
#' # Country mode: check Belgium and Italy
#' results <- nfsa_seas_level1(country = c("BE", "IT"))
#'
#' # Country mode: check France with custom directories
#' results <- nfsa_seas_level1(country = "FR",
#'                              input_sel = "/path/to/input",
#'                              output_sel = "/path/to/output")
#'
#' # Aggregate mode: check the Euro Area aggregate
#' results <- nfsa_seas_level1(mode = "agg", agg = "EA")
#'
#' # Aggregate mode: check the EU aggregate
#' results <- nfsa_seas_level1(mode = "agg", agg = "EU")
#' }
#'
#' @export
nfsa_seas_level1 <- function(mode = "country",
                             country = NULL,
                             agg = NULL,
                             time_min = "1999-Q1",
                             input_sel = "M:/nas/Rprod/data/",
                             output_sel = here::here("output", "seas")) {

  # ---- Input validation ----
  mode <- match.arg(mode, choices = c("country", "agg"))

  if (mode == "country" && is.null(country)) {
    cli::cli_abort("{.arg country} must be provided when {.arg mode} is \"country\".")
  }
  if (mode == "agg" && is.null(agg)) {
    cli::cli_abort("{.arg agg} must be provided when {.arg mode} is \"agg\".")
  }

  # ---- Data loading ----
  cli::cli_inform("Collecting data...")

  if (mode == "country") {

    nsa <- nfsa_get_data(country = country,
                         table = "T0801",
                         type = "new",
                         input_sel = input_sel) |>
      dplyr::rename(NSA = obs_value)

    sca <- nfsa_get_data(country = country,
                         table = "T0801SA",
                         type = "new",
                         input_sel = input_sel) |>
      dplyr::rename(SCA = obs_value)

    data <- dplyr::full_join(nsa, sca, by = dplyr::join_by(ref_area, id, time_period)) |>
      dplyr::filter(time_period >= time_min) |>
      stats::na.omit()

  } else {  # mode == "agg"

    raw <- nfsa_read_matis(paste0(input_sel, agg, "_data/", agg, "_new.csv")) |>
      dplyr::left_join(y = nfsa_sto_lookup,
                       by = dplyr::join_by(counterpart_area, ref_sector,
                                           counterpart_sector, consolidation,
                                           accounting_entry, sto, instr_asset,
                                           unit_measure, prices)) |>
      dplyr::select(ref_area, id, adjustment, time_period, obs_value) |>
      stats::na.omit()

    nsa <- raw |>
      dplyr::filter(adjustment == "N") |>
      dplyr::select(ref_area, id, time_period, NSA = obs_value)

    sca <- raw |>
      dplyr::filter(adjustment == "Y") |>
      dplyr::select(ref_area, id, time_period, SCA = obs_value)

    data <- dplyr::full_join(nsa, sca, by = dplyr::join_by(ref_area, id, time_period)) |>
      stats::na.omit()

  }

  # ---- Remove series with too many zeros ----
  tmp <- nsa |>
    dplyr::filter(NSA == 0) |>
    dplyr::group_by(ref_area, id) |>
    dplyr::add_count() |>
    dplyr::filter(n > 10) |>
    dplyr::select(ref_area, id) |>
    dplyr::ungroup() |>
    dplyr::distinct()

  # ---- Run level 1 tests ----
  cli::cli_inform("Running tests...")

  data <- dplyr::anti_join(data, tmp, dplyr::join_by(ref_area, id)) |>
    dplyr::mutate(time_period = lubridate::yq(time_period)) |>
    tidyr::nest(.by = c(ref_area, id)) |>
    dplyr::mutate(
      nsa = purrr::map(data, ~stats::ts(.x$NSA,
                                        start = c(min(stringr::str_sub(.x$time_period, 1, 4)), 1),
                                        frequency = 4)),
      sca = purrr::map(data, ~stats::ts(.x$SCA,
                                        start = c(min(stringr::str_sub(.x$time_period, 1, 4)), 1),
                                        frequency = 4)),
      level1_X13 = purrr::map2(
        .x = nsa,
        .y = sca,
        .f = ~SAvalidation::level1_validation(.x, .y,
                                              default_type     = "X13",
                                              default_spec_nsa = "RSA1",
                                              default_spec_sa  = "RSA2c")
      )
      # level1_TS = purrr::map2(.x = nsa, .y = sca,
      #   .f = ~SAvalidation::level1_validation(.x, .y, default_type = "TS",
      #                                         default_spec_sa = "RSA2"))
    ) |>
    dplyr::select(ref_area, id, level1_X13) |>
    tidyr::unnest(c(level1_X13)) |>
    dplyr::select(ref_area, id, level1 = level1_X13) |>
    dplyr::filter(level1 %in% c(
      "FAIL: EVIDENCE OF RESIDUAL SEASONALITY OR CALENDAR EFFECTS IN SA SERIES",
      "FAIL: NO EVIDENCE OF SEASONALITY IN NSA BUT SERIES ADJUSTED",
      "FAIL: EVIDENCE OF SEASONALITY IN NSA BUT SA IS NOT ADJUSTED",
      "WARNING: ANNUAL TOTALS CHECK FAILED",
      "WARNING: SA SERIES HAS EVIDENCE OF OVER-ADJUSTMENT",
      "WARNING: SA SERIES HAS NEGATIVE VALUES"
    ))

  # ---- Output ----
  if (nrow(data) == 0) {
    cli::cli_alert_success("No issues")
  } else {
    timestamp    <- as.character(format(Sys.time(), "%Y%m%d_%H%M%S"))
    output_file  <- paste0(output_sel, "/level1_", timestamp, ".xlsx")

    openxlsx::write.xlsx(data, file = output_file, overwrite = TRUE)
    cli::cli_inform(paste0("File created at ", output_file))
    nfsa::nfsa_to_excel(data)
    return(data)
  }

}

