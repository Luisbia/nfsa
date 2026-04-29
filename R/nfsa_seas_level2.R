#' @title Perform Level 2 Seasonal Adjustment Validation
#'
#' @description Performs level 2 seasonal adjustment validation on quarterly
#'   non-financial sector accounts data, generating a Quarto dashboard report
#'   for each flagged series. Supports three modes: \code{"country"} for
#'   individual countries (loaded via \code{nfsa_get_data}), and \code{"agg"}
#'   for aggregates (EA, EU, etc.) loaded from a flat CSV file.
#' @param series Path to an Excel file containing a list of series to process.
#'   Defaults to `here("assets", "seas_level1.xlsx")`. This file should contain
#'   at least `ref_area` and `id` columns.
#' @param mode A character string specifying the data loading mode. Either
#'   \code{"country"} (default) or \code{"agg"}.
#' @param agg A character string specifying the aggregate area, used when
#'   \code{mode = "agg"} (e.g., \code{"EA"} or \code{"EU"}). Ignored when
#'   \code{mode = "country"}.
#' @param time_min The earliest quarter to include in the analysis. Defaults to
#'   \code{"1999-Q1"}. Only applied when \code{mode = "country"}.
#' @param input_sel Path to the directory containing input data files. Defaults
#'   to \code{"M:/nas/Rprod/data/"}. In \code{"agg"} mode this should point to
#'   the parent data directory (the function appends \code{<agg>_data/<agg>_new.csv}).
#' @param output_sel Path to the directory where Quarto reports will be saved.
#'   Defaults to \code{here::here("output", "seas")}.
#' @param dashboard_sel Path to the Quarto dashboard template. Defaults to
#'   \code{here::here("assets", "level2_eurostat_short.qmd")}.
#' @param default_type_sel Default seasonal adjustment method. Defaults to
#'   \code{"X13"}.
#' @param default_spec_nsa_sel Default specification for NSA series. Defaults
#'   to \code{"RSA1"}.
#' @param default_spec_sa_sel Default specification for SA series. Defaults to
#'   \code{"RSA2c"}.
#'
#' @return Called for its side effects. Generates one Quarto dashboard report
#'   per series in \code{output_sel}. Returns \code{NULL} invisibly.
#'
#' @examples
#' \dontrun{
#' # Country mode: process series listed in the default Excel file
#' nfsa_seas_level2()
#'
#' # Country mode: custom output directory
#' nfsa_seas_level2(output_sel = here::here("my_output"))
#'
#' # Aggregate mode: Euro Area
#' nfsa_seas_level2(mode = "agg", agg = "EA")
#'
#' # Aggregate mode: European Union
#' nfsa_seas_level2(mode = "agg", agg = "EU")
#' }
#'
#' @export
nfsa_seas_level2 <- function(mode               = "country",
                             agg                = NULL,
                             series             = here::here("assets", "seas_level1.xlsx"),
                             time_min           = "1999-Q1",
                             input_sel          = "M:/nas/Rprod/data/",
                             output_sel         = here::here("output", "seas"),
                             dashboard_sel      = here::here("assets", "level2_eurostat_short.qmd"),
                             default_type_sel   = "X13",
                             default_spec_nsa_sel = "RSA1",
                             default_spec_sa_sel  = "RSA2c") {

  # ---- Input validation ----
  mode <- match.arg(mode, choices = c("country", "agg"))

  if (mode == "agg" && is.null(agg)) {
    cli::cli_abort("{.arg agg} must be provided when {.arg mode} is \"agg\".")
  }


  # ---- Load series list ----
  series_list <- readxl::read_xlsx(series) |>
    dplyr::distinct()


  # ---- Load NSA / SCA data ----
  if (mode == "country") {

    nsa <- nfsa_get_data(country = unique(series_list$ref_area),
                         table   = "T0801",
                         type    = "new") |>
      dplyr::rename(NSA = obs_value)

    sca <- nfsa_get_data(country = unique(series_list$ref_area),
                         table   = "T0801SA",
                         type    = "new") |>
      dplyr::rename(SCA = obs_value)

    nsa_sca <- dplyr::full_join(nsa, sca, by = dplyr::join_by(ref_area, id, time_period)) |>
      dplyr::filter(time_period >= time_min)

  } else {  # mode == "agg"

    raw <- nfsa_read_matis(paste0(input_sel, agg, "_data/", agg, "_new.csv")) |>
      dplyr::left_join(y  = nfsa_sto_lookup,
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

    nsa_sca <- dplyr::full_join(nsa, sca, by = dplyr::join_by(ref_area, id, time_period))

  }

  # ---- Join with series list and nest ----
  nsa_sca <- dplyr::left_join(series_list, nsa_sca, by = dplyr::join_by(ref_area, id)) |>
    dplyr::mutate(time_period = lubridate::yq(time_period)) |>
    dplyr::arrange(time_period) |>
    tidyr::unite(col = "id", c(ref_area, id), sep = "_") |>
    tidyr::nest(.by = id)

  # ---- Inner helper: render one Quarto dashboard ----
  level2_validation_eurostat <- function(nsa, sa, series_name,
                                         dataset_name       = "Quarterly non-financial sector accounts",
                                         title              = series_name,
                                         output_directory   = output_sel,
                                         dashboard_template = dashboard_sel,
                                         java_home          = Sys.getenv("JAVA_HOME")) {

    if (is.null(output_directory)) output_directory <- getwd()
    if (!dir.exists(output_directory)) dir.create(output_directory, recursive = TRUE)

    dashboard_template_to_copy <- if (dashboard_template == "skeleton.qmd") {
      file.path(system.file("rmarkdown/templates/level2_report/skeleton3",
                            package = "SAvalidation"),
                dashboard_template)
    } else {
      dashboard_template
    }

    dashboard_to_create <- file.path(
      paste0(output_directory, "/level2_", series_name, "_", default_type_sel, ".qmd")
    )

    file.copy(dashboard_template_to_copy, dashboard_to_create, overwrite = TRUE)

    quarto::quarto_render(
      dashboard_to_create,
      execute_params = list(
        nsa              = nsa,
        sa               = sa,
        name             = series_name,
        dataset_name     = dataset_name,
        title            = title,
        ts_start         = stats::start(nsa),
        ts_freq          = stats::frequency(nsa),
        default_type     = default_type_sel,
        default_spec_nsa = default_spec_nsa_sel,
        default_spec_sa  = default_spec_sa_sel,
        java_home        = java_home
      )
    )
  }

  # ---- Render a dashboard for each series ----
  for (i in seq_along(nsa_sca$id)) {
    ts_start <- c(
      as.integer(stringr::str_sub(nsa_sca$data[[i]][[1]][[1]], 1, 4)),
      as.integer(stringr::str_sub(nsa_sca$data[[i]][[1]][[1]], 7, 7))
    )
    level2_validation_eurostat(
      nsa         = stats::ts(nsa_sca$data[[i]][[2]], start = ts_start, frequency = 4),
      sa          = stats::ts(nsa_sca$data[[i]][[3]], start = ts_start, frequency = 4),
      series_name = nsa_sca$id[i]
    )
  }

  invisible(NULL)
}

