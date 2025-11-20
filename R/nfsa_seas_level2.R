#' Level 2 Seasonal Adjustment Validation
#'
#' This function performs level 2 validation of seasonally adjusted data, generating
#' a dashboard report using a Quarto template. It reads time series data, performs
#' checks, and renders a report with specified parameters.
#'
#' @param series Path to an Excel file containing a list of series to process.
#'   Defaults to `here("assets", "seas_level1.xlsx")`. This file should contain
#'   at least columns named `ref_area` and `id` to identify the series.
#' @param input_sel Path to the input data directory. Defaults to `here::here("data")`.
#'   Not currently used in the function's logic.
#' @param output_sel Path to the output directory where the generated dashboard
#'   reports will be saved. Defaults to `here::here("output", "seas")`.
#' @param dashboard_sel Path to the Quarto dashboard template file. Defaults to
#'   `here::here("assets", "level2_eurostat_short.qmd")`.
#' @param default_type_sel Default seasonal adjustment type. Defaults to `"X13"`.
#'   This parameter is passed to the Quarto report.
#' @param default_spec_nsa_sel Default specification for the non-seasonally adjusted (NSA)
#'   series. Defaults to `"RSA1"`. This parameter is passed to the Quarto report.
#' @param default_spec_sa_sel Default specification for the seasonally adjusted (SA)
#'   series. Defaults to `"RSA2c"`. This parameter is passed to the Quarto report.
#'
#' @return This function does not return a value. It generates Quarto dashboard
#'   reports in the specified output directory.

#'
#' @examples
#' \dontrun{
#' # Example usage:
#' nfsa_seas_level2(
#'   series = here::here("assets", "seas_level1.xlsx"),
#'   output_sel = here::here("output", "seas"),
#'   dashboard_sel = here::here("assets", "level2_eurostat_short.qmd"),
#'   default_type_sel = "X13",
#'   default_spec_nsa_sel = "RSA1",
#'   default_spec_sa_sel = "RSA2c"
#' )
#' }
#' @export
nfsa_seas_level2 <- function(series = here("assets", "seas_level1.xlsx"),
                             input_sel = here::here("data"),
                             output_sel = here::here("output", "seas"),
                             dashboard_sel = here::here("assets", "level2_eurostat_short.qmd"),
                             default_type_sel = "X13",
                             default_spec_nsa_sel = "RSA1",
                             default_spec_sa_sel = "RSA2c"){

  library(nfsa)
  library(tidyverse)
  library(arrow)
  library(readxl)
  library(SAvalidation)
  library(here)

  series_list <- readxl::read_xlsx(series)

  nsa <- nfsa_get_data(country = unique(series_list$ref_area),
                       table = "T0801",
                       type = "new") |>
    rename(NSA = obs_value)

  sca <- nfsa_get_data(country = unique(series_list$ref_area),
                       table = "T0801SA",
                       type = "new")|>
    rename(SCA = obs_value)

  nsa_sca <- full_join(nsa,sca,by = join_by(ref_area, id, time_period) ) |>
    filter(time_period >= "1999-Q1")

  nsa_sca <- left_join(series_list, nsa_sca,by = join_by(ref_area, id) ) |>
    mutate(time_period = lubridate::yq(time_period)) |>
    arrange(time_period) |>
    na.omit() |>
    unite(col = "id", c(ref_area,id),sep ="_") |>
    group_by(id) |>
    nest()

  level2_validation_eurostat <- function(nsa,sa,series_name,
                                         title = series_name,
                                         output_directory = output_sel,
                                         dashboard_template= dashboard_sel,
                                         start_date= "1999-01-01",
                                         java_home = Sys.getenv("JAVA_HOME")){

    if(is.null(output_directory)){
      output_directory <- getwd()
    }
    if(!dir.exists(output_directory)){
      dir.create(output_directory,recursive = TRUE)
    }

    if (dashboard_template == "skeleton.qmd"){
      dashboard_template_to_copy <- file.path(system.file("rmarkdown/templates/level2_report/skeleton",
                                                          package="SAvalidation"),dashboard_template)
    } else {
      dashboard_template_to_copy <- dashboard_template
    }

    dashboard_to_create <- file.path(paste0(output_directory,"/level2_",series_name,"_",default_type_sel,".qmd"))

    file.copy(dashboard_template_to_copy,dashboard_to_create,overwrite = TRUE)


    check_nsa_sa_ts(nsa,sa)

    ts_start <- stats::start(nsa)
    ts_freq <- stats::frequency(nsa)
    quarto::quarto_render(dashboard_to_create,
                          execute_params =  list(
                            nsa = nsa,
                            sa = sa,
                            name = series_name,
                            dataset_name = "Quarterly non-financial sector accounts",
                            title = title,
                            ts_start = ts_start,
                            ts_freq = ts_freq,
                            start_date = start_date,
                            default_type = default_type_sel,
                            default_spec_nsa = default_spec_nsa_sel,
                            default_spec_sa = default_spec_sa_sel,
                            java_home = java_home
                          ))
  }

  for (i in seq_along(nsa_sca$id)) {
    level2_validation_eurostat(ts(nsa_sca$data[[i]][[2]], start = c(1999,1),frequency = 4),
                               ts(nsa_sca$data[[i]][[3]], start = c(1999,1),frequency = 4),
                               nsa_sca$id[i])
  }
}

