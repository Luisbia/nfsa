#' Perform Level 3 Seasonal Adjustment Validation for Eurostat Data
#'
#' This function performs Level 3 seasonal adjustment validation on Eurostat
#' data. It retrieves data, joins NSA and SCA series, and generates validation
#' reports using a Quarto dashboard.
#'
#' @param series Path to an Excel file containing a list of series to process.
#'   Defaults to `here("assets", "seas_level1.xlsx")`. This file should contain
#'   at least `ref_area` and `id` columns.
#' @param time_min from which quarter the analysis starts
#' @param input_sel Path to the directory where input data is stored. Defaults
#'   to `"M:/nas/Rprod/data/"`.
#' @param output_sel Path to the directory where output reports should be
#'   saved. Defaults to `here::here("output", "seas")`.
#' @param dashboard_sel Path to the Quarto dashboard template. Defaults to
#'   `here::here("assets", "level3_eurostat.qmd")`.
#' @param default_type_sel Default seasonal adjustment type. Defaults to `"X13"`.
#' @param default_spec_nsa_sel Default specification for NSA series. Defaults to
#'   `"RSA1"`.
#' @param default_spec_sa_sel Default specification for SA series. Defaults to
#'   `"RSA2c"`.
#'
#' @return This function does not explicitly return a value.  It generates
#'   Quarto dashboard reports and saves them to the `output_sel` directory.
#'
#' @details The function performs the following steps:
#'   \enumerate{
#'     \item Reads the list of series from the Excel file specified by `series`.
#'     \item Retrieves Non-Seasonally Adjusted (NSA) and Seasonally Adjusted (SCA)
#'       data from Eurostat using `nfsa_get_data`.
#'     \item Joins the NSA and SCA series based on `ref_area`, `id`, and
#'       `time_period`.
#'     \item Creates time series objects from the joined data.
#'     \item Generates Level 3 validation reports for each series using the
#'       specified Quarto dashboard template.
#'   }
#'
#' @examples
#' \dontrun{
#' # Example usage with default parameters
#' nfsa_seas_level3()
#'
#' # Example usage with custom output directory
#' nfsa_seas_level3(output_sel = here::here("my_output"))
#' }
#'
#' @export
nfsa_seas_level3 <- function(series = here::here("assets", "seas_level1.xlsx"),
                             time_min = "1999-Q1",
                             input_sel = "M:/nas/Rprod/data/",
                             output_sel = here::here("output", "seas"),
                             dashboard_sel = here::here("assets", "level3_eurostat.qmd"),
                             default_type_sel = "X13",
                             default_spec_nsa_sel = "RSA1",
                             default_spec_sa_sel = "RSA2c"){

  series_list <- readxl::read_xlsx(series)

  nsa <- nfsa_get_data(country = unique(series_list$ref_area),
                       table = "T0801",
                       type = "new") |>
    dplyr::rename(NSA = obs_value)

  sca <- nfsa_get_data(country = unique(series_list$ref_area),
                       table = "T0801SA",
                       type = "new")|>
    dplyr::rename(SCA = obs_value)

  nsa_sca <- dplyr::full_join(nsa,sca,by = dplyr::join_by(ref_area, id, time_period) ) |>
    dplyr::filter(time_period >= time_min)
  nsa_sca <- dplyr::left_join(series_list, nsa_sca,by = dplyr::join_by(ref_area, id) ) |>
    dplyr::mutate(time_period = lubridate::yq(time_period)) |>
    dplyr::arrange(time_period) |>
    tidyr::unite(col = "id", c(ref_area,id),sep ="_") |>
    tidyr::nest(.by = id)

  level3_validation_eurostat <- function(nsa,sa,series_name,
                                         dataset_name = "Quarterly non-financial sector accounts",
                                         title = series_name,
                                         output_directory = output_sel,
                                         dashboard_template= dashboard_sel,
                                         java_home = Sys.getenv("JAVA_HOME")){
    if(is.null(output_directory)){
      output_directory <- getwd()
    }
    if(!dir.exists(output_directory)){
      dir.create(output_directory,recursive = TRUE)
    }


    if (dashboard_template == "skeleton.qmd"){
      dashboard_template_to_copy <- file.path(system.file("rmarkdown/templates/level3_report/skeleton3",
                                                          package="SAvalidation"),dashboard_template)
    } else {
      dashboard_template_to_copy <- dashboard_template
    }


    dashboard_to_create <- file.path(paste0(output_directory,"/level3_",series_name,"_",default_type_sel,".qmd"))

    file.copy(dashboard_template_to_copy,dashboard_to_create,overwrite = TRUE)





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
                            default_type = default_type_sel,
                            default_spec_nsa = default_spec_nsa_sel,
                            default_spec_sa = default_spec_sa_sel,
                            java_home = java_home
                          ))

  }

  for (i in seq_along(nsa_sca$id)) {
    level3_validation_eurostat(stats::ts(nsa_sca$data[[i]][[2]], start = c(stringr::str_sub(nsa_sca$data[[i]][[1]][[1]],1,4),
                                                                    stringr::str_sub(nsa_sca$data[[i]][[1]][[1]],7,7)),
                                  frequency = 4),
                               stats::ts(nsa_sca$data[[i]][[3]], start = c(stringr::str_sub(nsa_sca$data[[i]][[1]][[1]],1,4),
                                                                    stringr::str_sub(nsa_sca$data[[i]][[1]][[1]],7,7)),
                                  frequency = 4),
                               nsa_sca$id[i])
  }
}
