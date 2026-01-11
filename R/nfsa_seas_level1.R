#' @title Perform Level 1 Seasonality Checks
#'
#' @description This function performs level 1 seasonality checks on time series data, comparing seasonally adjusted (SCA) and non-seasonally adjusted (NSA) data.
#'   It identifies series with potential issues like residual seasonality, over-adjustment, or inconsistencies between NSA and SCA data.
#'
#' @param country A character vector specifying the countries to process (e.g., "BE", "IT", "FR").  Defaults to c("BE","IT", "FR", "FI", "PL", "EE", "SK", "SI").
#' @param time_min From which quarter the series are analysed, by default "1999-Q1"
#' @param input_sel The path to the directory containing the input data files. Defaults to `"M:/nas/Rprod/data/"`.
#' @param output_sel The path to the directory where the output Excel file will be saved. Defaults to `here::here("output", "seas")`.
#'
#' @return A data frame containing the results of the level 1 seasonality checks, including the `ref_area` (country), `id`, and the `level1` result (either "PASS" or a specific "FAIL" or "WARNING" message).  Also saves the data frame as an Excel file in the `output_sel` directory.
#'
#' @examples
#' \dontrun{
#' # Perform level 1 seasonality checks for Belgium and Italy,
#' # using default input and output directories.
#' results <- nfsa_seas_level1(country = c("BE", "IT"))
#'
#' # Perform level 1 seasonality checks for France, specifying custom
#' # input and output directories.
#' results <- nfsa_seas_level1(country = "FR",
#'                              input_sel = "/path/to/input/data",
#'                              output_sel = "/path/to/output/directory")
#' }
#'
#' @export
nfsa_seas_level1 <- function(country ,
                             time_min = "1999-Q1",
                             input_sel = "M:/nas/Rprod/data/",
                             output_sel = here::here("output", "seas")){

  library(nfsa)
  library(tidyverse)
  library(arrow)
  library(SAvalidation)
  library(openxlsx)
  library(here)

  cli::cli_inform("Collecting data...")
  nsa <- nfsa_get_data(country = country,
                       table = "T0801",
                       type = "new",
                       input_sel = input_sel) |>
    rename(NSA = obs_value)

  sca <- nfsa_get_data(country = country,
                       table = "T0801SA",
                       type = "new",
                       input_sel = input_sel) |>
    rename(SCA = obs_value)

  data <- full_join(nsa, sca, by = join_by(ref_area, id,
                                           time_period)) |>
    filter(time_period >= time_min) |>
    na.omit()

  ## Need to remove series entirely 0's or with several 0's
  tmp <- nsa |>
    filter(NSA == 0) |>
    group_by (ref_area,id) |>
    add_count() |>
    filter(n >10) |>
    select(ref_area,id) |>
	  ungroup() |>
    distinct()


  cli::cli_inform("Running tests...")

  data <- anti_join(data,tmp,join_by(ref_area, id)) |>
    mutate(time_period = lubridate::yq(time_period)) |>
    group_by(ref_area,id) |>
    nest() |>
    mutate(nsa = map(data,~ts(.x$NSA,start = c(min(str_sub(.x$time_period,1,4)),1),frequency = 4)),
           sca = map(data,~ts(.x$SCA,start = c(min(str_sub(.x$time_period,1,4)),1),frequency = 4)),
           level1_X13 = map2(.x = nsa,
                             .y = sca,
                             .f=~SAvalidation::level1_validation(.x,.y,default_type = "X13",
                                                                 default_spec_nsa = "RSA1",
                                                                 default_spec_sa = "RSA2c"))) |>
           # level1_TS = map2(.x = nsa,
           #                  .y = sca,
           #                  .f=~SAvalidation::level1_validation(.x,.y,default_type = "TS", default_spec_sa = "RSA2"))) |>
    select(ref_area,id,level1_X13) |>
    unnest(c(level1_X13)) |>
    select(ref_area, id,level1=level1_X13) |>
    filter(level1 == "FAIL: EVIDENCE OF RESIDUAL SEASONALITY OR CALENDAR EFFECTS IN SA SERIES" |
             level1 == "FAIL: NO EVIDENCE OF SEASONALITY IN NSA BUT SERIES ADJUSTED" |
             level1 == "FAIL: EVIDENCE OF SEASONALITY IN NSA BUT SA IS NOT ADJUSTED"|
             level1 == "WARNING: ANNUAL TOTALS CHECK FAILED"|
             level1 == "WARNING: SA SERIES HAS EVIDENCE OF OVER-ADJUSTMENT"|
             level1 == "WARNING: SA SERIES HAS NEGATIVE VALUES")

  if(nrow(data) == 0){
    cli::cli_alert_success("No issues")
    } else {

  openxlsx::write.xlsx(data, file = paste0(output_sel,"/level1_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"),
                       overwrite = TRUE)

  cli::cli_inform(paste0("File created at ",output_sel,"/level1_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"))
  return(data)}

}
