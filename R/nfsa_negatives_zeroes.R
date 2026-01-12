#' @title Identify and Export Negative and Zero Values from NFSA Data
#'
#' @description This function retrieves data from the NFSA database, filters it to identify negative and zero values, performs a basic analysis, and exports the results to an Excel file. It supports different tables ("T0801", "T0800", "T0801SA") and countries.
#'
#' @param table A character string specifying the table to retrieve data from.  Must be one of "T0801", "T0800", or "T0801SA".
#' @param country A character vector of country codes to filter the data by. Defaults to a selection of European countries.
#' @param time_min A character string specifying the minimum time period to include in the data.  Must be in "YYYY-QX" format.  Defaults to "1999-Q1".
#' @param output_sel A character string specifying the directory where the output Excel file should be saved. Defaults to "output/negatives_zeroes" within the project directory (using `here::here()`).
#'
#' @return None.  The function saves an Excel file to the specified `output_sel` directory.  The file contains two sheets: "raw_data" with the filtered data, and "analysis" with a summarized count of negative and zero values. A message confirming file creation and location is printed to the console using `cli::cli_alert_success()`.
#'
#' @examples
#' \dontrun{
#' # Find negative/zero values in table T0801 for Germany (DE) from 2010-Q1 onwards
#' nfsa_negatives_zeroes(table = "T0801", country = "DE", time_min = "2010-Q1")
#'
#' # Find negative/zero values in table T0800 for all default countries from the beginning of the dataset.
#' nfsa_negatives_zeroes(table = "T0800")
#'
#' # Find negative/zero values in table T0801SA for a specific set of countries.
#' nfsa_negatives_zeroes(table = "T0801SA", country = c("US", "CA", "MX"), time_min = "2000-Q1")
#' }
#' @export
nfsa_negatives_zeroes <- function(table = "T0801",
                                  country ,
                                  time_min = "1999-Q1",
                                  output_sel = here::here("output", "negatives_zeroes")) {

  library(tidyverse)
  library(openxlsx)
  library(here)

  if(table == "T0801"){
    zero_negatives <- nfsa::nfsa_get_data(country = country,
                                          table = "T0801",
                                          type = "new") |>
      filter(time_period >= time_min,
             obs_value <=0) |>
      nfsa::nfsa_separate_id() |>
      filter(!sto %in% c("B9", "NP", "B101", "D43", "P5M", "B9X9F", "B11", "B12"))


    zero_negatives_id <- zero_negatives |>
      mutate(type = if_else(obs_value<0, "negative", "zero")) |>
      group_by(ref_area,ref_sector,sto,accounting_entry,type) |>
      tally()


    l <- list(raw_data = zero_negatives,
              analysis = zero_negatives_id)



    write.xlsx(l, file = paste0(output_sel,"/", as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),"_negatives_zeroes_T0801.xlsx"),
               overwrite = TRUE)

    cli::cli_alert_success(paste0("File created in ", output_sel,"/",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),"_negatives_zeroes_T0801.xlsx"))
  }

  if(table == "T0800"){
    zero_negatives <- nfsa::nfsa_get_data(country = country,
                                          table = "T0800",
                                          type = "new") |>
      filter(time_period >= time_min,
             obs_value <=0) |>
      nfsa::nfsa_separate_id() |>
      filter(!sto %in% c("B9", "NP", "B101", "D43", "P5M", "B9X9F", "B11", "B12"))


    zero_negatives_id <- zero_negatives |>
      mutate(type = if_else(obs_value<0, "negative", "zero")) |>
      group_by(ref_area,ref_sector,sto,accounting_entry,type) |>
      tally()


    l <- list(raw_data = zero_negatives,
              analysis = zero_negatives_id)

    write.xlsx(l, file = paste0(output_sel,"/", as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),"_negatives_zeroes_T0800.xlsx"),
               overwrite = TRUE)

    cli::cli_alert_success(paste0("File created in ", output_sel,"/",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),"_negatives_zeroes_T0800.xlsx"))
  }

  if(table == "T0801SA"){
    zero_negatives <- nfsa::nfsa_get_data(country = country,
                                          table = "T0801SA",
                                          type = "new") |>
      filter(time_period >= time_min,
             obs_value <=0) |>
      nfsa::nfsa_separate_id() |>
      filter(!sto %in% c("B9", "NP", "B101", "D43", "P5M", "B9X9F", "B11", "B12"))


    zero_negatives_id <- zero_negatives |>
      mutate(type = if_else(obs_value<0, "negative", "zero")) |>
      group_by(ref_area,ref_sector,sto,accounting_entry,type) |>
      tally()


    l <- list(raw_data = zero_negatives,
              analysis = zero_negatives_id)

    write.xlsx(l, file = paste0(output_sel,"/", as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),"_negatives_zeroes_T0801SA.xlsx"),
               overwrite = TRUE)

    cli::cli_alert_success(paste0("File created in ", output_sel,"/",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),"_negatives_zeroes_T0801SA.xlsx"))
  }
  return(l)
}
