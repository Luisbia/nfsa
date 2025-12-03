#' @title Show data as in ESA 2010 TP Regulation
#'
#' @description This function takes T0800 data formats the output, and exports it to an Excel file based on a
#'   specified template. It processes data for selected countries and time periods.
#'
#' @param country A character vector specifying the countries to process (e.g., "BE", "NL").
#' @param time_min A numeric value specifying the minimum time period to consider (default: 2020).
#' @param template A character string specifying the file path to the Excel template file. Default: `here("assets","seq_accounts_T0800_reg.xlsx")`.
#' @param output_sel A character string specifying the file path to the directory where the output Excel file will be saved. Default: `here("output","sequence")`.
#'
#' @return None. The function saves an Excel file to the specified output directory.
#'   The filename includes the country code(s) and a timestamp. A success message is printed to the console
#'   indicating the location of the created file.
#'
#' @details The function performs the following steps:
#'   \enumerate{
#'     \item Reads the data from the parquet files using `arrow::open_dataset()`.
#'     \item Filters the data based on sector, time period, and other criteria.
#'     \item Joins the new and previous data based on common identifiers.
#'     \item Saves the Excel file to the output directory with a timestamped filename.
#'   }
#'
#' @examples
#' \dontrun{
#'   # Example usage with specific country and default settings
#'   nfsa_sequence_T0800_reg(country = "BE")
#'
#'   # Example usage with multiple countries and custom time period
#'   nfsa_sequence_T0800_reg(country = c("BE", "NL"), time_min = 2021)
#'
#' }
#'
#' @export
nfsa_sequence_T0800_reg <- function(country,
                                time_min = 2020,
                                template = here("assets","seq_accounts_T0800_reg.xlsx"),
                                output_sel = here("output","sequence")) {
library(tidyverse)
library(arrow)
library(here)
library(openxlsx)

lookup <- nfsa::nfsa_sto_lookup


nfsa_new_data <- nfsa::nfsa_get_data(country = country, table = "T0800", type = "new") |>
  separate_wider_delim(cols = id,
                       delim = ".",
                       names = c("ref_sector", "sto", "accounting_entry")) |>
  filter(ref_sector %in% c("S1", "S1N", "S11", "S12", "S13","S1M","S14","S15", "S2"),
         time_period >= time_min) |>
  na.omit() |>
  unite("id",c(ref_sector,sto,accounting_entry),sep = ".") |>
  select(ref_area,id,time_period,obs_value) |>
  unite("id",c(time_period,ref_area,id),sep = ".")


  nfsa_data <- nfsa_new_data |>
    mutate(obs_value = round(obs_value,1)) |>
    mutate(obs_value = ifelse(is.na(obs_value), ':', obs_value))

  wb <- loadWorkbook(template)
  writeData(wb, "data", nfsa_data, startRow = 1, startCol = 1)


  if(length(country) == 1) {
    saveWorkbook(wb, ,file =paste0(output_sel,"/T0800_",country,"_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),"_seq_accounts_reg.xlsx"))

    cli::cli_alert_success(paste0("File created in ",output_sel,"/T0800_",country,"_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),"_seq_accounts_reg.xlsx"))
  }

  if(length(country) > 1) {
    saveWorkbook(wb, ,file = paste0(output_sel,"/T0800_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),"_seq_accounts_reg.xlsx"))

    cli::cli_alert_success(paste0("File created in ",output_sel,"/T0800_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),"_seq_accounts_reg.xlsx"))
  }

}
