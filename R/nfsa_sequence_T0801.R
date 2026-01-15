#' @title Show data or revisions as sequence of accounts
#'
#' @description This function takes T0801 data or revisions, formats the output, and exports it to an Excel file based on a
#'   specified template. It processes data for selected countries and time periods.
#'
#' @param country A character vector specifying the countries to process (e.g., "BE", "NL").
#' @param time_min A numeric value specifying the minimum time period to consider (default: "2020-Q1").
#' @param type Either "values" by default or "revisions"
#' @param template A character string specifying the file path to the Excel template file. Default: `here("assets","seq_accounts_T0801.xlsx")`.
#' @param output_sel A character string specifying the file path to the directory where the output Excel file will be saved. Default: `here("output","sequence")`.
#'
#' @return None. The function saves an Excel file to the specified output directory.
#'   The filename includes the country code(s) and a timestamp. A success message is printed to the console
#'   indicating the location of the created file, or if there are no revisions.
#'
#' @details The function performs the following steps:
#'   \enumerate{
#'     \item Reads the data from the parquet files using `arrow::open_dataset()`.
#'     \item Filters the data based on sector, time period, and other criteria.
#'     \item Joins the new and previous data based on common identifiers.
#'     \item Calculates the revisions (new - previous).
#'     \item Formats the revisions and creates an Excel file using the specified template via `openxlsx`.
#'     \item Saves the Excel file to the output directory with a timestamped filename.
#'   }
#'
#' @examples
#' \dontrun{
#'   # Example usage with specific country and default settings
#'   nfsa_sequence_T0801(country = "BE", type = "values)
#'
#'   # Example usage with multiple countries and custom time period
#'   nfsa_sequence_T0801(country = c("BE", "NL"), time_min = 2021, type = "revisions)
#'
#' }
#'
#' @export
nfsa_sequence_T0801 <- function(country,
                                     time_min = "2020-Q1",
                                     type = "values",
                                     template = here("assets","seq_accounts_T0801.xlsx"),
                                     output_sel = here("output","sequence")) {

  library(tidyverse)
  library(arrow)
  library(here)
  library(openxlsx)

  lookup <- nfsa::nfsa_sto_lookup


  nfsa_new_data <- nfsa::nfsa_get_data(country = country, table = "T0801", type = "new") |>
    separate_wider_delim(cols = id,
                         delim = ".",
                         names = c("ref_sector", "sto", "accounting_entry")) |>
    filter(ref_sector %in% c("S1", "S1N", "S11", "S12", "S13","S1M", "S2"),
           time_period >= time_min) |>
    na.omit() |>
    unite("id",c(ref_sector,sto,accounting_entry), sep = ".") |>
    na.omit() |>
    select(ref_area,id,time_period,obs_value) |>
    unite("id",c(time_period,ref_area,id),sep = ".") |>
    rename(new = obs_value)

  nfsa_prev_data <- nfsa::nfsa_get_data(country = country, table = "T0801", type = "prev") |>
    separate_wider_delim(cols = id,
                         delim = ".",
                         names = c("ref_sector", "sto", "accounting_entry")) |>
    filter(ref_sector %in% c("S1", "S1N", "S11", "S12", "S13", "S14", "S15","S1M", "S2"),
           time_period >= time_min) |>
    na.omit() |>
    unite("id",c(ref_sector,sto,accounting_entry),sep = ".") |>
    na.omit() |>
    select(ref_area,id,time_period,obs_value) |>
    unite("id",c(time_period,ref_area,id),sep = ".") |>
    rename(prev = obs_value)


  if (type == "values"){

    nfsa_data <- nfsa_new_data |>
      rename(obs_value = new) |>
      mutate(obs_value = round(obs_value,1)) |>
      mutate(obs_value = ifelse(is.na(obs_value), ':', obs_value))

    wb <- loadWorkbook(template)
    writeData(wb, "data", nfsa_data, startRow = 1, startCol = 1)


    if(length(country) == 1) {
      saveWorkbook(wb, file =paste0(output_sel,"/T0801_",country,"_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),"_seq_accounts.xlsx"))

      cli::cli_alert_success(paste0("File created in ",output_sel,"/T0801_",country,"_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),"_seq_accounts.xlsx"))
    }

    if(length(country) > 1) {
      saveWorkbook(wb, ,file = paste0(output_sel,"/T0801_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),"_seq_accounts.xlsx"))

      cli::cli_alert_success(paste0("File created in ",output_sel,"/T0801_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),"_seq_accounts.xlsx"))
    }

  }
  if (type == "vintages"){
    nfsa_data <- full_join(nfsa_new_data,nfsa_prev_data,by = join_by(id)) |>
      mutate(obs_value = round(new-prev)) |>
      mutate(obs_value = ifelse(is.na(obs_value), ':', obs_value)) |>
      select(-new,-prev)

    if (nrow(nfsa_data) == 0) {
      cli::cli_alert_success("No revisions in T0801!")}


    wb <- loadWorkbook(template)
    writeData(wb, "data", nfsa_data, startRow = 1, startCol = 1)

    if(length(country) == 1) {
      saveWorkbook(wb, ,file =paste0(output_sel,"/T0801_",country,"_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),"_seq_accounts.xlsx"))

      cli::cli_alert_success(paste0("File created in ",output_sel,"/T0801_",country,"_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),"_seq_accounts.xlsx"))
    }

    if(length(country) > 1) {
      saveWorkbook(wb, ,file = paste0(output_sel,"/T0801_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),"_seq_accounts.xlsx"))

      cli::cli_alert_success(paste0("File created in ",output_sel,"/T0801_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),"_seq_accounts.xlsx"))
    }
  }

  if (type == "versions"){
    nfsa_data <- nfsa_get_data_all(country = country,table = "T0801") |>
      filter(time_period >= time_min) |>
      group_by(ref_area,id,time_period) |>
      arrange(version,.by_group = TRUE) |>
      mutate(obs_value = round(obs_value-lag(obs_value))) |>
      mutate(obs_value = ifelse(is.na(obs_value), ':', obs_value)) |>
      ungroup() |>
      unite("id",c(version,time_period,ref_area,id),sep = ".")

    if (nrow(nfsa_data) == 0) {
      cli::cli_alert_success("No revisions in T0801!")}


    wb <- loadWorkbook(template)
    writeData(wb, "data", nfsa_data, startRow = 1, startCol = 1)

    if(length(country) == 1) {
      saveWorkbook(wb, file =paste0(output_sel,"/T0801_",country,"_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),"_seq_accounts.xlsx"))

      cli::cli_alert_success(paste0("File created in ",output_sel,"/T0801_",country,"_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),"_seq_accounts.xlsx"))
    }

    if(length(country) > 1) {
      saveWorkbook(wb, file = paste0(output_sel,"/T0801_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),"_seq_accounts.xlsx"))

      cli::cli_alert_success(paste0("File created in ",output_sel,"/T0801_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),"_seq_accounts.xlsx"))
    }
  }
}

