#' @title NFSA Completeness Aggregation
#'
#' @description This function checks the completeness of NFSA data for a given country and table,
#'   comparing it against predefined data requirements. It identifies missing data points and
#'   exports them to an Excel file.
#'
#' @param country A character vector specifying the country(ies) codes (e.g., "BE").
#' @param table A character string specifying the table identifier ("T0801" or "T0800").  Defaults to "T0801".
#' @param output_sel A character string specifying the output directory for the completeness report.
#'   Defaults to `here::here("output", "completeness")`.
#'
#' @return A data frame containing the missing data points. Returns an empty dataframe if data requirements are fulfilled.
#' @examples
#' \dontrun{
#' # Check completeness for Belgium (BE) for table T0801 and save the report to the default output directory
#' missing_data <- nfsa_completeness_aggregation(country = "BE", table = "T0801")
#'
#' # Check completeness for Germany (DE) for table T0800 and save the report to a custom output directory
#' missing_data <- nfsa_completeness_aggregation(country = "DE", table = "T0800", output_sel = here::here("custom_output"))
#' }
#'
#' @export
nfsa_completeness_aggregation <- function(country ,
                                          table = "T0801",
                                          output_sel = here::here("output", "completeness")) {
  library(tidyverse)
  library(arrow)
  library(openxlsx)


  lookup <- nfsa::nfsa_sto_lookup

  # Initialize dat_missing to avoid scope issues
  dat_missing <- NULL

  if (table == "T0801"){
    requirements <- here::here("assets", "completeness_aggregation.xlsx")

    req_time <- nfsa::nfsa_get_data (country = c("FR","SE"), # first to report
                                     table = "T0801",
                                     filters = "sto == 'B1GQ'") |>
      select(time_period) |>
      filter(time_period>= "1999-Q1") |>
      select(time_period) |>
      distinct() |>
      arrange()

    req <- readxl::read_xlsx(requirements)%>%
      expand_grid(.,req_time,country) |>
      rename(ref_area = country)

    dat <- nfsa_get_data(country = country) |>
      nfsa::nfsa_separate_id() |>
      filter(time_period >= "1999-Q1",
             ref_sector %in% c("S1", "S1N", "S11", "S12", "S13", "S1M", "S2")) |>
      nfsa_unite_id() %>%
      left_join(req,., by = join_by(ref_area,id, time_period))

    dat_missing <- dat |>
      filter(is.na(obs_value))


    if (nrow(dat_missing) == 0) {
      cli::cli_inform(paste0("Data requirements for ",table, " are fulfilled" ))
    } else {

      nfsa::nfsa_to_excel(dat_missing)
      write.xlsx(dat_missing,
                 asTable = TRUE,
                 file = paste0(output_sel, "/completeness_aggregation_",
                               table, "_", as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"),
                 overwrite = TRUE
      )

      cli::cli_alert_success(paste0("File created in ", output_sel, "/completeness_aggregation_",
                                    table, "_", as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"))
    }
  }
  if (table == "T0800"){
    requirements <- here::here("assets", "completeness_aggregation.xlsx")
    req_time <- nfsa::nfsa_get_data (country = c("IT","SE"),
                                     table = "T0800",
                                     filters = "sto == 'B1GQ'") |>
      select(time_period,sto) |>
      filter(time_period>= "1999") |>
      select(time_period) |>
      distinct() |>
      arrange()

    req <- readxl::read_xlsx(requirements)%>%
      expand_grid(.,req_time,country) |>
      rename(ref_area = country)

    dat <- nfsa_get_data(country = country, table = "T0800") |>
      nfsa::nfsa_separate_id() |>
      filter(time_period >= "1999",
             ref_sector %in% c("S1", "S1N", "S11", "S12", "S13", "S1M", "S2")) |>
      nfsa_unite_id() %>%
      left_join(req,., by = join_by(ref_area,id, time_period))

    dat_missing <- dat |>
      filter(is.na(obs_value))|>
      nfsa_separate_id() |>
      filter(!str_detect(sto,"N|M")) #D7N,P5M...



    if (nrow(dat_missing) == 0) {
      cli::cli_inform(paste0("Data requirements for ",table, " are fulfilled" ))
    } else {

      nfsa::nfsa_to_excel(dat_missing)

      write.xlsx(dat_missing,
                 asTable = TRUE,
                 file = paste0(output_sel, "/completeness_aggregation_",
                               table, "_", as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"),
                 overwrite = TRUE
      )

      cli::cli_alert_success(paste0("File created in ", output_sel, "/completeness_aggregation_",
                                    table, "_", as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"))
    }
  }




  return(dat_missing)
}

