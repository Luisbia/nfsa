#' @title Check NFSA Completeness Against Regulation
#'
#' @description This function checks the completeness of NFSA (National Financial Statistics Authority)
#' data against predefined regulatory requirements. It identifies missing data points
#' for a specified country and table, and generates an Excel file detailing these
#' gaps if any exist.
#'
#' @param country A character vector specifying the countries for which to check data completeness.
#' @param table A character string indicating the data table to check (e.g., "T0801", "T0801SA", "T0800").  Defaults to "T0801".
#' @param output_sel A file path specifying the directory where the output Excel file should be saved. Defaults to "output/completeness" inside the project.
#'
#' @return Invisible. The function's primary side effect is to produce an Excel file when data is missing.
#'   It also prints messages to the console indicating whether the data requirements are fulfilled or
#'   where the output file is saved.
#'
#' @details The function reads data from parquet files located in specific directories ("data/q/new/nsa" for quarterly data and "data/a/new" for annual)
#'   and compares it to requirements defined in Excel files located in the "assets" directory ("completeness_T0801.xlsx",
#'   "completeness_T0801SA.xlsx", or "completeness_T0800.xlsx" depending on the `table` argument).
#'   It performs joins, filters, and transformations on the data to identify missing observations based on
#'   the regulatory requirements.
#'
#'   The function uses `nfsa_separate_id()` (not defined in this stub) for a critical step of data processing. Ensure that this function is correctly defined or sourced in your environment.
#'
#'   The completeness requirements differ based on whether the country is considered a "small" country (BG, EE, HR, CY, LT, LV, LU, MT, SI, SK)
#'   or a "big" country (AT, BE, CZ, DE, DK, EL, ES, FI, FR, HU, IE, IT, NL, PL, PT, RO, SE). The requirements for "ALL" countries in the Excel
#'   files are applied to small countries.  This distinction only applies to the "T0801" and "T0801SA" tables.
#'
#' @examples
#' \dontrun{
#' # Check completeness for Germany for table T0801 and save the output in "my_output" directory
#' nfsa_completeness_regulation(country = "DE", table = "T0801", output_sel = "my_output")
#'
#' # Check completeness for multiple countries for table T0800
#' nfsa_completeness_regulation(country = c("DE", "FR", "IT"), table = "T0800")
#' }
#' @export
nfsa_completeness_regulation <- function(country  ,
                                         table = "T0801",
                                         output_sel = here::here("output", "completeness")) {
  library(tidyverse)
  library(readxl)
  library(arrow)
  library(openxlsx)
  lookup <- nfsa::nfsa_sto_lookup
  small <- c("BG", "EE", "HR", "CY", "LT", "LV", "LU", "MT", "SI", "SK")
  big <- c("AT", "BE", "CZ", "DE", "DK","EL", "ES", "FI", "FR", "HU", "IE", "IT",
           "NL", "PL", "PT", "RO", "SE")
  if (table == "T0801"){
    requirements <- here::here("assets", "completeness_T0801.xlsx")
    req <- readxl::read_xlsx(requirements)
    req_time <- list.files(path = here("data", "q", "new", "nsa"),
                           pattern = ".parquet",
                           full.names = TRUE) |>
      open_dataset() |>
      select(time_period) |>
      filter(time_period>= "1999-Q1") |>
      distinct() |>
      collect()

    req_small <- req |>
      filter(countries == "ALL") |>
      select(-countries)  %>%
      cross_join(.,data.frame(ref_area = small))

    req_big <- req |>
      select(-countries)  %>%
      cross_join(.,data.frame(ref_area = big))

    req <- bind_rows(req_small, req_big) |>
      filter(ref_area %in% country)%>%
      cross_join(., req_time)

    rm(req_small,req_big, req_time)


    dat <- list.files(path = here("data", "q", "new", "nsa"),
                      pattern = ".parquet",
                      full.names = TRUE) |>
      as_tibble() |>
      mutate(
        version = as.numeric(str_extract(value, "(?<=_Q_..............).{4}")),
        country_sel = str_extract(value, "(?<=_Q_)..")
      ) |>
      filter(country_sel %in% country) |>
      group_by(country_sel) |>
      arrange(version) |>
      slice_tail(n = 1) |>
      pull(value) |>
      open_dataset() |>
      select(-received, -embargo_date,-version) |>
      filter(time_period >= "1999-Q1",
             ref_sector %in% c("S1", "S1N", "S11", "S12", "S13", "S1M")) |>
      collect()  %>%
      left_join(req,.,by = join_by(table_identifier, freq,
                                   adjustment, counterpart_area, ref_sector,
                                   counterpart_sector, consolidation, accounting_entry,
                                   sto, instr_asset, maturity, expenditure,
                                   unit_measure, currency_denom, valuation, prices,
                                   transformation, cust_breakdown, ref_area, time_period)) %>%
      left_join(.,lookup,by = join_by(counterpart_area,
                                      ref_sector, counterpart_sector, consolidation,
                                      accounting_entry, sto, instr_asset, unit_measure,
                                      prices)) |>
      select(ref_area,table_identifier,id,time_period,obs_value,obs_status) |>
      nfsa_separate_id()

    dat_missing <- dat |>
      filter(is.na(obs_value))


    if (nrow(dat_missing) == 0) {
      cli::cli_inform(paste0("Data requirements for ",table, " are fulfilled" ))
    } else {


      write.xlsx(dat_missing,
                 asTable = TRUE,
                 file = paste0(output_sel, "/completeness_regulation_",
                               table,"_", as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"),
                 overwrite = TRUE
      )

      cli::cli_alert_success(paste0("File created in ", output_sel, "/completeness_regulation_",
                                    table,"_", as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"))
    }
  }
  if (table == "T0801SA"){
    requirements <- here::here("assets", "completeness_T0801SA.xlsx")

    req <- readxl::read_xlsx(requirements)
    req_time <- list.files(path = here("data", "q", "new", "sca"),
                           pattern = ".parquet",
                           full.names = TRUE) |>
      open_dataset() |>
      select(time_period) |>
      filter(time_period>= "1999-Q1") |>
      distinct() |>
      collect()

    req_small <- req |>
      filter(countries == "ALL") |>
      select(-countries)  %>%
      cross_join(.,data.frame(ref_area = small))

    req_big <- req |>
      select(-countries)  %>%
      cross_join(.,data.frame(ref_area = big))

    req <- bind_rows(req_small, req_big) |>
      filter(ref_area %in% country)%>%
      cross_join(., req_time)

    rm(req_small,req_big, req_time)


    dat <- list.files(path = here("data", "q", "new", "sca"),
                      pattern = ".parquet",
                      full.names = TRUE) |>
      as_tibble() |>
      mutate(
        version = as.numeric(str_extract(value, "(?<=_Q_..............).{4}")),
        country_sel = str_extract(value, "(?<=_Q_)..")
      ) |>
      filter(country_sel %in% country) |>
      group_by(country_sel) |>
      arrange(version) |>
      slice_tail(n = 1) |>
      pull(value) |>
      open_dataset() |>
      select(-received, -embargo_date,-version) |>
      filter(time_period >= "1999-Q1",
             ref_sector %in% c("S1", "S1N", "S11", "S12", "S13", "S1M")) |>
      collect()  %>%
      left_join(req,.,by = join_by(table_identifier, freq,
                                   adjustment, counterpart_area, ref_sector,
                                   counterpart_sector, consolidation, accounting_entry,
                                   sto, instr_asset, maturity, expenditure,
                                   unit_measure, currency_denom, valuation, prices,
                                   transformation, cust_breakdown, ref_area,time_period)) %>%
      left_join(.,lookup,by = join_by(counterpart_area,
                                      ref_sector, counterpart_sector, consolidation,
                                      accounting_entry, sto, instr_asset, unit_measure,
                                      prices)) |>
      select(ref_area,table_identifier,id,time_period,obs_value,obs_status) |>
      nfsa_separate_id()

    dat_missing <- dat |>
      filter(is.na(obs_value))


    if (nrow(dat_missing) == 0) {
      cli::cli_inform(paste0("Data requirements for ",table, " are fulfilled" ))
    } else {


      write.xlsx(dat_missing,
                 asTable = TRUE,
                 file = paste0(output_sel, "/completeness_regulation_",
                               table,"_", as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"),
                 overwrite = TRUE
      )

      cli::cli_alert_success(paste0("File created in ", output_sel, "/completeness_regulation_",
                                    table, "_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"))
    }

  }
  if (table == "T0800"){
    requirements <- here::here("assets", "completeness_T0800.xlsx")

    req <- readxl::read_xlsx(requirements)

    req_time <- list.files(path = here("data", "a", "new"),
                           pattern = ".parquet",
                           full.names = TRUE) |>
      open_dataset() |>
      select(time_period) |>
      filter(time_period>= "1995") |>
      distinct() |>
      collect()

    req <- req %>%
      cross_join(.,data.frame(ref_area = country))%>%
      cross_join(., req_time)



    dat <- list.files(path = here("data", "a", "new"),
                      pattern = ".parquet",
                      full.names = TRUE) |>
      as_tibble() |>
      mutate(
        version = as.numeric(str_extract(value, "(?<=_A_..............).{4}")),
        country_sel = str_extract(value, "(?<=_A_)..")
      ) |>
      filter(country_sel %in% country) |>
      group_by(country_sel) |>
      arrange(version) |>
      slice_tail(n = 1) |>
      pull(value) |>
      open_dataset() |>
      select(-received, -embargo_date,-version) |>
      filter(time_period >= "1995",
             ref_sector %in% c("S1", "S1N", "S11", "S12", "S13", "S1M")) |>
      collect()  %>%
      left_join(req,.,by = join_by(table_identifier, freq,
                                   adjustment, counterpart_area, ref_sector,
                                   counterpart_sector, consolidation, accounting_entry,
                                   sto, instr_asset, maturity, expenditure,
                                   unit_measure, currency_denom, valuation, prices,
                                   transformation, cust_breakdown, ref_area,time_period)) %>%
      left_join(.,lookup,by = join_by(counterpart_area,
                                      ref_sector, counterpart_sector, consolidation,
                                      accounting_entry, sto, instr_asset, unit_measure,
                                      prices)) |>
      select(ref_area,table_identifier,id,time_period,obs_value,obs_status) |>
      nfsa_separate_id()

    dat_missing <- dat |>
      filter(is.na(obs_value))


    if (nrow(dat_missing) == 0) {
      cli::cli_inform(paste0("Data requirements for ",table, " are fulfilled" ))
    } else {



      write.xlsx(dat_missing,
                 asTable = TRUE,
                 file = paste0(output_sel, "/completeness_regulation_",
                               table, "_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"),
                 overwrite = TRUE
      )

      cli::cli_alert_success(paste0("File created in ", output_sel, "/completeness_regulation_",
                                    table, "_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"))
    }
  }
  return(dat_missing)
}

