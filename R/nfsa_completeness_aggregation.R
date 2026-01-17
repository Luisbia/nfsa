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
  lookup <- nfsa::nfsa_sto_lookup

  # Initialize dat_missing to avoid scope issues
  dat_missing <- NULL

  if (table == "T0801"){
    requirements <- here::here("assets", "completeness_aggregation.xlsx")

    req_time <- list.files(path = "M:/nas/Rprod/data/q/new/nsa/",
                           pattern = ".parquet",
                           full.names = TRUE) |>
      open_dataset() |>
      select(time_period) |>
      filter(time_period>= "1999-Q1") |>
      distinct() |>
      collect()

    req <- readxl::read_xlsx(requirements)%>%
      cross_join(.,req_time)

    dat <- list.files(path = "M:/nas/Rprod/data/q/new/nsa/",
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
      left_join(.,lookup,by = join_by(counterpart_area,
                                      ref_sector, counterpart_sector, consolidation,
                                      accounting_entry, sto, instr_asset, unit_measure,
                                      prices)) |>
      select(ref_area,table_identifier,id,time_period,obs_value,obs_status) %>%
      left_join(req,., by = join_by(id, time_period))

    dat_missing <- dat |>
      filter(is.na(obs_value))|>
      nfsa_separate_id()


    if (nrow(dat_missing) == 0) {
      cli::cli_inform(paste0("Data requirements for ",table, " are fulfilled" ))
    } else {


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
    req_time <- list.files(path = "M:/nas/Rprod/data/a/new/",
                           pattern = ".parquet",
                           full.names = TRUE) |>
      open_dataset() |>
      select(time_period) |>
      filter(time_period>= "1999") |>
      distinct() |>
      collect()

    req <- readxl::read_xlsx(requirements)%>%
      cross_join(.,req_time)

    dat <- list.files(path = "M:/nas/Rprod/data/a/new/",
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
      filter(time_period >= "1999",
             ref_sector %in% c("S1", "S1N", "S11", "S12", "S13", "S1M")) |>
      collect()  %>%
      left_join(.,lookup,by = join_by(counterpart_area,
                                      ref_sector, counterpart_sector, consolidation,
                                      accounting_entry, sto, instr_asset, unit_measure,
                                      prices)) |>
      select(ref_area,table_identifier,id,time_period,obs_value,obs_status) %>%
      left_join(req,., by = join_by(id, time_period))

    dat_missing <- dat |>
      filter(is.na(obs_value))|>
      nfsa_separate_id()



    if (nrow(dat_missing) == 0) {
      cli::cli_inform(paste0("Data requirements for ",table, " are fulfilled" ))
    } else {


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

  # Check if table parameter was valid
  if (is.null(dat_missing)) {
    cli::cli_abort("Invalid table parameter. Must be 'T0801' or 'T0800'.")
  }

  nfsa::nfsa_to_excel(dat_missing)
  return(dat_missing)
}

