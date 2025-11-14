nfsa_completeness_aggregation <- function(country ,
                                          table = "T0801",
                                          output_sel = here::here("output", "completeness")) {
  library(tidyverse)
  library(readxl)
  library(arrow)
  library(openxlsx)
  lookup <- nfsa::nfsa_sto_lookup

  if (table == "T0801"){
    requirements <- here::here("assets", "completeness_aggregation_Q.xlsx")

    req_time <- list.files(path = here("data", "q", "new", "nsa"),
                           pattern = ".parquet",
                           full.names = TRUE) |>
      open_dataset() |>
      select(time_period) |>
      filter(time_period>= "1999-Q1") |>
      distinct() |>
      collect()

    req <- readxl::read_xlsx(requirements)%>%
      cross_join(.,req_time)

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
    requirements <- here::here("assets", "completeness_aggregation_A.xlsx")
    req_time <- list.files(path = here("data", "a", "new"),
                           pattern = ".parquet",
                           full.names = TRUE) |>
      open_dataset() |>
      select(time_period) |>
      filter(time_period>= "1999") |>
      distinct() |>
      collect()

    req <- readxl::read_xlsx(requirements)%>%
      cross_join(.,req_time)

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
}

