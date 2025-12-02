#' @title Analyze and Report Observation Status
#'
#' @description This function analyzes observation status data from NFSA datasets,
#'   aggregates it by country, statistical object (STO), and time period, and
#'   writes the results to an Excel file. It supports both annual (T0800) and
#'   quarterly (T0801) datasets, as well as new and previous versions.
#'
#' @param table Character string indicating the table to analyze.  Must be
#'   "T0800" or "T0801".  Defaults to "T0801".
#' @param type Character string indicating whether to process "new" or "prev"
#'   (previous) data. Defaults to "new".
#' @param input_sel Character string specifying the base path to the data
#'   directory. Defaults to `here::here("data")`.
#' @param output_sel Character string specifying the path to the output
#'   directory where the Excel file will be saved. Defaults to
#'   `here::here()`.
#'
#' @return None. The function's primary effect is to write an Excel file
#'   containing the analysis results to the specified `output_sel`.  It also
#'   prints a success message to the console using `cli::cli_alert_success()`.
#'
#' @details The function reads data from arrow files located in subdirectories
#'   of the `input_sel` directory, based on the `table` and `type`
#'   parameters. It then joins the data with a lookup table (`nfsa::nfsa_sto_lookup`),
#'   performs aggregations, and writes the results to an Excel file. The file
#'   name includes a timestamp and indicates the dataset being analyzed.
#'
#' @examples
#' \dontrun{
#' # Analyze the new T0801 data and save the output to the default location
#' nfsa_obs_status(table = "T0801", type = "new")
#'
#' # Analyze the previous T0800 data and save the output to a specific folder
#' nfsa_obs_status(table = "T0800", type = "prev", output_sel = here::here("my_output"))
#' }
#' @export
nfsa_obs_status <- function(table = "T0801",
                            type = "new",
                            input_sel = here::here("data"),
                            output_sel = here::here("output", "flags")){

  library(tidyverse)
  library(openxlsx)
  library(arrow)

  lookup <- nfsa::nfsa_sto_lookup
  tst <- paste0(table,type)

  ###T0800----
  if (tst == "T0800new"){
    obs_status<- list.files(path = here::here("data","a", "new"),
                                 recursive = FALSE,
                                 full.names = TRUE) |>
    as_tibble() |>
    mutate(version = as.numeric(str_extract(value, "(?<=_A_..............).{4}")),
           country = str_extract(value, "(?<=_A_)..")) |>
    group_by(country) |>
    arrange(version) |>
    slice_tail(n=1) |>
    pull(value) |>
    open_dataset() |>
    select(-embargo_date,-received) |>
    collect() %>%
    left_join(.,lookup,by = join_by(counterpart_area, ref_sector, counterpart_sector,
                                    consolidation, accounting_entry, sto, instr_asset, unit_measure, prices)) |>
    na.omit() |>
    select(ref_area,id,time_period,obs_status) |>
      filter(time_period >= 1995)

  obs_status_country <- obs_status|>
    group_by(ref_area,obs_status) |>
    tally() |>
    pivot_wider(names_from = obs_status,
                values_from = n)

  obs_status_sto <- obs_status|>
    group_by(id,obs_status) |>
    tally() |>
    pivot_wider(names_from = obs_status,
                values_from = n)

  obs_status_year <- obs_status|>
    group_by(time_period,obs_status) |>
    tally() |>
    pivot_wider(names_from = obs_status,
                values_from = n)

  l <- list(country = obs_status_country,
            sto = obs_status_sto,
            year= obs_status_year)



  write.xlsx(l, file = paste0(output_sel,
                             as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),"_obs_status_T0800_new.xlsx"),
             overwrite = TRUE)

  cli::cli_alert_success(paste0("File created in ", output_sel,as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),"_obs_status_T0800_new.xlsx"))

  } else if (tst == "T0800prev"){
  obs_status<- list.files(path = here::here("data","a", "prev"),
                          recursive = FALSE,
                          full.names = TRUE) |>
    as_tibble() |>
    mutate(version = as.numeric(str_extract(value, "(?<=_A_..............).{4}")),
           country = str_extract(value, "(?<=_A_)..")) |>
    group_by(country) |>
    arrange(version) |>
    slice_tail(n=1) |>
    pull(value) |>
    open_dataset() |>
    select(-embargo_date,-received) |>
    collect() %>%
    left_join(.,lookup,by = join_by(counterpart_area, ref_sector, counterpart_sector,
                                    consolidation, accounting_entry, sto, instr_asset, unit_measure, prices)) |>
    na.omit() |>
    select(ref_area,id,time_period,obs_status)|>
    filter(time_period >= 1995)

obs_status_country <- obs_status|>
  group_by(ref_area,obs_status) |>
  tally() |>
  pivot_wider(names_from = obs_status,
              values_from = n)

obs_status_sto <- obs_status|>
  group_by(id,obs_status) |>
  tally() |>
  pivot_wider(names_from = obs_status,
              values_from = n)

obs_status_year <- obs_status|>
  group_by(time_period,obs_status) |>
  tally() |>
  pivot_wider(names_from = obs_status,
              values_from = n)

l <- list(country = obs_status_country,
          sto = obs_status_sto,
          year= obs_status_year)

write.xlsx(l, file = paste0(output_sel,
                            as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),"_obs_status_A_prev.xlsx"),
           overwrite = TRUE)

cli::cli_alert_success(paste0("File created in ", output_sel,as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),"_obs_status_T0800_prev.xlsx"))
} else if  (tst == "T0801new"){
  obs_status<- list.files(path = here::here("data","q", "new", "nsa"),
                          recursive = FALSE,
                          full.names = TRUE) |>
    as_tibble() |>
    mutate(version = as.numeric(str_extract(value, "(?<=_Q_..............).{4}")),
           country = str_extract(value, "(?<=_Q_)..")) |>
    group_by(country) |>
    arrange(version) |>
    slice_tail(n=1) |>
    pull(value) |>
    open_dataset() |>
    select(-embargo_date,-received) |>
    collect() %>%
    left_join(.,lookup,by = join_by(counterpart_area, ref_sector, counterpart_sector,
                                    consolidation, accounting_entry, sto, instr_asset, unit_measure, prices)) |>
    na.omit() |>
    select(ref_area,id,time_period,obs_status) |>
    filter(time_period >= "1999-Q1")

  obs_status_country <- obs_status|>
    group_by(ref_area,obs_status) |>
    tally() |>
    pivot_wider(names_from = obs_status,
                values_from = n)

  obs_status_sto <- obs_status|>
    group_by(id,obs_status) |>
    tally() |>
    pivot_wider(names_from = obs_status,
                values_from = n)

  obs_status_quarter <- obs_status|>
    group_by(time_period,obs_status) |>
    tally() |>
    pivot_wider(names_from = obs_status,
                values_from = n)

  l <- list(country = obs_status_country,
            sto = obs_status_sto,
            quarter= obs_status_quarter)

  write.xlsx(l, file = paste0(output_sel,
                              as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),"_obs_status_T0801_new.xlsx"),
             overwrite = TRUE)

  cli::cli_alert_success(paste0("File created in ", output_sel,as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),"_obs_status_T0801_new.xlsx"))

  } else if (tst == "T0801prev"){
  obs_status<- list.files(path = here::here("data","q", "prev", "nsa"),
                          recursive = FALSE,
                          full.names = TRUE) |>
    as_tibble() |>
    mutate(version = as.numeric(str_extract(value, "(?<=_Q_..............).{4}")),
           country = str_extract(value, "(?<=_Q_)..")) |>
    group_by(country) |>
    arrange(version) |>
    slice_tail(n=1) |>
    pull(value) |>
    open_dataset() |>
    select(-embargo_date,-received) |>
    collect() %>%
    left_join(.,lookup,by = join_by(counterpart_area, ref_sector, counterpart_sector,
                                    consolidation, accounting_entry, sto, instr_asset, unit_measure, prices)) |>
    na.omit() |>
    select(ref_area,id,time_period,obs_status)|>
    filter(time_period >= "1999-Q1")

  obs_status_country <- obs_status|>
    group_by(ref_area,obs_status) |>
    tally() |>
    pivot_wider(names_from = obs_status,
                values_from = n)

  obs_status_sto <- obs_status|>
    group_by(id,obs_status) |>
    tally() |>
    pivot_wider(names_from = obs_status,
                values_from = n)

  obs_status_quarter <- obs_status|>
    group_by(time_period,obs_status) |>
    tally() |>
    pivot_wider(names_from = obs_status,
                values_from = n)

  l <- list(country = obs_status_country,
            sto = obs_status_sto,
            quarter= obs_status_quarter)

  write.xlsx(l, file = paste0(output_sel,
                              as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),"_obs_status_T0801_prev.xlsx"),
             overwrite = TRUE)

  cli::cli_alert_success(paste0("File created in ", output_sel,as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),"_obs_status_T0801_prev.xlsx"))
  } else if  (tst == "T0801SAnew"){
    obs_status<- list.files(path = here::here("data","q", "new", "sca"),
                            recursive = FALSE,
                            full.names = TRUE) |>
      as_tibble() |>
      mutate(version = as.numeric(str_extract(value, "(?<=_Q_..............).{4}")),
             country = str_extract(value, "(?<=_Q_)..")) |>
      group_by(country) |>
      arrange(version) |>
      slice_tail(n=1) |>
      pull(value) |>
      open_dataset() |>
      select(-embargo_date,-received) |>
      collect() %>%
      left_join(.,lookup,by = join_by(counterpart_area, ref_sector, counterpart_sector,
                                      consolidation, accounting_entry, sto, instr_asset, unit_measure, prices)) |>
      na.omit() |>
      select(ref_area,id,time_period,obs_status) |>
      filter(time_period >= "1999-Q1")

    obs_status_country <- obs_status|>
      group_by(ref_area,obs_status) |>
      tally() |>
      pivot_wider(names_from = obs_status,
                  values_from = n)

    obs_status_sto <- obs_status|>
      group_by(id,obs_status) |>
      tally() |>
      pivot_wider(names_from = obs_status,
                  values_from = n)

    obs_status_quarter <- obs_status|>
      group_by(time_period,obs_status) |>
      tally() |>
      pivot_wider(names_from = obs_status,
                  values_from = n)

    l <- list(country = obs_status_country,
              sto = obs_status_sto,
              year= obs_status_quarter)

    write.xlsx(l, file = paste0(output_sel,
                                as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),"_obs_status_T0801SA_new.xlsx"),
               overwrite = TRUE)

    cli::cli_alert_success(paste0("File created in ", output_sel,as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),"_obs_status_T0801SA_new.xlsx"))

  } else if (tst == "T0801SAprev"){
    obs_status<- list.files(path = here::here("data","q", "prev", "nsa"),
                            recursive = FALSE,
                            full.names = TRUE) |>
      as_tibble() |>
      mutate(version = as.numeric(str_extract(value, "(?<=_Q_..............).{4}")),
             country = str_extract(value, "(?<=_Q_)..")) |>
      group_by(country) |>
      arrange(version) |>
      slice_tail(n=1) |>
      pull(value) |>
      open_dataset() |>
      select(-embargo_date,-received) |>
      collect() %>%
      left_join(.,lookup,by = join_by(counterpart_area, ref_sector, counterpart_sector,
                                      consolidation, accounting_entry, sto, instr_asset, unit_measure, prices)) |>
      na.omit() |>
      select(ref_area,id,time_period,obs_status)|>
      filter(time_period >= "1999-Q1")

    obs_status_country <- obs_status|>
      group_by(ref_area,obs_status) |>
      tally() |>
      pivot_wider(names_from = obs_status,
                  values_from = n)

    obs_status_sto <- obs_status|>
      group_by(id,obs_status) |>
      tally() |>
      pivot_wider(names_from = obs_status,
                  values_from = n)

    obs_status_quarter <- obs_status|>
      group_by(time_period,obs_status) |>
      tally() |>
      pivot_wider(names_from = obs_status,
                  values_from = n)

    l <- list(country = obs_status_country,
              sto = obs_status_sto,
              year= obs_status_quarter)

    write.xlsx(l, file = paste0(output_sel,
                                as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),"_obs_status_T0801SA_prev.xlsx"),
               overwrite = TRUE)

    cli::cli_alert_success(paste0("File created in ", output_sel,as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),"_obs_status_T0801SA_prev.xlsx"))
  }
  return(l)
}

