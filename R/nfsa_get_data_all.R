#' @title Get NFSA Data
#'
#' @description This function retrieves data from the NFSA database based on specified criteria.
#'
#' @param input_sel Character string specifying the path to the data directory. Default is \code{here("data")}.
#' @param country Character vector specifying the country or countries to retrieve data for.
#' @param table Character string specifying the table to retrieve data from (e.g., "T0801", "T0801SA", "T0800").
#' @param type Character string specifying the type of data to retrieve ("new" or "prev").
#'
#' @return A tibble containing the selected NFSA data with columns: \code{ref_area}, \code{id}, \code{time_period}, and \code{obs_value}.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' # Assuming you have data stored in a directory accessible via here("data")
#' # and you want to retrieve the newest version of table T0801 for "IT":
#' # data <- nfsa_get_data(country = "IT", table = "T0801", type = "new")
#' }
#'
#' @export
nfsa_get_data_all <- function(input_sel = here::here("data"),
                          country,
                          table,
                          type ){

library(tidyverse)
library(arrow)
library(here)
lookup <- nfsa::nfsa_sto_lookup

  if (paste0(table,type) == "T0801new") {
  nfsa_new_files <- list.files(path = paste0(input_sel,"/q/new/nsa/"),
                               recursive = FALSE,
                               full.names = TRUE) |>
    as_tibble() |>
    mutate(version = as.numeric(str_extract(value, "(?<=_Q_..............).{4}")),
           countries = str_extract(value, "(?<=_Q_)..")) |>
    filter(countries %in% country) |>
    group_by(countries) |>
    arrange(version) |>
    pull(value) |>
    open_dataset() |>
    select(-embargo_date,-received) |>
    collect() %>%
    left_join(.,lookup,by = join_by(counterpart_area, ref_sector, counterpart_sector,
                                    consolidation, accounting_entry, sto, instr_asset, unit_measure, prices)) |>
    na.omit() |>
    select(ref_area,version,id,time_period,obs_value)
  }
  else if (paste0(table,type) == "T0801prev") {
    nfsa_new_files <- list.files(path = paste0(input_sel,"/q/prev/nsa/"),
                                 recursive = FALSE,
                                 full.names = TRUE) |>
      as_tibble() |>
      mutate(version = as.numeric(str_extract(value, "(?<=_Q_..............).{4}")),
             countries = str_extract(value, "(?<=_Q_)..")) |>
      filter(countries %in% country) |>
      group_by(countries) |>
      arrange(version) |>
          pull(value) |>
      open_dataset() |>
      select(-embargo_date,-received) |>
      collect() %>%
      left_join(.,lookup,by = join_by(counterpart_area, ref_sector, counterpart_sector,
                                      consolidation, accounting_entry, sto, instr_asset, unit_measure, prices)) |>
      na.omit() |>
      select(ref_area,version,id,time_period,obs_value)
  }
  else if (paste0(table,type) == "T0801SAnew") {
    nfsa_new_files <- list.files(path = paste0(input_sel,"/q/new/sca/"),
                                 recursive = FALSE,
                                 full.names = TRUE) |>
      as_tibble() |>
      mutate(version = as.numeric(str_extract(value, "(?<=_Q_..............).{4}")),
             countries = str_extract(value, "(?<=_Q_)..")) |>
      filter(countries %in% country) |>
      group_by(countries) |>
      arrange(version) |>
      pull(value) |>
      open_dataset() |>
      select(-embargo_date,-received) |>
      collect() %>%
      left_join(.,lookup,by = join_by(counterpart_area, ref_sector, counterpart_sector,
                                      consolidation, accounting_entry, sto, instr_asset, unit_measure, prices)) |>
      na.omit() |>
      select(ref_area,version,id,time_period,obs_value)
  }
else if (paste0(table,type) == "T0801SAprev") {
    nfsa_new_files <- list.files(path = paste0(input_sel,"/q/prev/sca/"),
                                 recursive = FALSE,
                                 full.names = TRUE) |>
      as_tibble() |>
      mutate(version = as.numeric(str_extract(value, "(?<=_Q_..............).{4}")),
             countries = str_extract(value, "(?<=_Q_)..")) |>
      filter(countries %in% country) |>
      group_by(countries) |>
      arrange(version) |>
      pull(value) |>
      open_dataset() |>
      select(-embargo_date,-received) |>
      collect() %>%
      left_join(.,lookup,by = join_by(counterpart_area, ref_sector, counterpart_sector,
                                      consolidation, accounting_entry, sto, instr_asset, unit_measure, prices)) |>
      na.omit() |>
      select(ref_area,version,id,time_period,obs_value)
  }
else if (paste0(table,type) == "T0800new") {
    nfsa_new_files <- list.files(path = paste0(input_sel,"/a/new/"),
                                 recursive = FALSE,
                                 full.names = TRUE) |>
      as_tibble() |>
      mutate(version = as.numeric(str_extract(value, "(?<=_A_..............).{4}")),
             countries = str_extract(value, "(?<=_A_)..")) |>
      filter(countries %in% country) |>
      group_by(countries) |>
      arrange(version) |>
      pull(value) |>
      open_dataset() |>
      select(-embargo_date,-received) |>
      collect() %>%
      left_join(.,lookup,by = join_by(counterpart_area, ref_sector, counterpart_sector,
                                      consolidation, accounting_entry, sto, instr_asset, unit_measure, prices)) |>
      na.omit() |>
      select(ref_area,version,id,time_period,obs_value)
  }
else if (paste0(table,type) == "T0800prev") {
    nfsa_new_files <- list.files(path = paste0(input_sel,"/a/prev/"),
                                 recursive = FALSE,
                                 full.names = TRUE) |>
      as_tibble() |>
      mutate(version = as.numeric(str_extract(value, "(?<=_A_..............).{4}")),
             countries = str_extract(value, "(?<=_A_)..")) |>
      filter(countries %in% country) |>
      group_by(countries) |>
      arrange(version) |>
      pull(value) |>
      open_dataset() |>
      select(-embargo_date,-received) |>
      collect() %>%
      left_join(.,lookup,by = join_by(counterpart_area, ref_sector, counterpart_sector,
                                      consolidation, accounting_entry, sto, instr_asset, unit_measure, prices)) |>
      na.omit() |>
      select(ref_area,version,id,time_period,obs_value)
  }
  else cli::cli_abort("Probably wrong input to the function.
                      table = T0801 or T0801SA or T0800,
                      type = new or prev")
}

