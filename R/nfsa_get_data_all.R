#' @title Get NFSA Data
#'
#' @description This function retrieves data from the NFSA database based on specified criteria.
#'
#' @param input_sel Character string specifying the path to the data directory. Default is \code{"M:/nas/Rprod/data")}.
#' @param country Character vector specifying the country or countries to retrieve data for.
#' @param table Character string specifying the table to retrieve data from (e.g., "T0801", "T0801SA", "T0800").
#' @return A tibble containing the selected NFSA data with columns: \code{ref_area}, \code{id}, \code{time_period}, and \code{obs_value}.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' # Assuming you have data stored in a directory accessible via here("data")
#' # and you want to retrieve the newest version of table T0801 for "IT":
#' # data <- nfsa_get_data_all(country = "IT", table = "T0801")
#' }
#'
#' @export
nfsa_get_data_all <- function(input_sel = "M:/nas/Rprod/data/",
                              country,
                              table = "T0801"){

library(tidyverse)
library(arrow)
library(here)
lookup <- nfsa::nfsa_sto_lookup

  if (table == "T0801") {
  nfsa_new_files <- list.files(path = input_sel,
                               pattern = "NASEC_T0801_Q",
                               recursive = TRUE,
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

  else if (table == "T0801SA") {
    nfsa_new_files <- list.files(path = input_sel,
                                 pattern = "NASEC_T0801SA_Q",
                                 recursive = TRUE,
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

else if (table == "T0800") {
    nfsa_new_files <- list.files(path = input_sel,
                                 pattern = "NASEC_T0800_A",
                                 recursive = TRUE,
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

