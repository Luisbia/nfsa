#' @title Calculate data per capita
#'
#' @description Converts data to per capita using population data from Eurostat.
#'
#' @param data A data frame containing data in millions of monetary units, with columns `ref_area` (country code) and `time_period` (time period).
#' @param freq A character string indicating the frequency of the data ("Q" for quarterly, "A" for annual). Defaults to "Q".
#'
#' @return A data frame with an additional `obs_value` column, where the original values have been converted per capita.
#'
#' @examples
#' \dontrun{
#' # Example usage with quarterly data
#' data_pop <- nfsa_percapita(data = your_data, freq = "Q")
#'
#' # Example usage with annual data
#' data_pop <- nfsa_percapita(data = your_data, freq = "A")
#' }
#' @export
nfsa_percapita <- function(data, freq = "Q"){
  library(tidyverse)

  if(freq == "Q"){
    pop <- restatapi::get_eurostat_data("namq_10_pe",
                                          filters = list(na_item = "POP_NC",
                                                         s_adj = "NSA",
                                                         unit = "THS_PER"),
                                          stringsAsFactors = FALSE) |>
      dplyr::select(ref_area = geo,time_period=time,pop = values) |>
      tibble::as_tibble()

    tmp <- data |>
      filter(str_detect(id,".PS|.HW"))

    data_pop<- left_join(data,pop, by = join_by(ref_area, time_period)) |>
      mutate(obs_value = (obs_value/pop)*1000) |>
      select(-pop) |>
      filter(!str_detect(id,".PS|.HW"))

    data_pop <- bind_rows(data_pop,tmp)
  }
  else if (freq == "A"){
    pop <- restatapi::get_eurostat_data("nama_10_pe",
                                          filters = list(na_item = "POP_NC",
                                                         unit = "THS_PER"),
                                          stringsAsFactors = FALSE) |>
      select(ref_area = geo,time_period=time,pop = values)|>
      as_tibble()

    tmp <- data |>
      filter(str_detect(id,".PS|.HW"))

    data_pop <- left_join(data,pop, by = join_by(ref_area, time_period)) |>
      mutate(obs_value = (obs_value/pop)*1000) |>
      select(-pop) |>
      filter(!str_detect(id,".PS|.HW"))

    data_pop <- bind_rows(data_pop,tmp)
  }
  return(data_pop)
}


