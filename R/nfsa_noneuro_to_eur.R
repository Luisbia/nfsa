#' @title Convert National Currency to Euro
#'
#' @description Converts data in national currencies to Euro using exchange rates from Eurostat.
#'
#' @param data A data frame containing data in national currencies, with columns `ref_area` (country code) and `time_period` (time period).
#' @param freq A character string indicating the frequency of the data ("Q" for quarterly, "A" for annual). Defaults to "Q".
#'
#' @return A data frame with an additional `obs_value` column, where the original values have been converted to Euro.
#'
#' @examples
#' \dontrun{
#' # Example usage with quarterly data
#' data_eur <- nfsa_noneuro_to_euro(data = your_data, freq = "Q")
#'
#' # Example usage with annual data
#' data_eur <- nfsa_noneuro_to_euro(data = your_data, freq = "A")
#' }
#' @export
nfsa_noneuro_to_euro <- function(data, freq = "Q"){

  library(restatapi)
  library(tidyverse)
  if(freq == "Q"){
    erate <- restatapi::get_eurostat_data("ert_bil_eur_q",
                                          filters = list(statinfo = "AVG",
                                                         currency = c("BGN","CZK","DKK","HUF", "PLN", "RON", "SEK",
                                                                      "CHF", "ISK", "NOK", "ALL", "MKD", "RSD", "TRY")),
                                          stringsAsFactors = FALSE) |>
      mutate(ref_area = case_when(currency == "BGN" ~"BG",
                                  currency == "CZK" ~"CZ",
                                  currency == "DKK" ~"DK",
                                  currency == "HUF" ~"HU",
                                  currency == "PLN" ~"PL",
                                  currency == "RON" ~"RO",
                                  currency == "SEK" ~"SE",
                                  currency == "CHF" ~"CH",
                                  currency == "ISK" ~"IS",
                                  currency == "NOK" ~"NO",
                                  currency == "ALL" ~"AL",
                                  currency == "MKD" ~"MK",
                                  currency == "RSD" ~"RS",
                                  currency == "TRY" ~"TE")) |>
      select(ref_area,time_period=time,erate = values) |>
      as_tibble()

    tmp <- data |>
      filter(str_detect(id,".PS|.HW"))

    data_eur <- left_join(data,erate, by = join_by(ref_area, time_period)) |>
      mutate(obs_value = obs_value/erate) |>
      select(-erate) |>
      filter(!str_detect(id,".PS|.HW"))

    data_eur <- bind_rows(data_eur,tmp)
  }
else if (freq == "A"){
  erate <- restatapi::get_eurostat_data("ert_bil_eur_a",
                                        filters = list(statinfo = "AVG",
                                                       currency = c("BGN","CZK","DKK","HUF", "PLN", "RON", "SEK",
                                                                    "CHF", "ISK", "NOK", "ALL", "MKD", "RSD", "TRY")),
                                        stringsAsFactors = FALSE) |>
    mutate(ref_area = case_when(currency == "BGN" ~"BG",
                                currency == "CZK" ~"CZ",
                                currency == "DKK" ~"DK",
                                currency == "HUF" ~"HU",
                                currency == "PLN" ~"PL",
                                currency == "RON" ~"RO",
                                currency == "SEK" ~"SE",
                                currency == "CHF" ~"CH",
                                currency == "ISK" ~"IS",
                                currency == "NOK" ~"NO",
                                currency == "ALL" ~"AL",
                                currency == "MKD" ~"MK",
                                currency == "RSD" ~"RS",
                                currency == "TRY" ~"TE")) |>
    select(ref_area,time_period=time,erate = values)|>
    as_tibble()

  tmp <- data |>
    filter(str_detect(id,".PS|.HW"))

  data_eur <- left_join(data,erate, by = join_by(ref_area, time_period)) |>
    mutate(obs_value = obs_value/erate) |>
    select(-erate) |>
    filter(!str_detect(id,".PS|.HW"))

  data_eur <- bind_rows(data_eur,tmp)
}
  return(data_eur)
  }


