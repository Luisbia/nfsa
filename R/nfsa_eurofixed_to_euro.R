#' @title Convert Euro fixed  to Euro
#'
#' @description Converts data in Euro fixed to Euro using exchange rates from Eurostat.
#'
#' @param data A data frame containing data in Euro fixed, with columns `ref_area` (country code) and `time_period` (time period).
#' @param freq A character string indicating the frequency of the data ("Q" for quarterly, "A" for annual). Defaults to "Q".
#'
#' @return A data frame with an additional `obs_value` column, where the original values have been converted to Euro.
#'
#' @examples
#' \dontrun{
#' # Example usage with quarterly data
#' data_eur <- nfsa_eurofixed_to_euro(data = your_data, freq = "Q")
#'
#' # Example usage with annual data
#' data_eur <- nfsa_eurofixed_to_euro(data = your_data, freq = "A")
#' }
#' @export
nfsa_eurofixed_to_eur <- function(data, freq = "Q"){

  library(restatapi)
  library(tidyverse)
  if(freq == "Q"){
    erate <- restatapi::get_eurostat_data("ert_bil_conv_q",
                                          filters = list(statinfo = "AVG"),
                                          stringsAsFactors = FALSE) |>
      select(ref_area=geo,time_period=time,erate = values) |>
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
  erate <- restatapi::get_eurostat_data("ert_bil_conv_a",
                                        filters = list(statinfo = "AVG"),
                                        stringsAsFactors = FALSE) |>
    select(ref_area = geo,time_period=time,erate = values)|>
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


