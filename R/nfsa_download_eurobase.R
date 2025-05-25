#' Download from Eurobase different tables
#'
#' @param dataset_sel the dataset name to download. Options are: "nasa_10_nf_tr","nasq_10_nf_tr",
#'                    "nama_10_gdp", "namq_10_gdp"
#' @param geo_sel select the country (or countries) you need.
#' @param time_sel for annual data, select the years to download. By defaulq quarterly data is
#'                  downloaded for all available quarters
#'
#' @return a data.frame/data.table
#' @importFrom restatapi get_eurostat_data
#' @importFrom lubridate yq
#' @import dplyr
#' @export nfsa_download_eurobase
#'
#' @examples
#' tmp <- nfsa_download_eurobase ("nasa_10_nf_tr", c("ES","DE"), "2023")
#' tmp <- nfsa_download_eurobase ("nasq_10_nf_tr", "ES")

nfsa_download_eurobase <- function (dataset_sel, geo_sel, time_sel){

  library(dplyr)
  if (dataset_sel == "nasa_10_nf_tr"){

  dt<-restatapi::get_eurostat_data(dataset_sel,
                                   filters=list(geo = geo_sel),
                                   date_filter = time_sel,
                                   verbose = FALSE,
                                   keep_flags = TRUE,
                                   mode = "csv",
                                   stringsAsFactors = FALSE) |>
    rename(ref_area = geo,
           sto = na_item,
           ref_sector = sector,
           accounting_entry = direct,
           unit_measure = unit,
           time_period = time,
           obs_value = values,
           obs_status = flags) |>
    mutate(time_period = as.integer(time_period),
           unit_measure = case_when(unit_measure == "CP_MNAC" ~ "XDC",
                                    unit_measure == "CP_MEUR" ~ "EUR",
                                    unit_measure == "PPS_EU27_2020_HAB" ~ "PPS_EU27_2020_HAB"),
           accounting_entry = if_else(accounting_entry == "PAID", "D", "C"))

  return(dt)
}

  if (dataset_sel == "nasq_10_nf_tr"){


dt<-restatapi::get_eurostat_data(dataset_sel,
                                 filters=list(geo = geo_sel),
                                 verbose = FALSE,
                                 keep_flags = TRUE,
                                 mode = "csv",
                                 stringsAsFactors = FALSE) |>
  rename(ref_area = geo,
         sto = na_item,
         unit_measure = unit,
         adjustment = s_adj,
         time_period = time,
         obs_value = values,
         obs_status = flags) |>
  mutate(time_period = lubridate::yq (time_period),
         unit_measure = if_else(unit_measure == "CP_MEUR", "EUR", "XDC"),
         adjustment = if_else(adjustment == "NSA", "N", "Y"))

return(dt)}

  if (dataset_sel == "nama_10_gdp"){

    dt<-restatapi::get_eurostat_data(dataset_sel,
                                     filters=list(geo = geo_sel,
                                                  unit = c("CP_MEUR", "CP_MNAC")),
                                     date_filter = time_sel,
                                     verbose = FALSE,
                                     keep_flags = TRUE,
                                     mode = "csv",
                                     stringsAsFactors = FALSE) |>
      rename(ref_area = geo,
             sto = na_item,
             unit_measure = unit,
             time_period = time,
             obs_value = values,
             obs_status = flags) |>
      mutate(time_period = as.integer(time_period),
             unit_measure = if_else(unit_measure == "CP_MEUR", "EUR", "XDC"))

    return(dt)
  }

  if (dataset_sel == "namq_10_gdp"){

    dt<-restatapi::get_eurostat_data(dataset_sel,
                                     filters=list(geo = geo_sel,
                                                  unit = c("CP_MEUR", "CP_MNAC")),
                                     verbose = FALSE,
                                     keep_flags = TRUE,
                                     mode = "csv",
                                     stringsAsFactors = FALSE) |>
      rename(ref_area = geo,
             sto = na_item,
             unit_measure = unit,
             time_period = time,
             obs_value = values,
             obs_status = flags) |>
      mutate(time_period = as.integer(time_period),
             unit_measure = if_else(unit_measure == "CP_MEUR", "EUR", "XDC"),
             adjustment = case_when(adjustment == "NSA" ~ "N",
                                    adjustment == "SCA" ~ "Y",
                                    adjustment == "SA" ~ "S",
                                    adjustment == "CA" ~ "C"))

    return(dt)
  }
}



