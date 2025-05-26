#' Download from Eurobase different tables for NFSA and related domains
#'
#' @param dataset_sel the dataset name to download. Options are: "nasa_10_nf_tr","nasq_10_nf_tr",
#'                    "nama_10_gdp", "namq_10_gdp", "gov_10a_taxag", "gov_10q_ggnfa", "bop_c6_a",
#'                    "bop_c6_q"
#' @param geo_sel select the country (or countries) you need.
#' @param time_sel for annual data, select the years to download. By default quarterly data is
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

  dt<- restatapi::get_eurostat_data(dataset_sel,
                                   filters=list(geo = geo_sel),
                                   date_filter = time_sel,
                                   verbose = FALSE,
                                   keep_flags = FALSE,
                                   mode = "csv",
                                   stringsAsFactors = FALSE) |>
    rename(ref_area = geo,
           sto = na_item,
           ref_sector = sector,
           accounting_entry = direct,
           unit_measure = unit,
           time_period = time,
           obs_value = values) |>
    mutate(time_period = as.integer(time_period),
           unit_measure = case_when(unit_measure == "CP_MNAC" ~ "XDC",
                                    unit_measure == "CP_MEUR" ~ "EUR",
                                    unit_measure == "PPS_EU27_2020_HAB" ~ "PPS_EU27_2020_HAB"),
           accounting_entry = if_else(accounting_entry == "PAID", "D", "C"))

  return(dt)
}

  if (dataset_sel == "nasq_10_nf_tr"){


dt<- restatapi::get_eurostat_data(dataset_sel,
                                 filters=list(geo = geo_sel),
                                 verbose = FALSE,
                                 keep_flags = FALSE,
                                 mode = "csv",
                                 stringsAsFactors = FALSE) |>
  rename(ref_area = geo,
         sto = na_item,
         unit_measure = unit,
         adjustment = s_adj,
         time_period = time,
         obs_value = values) |>
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
                                     keep_flags = FALSE,
                                     mode = "csv",
                                     stringsAsFactors = FALSE) |>
      rename(ref_area = geo,
             sto = na_item,
             unit_measure = unit,
             time_period = time,
             obs_value = values) |>
      mutate(time_period = as.integer(time_period),
             unit_measure = if_else(unit_measure == "CP_MEUR", "EUR", "XDC"))

    return(dt)
  }

  if (dataset_sel == "namq_10_gdp"){

    dt<- restatapi::get_eurostat_data(dataset_sel,
                                     filters=list(geo = geo_sel,
                                                  unit = c("CP_MEUR", "CP_MNAC")),
                                     verbose = FALSE,
                                     keep_flags = FALSE,
                                     mode = "csv",
                                     stringsAsFactors = FALSE) |>
      rename(ref_area = geo,
             sto = na_item,
             unit_measure = unit,
             time_period = time,
             obs_value = values) |>
      mutate(time_period = lubridate::yq (time_period),
             unit_measure = if_else(unit_measure == "CP_MEUR", "EUR", "XDC"),
             adjustment = case_when(adjustment == "NSA" ~ "N",
                                    adjustment == "SCA" ~ "Y",
                                    adjustment == "SA" ~ "S",
                                    adjustment == "CA" ~ "C"))

    return(dt)
  }


  if (dataset_sel == "gov_10a_taxag"){

    dt<- restatapi::get_eurostat_data(dataset_sel,
                                     filters=list(geo = geo_sel),
                                     date_filter = time_sel,
                                     verbose = FALSE,
                                     keep_flags = FALSE,
                                     mode = "csv",
                                     stringsAsFactors = FALSE) |>
      filter(sector == "S13") |>
      rename(ref_area = geo,
             sto = na_item,
             ref_sector = sector,
             unit_measure = unit,
             time_period = time,
             obs_value = values) |>
      mutate(time_period = as.integer(time_period),
             unit_measure = if_else(unit_measure == "MIO_EUR", "EUR", "XDC"))

    return(dt)
  }

  if (dataset_sel == "gov_10q_ggnfa"){

    dt<- restatapi::get_eurostat_data(dataset_sel,
                                     filters=list(geo = geo_sel),
                                     verbose = FALSE,
                                     keep_flags = FALSE,
                                     mode = "csv",
                                     stringsAsFactors = FALSE) |>
      filter(sector == "S13") |>
      rename(ref_area = geo,
             sto = na_item,
             ref_sector = sector,
             adjustment = s_adj,
             unit_measure = unit,
             time_period = time,
             obs_value = values) |>
      mutate(time_period = lubridate::yq (time_period),
             unit_measure = if_else(unit_measure == "MIO_EUR", "EUR", "XDC"),
             adjustment = case_when(adjustment == "NSA" ~ "N",
                                    adjustment == "SCA" ~ "Y"))


    return(dt)
  }

  if (dataset_sel == "bop_c6_a"){

    dt<- restatapi::get_eurostat_data(dataset_sel,
                                      filters=list(geo = "ES",
                                                   sectpart = "S1",
                                                   sector10 = "S1",
                                                   stk_flow = c("CRE", "DEB", "NET"),
                                                   partner = c("WRL_REST", "EXT_EU27_2020", "EXT_EA20", "EU27_2020", "EA20")),
                                      date_filter = "2020",
                                      verbose = FALSE,
                                      keep_flags = FALSE,
                                      mode = "csv",
                                      stringsAsFactors = FALSE) |>
      select(ref_area = geo,
             sto = bop_item,
             ref_sector = sector10,
             counterpart_area = partner,
             accounting_entry = stk_flow,
             unit_measure = currency,
             time_period = time,
             obs_value = values) |>
      mutate(time_period = as.integer(time_period),
             unit_measure = if_else(unit_measure == "MIO_EUR", "EUR", "XDC"),
             accounting_entry = case_when(accounting_entry == "CRE" ~ "C",
                                          accounting_entry == "DEB" ~ "D",
                                          accounting_entry == "BAL" ~ "B"),
             ref_sector = "S2") #?

    return(dt)
  }

  if (dataset_sel == "bop_c6_q"){

    dt<- restatapi::get_eurostat_data(dataset_sel,
                                      filters=list(geo = "ES",
                                                   sectpart = "S1",
                                                   sector10 = "S1",
                                                   stk_flow = c("CRE", "DEB", "NET"),
                                                   partner = c("WRL_REST", "EXT_EU27_2020", "EXT_EA20", "EU27_2020", "EA20")),
                                      verbose = FALSE,
                                      keep_flags = FALSE,
                                      mode = "csv",
                                      stringsAsFactors = FALSE) |>
      select(ref_area = geo,
             sto = bop_item,
             ref_sector = sector10,
             counterpart_area = partner,
             accounting_entry = stk_flow,
             unit_measure = currency,
             time_period = time,
             obs_value = values) |>
      mutate(time_period = lubridate::yq (time_period),
             unit_measure = if_else(unit_measure == "MIO_EUR", "EUR", "XDC"),
             accounting_entry = case_when(accounting_entry == "CRE" ~ "C",
                                          accounting_entry == "DEB" ~ "D",
                                          accounting_entry == "BAL" ~ "B"),
             ref_sector = "S2",
             adjustment = "N")

    return(dt)
  }
}



