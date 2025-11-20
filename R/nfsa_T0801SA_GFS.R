#' Compare NFSA and GFS data for a given country.
#'
#' This function retrieves NFSA and GFS data, joins them, calculates the difference,
#' and identifies discrepancies exceeding a specified threshold.
#'
#' @param country A character string representing the country code (e.g., "DE").
#' @param threshold A numeric value specifying the minimum absolute difference between NFSA and GFS data to be considered a discrepancy. Default is 1.
#' @param input_sel A file path to the location of the 'sca' dataset. Default is here::here("data", "q", "new", "sca").
#' @param output_sel A file path to the directory where the output Excel file will be saved. Default is here::here("output", "inter_domain").
#'
#' @return A data frame containing the joined NFSA and GFS data, the difference between them,
#'         and the difference as a percentage of GDP.  Returns the data frame `gfs_nfsa`.
#'         If no discrepancies are found, the function prints a success message and returns an empty data frame.
#'
#' @examples
#' \dontrun{
#'   # Compare NFSA and GFS data for Germany with a threshold of 10
#'   result <- nfsa_T0801SA_GFS(country = "DE", threshold = 10)
#'
#'   # Compare NFSA and GFS data for France with default threshold
#'   result <- nfsa_T0801SA_GFS(country = "FR")
#' }
#'
#' @export
nfsa_T0801SA_GFS <- function(country,
                             threshold = 1,
                             input_sel = here::here("data", "q", "new", "sca"),
                             output_sel = here::here("output", "inter_domain")){
  pacman::p_load(tidyverse,arrow,writexl,here,restatapi)
  options(warn=-1)

  cli::cli_progress_message("Collecting NFSA...")
  nfsa <- nfsa_get_data(country = country,table = "T0801SA", type = "new") |>
    select(ref_area,id,time_period,nfsa = obs_value)

  ## GFS -----
  cli::cli_progress_message("Collecting GFS...")
  gfs <- get_eurostat_data("gov_10q_ggnfa",
                           filters = list(
                             geo = country,
                             unit = "MIO_NAC",
                             s_adj = "SCA",
                             sector = "S13"
                           ),
                           stringsAsFactors = FALSE
  ) |>
    mutate(sto = case_when(na_item == "B1G" ~ "B1G.B",
                           na_item == "B8G" ~ "B8G.B",
                           na_item == "B9" ~ "B9.B",
                           na_item == "D1PAY" ~ "D1.D",
                           na_item == "D21REC" ~ "D21.C",
                           na_item == "D29PAY" ~ "D29.D",
                           na_item == "D2REC" ~ "D2.C",
                           na_item == "D31PAY" ~ "D31.D",
                           na_item == "D39PAY" ~ "D39.D",
                           na_item == "D39REC" ~ "D39.C",
                           na_item == "D3PAY" ~ "D3.D",
                           na_item == "D41PAY" ~ "D41.D",
                           na_item == "D4PAY" ~ "D4.D",
                           na_item == "D4REC" ~ "D4.C",
                           na_item == "D5REC" ~ "D5.C",
                           na_item == "D4PAY" ~ "D5.D",
                           na_item == "D611REC" ~ "D6111.C",
                           na_item == "D62PAY" ~ "D62.D",
                           na_item == "D613REC" ~ "D6113.C",
                           na_item == "D61REC" ~ "D61.C",
                           na_item == "D7REC" ~ "D7.C",
                           na_item == "D7PAY" ~ "D7.D",
                           na_item == "D91REC" ~ "D91.C",
                           na_item == "D92PAY" ~ "D92.D",
                           na_item == "D9REC" ~ "D9.C",
                           na_item == "D9PAY" ~ "D9.D",
                           na_item == "P1" ~ "P1.C",
                           na_item == "P2" ~ "P2.D",
                           na_item == "P3" ~ "P3.D",
                           na_item == "P31" ~ "P31.D",
                           na_item == "P32" ~ "P32.D",
                           na_item == "P5" ~ "P5.D",
                           na_item == "P51G" ~ "P51G.D",
                           na_item == "TE" ~ "OTE.D",
                           na_item == "TR" ~ "OTR.C")) |>
    select(-unit, -s_adj,na_item) |>
    na.omit() |>
    unite("id", c(sector, sto), sep = ".") |>
    rename(time_period = time, ref_area = geo) |>
    select(ref_area,id, time_period, gfs = values) |>
    mutate(ref_area = if_else(ref_area == "GR", "EL", ref_area))



  gfs_nfsa <- full_join(gfs, nfsa,by = join_by(ref_area, id,time_period)) |>
    mutate(diff = round(nfsa-gfs,2)) |>
    group_by(ref_area,time_period) |>
    mutate(as_GDP = round(diff*100/nfsa[id =="S1.B1GQ.B"],3)) |>
    ungroup() |>
    na.omit() |>
    filter(abs(diff)>threshold)



  if (nrow(gfs_nfsa) == 0) {
    cli::cli_alert_success("T0801SA and GFS fully consistent!")

  }

  if (length(unique(gfs_nfsa$ref_area)) == 1) {
    openxlsx::write.xlsx(gfs_nfsa,
                         file = paste0(output_sel,"/T0801SA_QNA_",unique(gfs_nfsa$ref_area), "_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"),
                         overwrite = TRUE,
                         asTable = TRUE)

    cli::cli_alert_success(paste0("File created in ", output_sel,"/T0801SA_QNA_",unique(gfs_nfsa$ref_area), "_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"))

  }

  if (length(unique(gfs_nfsa$ref_area)) > 1) {
    openxlsx::write.xlsx(gfs_nfsa,
                         file = paste0(output_sel,"/T0801SA_GFS_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"),
                         overwrite = TRUE,
                         asTable = TRUE)

    cli::cli_alert_success(paste0("File created in ", output_sel,"/T0801SA_GFS_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"))
  }
  options(warn=0)
  return(gfs_nfsa)
}


