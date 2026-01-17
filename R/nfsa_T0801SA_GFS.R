#' Compare NFSA and GFS data for a given country.
#'
#' This function retrieves NFSA and GFS data, joins them, calculates the difference,
#' and identifies discrepancies exceeding a specified threshold.
#'
#' @param country A character string representing the country code (e.g., "DE").
#' @param quarter A character string specifying the quarter in the format "YYYYQQ" (e.g., "2023Q1").
#' @param threshold A numeric value specifying the minimum absolute difference between NFSA and GFS data to be considered a discrepancy. Default is 1.
#' @param input_sel A file path to the location of the 'sca' dataset. Default is "M:/nas/Rprod/q/new/sca/".
#' @param output_sel A file path to the directory where the output Excel file will be saved. Default is here::here("output", "inter_domain").
#'
#' @return A data frame containing the joined NFSA and GFS data, the difference between them,
#'         and the difference as a percentage of GDP.  Returns the data frame `gfs_nfsa`.
#'         If no discrepancies are found, the function prints a success message and returns an empty data frame.
#'
#' @examples
#' \dontrun{
#'   # Compare NFSA and GFS data for Germany with a threshold of 10
#'   result <- nfsa_T0801SA_GFS(country = "DE", quarter = "2025Q3")
#'
#' }
#'
#' @export
nfsa_T0801SA_GFS <- function(country,
                             quarter,
                             threshold = 1,
                             input_sel = "M:/nas/Rprod/q/new/sca/",
                             output_sel = here::here("output", "inter_domain")){
  pacman::p_load(tidyverse,arrow,writexl,here,readsdmx)

  cli::cli_progress_message("Collecting NFSA...")
  nfsa_data <- nfsa_get_data(country = country,table = "T0801SA", type = "new") |>
  select(ref_area,id,time_period,nfsa = obs_value) |>
  nfsa::nfsa_separate_id()

  ## GFS -----
  cli::cli_progress_message("Collecting GFS...")
  gfs_files <- nama_files <- list.files(path = paste0("M:/nas/QSA10/Production/",quarter,"/(1) QSA/(1_2) Validation in progress/(1_2_5) Consistency checks - QSA vs GFS/Input"),
                                        pattern = "xml$",
                                        full.names = TRUE,
                                        recursive = TRUE) |>
    as_tibble() |>
    mutate(ref_area = str_sub(value,-21,-20),
           update = as.numeric(str_sub(value,-16,-5))) |>
    filter(ref_area %in% country) |>
    group_by(ref_area) |>
    arrange(update,.by_group = TRUE) |>
    slice_tail(n=1) |>
    ungroup() |>
    select(value) |>
    pull(value)

  read_gfs_files <- function(file){
    gfs_data <- read_sdmx(file) |>
      janitor::clean_names() |>
      filter(adjustment == "Y") |>
      select(ref_area,ref_sector,sto,accounting_entry,time_period,gfs = obs_value) |>
      mutate(gfs = as.numeric(gfs)) |>
      distinct()
  }

  gfs_data <- map(gfs_files,read_gfs_files) |>
    list_rbind()

  gfs_nfsa <- full_join(gfs_data, nfsa_data,by = join_by(ref_area, ref_sector, sto, accounting_entry,
                                                         time_period)) |>
    na.omit() |>
    mutate(diff = round(nfsa-gfs,2)) |>
    filter(abs(diff)> threshold)

  tmp <- nfsa_data |>
    filter(sto == "B1GQ") |>
    select(ref_area,time_period,gdp=nfsa)

  gfs_nfsa <- left_join(gfs_nfsa,tmp, by = join_by(ref_area, time_period)) |>
    mutate(as_GDP = round(diff*100/gdp,3)) |>
    select(-gdp)



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
  nfsa::nfsa_to_excel(gfs_nfsa)
  return(gfs_nfsa)
}


