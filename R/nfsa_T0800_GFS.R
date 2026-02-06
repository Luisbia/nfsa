#' @title Compare NFSA T0800 data with GFS data
#'
#' @description This function compares NFSA T0800 data with GFS data for a given country and year, identifying discrepancies above a specified threshold.
#'
#' @param country A character vector specifying the country code(s) to process.
#' @param quarter A character string specifying the quarter in the format "YYYY" (e.g., 2023).
#' @param threshold A numeric value specifying the threshold for the absolute difference between NFSA and GFS values.  Defaults to 1.
#' @param input_sel A character string specifying the file path for the NFSA input data directory. Defaults to `"M:/nas/Rprod/data/a/new/"`.
#' @param output_sel A character string specifying the file path for the output directory. Defaults to `here::here("output", "inter_domain")`.
#'
#' @return This function does not return a value. It generates an Excel file containing the comparison results in the specified output directory if discrepancies are found. If no discrepancies are found, a success message is displayed.
#'
#' @details The function performs the following steps:
#'   \enumerate{
#'     \item Collects NFSA T0800 data using `nfsa_get_data`.
#'     \item Collects GFS data from XML files located in a specific directory structure based on the provided quarter.
#'     \item Joins the NFSA and GFS data based on `ref_area`, `ref_sector`, `sto`, `accounting_entry`, and `time_period`.
#'     \item Calculates the absolute difference between NFSA and GFS values.
#'     \item Filters for discrepancies where the absolute difference exceeds the specified threshold.
#'     \item Calculates the difference as a percentage of GDP.
#'     \item Flags observations where the difference as a percentage of GDP exceeds 0.3%.
#'     \item Writes the comparison results to an Excel file in the specified output directory.
#'   }
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' nfsa_T0800_GFS(country = "IE", year = "2023", threshold = 5)
#' }
#'
#' @export
nfsa_T0800_GFS <- function(country,
                           year,
                           threshold = 1,
                           input_sel = "M:/nas/Rprod/data/a/new/",
                           output_sel = here::here("output", "inter_domain")){

  pacman::p_load(tidyverse,here,arrow,openxlsx, readsdmx, readxl,janitor,cli)

  limit_validation <- paste0(as.numeric(str_sub(year,1,4)) -4)

  cli::cli_progress_message("Collecting NFSA...")

  nfsa_data <- nfsa_get_data(country = country,table = "T0800", type = "new") |>
    select(ref_area,id,time_period,nfsa = obs_value)  |>
    nfsa::nfsa_separate_id()

  ## GFS -----
  cli::cli_progress_message("Collecting GFS...")
  gfs_files <- list.files(path = paste0("M:/nas/ASA10/Production/",year,"/(1) ASA/(1_2) Validation in progress/(1_2_5) Consistency checks - ASA vs GFS/Input"),
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
      filter(adjustment == "N") |>
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
    #select(-gdp) |>
    mutate(threshold = ifelse(abs(as_GDP) > 0.3, TRUE,FALSE)) |>
    mutate(validate = ifelse(threshold == TRUE &
                               sto == "B9" &
                               time_period>= limit_validation,
                             "NOT VALIDATED",""))



  if (nrow(gfs_nfsa) == 0) {
    cli::cli_alert_success("T0800 and GFS fully consistent!")

  }

  if (length(unique(gfs_nfsa$ref_area)) == 1) {
    openxlsx::write.xlsx(gfs_nfsa,
                         file = paste0(output_sel,"/T0800_GFS_",unique(gfs_nfsa$ref_area), "_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"),
                         overwrite = TRUE,
                         asTable = TRUE)

    cli::cli_alert_success(paste0("File created in ", output_sel,"/T0801_GFS_",unique(gfs_nfsa$ref_area), "_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"))

  }

  if (length(unique(gfs_nfsa$ref_area)) > 1) {
    openxlsx::write.xlsx(gfs_nfsa,
                         file = paste0(output_sel,"/T0800_GFS_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"),
                         overwrite = TRUE,
                         asTable = TRUE)

    cli::cli_alert_success(paste0("File created in ", output_sel,"/T0800_GFS_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"))
  }
  return(gfs_nfsa)
}






