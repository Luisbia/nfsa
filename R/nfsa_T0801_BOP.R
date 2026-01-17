#' @title Compare NFSA T0801 data with BOP data and identify discrepancies.
#'
#' @description This function compares NFSA T0801 data with Balance of Payments (BOP) data,
#' identifies discrepancies exceeding a specified threshold, and outputs the results
#' to an Excel file.
#'
#' @param country A character vector specifying the country code (e.g., "AT").
#' @param quarter A character vector specifying the quarter of the data (e.g., "2025Q2").
#' @param threshold A numeric value specifying the threshold for the difference
#'   between NFSA and BOP data.  Differences exceeding this threshold are flagged.
#'   Default is 5.
#' @param input_sel A character string specifying the directory containing the input
#'   NFSA data. Default is `M:/nas/Rprod/data/q/new/nsa/`.
#' @param output_sel A character string specifying the directory where the output
#'   Excel file will be saved. Default is `here::here("output", "inter_domain")`.
#'
#' @return This function does not return a value. It generates an Excel file
#'   containing the comparison results in the specified output directory.  A
#'   console message indicates the file creation. If no inconsistencies are found,
#'   a success message is printed.
#'
#' @details The function reads NFSA data using `nfsa_get_data` and BOP data from
#'   XML files in a specified directory. It joins these datasets, calculates the
#'   difference between NFSA and BOP values, and filters for differences exceeding
#'   the provided threshold. The results are then written to an Excel file.

#'
#' @examples
#' \dontrun{
#' # Example usage:
#' nfsa_T0801_BOP(country = "AT", quarter = "2024Q2", threshold = 10)
#' }
#'
#' @export
nfsa_T0801_BOP <- function(country,
                           quarter,
                           threshold = 5,
                           input_sel = "M:/nas/Rprod/data/q/new/nsa/",
                           output_sel = here::here("output", "inter_domain")
){

  pacman::p_load(tidyverse,here,arrow,openxlsx, readsdmx, readxl,janitor,cli)

  cli::cli_progress_message("Collecting NFSA...")



  nfsa_data <- nfsa_get_data(country = country,table = "T0801", type = "new") |>
    select(ref_area,id,time_period,nfsa = obs_value)  |>
    nfsa::nfsa_separate_id()


  ## BOP

  cli::cli_progress_message("Collecting BOP...")
  bop_files <- nama_files <- list.files(path = paste0("M:/nas/QSA10/Production/",quarter,"/(1) QSA/(1_2) Validation in progress/(1_2_6) Consistency checks - QSA vs BoP/Input"),
                                        pattern = "xml$",
                                        full.names = TRUE,
                                        recursive = TRUE) |>
    as_tibble() |>
    mutate(ref_area = str_sub(value,-22,-21)) |>
    filter(ref_area %in% country) |>
    select(value) |>
    pull(value)

  read_bop_files <- function(file){
    bop_data <- read_sdmx(file) |>
      janitor::clean_names() |>
      select(ref_area,ref_sector,sto,accounting_entry,time_period,bop = obs_value) |>
      mutate(ref_sector = "S2") |>
      mutate(bop = as.numeric(bop)) |>
      distinct()
    return(bop_data)
  }

  bop_data <- map(bop_files,read_bop_files) |>
    list_rbind()

  bop_nfsa <- full_join(bop_data, nfsa_data,
                        by = join_by(ref_area,ref_sector, sto,
                                     accounting_entry, time_period)) |>
    na.omit() |>
    mutate(diff = round(nfsa-bop,1)) |>
    filter(abs(diff)>threshold)

  tmp <- nfsa_data |>
    filter(sto == "B1GQ") |>
    select(ref_area,time_period,gdp=nfsa)

  bop_nfsa <- left_join(bop_nfsa,tmp, by = join_by(ref_area, time_period)) |>
    mutate(as_GDP = round(diff*100/gdp,3)) |>
    mutate(threshold = ifelse(abs(as_GDP) > 0.3, TRUE,FALSE))



  if (nrow(bop_nfsa) == 0) {
    cli::cli_alert_success("T0801 and BOP fully consistent!")

  }

  if (length(unique(bop_nfsa$ref_area)) == 1) {
    openxlsx::write.xlsx(bop_nfsa,
                         file = paste0(output_sel,"/T0801_BOP_",unique(bop_nfsa$ref_area), "_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"),
                         overwrite = TRUE,
                         asTable = TRUE)

    cli::cli_alert_success(paste0("File created in ", output_sel,"/T0801_BOP_",unique(bop_nfsa$ref_area), "_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"))

  }

  if (length(unique(bop_nfsa$ref_area)) > 1) {
    openxlsx::write.xlsx(bop_nfsa,
                         file = paste0(output_sel,"/T0801_BOP_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"),
                         overwrite = TRUE,
                         asTable = TRUE)

    cli::cli_alert_success(paste0("File created in ", output_sel,"/T0801_BOP_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"))
  }
  nfsa::nfsa_to_excel(bop_nfsa)
  return(bop_nfsa)
}


