#' Compares NFSA T0800 data with ANA data for consistency.
#'
#' This function retrieves and compares NFSA  T0800 data with ANA (Annual National Accounts) data.
#' It identifies discrepancies between the two datasets based on a specified threshold and generates an Excel file
#' containing the comparison results.
#'
#' @param country A character vector specifying the country code(s) to filter data for.
#' @param year A numeric value specifying the year for which to compare the data.
#' @param threshold A numeric value specifying the threshold for acceptable differences between NFSA and ANA data.
#'                  Defaults to 1.
#' @param input_sel A file path to the directory containing NFSA data. Defaults to `"data/a/new"` relative to the project root.
#' @param output_sel A file path to the directory where the output Excel file will be saved. Defaults to `"output/inter_domain"` relative to the project root.
#'
#' @return This function does not explicitly return a value. It generates an Excel file containing the comparison results
#'         and saves it to the directory specified by `output_sel`. A success message is displayed in the console indicating
#'         the file creation and location.  If no inconsistencies are found, a message indicating full consistency is displayed.
#'
#' @details The function performs the following steps:
#'   1. Retrieves NFSA T0800 data for the specified country and year using `nfsa_get_data()`.
#'   2. Retrieves relevant ANA data from XML files located in a specific directory structure.
#'   3. Cleans and preprocesses both NFSA and ANA data.
#'   4. Joins the two datasets based on common identifiers (ref_area, ref_sector, sto, accounting_entry, time_period).
#'   5. Calculates the difference between NFSA and ANA values and expresses it as a percentage of GDP.
#'   6. Filters the joined data to identify discrepancies exceeding the specified `threshold`.
#'   7. Writes the comparison results to an Excel file using `openxlsx::write.xlsx()`.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' nfsa_T0800_ANA(country = "AT", year = 2024, threshold = 0.5)
#' }
#'
#' @export
nfsa_T0800_ANA <- function(country,
                           year,
                           threshold = 1,
                           input_sel = here::here("data", "a", "new"),
                           output_sel = here::here("output", "inter_domain")){
  pacman::p_load(tidyverse,arrow,readxl,writexl,here,readsdmx)
  options(warn=-1)

  cli::cli_progress_message("Collecting NFSA...")

  limit_validation <- paste0(as.numeric(year -4))
  nfsa_data <- nfsa_get_data(country = country,table = "T0800", type = "new") |>
    select(ref_area,id,time_period,nfsa = obs_value)  |>
    nfsa::nfsa_separate_id()

  ## NAMA -----
  cli::cli_progress_message("Collecting ANA...")


  nama_files <- list.files(path = paste0("M:/nas/ASA10/Production/",year,"/(1) ASA/(1_2) Validation in progress/(1_2_4) Consistency checks - ASA vs ANA/Input"),
                           pattern = "xml$",
                           full.names = TRUE,
                           recursive = TRUE) |>
    as_tibble() |>
    mutate(ref_area = str_sub(value,-30,-29),
           update = as.numeric(str_sub(value,-18,-4))) |>
    filter(ref_area %in% country) |>
    group_by(ref_area) |>
    arrange(update,.by_group = TRUE) |>
    slice_tail(n=1) |>
    ungroup() |>
    select(value) |>
    pull(value)


  read_nama_files <- function(file){
    nama_data <- read_sdmx(file) |>
      janitor::clean_names() |>
      select(ref_area,ref_sector,sto,unit_measure,accounting_entry,counterpart_area,time_period,nama = obs_value) |>
      mutate(accounting_entry = if_else(sto =="EMP" & unit_measure =="PS"& counterpart_area == "W2","PS",accounting_entry),
             accounting_entry = if_else(sto =="EMP" & unit_measure =="HW"& counterpart_area == "W2","HW",accounting_entry)) |>
      select(-unit_measure,-counterpart_area) |>
      mutate(nama = as.numeric(nama)) |>
      distinct()
    return(nama_data)
  }

  nama_data <- map(nama_files,read_nama_files) |>
    list_rbind()


  nama_nfsa <- full_join(nama_data, nfsa_data,by = join_by(ref_area, ref_sector, sto, accounting_entry,
                                                           time_period)) |>
    na.omit() |>
    mutate(diff = round(nfsa-nama,2)) |>
    group_by(ref_area,time_period) |>
    mutate(as_GDP = round(diff*100/nama[ref_sector == "S1" & sto == "B1GQ" & accounting_entry == "B"],3)) |>
    ungroup() |>
    filter(abs(diff)>threshold) |>
    mutate(threshold = ifelse(abs(as_GDP) > 0.3, TRUE,FALSE)) |>
    mutate(validate = ifelse(threshold == TRUE &
                               sto == "B1GQ" &
                               time_period>= limit_validation,
                             "NOT VALIDATED",""))



  if (nrow(nama_nfsa) == 0) {
    cli::cli_alert_success("T0800 and ANA fully consistent!")

  }

  if (length(unique(nama_nfsa$ref_area)) == 1) {
    openxlsx::write.xlsx(nama_nfsa,
                         file = paste0(output_sel,"/T0800_ANA_",unique(nama_nfsa$ref_area), "_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"),
                         overwrite = TRUE,
                         asTable = TRUE)

    cli::cli_alert_success(paste0("File created in ", output_sel,"/T0800_ANA_",unique(nama_nfsa$ref_area), "_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"))

  }

  if (length(unique(nama_nfsa$ref_area)) > 1) {
    openxlsx::write.xlsx(nama_nfsa,
                         file = paste0(output_sel,"/T0800_ANA_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"),
                         overwrite = TRUE,
                         asTable = TRUE)

    cli::cli_alert_success(paste0("File created in ", output_sel,"/T0800_ANA_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"))
  }
  return(nama_nfsa)
  options(warn=0)
}


