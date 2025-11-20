#' @title Compare NFSA T0801 Data with Quarterly National Accounts (QNA)
#'
#' @description This function compares the NFSA T0801 data with the Quarterly National Accounts (QNA) data, identifies discrepancies based on a specified threshold, and generates an Excel file containing the comparison results.
#'
#' @param country A character vector specifying the country or countries to process (ISO2 code).
#' @param quarter A character string specifying the quarter in "YYYYQQ" format (e.g., "2023Q1").
#' @param threshold A numeric value indicating the absolute difference threshold for flagging discrepancies between NFSA and QNA data. Default is 1.
#' @param input_sel A character string specifying the directory containing the NFSA data. Default is `here::here("data", "q", "new", "nsa")`.
#' @param output_sel A character string specifying the directory to save the output Excel file. Default is `here::here("output", "inter_domain")`.
#'
#' @return None. The function generates an Excel file in the `output_sel` directory with the comparison results.  It also prints a success message to the console indicating where the file was saved or that the data is consistent.
#'
#' @details This function performs the following steps:
#'   \enumerate{
#'     \item Loads NFSA T0801 data using the `nfsa_get_data` function.
#'     \item Reads relevant QNA data from XML files within a specified directory structure.
#'     \item Merges the NFSA and QNA data based on common keys.
#'     \item Calculates the absolute difference between NFSA and QNA values.
#'     \item Flags discrepancies where the absolute difference exceeds the specified `threshold`.
#'     \item Generates an Excel file containing the comparison results.
#'   }
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' nfsa_T0801_QNA(country = "BE", quarter = "2023Q2", threshold = 2)
#' }
#'
#' @export
nfsa_T0801_QNA <- function(country,
                           quarter,
                           threshold = 1,
                           input_sel = here::here("data", "q", "new", "nsa"),
                           output_sel = here::here("output", "inter_domain")){
  pacman::p_load(tidyverse,arrow,readxl,writexl,here,readsdmx)
  options(warn=-1)
  limit_validation <- paste0(as.numeric(str_sub(quarter,1,4)) -4,"-",str_sub(quarter,5,6))


  cli::cli_progress_message("Collecting NFSA...")
  nfsa_data <- nfsa_get_data(country = country,table = "T0801", type = "new") |>
    select(ref_area,id,time_period,nfsa = obs_value)  |>
    nfsa::nfsa_separate_id()

  ## NAMA -----
  cli::cli_progress_message("Collecting QNA...")


  nama_files <- list.files(path = paste0("M:/nas/QSA10/Production/",quarter,"/(1) QSA/(1_2) Validation in progress/(1_2_4) Consistency checks - QSA vs QNA/Input"),
                           pattern = "xml$",
                           full.names = TRUE,
                           recursive = TRUE) |>
    as_tibble() |>
    mutate(ref_area = str_sub(value,-32,-31),
           update = as.numeric(str_sub(value,-18,-5))) |>
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
    cli::cli_alert_success("T0801 and QNA fully consistent!")

  }

  if (length(unique(nama_nfsa$ref_area)) == 1) {
    openxlsx::write.xlsx(nama_nfsa,
                         file = paste0(output_sel,"/T0801_QNA_",unique(nama_nfsa$ref_area), "_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"),
                         overwrite = TRUE,
                         asTable = TRUE)

    cli::cli_alert_success(paste0("File created in ", output_sel,"/T0801_QNA_",unique(nama_nfsa$ref_area), "_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"))

  }

  if (length(unique(nama_nfsa$ref_area)) > 1) {
    openxlsx::write.xlsx(nama_nfsa,
                         file = paste0(output_sel,"/T0801_QNA_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"),
                         overwrite = TRUE,
                         asTable = TRUE)

    cli::cli_alert_success(paste0("File created in ", output_sel,"/T0801_QNA_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"))
  }
  options(warn=0)
  return(nama_nfsa)
}


