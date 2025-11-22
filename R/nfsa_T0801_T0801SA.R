#' Check Consistency Between NFSA Tables T0801 and T0801SA
#'
#' This function compares data from NFSA tables T0801 and T0801SA for a given country,
#' identifies discrepancies exceeding a specified threshold, and outputs the results to an Excel file.
#'
#' @param country A character string specifying the country code for the NFSA data.
#' @param threshold A numeric value indicating the threshold expressed as % for acceptable differences between T0801 and T0800 (default is 2%).
#' @param output_sel A character string specifying the directory where the output Excel file should be saved (default is "output/frequency" relative to the project root).
#'
#' @return This function primarily operates for its side effect of writing an Excel file.
#'   It returns NULL invisibly. A success message is printed to the console if the tables are consistent or if the output file is created.
#'
#' @examples
#' \dontrun{
#' nfsa_T0801_T0801SA(country = "BE", threshold = 1.5)
#' }
#'
#' @export
nfsa_T0801_T0801SA <- function(country,
                               threshold = 3,
                               output_sel = here::here("output", "frequency")){

  pacman::p_load(tidyverse,here,arrow,openxlsx, readsdmx, janitor)

  # T0801
  q_nfsa_data <- nfsa::nfsa_get_data(country = country, table = "T0801", type = "new")  |>
    nfsa::nfsa_separate_id() |>
    mutate(year=str_sub(time_period,1,4)) |>
    group_by(ref_area,ref_sector,sto,accounting_entry,year) |>
    mutate(n=n()) |>
    ungroup() |>
    filter(n==4) |>
    select(-n,-time_period) |>
    rename(time_period = year) |>
    summarise(nsa = sum(obs_value),.by = c(ref_area,ref_sector,sto,accounting_entry,time_period))

  # T0801SA
  qy_nfsa_data <- nfsa::nfsa_get_data(country = country, table = "T0801SA", type = "new")  |>
    nfsa::nfsa_separate_id() |>
    mutate(year=str_sub(time_period,1,4)) |>
    group_by(ref_area,ref_sector,sto,accounting_entry,year) |>
    mutate(n=n()) |>
    ungroup() |>
    filter(n==4) |>
    select(-n,-time_period) |>
    rename(time_period = year) |>
    summarise(sca = sum(obs_value),.by = c(ref_area,ref_sector,sto,accounting_entry,time_period))


  qnfsa_qynfsa <- left_join(qy_nfsa_data,q_nfsa_data, by = join_by(ref_area, ref_sector, sto, accounting_entry,
                                                                   time_period)) |>
    mutate(diff = round(nsa-sca,2),
           diff_p = 100*(nsa/sca)-100) |>
    filter(abs(diff_p) > threshold)



  if (nrow(qnfsa_qynfsa) == 0) {
    cli::cli_alert_success("T0801 and T0801SA are consistent according to the threshold used!")

  } else {

    openxlsx::write.xlsx(qnfsa_qynfsa,
                         file = paste0(output_sel,"/T0801_T0801SA_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"),
                         overwrite = TRUE,
                         asTable = TRUE)
    cli::cli_alert_success(paste0("File created in ", paste0(output_sel,"/T0801_T0801SA_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx")))
  }
  return(qnfsa_qynfsa)
}

