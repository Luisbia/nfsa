#' Check Consistency Between NFSA Tables T0801 and T0800
#'
#' This function compares data from NFSA tables T0801 and T0800 for a given country,
#' identifies discrepancies exceeding a specified threshold, and outputs the results to an Excel file.
#'
#' @param country A character string specifying the country code for the NFSA data.
#' @param threshold A numeric value indicating the threshold for acceptable differences between T0801 and T0800 (default is 2).
#' @param output_sel A character string specifying the directory where the output Excel file should be saved (default is "output/frequency" relative to the project root).
#'
#' @return This function primarily operates for its side effect of writing an Excel file.
#'   It returns NULL invisibly. A success message is printed to the console if the tables are consistent or if the output file is created.
#'
#' @examples
#' \dontrun{
#' nfsa_T0801_T0800(country = "BE", threshold = 1.5)
#' }
#'
#' @export
nfsa_T0801_T0800 <- function(country,
                             threshold = 2,
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
    summarise(qnfsa = sum(obs_value),.by = c(ref_area,ref_sector,sto,accounting_entry,time_period))

  # T0800
  a_nfsa_data <- nfsa::nfsa_get_data(country = country, table = "T0800", type = "new")  |>
    nfsa::nfsa_separate_id() |>
    rename(anfsa= obs_value)


  qnfsa_anfsa <- left_join(q_nfsa_data,a_nfsa_data, by = join_by(ref_area, ref_sector, sto, accounting_entry,
                                                                 time_period)) |>
    mutate(diff = round(qnfsa-anfsa,2)) |>
    filter(abs(diff)> threshold)

  tmp <- a_nfsa_data |>
    filter(sto == "B1GQ") |>
    select(ref_area,time_period,gdp=anfsa)

  qnfsa_anfsa <- left_join(qnfsa_anfsa,tmp, by = join_by(ref_area, time_period)) |>
    mutate(as_GDP = round(diff*100/gdp,3)) |>
    mutate(threshold = ifelse(abs(as_GDP) > 0.3, TRUE,FALSE))



  if (nrow(qnfsa_anfsa) == 0) {
    cli::cli_alert_success("T0801 and T0800 fully consistent!")

  } else {

  openxlsx::write.xlsx(qnfsa_anfsa,
                       file = paste0(output_sel,"/T0801_T0800_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"),
  overwrite = TRUE,
  asTable = TRUE)
cli::cli_alert_success(paste0("File created in", paste0(output_sel,"/T0801_T0800_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx")))
  }
  return(qnfsa_anfsa)
}



