#' Reads and cleans NFSA data from a semicolon-separated CSV file.
#'
#' This function reads a CSV file, normally found in `I:/econ/NFSA/OUTPUT/` cleans the column names, and selects
#' a specific set of columns relevant to NFSA data.
#'
#' @param file A character string specifying the path to the CSV file.
#' @param full Bring collapsed columns by default (`FALSE`) or all columns (`TRUE`).
#'
#' @return A data frame containing the cleaned and selected NFSA data.
#' The data frame includes the following columns: `table_identifier`,
#' `freq`, `adjustment`, `ref_area`, `counterpart_area`, `ref_sector`,
#' `counterpart_sector`, `consolidation`, `accounting_entry`, `sto`,
#' `instr_asset`, `maturity`, `expenditure`, `unit_measure`,
#' `currency_denom`, `valuation`, `prices`, `transformation`,
#' `cust_breakdown`, `time_period`, `obs_value`, `obs_status`,
#' `conf_status`, `id`.
#' @importFrom janitor clean_names
#' @importFrom readr read_delim
#' @import dplyr

#' @export
#'
#' @examples
#' \dontrun{
#' # Read an NFSA CSV file
#' nfsa_data <- nfsa_read_xlsx_csv("I:/econ/NFSA/OUTPUT/SE1_2025Q2_F.xlsx.csv")
#'
#' # Display the first few rows of the data
#' head(nfsa_data)
#' }
nfsa_read_xlsx_csv <- function(file,
                               full = FALSE){

  data <- readr::read_delim(file,
                            delim = ";",
                            , show_col_types = FALSE) |>
    janitor::clean_names() |>
    dplyr::select(table_identifier, freq, adjustment, ref_area,
                  counterpart_area,ref_sector, counterpart_sector,
                  consolidation, accounting_entry, sto, instr_asset,
                  maturity, expenditure,unit_measure,currency_denom,
                  valuation, prices, transformation, cust_breakdown,
                  time_period,obs_value,obs_status,conf_status)|>
    dplyr::left_join(y = nfsa_sto_lookup)

  if (full == FALSE){
    data <- data |>
      dplyr::select(ref_area,id,adjustment,time_period,obs_value)
  }
  if (stringr::str_detect(head(data$time_period,n=1),"Q") == TRUE){
    data <- data |>
      dplyr::mutate(time_period = paste0(stringr::str_sub(time_period,1,4),
                                         "-",
                                         stringr::str_sub(time_period,5,6)))
  }

  return(data)
}
