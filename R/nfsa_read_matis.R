#' Read and transform MATIS data from a CSV file.
#'
#' This function reads a CSV file containing MATIS data, transforms it into a long format,
#' separates variables, and converts the observation value to numeric.
#'
#' @param file The path to the CSV file containing the MATIS data.
#'
#' @return A tibble containing the transformed MATIS data. The tibble includes columns for
#'   various MATIS dimensions extracted from the variable names in the original CSV
#'
#'
#' @examples
#' \dontrun{
#' # Assuming you have a MATIS data file named "matis_data.csv" in your working directory
#' matis_data <- nfsa_read_matis("matis_data.csv")
#'
#' # Print the first few rows of the transformed data
#' head(matis_data)
#' }
#'
#' @export
nfsa_read_matis <- function(file) {
library(tidyverse)
data <-read_csv(file, show_col_types = FALSE) |>
  pivot_longer(cols = 2:last_col(),
               names_to = "time_period",
               values_to = "obs_value") |>
  separate_wider_delim(cols = 1,
                       delim = ".",
                       names = c("d1","type","table_identifier", "freq", "adjustment", "ref_area",
                                 "counterpart_area","ref_sector", "counterpart_sector",
                                 "consolidation", "accounting_entry", "sto", "instr_asset",
                                 "maturity", "expenditure","unit_measure","currency_denom",
                                 "valuation", "prices", "transformation", "cust_breakdown"),
                       too_many = "merge") |>
mutate(obs_value = as.numeric(obs_value))

return(data)
}


