#' @title Check for Negative Values in T0801SA when T0801 is all Positive
#'
#' @description This function checks for negative values in the T0801SA table from the nfsa database
#'  when all corresponding values in the T0801 table are positive.
#'
#' @param country A character vector specifying the countries to retrieve data for.
#'   Defaults to a selection of European countries (e.g., "AL", "AT", "BE").
#'   See the nfsa database for a complete list of available countries.
#'
#' @param output_sel A character string specifying the directory where the output Excel file will be saved.
#'  Defaults to "output/negatives_zeroes" within the project directory (using `here::here()`).
#'
#' @return  If negative values are found in T0801SA for IDs where all T0801 values are positive,
#' an Excel file is written to the directory specified by `output_sel`.
#' The file name includes a timestamp. A success message with the file path is printed to the console.
#' If no such negative values are found, an informative message is printed to the console. The function returns NULL invisibly.
#'
#' @examples
#' \dontrun{
#' # Run the check for a specific country and save the output to a custom directory
#' nfsa_negatives_T0801_T0801SA(country = "DE", output_sel = "my_output_folder")
#' }
#'
#' @export
nfsa_negatives_T0801_T0801SA <- function(country =c("AL", "AT", "BE", "CY", "CZ", "DE", "DK", "EE",
                                                    "EL", "ES", "FI", "FR", "HR", "HU", "IE", "IT",
                                                    "IS","LT", "LU","LV", "MT", "NL", "NO", "PL",
                                                    "PT", "RO", "SE", "SI", "SK", "TR", "CH", "RS",
                                                    "ME", "MK"),
                                         output_sel = here::here("output", "negatives_zeroes")){

  pacman::p_load(tidyverse, arrow, here)

  nsa_pos <- nfsa::nfsa_get_data(country = country,
                                 table = "T0801",
                                 type = "new") |>
    group_by(ref_area,id) |>
    mutate(n = n()) |>
    ungroup() |>
    group_by(ref_area,id, time_period) |>
    mutate(pos = ifelse(obs_value >= 0, 1, 0)) |>
    ungroup() |>
    mutate(pos = sum(pos), .by = c(ref_area,id)) |>
    filter(pos == n) |>
    select(ref_area,id) |>
    distinct()

  # T0801SA
  t0801sa <- nfsa::nfsa_get_data(country = country,
                                 table = "T0801SA",
                                 type = "new") |>
    filter(obs_value <0)


  check <- left_join(nsa_pos, t0801sa, by = join_by(ref_area,id)) |>
    na.omit()


  if(nrow(check) == 0){ cli::cli_inform("No negatives in T0801SA when all values positive in T0801")
  }  else {


    openxlsx::write.xlsx(check,
                         file = paste0(output_sel, "/_T0801_T0801SA_negatives_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"),
                         overwrite = TRUE,
                         asTable = TRUE)
    cli::cli_alert_success(paste0("File created in ",  output_sel, "/_T0801_T0801SA_negatives_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"))
  }
  return(check)
}

