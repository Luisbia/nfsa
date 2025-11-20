#' Identify and report revisions in T0801SA data.
#'
#' This function compares new and previous versions of T0801SA data, identifies
#' revisions exceeding specified thresholds, and generates an Excel report.
#'
#' @param country A character vector specifying the countries to analyze (ISO2 codes).
#' @param abs_threshold A numeric value specifying the absolute threshold for revisions. Defaults to 10.
#' @param rel_threshold A numeric value specifying the relative threshold (as % of GDP) for revisions. Defaults to 0.
#' @param output_sel A character string specifying the path to the directory where the revision report should be saved.
#'
#' @return This function does not return a value, but it saves an Excel file containing the identified revisions to the `output_sel` directory.  It also prints a success message to the console indicating the location of the file. If no revisions are found, a corresponding message is printed to the console.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' nfsa_revision_T0801SA(country = c("IE", "ES"),
#'                       abs_threshold = 5,
#'                       rel_threshold = 0.1,
#'                       output_sel = here("output", "revisions"))
#' }
#'
#' @export
nfsa_revision_T0801SA <- function(country,
                                abs_threshold = 10,
                                rel_threshold = 0,
                                output_sel = here("output", "revisions")){
  pacman::p_load(tidyverse,arrow, here,openxlsx, readxl)
  lookup <- nfsa::nfsa_sto_lookup

new_db <- nfsa::nfsa_get_data(country = country, table = "T0801SA", type = "new") |>
  select(ref_area,id,time_period,new = obs_value)  |>
  nfsa::nfsa_separate_id()


prev_db <- nfsa::nfsa_get_data(country = country, table = "T0801SA", type = "prev") |>
  select(ref_area,id,time_period,prev = obs_value)  |>
  nfsa::nfsa_separate_id()



new_prev_db <- full_join(new_db,prev_db,by = join_by(ref_area, ref_sector, sto, accounting_entry,
                                                     time_period)) |>
  mutate(rev = new - prev) |>
  mutate(revp= round(rev*100/prev,1))

tmp <- prev_db |>
  filter(sto == "B1GQ") |>
  select(ref_area,time_period,GDP = prev)

new_prev_db <- left_join(new_prev_db,tmp, by = join_by(ref_area,time_period)) |>
  mutate(as_GDP = round(rev*100/GDP,1)) |>
  select(-GDP) |>
  filter(abs(rev) > abs_threshold) |>
  filter(abs(as_GDP) > rel_threshold)



if (nrow(new_prev_db) == 0) {
  cli::cli_alert_success("No revisions in T0801SA!")}

if(length(unique(new_prev_db$ref_area)) == 1) {
    openxlsx::write.xlsx(new_prev_db,
                         file = paste0(output_sel,"/revisions_T0801SA_",country,"_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"),
                         overwrite = TRUE,
                         asTable = TRUE)

    cli::cli_alert_success(paste0("File created in ",output_sel,"/revisions_T0801SA_",country,"_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"))
    }

if(length(unique(new_prev_db$ref_area)) > 1) {
  openxlsx::write.xlsx(new_prev_db,
                       file = paste0(output_sel,"/revisions_T0801SA_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"),
                       overwrite = TRUE,
                       asTable = TRUE)

  cli::cli_alert_success(paste0("File created in ",output_sel,"/revisions_T0801SA_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"))
}
return(new_prev_db)
}
