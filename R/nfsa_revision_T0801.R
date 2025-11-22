#' Detects and reports revisions in T0801 data between two versions of the database.
#'
#' This function compares new and previous versions of T0801 data, identifies revisions based on absolute and relative thresholds,
#' and exports the revisions to an Excel file.  It utilizes `arrow` for efficient data handling and `openxlsx` for Excel output.
#'
#' @param country A character vector specifying the countries to analyze (ISO2 codes).
#' @param abs_threshold A numeric value specifying the absolute threshold for revision detection. Revisions with an absolute difference greater than this threshold are flagged. Defaults to 10.
#' @param rel_threshold A numeric value specifying the relative threshold (percentage of GDP) for revision detection. Revisions with a relative difference greater than this threshold are flagged. Defaults to 0.
#' @param output_sel Path to the directory where the output Excel file containing the revisions will be saved. Defaults to `here("output", "revisions")`.
#'
#' @return This function does not return a value. It generates an Excel file containing the identified revisions in the specified output directory.  A success message is printed to the console indicating the file name and location.
#'
#' @examples
#' \dontrun{
#' # Example usage with default thresholds:
#' nfsa_revision_T0801(country = c("ES", "IT"))
#'
#' # Example usage with custom thresholds:
#' nfsa_revision_T0801(country = c("ES", "IT"), abs_threshold = 20, rel_threshold = 0.5)
#' }
#'
#' @export
nfsa_revision_T0801 <- function(country,
                                abs_threshold = 10,
                                rel_threshold = 0,
                                output_sel = here("output", "revisions")){
  pacman::p_load(tidyverse,arrow, here,openxlsx, readxl)
  lookup <- nfsa::nfsa_sto_lookup

new_db <- nfsa::nfsa_get_data(country = country, table = "T0801", type = "new") |>
  select(ref_area,id,time_period,new = obs_value)  |>
  separate_wider_delim(cols = id,
                       delim = ".",
                       names = c("ref_sector", "sto", "accounting_entry"))


prev_db <- nfsa::nfsa_get_data(country = country, table = "T0801", type = "prev") |>
  select(ref_area,id,time_period,prev = obs_value)  |>
  separate_wider_delim(cols = id,
                       delim = ".",
                       names = c("ref_sector", "sto", "accounting_entry"))


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
  cli::cli_alert_success("No revisions in T0801!")}

if(length(unique(new_prev_db$ref_area)) == 1) {
    openxlsx::write.xlsx(new_prev_db,
                         file = paste0(output_sel,"/revisions_T0801_",country,"_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"),
                         overwrite = TRUE,
                         asTable = TRUE)

    cli::cli_alert_success(paste0("File created in ",output_sel,"/revisions_T0801_",country,"_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"))
    }

if(length(unique(new_prev_db$ref_area)) > 1) {
  openxlsx::write.xlsx(new_prev_db,
                       file = paste0(output_sel,"/revisions_T0801_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"),
                       overwrite = TRUE,
                       asTable = TRUE)

  cli::cli_alert_success(paste0("File created in ",output_sel,"/revisions_T0801_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"))
}
return(new_prev_db)
}
