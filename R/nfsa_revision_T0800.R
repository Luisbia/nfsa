#' @title Identify and Report Revisions in T0800 Data
#'
#' @description This function identifies and reports revisions between two versions of T0800 data.
#' It compares the `obs_value` in the new and previous datasets, calculates the absolute and
#' relative revisions, and filters the results based on specified thresholds.
#'
#' @param country A character vector specifying the country or countries to analyze.
#'        This should correspond to the country codes used in the input file names.
#' @param abs_threshold A numeric value specifying the minimum absolute revision value to consider.
#'        Revisions with an absolute value less than this threshold will be filtered out. Defaults to 100.
#' @param rel_threshold A numeric value specifying the minimum relative revision value (as a percentage of GDP) to consider.
#'        Revisions with an absolute relative value less than this threshold will be filtered out. Defaults to 0.
#' @param output_sel A character string specifying the path to the directory where the output Excel file will be saved.
#'        Defaults to `here("output", "revisions")`.
#'
#' @return This function does not return a value, but saves an Excel file containing the identified revisions to the specified output directory.
#' The file name includes a timestamp and the country code (if a single country is selected).  If no revisions meet the threshold criteria, a message will be displayed in the console.
#'
#' @details The function performs the following steps:
#'   \enumerate{
#'     \item Loads the new and previous T0800 data files from the specified directories. The function extracts the version and country code from the filenames.
#'     \item Cleans and transforms the data, including joining with a lookup table (`nfsa::nfsa_sto_lookup`),
#'           separating the `id` column into `ref_sector`, `sto`, and `accounting_entry`, and removing rows with missing values.
#'     \item Calculates the absolute revision (`rev`) as the difference between the new and previous `obs_value`.
#'     \item Calculates the relative revision (`revp`) as the percentage change relative to the previous `obs_value`.
#'     \item Calculates the revision as a percentage of GDP (`as_GDP`).
#'     \item Filters the revisions based on the specified `abs_threshold` and `rel_threshold`.
#'     \item Saves the filtered revisions to an Excel file in the specified output directory.
#'   }
#'
#'
#' @examples
#' \dontrun{
#' # Example usage with default thresholds and file paths:
#' nfsa_revision_T0800(country = "BE")
#'
#' # Example usage with custom thresholds:
#' nfsa_revision_T0800(country = c("IT", "ES"),
#'                      abs_threshold = 50,
#'                      rel_threshold = 0.5,
#'                      output_sel = here("path", "to", "output"))
#' }
#'
#' @export
nfsa_revision_T0800 <- function(country,
                                abs_threshold = 100,
                                rel_threshold = 0,
                                output_sel = here("output", "revisions")){
  pacman::p_load(tidyverse,arrow, here,openxlsx, readxl)
  lookup <- nfsa::nfsa_sto_lookup

new_db <- nfsa::nfsa_get_data(country = country, table = "T0800", type = "new") |>
  select(ref_area,id,time_period,new = obs_value)  |>
  nfsa::nfsa_separate_id() |>
  na.omit()

prev_db <- nfsa::nfsa_get_data(country = country, table = "T0800", type = "prev") |>
  select(ref_area,id,time_period,prev = obs_value)  |>
  nfsa::nfsa_separate_id() |>
  na.omit()


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
  cli::cli_alert_success("No revisions in T0800!")}

if(length(unique(new_prev_db$ref_area)) == 1) {
    openxlsx::write.xlsx(new_prev_db,
                         file = paste0(output_sel,"/revisions_T0800_",unique(new_prev_db$ref_area),"_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"),
                         overwrite = TRUE,
                         asTable = TRUE)

    cli::cli_alert_success(paste0("File created in ",output_sel,"/revisions_T0800_",unique(new_prev_db$ref_area),"_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"))
    }

if(length(unique(new_prev_db$ref_area)) > 1) {
  openxlsx::write.xlsx(new_prev_db,
                       file = paste0(output_sel,"/revisions_T0800_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"),
                       overwrite = TRUE,
                       asTable = TRUE)

  cli::cli_alert_success(paste0("File created in ",output_sel,"/revisions_T0800_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"))
}
return(new_prev_db)
}
