#' Identify and extract revisions in NASEC T0801 data across different versions.
#'
#' This function identifies and extracts revisions in the NASEC T0801 data for a specified country,
#' comparing different versions of the data stored as Parquet files. It joins the data with a lookup table,
#' calculates the changes between versions, and prepares the data for export to Excel.
#'
#' @param country A character string specifying the country code to filter the data.
#' @param input_sel A character string specifying the path to the directory containing the Parquet files.
#'   Defaults to `"data/q"` relative to the project root.
#' @param output_sel Path to the directory where the output Excel file containing the revisions will be saved. Defaults to `here("output", "revisions")`.
#' @return An Excel workbook containing the identified revisions,
#'   with columns for the reference area, ID, time period, reference sector, STO, accounting entry,
#'   and the changes between different versions of the data.
#'
#' @examples
#' \dontrun{
#' # Example usage: Identify and extract revisions for country code "IT"
#' # from Parquet files located in the default "data/q" directory.
#' nfsa_revision_T0801_all(country = "IT")
#'
#' # Example usage: Identify and extract revisions for country code "BE"
#' # from Parquet files located in a custom directory.
#' nfsa_revision_T0801_all(country = "BE", input_sel = "path/to/my/data")
#' }
#'
#' @export
nfsa_revision_T0801_all <- function(country,
                                    input_sel = here::here("data", "q"),
                                    output_sel = here::here("output", "revisions")){

library(arrow)
library(tidyverse)
lookup <- nfsa::nfsa_sto_lookup

revisions <- list.files(path = input_sel,
                        pattern = paste0("^NASEC_T0801_Q_", country, "_.*\\.parquet$"),
                        full.names = TRUE,
                        recursive = TRUE) |>
  open_dataset() |>
  collect() %>%
  left_join(.,lookup,by = join_by(counterpart_area, ref_sector, counterpart_sector,
                                  consolidation, accounting_entry, sto, instr_asset, unit_measure, prices)) |>
  na.omit() |>
  select(version,ref_area,id,time_period,obs_value) |>
  arrange(version) |>
  group_by(ref_area,id,time_period) |>
  mutate(change = obs_value-lag(obs_value)) |>
  na.omit() |>
  filter(change != 0) |>
  ungroup() |>
  select(-obs_value) |>
  nfsa_separate_id() |>
  pivot_wider(names_from = version,
              values_from = change)

if(length(unique(revisions$ref_area)) == 1) {
  openxlsx::write.xlsx(revisions,
                       file = paste0(output_sel,"/revisions_T0801_all_",unique(revisions$ref_area),"_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"),
                       overwrite = TRUE,
                       asTable = TRUE)

  cli::cli_alert_success(paste0("File created in ",output_sel,"/revisions_T0801_all_",unique(revisions$ref_area),"_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"))
}

if(length(unique(revisions$ref_area)) > 1) {
  openxlsx::write.xlsx(revisions,
                       file = paste0(output_sel,"/revisions_T0801_all_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"),
                       overwrite = TRUE,
                       asTable = TRUE)

  cli::cli_alert_success(paste0("File created in ",output_sel,"/revisions_T0801_all_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"))
}
  return(revisions)
}
