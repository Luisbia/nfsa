#' Identify and extract revisions in NASEC T0801SA data across different versions.
#'
#' This function identifies and extracts revisions in the NASEC T0801SA data for a specified country,
#' comparing different versions of the data stored as Parquet files. It joins the data with a lookup table,
#' calculates the changes between versions, and prepares the data for export to Excel.
#'
#' @param country A character string specifying the country code to filter the data.
#' @param input_sel A character string specifying the path to the directory containing the Parquet files.
#'   Defaults to `"M:/nas/Rprod/data/"`.
#' @param output_sel Path to the directory where the output Excel file containing the revisions will be saved.
#'   Defaults to `here("output", "revisions")`.
#' @param ... Additional filters to pass to nfsa_get_data_all(). Filters should be provided as character strings
#'   (e.g., "ref_sector == 'S13'", "sto == 'B9'"). Multiple filters can be provided as separate arguments.
#'
#' @return An Excel workbook containing the identified revisions,
#'   with columns for the reference area, ID, time period, reference sector, STO, accounting entry,
#'   and the changes between different versions of the data.
#'
#' @examples
#' \dontrun{
#' # Example usage: Identify and extract revisions for country code "IT"
#' nfsa_revision_T0801SA_all(country = "IT")
#'
#' # Example with filters: Only revisions for sector S13
#' nfsa_revision_T0801SA_all(country = "IT", "ref_sector == 'S13'")
#'
#' # Example with multiple filters
#' nfsa_revision_T0801SA_all(country = "IT", "ref_sector == 'S13'", "sto == 'B9'")
#'
#' # Example with custom directory
#' nfsa_revision_T0801SA_all(country = "BE", input_sel = "path/to/my/data")
#' }
#'
#' @export
nfsa_revision_T0801SA_all <- function(country,
                                      input_sel = "M:/nas/Rprod/data/",
                                      output_sel = here::here("output", "revisions"),
                                      ...) {

  # Capture filters from ...
  filters <- c(...)

  # Get all versions of data using nfsa_get_data_all with filters
  revisions <- nfsa_get_data_all(
    input_sel = input_sel,
    country = country,
    table = "T0801SA",
    filters = if (length(filters) > 0) filters else NULL
  ) |>
    dplyr::select(version, ref_area, id, time_period, obs_value) |>
    dplyr::arrange(version) |>
    dplyr::group_by(ref_area, id, time_period) |>
    dplyr::mutate(change = obs_value - dplyr::lag(obs_value)) |>
    tidyr::drop_na() |>
    dplyr::filter(change != 0) |>
    dplyr::ungroup() |>
    dplyr::select(-obs_value) |>
    nfsa::nfsa_separate_id() |>
    tidyr::pivot_wider(names_from = version,
                       values_from = change)

  # Write output
  if (nrow(revisions) > 0) {
    if (length(unique(revisions$ref_area)) == 1) {
      openxlsx::write.xlsx(revisions,
                           file = paste0(output_sel, "/revisions_T0801SA_all_",
                                         unique(revisions$ref_area), "_",
                                         as.character(format(Sys.time(), "%Y%m%d_%H%M%S")), ".xlsx"),
                           overwrite = TRUE,
                           asTable = TRUE)

      cli::cli_alert_success("File created in {output_sel}/revisions_T0801SA_all_{unique(revisions$ref_area)}_{as.character(format(Sys.time(), '%Y%m%d_%H%M%S'))}.xlsx")
    } else {
      openxlsx::write.xlsx(revisions,
                           file = paste0(output_sel, "/revisions_T0801SA_all_",
                                         as.character(format(Sys.time(), "%Y%m%d_%H%M%S")), ".xlsx"),
                           overwrite = TRUE,
                           asTable = TRUE)

      cli::cli_alert_success("File created in {output_sel}/revisions_T0801SA_all_{as.character(format(Sys.time(), '%Y%m%d_%H%M%S'))}.xlsx")
    }
    nfsa::nfsa_to_excel(revisions)
  } else {
    cli::cli_alert_info("No revisions found for {country}")
  }

  return(revisions)
}
