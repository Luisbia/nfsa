#' Identify and extract revisions in NASEC T0801 data across different versions.
#'
#' This function identifies and extracts revisions in the NASEC T0801 data for a specified country,
#' comparing different versions of the data stored as Parquet files. It joins the data with a lookup table,
#' calculates the changes between versions, and prepares the data for export to Excel.
#'
#' This is a wrapper around `nfsa_revision_all()` for backward compatibility.
#' Consider using `nfsa_revision_all(table = "T0801")` directly.
#'
#' @param country A character string specifying the country code to filter the data. Only one country!
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
#' nfsa_revision_T0801_all(country = "IT")
#'
#' # Example with filters: Only revisions for sector S13
#' nfsa_revision_T0801_all(country = "IT", "ref_sector == 'S13'")
#'
#' # Example with multiple filters
#' nfsa_revision_T0801_all(country = "IT", "ref_sector == 'S13'", "sto == 'B9'")
#'
#' # Example with custom directory
#' nfsa_revision_T0801_all(country = "BE", input_sel = "path/to/my/data")
#' }
#'
#' @export
nfsa_revision_T0801_all <- function(country,
                                    input_sel = "M:/nas/Rprod/data/",
                                    output_sel = here::here("output", "revisions"),
                                    ...) {

  # Deprecation warning
  lifecycle::deprecate_soft(
    when = "0.2.0",
    what = "nfsa_revision_T0801_all()",
    with = "nfsa_revision_all(table = 'T0801')"
  )

  nfsa_revision_all(
    country = country,
    table = "T0801",
    input_sel = input_sel,
    output_sel = output_sel,
    ...
  )
}
