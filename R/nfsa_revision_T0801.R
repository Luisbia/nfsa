#' Detects and reports revisions in T0801 data between two versions of the database.
#'
#' This function compares new and previous versions of T0801 data, identifies revisions based on absolute and relative thresholds,
#' and exports the revisions to an Excel file.  It utilizes `arrow` for efficient data handling and `openxlsx` for Excel output.
#'
#' This is a wrapper around `nfsa_revision()` for backward compatibility.
#' Consider using `nfsa_revision(table = "T0801")` directly.
#'
#' @param country A character vector specifying the countries to analyze (ISO2 codes).
#' @param type Either to compare with the last version of the previous quarter ("vintage") or with the previous version of this period ("version")
#' @param abs_threshold A numeric value specifying the absolute threshold for revision detection. Revisions with an absolute difference greater than this threshold are flagged. Defaults to 10.
#' @param rel_threshold A numeric value specifying the relative threshold (percentage of GDP) for revision detection. Revisions with a relative difference greater than this threshold are flagged. Defaults to 0.
#' @param output_sel Path to the directory where the output Excel file containing the revisions will be saved. Defaults to `here("output", "revisions")`.
#'
#' @return This function returns a data frame containing the identified revisions. It generates an Excel file containing the identified revisions in the specified output directory.  A success message is printed to the console indicating the file name and location.
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
                                type = "vintage",
                                abs_threshold = 10,
                                rel_threshold = 0,
                                output_sel = here::here("output", "revisions")) {

  # Deprecation warning
  lifecycle::deprecate_soft(
    when = "0.2.0",
    what = "nfsa_revision_T0801()",
    with = "nfsa_revision(table = 'T0801')"
  )

  nfsa_revision(
    country = country,
    table = "T0801",
    type = type,
    abs_threshold = abs_threshold,
    rel_threshold = rel_threshold,
    output_sel = output_sel
  )
}
