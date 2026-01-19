#' @title Identify and Report Revisions in T0801SA (Seasonally Adjusted) Data
#'
#' @description This function identifies and reports revisions between two versions of T0801SA data.
#' It compares the `obs_value` in the new and previous datasets, calculates the absolute and
#' relative revisions, and filters the results based on specified thresholds.
#'
#' This is a wrapper around `nfsa_revision()` for backward compatibility.
#' Consider using `nfsa_revision(table = "T0801SA")` directly.
#'
#' @param country A character vector specifying the country or countries to analyze.
#'        This should correspond to the country codes used in the input file names.
#' @param type Either to compare with the last version of the previous quarter ("vintage") or with the previous version of this quarter ("version")
#' @param abs_threshold A numeric value specifying the minimum absolute revision value to consider.
#'        Revisions with an absolute value less than this threshold will be filtered out. Defaults to 10.
#' @param rel_threshold A numeric value specifying the minimum relative revision value (as a percentage of GDP) to consider.
#'        Revisions with an absolute relative value less than this threshold will be filtered out. Defaults to 0.
#' @param output_sel A character string specifying the path to the directory where the output Excel file will be saved.
#'        Defaults to `here("output", "revisions")`.
#'
#' @return This function returns a data frame containing the identified revisions and saves an Excel file
#' to the specified output directory. If no revisions meet the threshold criteria, a message will be displayed in the console.
#'
#' @examples
#' \dontrun{
#' # Example usage with default thresholds and file paths:
#' nfsa_revision_T0801SA(country = "BE")
#'
#' # Example usage with custom thresholds:
#' nfsa_revision_T0801SA(country = c("IT", "ES"),
#'                        abs_threshold = 20,
#'                        rel_threshold = 0.5,
#'                        output_sel = here("path", "to", "output"))
#' }
#'
#' @export
nfsa_revision_T0801SA <- function(country,
                                   type = "vintage",
                                   abs_threshold = 10,
                                   rel_threshold = 0,
                                   output_sel = here::here("output", "revisions")) {

  # Deprecation warning
  lifecycle::deprecate_soft(
    when = "1.0.0",
    what = "nfsa_revision_T0801SA()",
    with = "nfsa_revision(table = 'T0801SA')"
  )

  nfsa_revision(
    country = country,
    table = "T0801SA",
    type = type,
    abs_threshold = abs_threshold,
    rel_threshold = rel_threshold,
    output_sel = output_sel
  )
}
