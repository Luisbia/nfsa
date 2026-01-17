#' Detect and Report Revisions in NFSA Data Between Versions
#'
#' This function compares new and previous versions of NFSA data (T0800, T0801, or T0801SA),
#' identifies revisions based on absolute and relative thresholds, and exports the revisions
#' to an Excel file. It utilizes `arrow` for efficient data handling and `openxlsx` for Excel output.
#'
#' @param country A character vector specifying the countries to analyze (ISO2 codes).
#' @param table A character string specifying which table to analyze. Must be one of: "T0800", "T0801", or "T0801SA".
#' @param type Either "vintage" to compare with the last version of the previous period, or "version"
#'   to compare with the previous version of this period. Defaults to "vintage".
#' @param abs_threshold A numeric value specifying the absolute threshold for revision detection.
#'   Revisions with an absolute difference greater than this threshold are flagged.
#'   If NULL (default), automatically sets to 100 for T0800 and 10 for T0801/T0801SA.
#' @param rel_threshold A numeric value specifying the relative threshold (percentage of GDP) for
#'   revision detection. Revisions with a relative difference greater than this threshold are flagged.
#'   Defaults to 0.
#' @param output_sel Path to the directory where the output Excel file containing the revisions will be saved.
#'   Defaults to `here("output", "revisions")`.
#'
#' @return A data frame containing the identified revisions with columns for reference area, sector,
#'   STO, accounting entry, time period, new value, previous value, revision, revision percentage,
#'   and revision as percentage of GDP. If no revisions are found, returns an empty data frame.
#'
#' @examples
#' \dontrun{
#' # Example usage with default thresholds for T0800:
#' nfsa_revision(country = "BE", table = "T0800")
#'
#' # Example usage with T0801 and custom thresholds:
#' nfsa_revision(country = c("ES", "IT"), table = "T0801",
#'               abs_threshold = 20, rel_threshold = 0.5)
#'
#' # Compare with previous version instead of vintage:
#' nfsa_revision(country = "DE", table = "T0801SA", type = "version")
#' }
#'
#' @export
nfsa_revision <- function(country,
                          table = c("T0800", "T0801", "T0801SA"),
                          type = c("vintage", "version"),
                          abs_threshold = NULL,
                          rel_threshold = 0,
                          output_sel = here::here("output", "revisions")) {

  # Validate arguments
  table <- match.arg(table)
  type <- match.arg(type)

  # Set default abs_threshold based on table if not provided
  if (is.null(abs_threshold)) {
    abs_threshold <- if (table == "T0800") 100 else 10
  }

  # Configuration for different tables
  table_config <- list(
    "T0800" = list(
      freq = "annual",
      path = "M:/nas/Rprod/data/a/new"
    ),
    "T0801" = list(
      freq = "quarterly",
      path = "M:/nas/Rprod/data/q/new/nsa"
    ),
    "T0801SA" = list(
      freq = "quarterly",
      path = "M:/nas/Rprod/data/q/new/sca"
    )
  )

  config <- table_config[[table]]
  lookup <- nfsa::nfsa_sto_lookup

  # Get new data
  new_db <- nfsa::nfsa_get_data(country = country, table = table, type = "new") |>
    dplyr::select(ref_area, id, time_period, new = obs_value) |>
    tidyr::separate_wider_delim(cols = id,
                                 delim = ".",
                                 names = c("ref_sector", "sto", "accounting_entry"))

  # Get previous data based on type
  if (type == "vintage") {
    prev_db <- nfsa::nfsa_get_data(country = country, table = table, type = "prev") |>
      dplyr::select(ref_area, id, time_period, prev = obs_value) |>
      tidyr::separate_wider_delim(cols = id,
                                   delim = ".",
                                   names = c("ref_sector", "sto", "accounting_entry"))
  } else if (type == "version") {
    # Determine frequency pattern
    freq_pattern <- if (config$freq == "annual") "_A_" else "_Q_"

    prev_db <- list.files(path = config$path,
                         recursive = FALSE,
                         full.names = TRUE) |>
      tibble::as_tibble() |>
      dplyr::mutate(
        version = as.numeric(stringr::str_extract(value, paste0("(?<=", freq_pattern, "..............).{4}"))),
        countries = stringr::str_extract(value, paste0("(?<=", freq_pattern, ").."))
      ) |>
      dplyr::filter(countries %in% country) |>
      dplyr::group_by(countries) |>
      dplyr::arrange(version) |>
      dplyr::slice_tail(n = 2) |>
      dplyr::slice_head(n = 1) |>
      dplyr::pull(value) |>
      arrow::open_dataset() |>
      dplyr::select(-embargo_date, -received) |>
      dplyr::collect() |>
      dplyr::left_join(nfsa::nfsa_sto_lookup,
                      by = dplyr::join_by(counterpart_area, ref_sector, counterpart_sector,
                                         consolidation, accounting_entry, sto,
                                         instr_asset, unit_measure, prices)) |>
      tidyr::drop_na() |>
      dplyr::select(ref_area, id, time_period, prev = obs_value) |>
      tidyr::separate_wider_delim(cols = id,
                                   delim = ".",
                                   names = c("ref_sector", "sto", "accounting_entry"))
  }

  # Calculate revisions
  new_prev_db <- dplyr::full_join(new_db, prev_db,
                                  by = dplyr::join_by(ref_area, ref_sector, sto,
                                                     accounting_entry, time_period)) |>
    dplyr::mutate(rev = new - prev) |>
    dplyr::mutate(revp = round(rev * 100 / prev, 1))

  # Calculate GDP for relative comparisons
  tmp <- prev_db |>
    dplyr::filter(sto == "B1GQ") |>
    dplyr::select(ref_area, time_period, GDP = prev)

  new_prev_db <- dplyr::left_join(new_prev_db, tmp,
                                  by = dplyr::join_by(ref_area, time_period)) |>
    dplyr::mutate(as_GDP = round(rev * 100 / GDP, 1)) |>
    dplyr::select(-GDP) |>
    dplyr::filter(abs(rev) > abs_threshold) |>
    dplyr::filter(abs(as_GDP) > rel_threshold)

  # Handle output
  if (nrow(new_prev_db) == 0) {
    cli::cli_alert_success("No revisions found in {table}!")
    return(new_prev_db)
  }

  # Generate filename
  if (length(unique(new_prev_db$ref_area)) == 1) {
    filename <- paste0("revisions_", table, "_", unique(new_prev_db$ref_area), "_",
                      as.character(format(Sys.time(), "%Y%m%d_%H%M%S")), ".xlsx")
  } else {
    filename <- paste0("revisions_", table, "_",
                      as.character(format(Sys.time(), "%Y%m%d_%H%M%S")), ".xlsx")
  }

  # Write Excel file
  openxlsx::write.xlsx(new_prev_db,
                       file = paste0(output_sel, "/", filename),
                       overwrite = TRUE,
                       asTable = TRUE)

  cli::cli_alert_success("File created in {output_sel}/{filename}")

  nfsa::nfsa_to_excel(new_prev_db)
  return(new_prev_db)
}
