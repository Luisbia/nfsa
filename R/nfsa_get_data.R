#' @title Get NFSA Data
#'
#' @description This function retrieves data from the NFSA database based on specified criteria.
#'
#' @param input_sel Character string specifying the path to the data directory. Default is \code{"M:/nas/Rprod/data"}.
#' @param country Character vector specifying the country or countries to retrieve data for.
#' @param table Character string specifying the table to retrieve data from (e.g., "T0801", "T0801SA", "T0800").
#' @param type Character string specifying the type of data to retrieve ("new" or "prev").
#' @param filters one or several filters to be applied before collecting the data using all columns in the original parquet files
#'
#' @return A tibble containing the selected NFSA data with columns: \code{ref_area}, \code{id}, \code{time_period}, and \code{obs_value}.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' # Assuming you have data stored in a directory accessible via here("data")
#' # and you want to retrieve the newest version of table T0800 for "IT":
#' # data <- nfsa_get_data(country = "IT", table = "T0800", type = "new")
#' Single filter
#' data <- nfsa_get_data(country = "IT", filters = "ref_sector == 'S13'")
#' Multiple filters
#' data <- nfsa_get_data(country = "IT", filters = c("ref_sector == 'S13'","sto == 'B9'"))

#' }
#'
#' @export
nfsa_get_data <- function(input_sel = "M:/nas/Rprod/data/",
                          country,
                          table = "T0801",
                          type = "new",
                          filters = NULL) {

  # 1. Config Map ----------------------------------------------------------
  # Maps table names to path segments and regex patterns
  cfg_map <- list(
    "T0800"   = list(sub = "/a/", freq_code = "_A_"),
    "T0801"   = list(sub = "/q/", freq_code = "_Q_"),
    "T0801SA" = list(sub = "/q/", freq_code = "_Q_")
  )

  if (!table %in% names(cfg_map)) {
    cli::cli_abort("Invalid table: {table}. Choose T0800, T0801, or T0801SA.")
  }
  cfg <- cfg_map[[table]]

  # Determine sub-folder for SA vs NSA
  adj_path <- if (table == "T0801SA") "sca/" else if (table == "T0801") "nsa/" else ""
  full_input_path <- file.path(input_sel, cfg$sub, type, adj_path)

  # 2. File Selection (Latest Version for Selected Countries) --------------
  target_files <- list.files(path = full_input_path, full.names = TRUE) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      version = as.numeric(stringr::str_extract(value, paste0("(?<=", cfg$freq_code, "..............).{4}"))),
      countries = stringr::str_extract(value, paste0("(?<=", cfg$freq_code, ").."))
    ) |>
    dplyr::filter(countries %in% !!country) |>
    dplyr::group_by(countries) |>
    dplyr::slice_max(version, n = 1, with_ties = FALSE) |>
    dplyr::pull(value)

  if (length(target_files) == 0) {
    cli::cli_alert_danger("No files found for country: {country} in {full_input_path}")
    return(NULL)
  }

  # 3. Data Processing -----------------------------------------------------
  lookup <- nfsa::nfsa_sto_lookup

  # Start arrow connection
  ds <- arrow::open_dataset(target_files) |>
    dplyr::select(-embargo_date, -received)

  # Apply dynamic filters BEFORE collect (Push-down filtering for speed)
  if (!is.null(filters)) {
    parsed_filters <- rlang::parse_exprs(filters)
    ds <- ds |> dplyr::filter(!!!parsed_filters)
  }

  # Collect and Join
  res <- ds |>
    dplyr::collect() |>
    dplyr::left_join(lookup, by = c("counterpart_area", "ref_sector", "counterpart_sector",
                                    "consolidation", "accounting_entry", "sto",
                                    "instr_asset", "unit_measure", "prices")) |>
    tidyr::drop_na() |>
    dplyr::select(ref_area, id, time_period, obs_value) |>
    dplyr::arrange(time_period,ref_area,id)

  return(res)
}
