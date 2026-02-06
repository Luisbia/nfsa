#' @title Get NFSA Data
#'
#' @description This function retrieves data from the NFSA database based on specified criteria.
#'
#' @param input_sel Character string specifying the path to the data directory. Default is \code{"M:/nas/Rprod/data")}.
#' @param country Character vector specifying the country or countries to retrieve data for.
#' @param table Character string specifying the table to retrieve data from (e.g., "T0801", "T0801SA", "T0800").
#' @param filters one or several filters to be applied before collecting the data using all columns in the original parquet files.
#' @param complete By default `FALSE` if we only want to bring `ref_area`,`id`,`time_period`,`obs_value`. If set to `TRUE` brings all columns.


#' @return A tibble containing the selected NFSA data with columns: \code{ref_area}, \code{id}, \code{time_period}, and \code{obs_value}.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' # Assuming you have data stored in a directory accessible via here("data")
#' # and you want to retrieve the newest version of table T0801 for "IT":
#' # data <- nfsa_get_data_all(country = "IT", table = "T0801")
#' Single filter
#' data <- nfsa_get_data_all(country = "IT", filters = "ref_sector == 'S13'")
#' Multiple filters
#' data <- nfsa_get_data_all(country = "IT", filters = c("ref_sector == 'S13'","sto == 'B9'"))
#' }
#'
#' @export
nfsa_get_data_all <- function(input_sel = "M:/nas/Rprod/data/",
                              country,
                              table = "T0801",
                              filters = NULL,
                              complete = FALSE) {

  # 1. Setup Configuration -------------------------------------------------
  cfg_map <- list(
    "T0800"   = list(freq_code = "_A_", pattern = "NASEC_T0800_A"),
    "T0801"   = list(freq_code = "_Q_", pattern = "NASEC_T0801_Q"),
    "T0801SA" = list(freq_code = "_Q_", pattern = "NASEC_T0801SA_Q")
  )

  if (!table %in% names(cfg_map)) {
    cli::cli_abort("Invalid table: {table}. Choose T0800, T0801, or T0801SA.")
  }
  cfg <- cfg_map[[table]]

  # 2. File Discovery (All Versions) ---------------------------------------
  target_files <- list.files(path = input_sel,
                             pattern = cfg$pattern,
                             recursive = TRUE,
                             full.names = TRUE) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      version = as.numeric(stringr::str_extract(value, paste0("(?<=", cfg$freq_code, "..............).{4}"))),
      countries = stringr::str_extract(value, paste0("(?<=", cfg$freq_code, ").."))
    ) |>
    dplyr::filter(countries %in% !!country) |>
    # We omit slice_max() here to keep all versions
    dplyr::pull(value)

  if (length(target_files) == 0) {
    cli::cli_alert_danger("No files found matching {cfg$pattern} for {country}")
    return(NULL)
  }

  # 3. Data Processing Pipeline --------------------------------------------
  lookup <- nfsa::nfsa_sto_lookup

  ds <- arrow::open_dataset(target_files)
  # Apply Arrow filters (Push-down)
  if (!is.null(filters)) {
    parsed_filters <- rlang::parse_exprs(filters)
    ds <- ds |> dplyr::filter(!!!parsed_filters)
  }

  res <- ds |>
    dplyr::collect()

  if (complete == FALSE){
    res <-res |>
    dplyr::left_join(lookup, by = c("counterpart_area", "ref_sector", "counterpart_sector",
                                    "consolidation", "accounting_entry", "sto",
                                    "instr_asset", "unit_measure", "prices")) |>
    tidyr::drop_na() |>
    # Note: version is included here as requested
    dplyr::select(ref_area, version, id, time_period, obs_value) |>
    dplyr::mutate(ref_area = dplyr::if_else(ref_area == "GR", "EL", ref_area))
  }

  if (complete == TRUE){
    res <-res |>
      dplyr::left_join(lookup, by = c("counterpart_area", "ref_sector", "counterpart_sector",
                                      "consolidation", "accounting_entry", "sto",
                                      "instr_asset", "unit_measure", "prices")) |>
      dplyr::arrange(time_period,ref_area,id) |>
      dplyr::mutate(ref_area = dplyr::if_else(ref_area == "GR", "EL", ref_area))
  }

  return(res)
}
