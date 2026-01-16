#' @title Analyze and Report Confidential Status
#'
#' @description This function analyzes confidential status data from NFSA datasets,
#'   aggregates it by country, statistical object (STO), and time period, and
#'   writes the results to an Excel file. It supports both annual (T0800) and
#'   quarterly (T0801) datasets, as well as new and previous versions.
#'
#' @param table Character string indicating the table to analyze.  Must be
#'   "T0800" or "T0801".  Defaults to "T0801".
#' @param type Character string indicating whether to process "new" or "prev"
#'   (previous) data. Defaults to "new".
#' @param input_sel Character string specifying the base path to the data
#'   directory. Defaults to `"M:/nas/Rprod/data"`.
#' @param output_sel Character string specifying the path to the output
#'   directory where the Excel file will be saved. Defaults to
#'   `here::here()`.
#'
#' @return None. The function's primary effect is to write an Excel file
#'   containing the analysis results to the specified `output_sel`.  It also
#'   prints a success message to the console using `cli::cli_alert_success()`.
#'
#' @details The function reads data from arrow files located in subdirectories
#'   of the `input_sel` directory, based on the `table` and `type`
#'   parameters. It then joins the data with a lookup table (`nfsa::nfsa_sto_lookup`),
#'   performs aggregations, and writes the results to an Excel file. The file
#'   name includes a timestamp and indicates the dataset being analyzed.
#'
#' @examples
#' \dontrun{
#' # Analyze the new T0801 data and save the output to the default location
#' nfsa_conf_status(table = "T0801", type = "new")
#'
#' # Analyze the previous T0800 data and save the output to a specific folder
#' nfsa_conf_status(table = "T0800", type = "prev", output_sel = here::here("my_output"))
#' }
#' @export
nfsa_conf_status <- function(table = "T0801",
                             type = "new",
                             input_sel = "M:/nas/Rprod/data/",
                             output_sel = here::here("output", "flags")) {

  # 1. Config Map for Tables -----------------------------------------------
  path_map <- list(
    "T0800"   = list(sub = "/a/", freq_code = "_A_", date_min = 1995, time_col = "year"),
    "T0801"   = list(sub = "/q/", freq_code = "_Q_", date_min = "1999-Q1", time_col = "quarter"),
    "T0801SA" = list(sub = "/q/", freq_code = "_Q_", date_min = "1999-Q1", time_col = "quarter")
  )

  if (!table %in% names(path_map)) stop("Invalid table selection. Choose T0800, T0801, or T0801SA.")
  cfg <- path_map[[table]]

  # Determine seasonal adjustment sub-folder
  adj_path <- if (table == "T0801SA") "sca/" else if (table == "T0801") "nsa/" else ""
  full_input_path <- file.path(input_sel, cfg$sub, type, adj_path)

  # 2. File Selection (Latest Version) -------------------------------------
  target_files <- list.files(path = full_input_path, full.names = TRUE) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      version = as.numeric(stringr::str_extract(value, paste0("(?<=", cfg$freq_code, "..............).{4}"))),
      country = stringr::str_extract(value, paste0("(?<=", cfg$freq_code, ").."))
    ) |>
    dplyr::group_by(country) |>
    dplyr::slice_max(version, n = 1, with_ties = FALSE) |>
    dplyr::pull(value)

  if (length(target_files) == 0) {
    cli::cli_alert_danger("No files found in: {full_input_path}")
    return(NULL)
  }

  # 3. Data Processing -----------------------------------------------------
  lookup <- nfsa::nfsa_sto_lookup

  conf_status_raw <- arrow::open_dataset(target_files) |>
    dplyr::select(-embargo_date, -received) |>
    dplyr::collect() |>
    dplyr::left_join(lookup, by = c("counterpart_area", "ref_sector", "counterpart_sector",
                                    "consolidation", "accounting_entry", "sto",
                                    "instr_asset", "unit_measure", "prices")) |>
    dplyr::select(ref_area, id, time_period, conf_status) |>
    dplyr::filter(time_period >= cfg$date_min)

  # 4. Aggregation ---------------------------------------------------------
  get_tally <- function(data, group_var) {
    data |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_var)), conf_status) |>
      dplyr::tally() |>
      tidyr::pivot_wider(names_from = conf_status, values_from = n)
  }

  results_list <- list(
    country = get_tally(conf_status_raw, "ref_area"),
    sto     = get_tally(conf_status_raw, "id")
  )
  results_list[[cfg$time_col]] <- get_tally(conf_status_raw, "time_period")

  # 5. Export --------------------------------------------------------------
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  file_name <- paste0(timestamp, "_conf_status_", table, "_", type, ".xlsx")
  out_path <- file.path(output_sel, file_name)

  if (!dir.exists(output_sel)) dir.create(output_sel, recursive = TRUE)

  openxlsx::write.xlsx(results_list, file = out_path, overwrite = TRUE)
  cli::cli_alert_success("File created in {out_path}")

  return(invisible(results_list))
}
