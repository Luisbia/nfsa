#' @title Process and export Eurostat Comext data
#'
#' @description This function retrieves, processes, and exports Eurostat Comext
#'   data for a specified area (EA20 or EU27) and quarter.
#'
#' @param area A string indicating the geographical area ("EA20" or "EU27").
#' @param quarter A string representing the target quarter in the format "YYYYQQ" (e.g., "2023Q4").
#' @param output_sel A string specifying the directory where the output CSV file should be saved.
#' @param input_sel A string specifying the directory where the additional historical files are saved.
#'   Defaults to the standard Comext production directory. The directory must contain the appropriate
#'   historical data file: "EA20_lines to add.csv" for EA20 or "EU27_lines to add.csv" for EU27.
#'
#' @return A tibble containing the processed Comext data. The data is also written to a CSV file
#'   in the specified output directory with the naming format: "{area}_{quarter}_comext.csv".
#'
#' @examples
#' \dontrun{
#' # Example usage for EA20 in 2023 Q4
#' nfsa_comext(area = "EA20", quarter = "2023Q4", output_sel = "path/to/output/")
#'
#' # Example usage for EU27 in 2024 Q1
#' nfsa_comext(area = "EU27", quarter = "2024Q1", output_sel = "path/to/output/")
#' }
#'
#' @export
nfsa_comext <- function(area,
                             quarter,
                             output_sel,
                             input_sel = "M:/nas/QSA10/Production/2025Q3/(1) QSA/(1_1) Original transmission/(1_1_2) Other input/Comext/") {

  # Input validation ----
  validate_inputs(area, quarter, output_sel, input_sel)

  # Load required packages ----
  pacman::p_load(tidyverse, restatapi, cli)

  # Ensure output directory exists ----
  if (!dir.exists(output_sel)) {
    dir.create(output_sel, recursive = TRUE)
    cli::cli_alert_info("Created output directory: {output_sel}")
  }

  # Get area configuration ----
  config <- get_area_config(area)

  # Retrieve and process data ----
  cli::cli_progress_step("Retrieving {area} data from Eurostat...")

  raw_data <- tryCatch(
    get_eurostat_data(
      config$dataset,
      filters = list(
        indic_et = "TRD_VAL",
        partner = config$partner,
        bclas_bec = c("CAP", "CONS", "INT", "TOTAL"),
        stk_flow = c("EXP", "IMP")
      ),
      stringsAsFactors = FALSE
    ),
    error = function(e) {
      cli::cli_abort("Failed to retrieve Eurostat data: {e$message}")
    }
  )

  # Process data ----
  cli::cli_progress_step("Processing data...")

  processed_data <- process_comext_data(
    raw_data = raw_data,
    quarter = quarter,
    area = area,
    config = config
  )

  # Load and bind historical data ----
  cli::cli_progress_step("Adding historical data (1999Q1-2001Q4)...")

  historical_file <- file.path(input_sel, paste0(area, "_lines to add.csv"))

  if (!file.exists(historical_file)) {
    cli::cli_abort("Historical data file not found: {historical_file}")
  }

  historical_data <- tryCatch(
    readr::read_csv(historical_file, show_col_types = FALSE),
    error = function(e) {
      cli::cli_abort("Failed to read historical data file: {e$message}")
    }
  )

  final_data <- bind_rows(historical_data, processed_data)

  # Write output file ----
  output_file <- file.path(output_sel, sprintf("%s_%s_comext.csv", area, quarter))

  tryCatch(
    {
      readr::write_csv(final_data, output_file)
      cli::cli_alert_success("File created: {output_file}")
    },
    error = function(e) {
      cli::cli_abort("Failed to write output file: {e$message}")
    }
  )

  invisible(final_data)
}


# Helper functions ----

#' Validate function inputs
#' @keywords internal
validate_inputs <- function(area, quarter, output_sel, input_sel) {
  # Validate area
  valid_areas <- c("EA20", "EU27")
  if (!area %in% valid_areas) {
    cli::cli_abort("Invalid area: {area}. Must be one of: {paste(valid_areas, collapse = ', ')}")
  }

  # Validate quarter format
  if (!grepl("^\\d{4}Q[1-4]$", quarter)) {
    cli::cli_abort("Invalid quarter format: {quarter}. Expected format: YYYYQ# (e.g., 2023Q4)")
  }

  # Validate directories
  stopifnot(
    "output_sel must be a character string" = is.character(output_sel),
    "input_sel must be a character string" = is.character(input_sel)
  )

  # Check input directory exists
  if (!dir.exists(input_sel)) {
    cli::cli_abort("Input directory does not exist: {input_sel}")
  }
}


#' Get configuration for specified area
#' @keywords internal
get_area_config <- function(area) {
  configs <- list(
    EA20 = list(
      dataset = "ext_st_eabec",
      partner = "EA20",
      partner_output = "EA20_INTRA",
      reporter = "EA20"
    ),
    EU27 = list(
      dataset = "ext_st_eu27_2020bec",
      partner = "EU27_2020",
      partner_output = "EU27_2020_INTRA",
      reporter = "EU27"
    )
  )

  configs[[area]]
}


#' Convert monthly data to quarterly periods
#' @keywords internal
add_quarter_column <- function(data) {
  data |>
    mutate(
      year = str_sub(time, 1, 4),
      quarter = case_when(
        str_sub(time, 6, 7) %in% c("01", "02", "03") ~ "Q1",
        str_sub(time, 6, 7) %in% c("04", "05", "06") ~ "Q2",
        str_sub(time, 6, 7) %in% c("07", "08", "09") ~ "Q3",
        str_sub(time, 6, 7) %in% c("10", "11", "12") ~ "Q4",
        .default = NA_character_
      )
    )
}


#' Check if all months are available for complete quarters
#' @keywords internal
check_quarter_completeness <- function(data, target_quarter) {
  incomplete <- data |>
    group_by(year, quarter, bclas_bec, stk_flow) |>
    tally() |>
    filter(n < 3) |>
    unite("quarter_id", c(year, quarter), sep = "") |>
    filter(quarter_id == target_quarter)

  if (nrow(incomplete) > 0) {
    cli::cli_abort(
      c(
        "Cannot calculate complete quarter: {target_quarter}",
        "i" = "At least one month is missing from the data",
        "i" = "Each quarter requires data from all 3 months"
      )
    )
  }
}


#' Process and aggregate Comext data
#' @keywords internal
process_comext_data <- function(raw_data, quarter, area, config) {
  # Add quarter information
  data_with_quarters <- add_quarter_column(raw_data)

  # Check data completeness
  check_quarter_completeness(data_with_quarters, quarter)

  # Aggregate to quarterly level
  data_with_quarters |>
    unite("time", c(year, quarter), sep = "") |>
    filter(time <= quarter) |>
    summarise(
      values = sum(values, na.rm = TRUE) * 1e6,
      .by = c(geo, partner, bclas_bec, stk_flow, time, indic_et)
    ) |>
    select(
      REPORTER = geo,
      PARTNER = partner,
      PRODUCT = bclas_bec,
      FLOW = stk_flow,
      PERIOD = time,
      INDICATORS = indic_et,
      INDICATOR_VALUE = values
    ) |>
    mutate(
      REPORTER = config$reporter,
      PARTNER = config$partner_output,
      PRODUCT = if_else(PRODUCT != "TOTAL", paste0("/", PRODUCT), PRODUCT),
      FLOW = if_else(FLOW == "IMP", 1, 2),
      INDICATORS = "VALUE_IN_EUROS"
    )
}

