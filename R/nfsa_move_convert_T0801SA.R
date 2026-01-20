#' Convert SDMX data to Parquet format for NASEC_T0801SA_Q data.
#'
#' This function reads SDMX files, converts them to Parquet format, and saves them to a specified directory.
#'
#' @param country A character string specifying the country code.
#' @param yearquarter A character string specifying the year and quarter e.g.,(2025_0002 for 2025Q2)
#' @param input_sel A character string specifying the input directory where the SDMX files are located.
#'   Defaults to "M:/nas/Incoming_SDMX_files/".
#' @param output_sel A character string specifying the output directory where the Parquet files should be saved.
#'   Defaults to "M:/nas/Rprod/data/q/new/sca/".
#'
#' @returns This function primarily operates for side effects (writing Parquet files).  It prints messages
#'   to the console indicating whether a new file was processed, if no file was found, or if no new file exists.
#' @details The function searches for SDMX files matching the pattern
#'   "NASEC_T0801SA_Q_{country}_{yearquarter}" in the input directory. It identifies the file
#'   with the latest version number. If a new file is found (compared to existing files in the
#'   output directory), it reads the SDMX data, cleans the column names, adds a 'received'
#'   timestamp, selects relevant columns, converts the 'obs_value' column to numeric, adds an
#'   'embargo_date' column if missing and then writes the data to a Parquet file in the output
#'   directory.
#' @export
#'
#' @examples
#' nfsa_move_convert_T0801_SA(country= "BE",
#' yearquarter = "2025_0002",
#' input_sel = "M:/nas/Incoming_SDMX_files/",
#' output_sel = "M:/nas/Rprod/data/q/new/sca/")
nfsa_move_convert_T0801SA <- function(country,
                                 yearquarter,
                                 input_sel = "M:/nas/Incoming_SDMX_files/",
                                 output_sel = "M:/nas/Rprod/data/q/new/sca/"){

  # Deprecation warning
  lifecycle::deprecate_soft(
    when = "1.0.0",
    what = "nfsa_move_convert_T0801SA()",
    with = "nfsa_move_convert(table = 'T0801SA')"
  )

  nfsa_move_convert(
    country = country,
    table = "T0801SA",
    period = yearquarter,
    input_sel = input_sel,
    output_sel = output_sel,
    process_all = FALSE
  )
}

