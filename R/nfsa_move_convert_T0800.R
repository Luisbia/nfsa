#' Convert SDMX data to Parquet format for NASEC_T08000_A data.
#'
#' This function reads SDMX files, converts them to Parquet format, and saves them to a specified directory.
#'
#' @param country A character string specifying the country code.
#' @param year A character string specifying the year e.g.,(2024_0000 for 2024)
#' @param input_sel A character string specifying the input directory where the SDMX files are located.
#'   Defaults to "M:/nas/Incoming_SDMX_files/".
#' @param output_sel A character string specifying the output directory where the Parquet files should be saved.
#'   Defaults to "M:/nas/Rprod/data/a/new/".
#'
#' @returns This function primarily operates for side effects (writing Parquet files).  It prints messages
#'   to the console indicating whether a new file was processed, if no file was found, or if no new file exists.
#' @details The function searches for SDMX files matching the pattern
#'   "NASEC_T08000_A_{country}_{year}" in the input directory. It identifies the file
#'   with the latest version number. If a new file is found (compared to existing files in the
#'   output directory), it reads the SDMX data, cleans the column names, adds a 'received'
#'   timestamp, selects relevant columns, converts the 'obs_value' column to numeric, adds an
#'   'embargo_date' column if missing and then writes the data to a Parquet file in the output
#'   directory.
#' @export
#'
#' @examples
#' nfsa_move_convert_T0800(country= "BE",
#' year = "2024_0000",
#' input_sel = "M:/nas/Incoming_SDMX_files/",
#' output_sel = "M:/nas/Rprod/data/a/new/")
#'
nfsa_move_convert_T0800 <- function(country,
                           year,
                           input_sel = "M:/nas/Incoming_SDMX_files/",
                           output_sel = "M:/nas/Rprod/data/a/new/"){

  # Deprecation warning
  lifecycle::deprecate_soft(
    when = "0.2.0",
    what = "nfsa_move_convert_T0800()",
    with = "nfsa_move_convert(table = 'T0800')"
  )

  nfsa_move_convert(
    country = country,
    table = "T0800",
    period = year,
    input_sel = input_sel,
    output_sel = output_sel,
    process_all = FALSE
  )
}



