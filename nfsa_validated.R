#' @title Find and Compile Validation Timestamps
#'
#' @description This function searches for validation files in a specified directory,
#'   extracts the timestamps, and compiles the earliest timestamp for each table,
#'   frequency, adjustment, and reference area combination.
#'
#' @param time_min A date object or character string coercible to a date,
#'   specifying the minimum date for validation files to be considered.
#'   Files with timestamps earlier than this date will be ignored. Defaults to current date
#'
#' @param time_max A date object or character string coercible to a date,
#'   specifying the maximum date for validation files to be considered.
#'   Files with timestamps later than this date will be ignored. Defaults to current date
#'
#' @return  a data frame
#'
#' @examples
#' \dontrun{
#' nfsa_validation(time_min = "2023-01-01")
#' }
#'
#' @importFrom cli cli_alert_warning cli_progress_message
#' @importFrom data.table fread
#' @importFrom dplyr distinct filter mutate select
#' @importFrom lubridate today ymd
#' @importFrom purrr map
#' @importFrom stringr str_sub
#' @importFrom tibble as_tibble
#' @importFrom tidyr separate_wider_delim unnest
#' @export
nfsa_validated <- function(time_min = lubridate::ymd(lubridate::today()),
                            time_max = lubridate::ymd(lubridate::today())){

  cli::cli_progress_message("Searching for files in the server...")
  validated <- list.files("I:/econ/NFSA/VALID/results/",
                          full.names = TRUE,
                          pattern = "^valid_data2vintage_",
                          recursive = FALSE) |>
    tibble::as_tibble() |>
    dplyr::mutate(size = file.size(value)) |>
    dplyr::filter(size != 0) |>
    dplyr::select(-size) |>
    dplyr::mutate(time = str_sub(value,-17,-12),
           time = lubridate::ymd(time)) |>
    dplyr::filter(time >= time_min,
           time <= time_max)

  if (nrow(validated) == 0) {
    cli::cli_alert_warning("Nothing validated :(")

  } else {
    cli::cli_progress_message("Reading files...")
    validated <- validated |>
      dplyr::mutate(data=map(value,~ data.table::fread(.x,header = FALSE))) |>
      tidyr::unnest(data) |>
      tidyr::separate_wider_delim(V1,delim = ".", names = c("drop1",
                                                     "type",
                                                     "table",
                                                     "freq",
                                                     "adjustment",
                                                     "ref_area",
                                                     "drop3",
                                                     "drop4",
                                                     "drop5",
                                                     "drop6",
                                                     "drop7",
                                                     "drop8",
                                                     "drop9",
                                                     "drop10",
                                                     "drop11",
                                                     "drop12",
                                                     "drop13",
                                                     "drop14",
                                                     "drop15",
                                                     "drop16",
                                                     "drop17"
      ),
      too_many = "drop") |>
      dplyr::select(time,table,type,freq,adjustment,ref_area) |>
      dplyr::distinct()
    # group_by(table,freq,adjustment,ref_area) |>
    # arrange(time,.by_group = TRUE) |>
    # slice_head(n = 1)



  }

  return(validated)
}
