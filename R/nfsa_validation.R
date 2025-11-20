#' @title Find and Compile Validation Timestamps
#'
#' @description This function searches for validation files in a specified directory,
#'   extracts the timestamps, and compiles the earliest timestamp for each table,
#'   frequency, adjustment, and reference area combination.
#'
#' @param time_min A date object or character string coercible to a date,
#'   specifying the minimum date for validation files to be considered.
#'   Files with timestamps earlier than this date will be ignored.
#'
#' @param time_ax A date object or character string coercible to a date,
#'   specifying the maximum date for validation files to be considered.
#'   Files with timestamps later than this date will be ignored. Defaults to current date
#' @param output_sel A character string specifying the directory where the output
#'   Excel file will be saved. Defaults to `here::here("output", "logs")`.
#'
#' @return  This function creates and saves an Excel file. If validation files are found, a new excel document is generated in the specified directory.
#'   The file contains the earliest time of validation per combination of table, frequency, adjustment and reference area.
#'   If no validation files are found within the specified time frame, a warning message is displayed.
#'
#' @examples
#' \dontrun{
#' nfsa_validation(time_min = "2023-01-01", output_sel = "path/to/output")
#' }
#'
#' @export
nfsa_validation <- function(time_min,
                            time_max = lubridate::ymd(lubridate::today()),
                            output_sel = here::here("output", "logs")){

library(tidyverse)
library(data.table)

  options(warn = -1)
  cli::cli_progress_message("Searching for files in the server...")
validated <- list.files("I:/econ/NFSA/VALID/results/",
                  full.names = TRUE,
                  pattern = "^valid_data2vintage_",
                  recursive = FALSE) |>
  as_tibble() |>
  mutate(time = str_sub(value,-17,-12),
         time = lubridate::ymd(time)) |>
  filter(time >= time_min,
         time <= time_max)

if (nrow(validated) == 0) {
  cli::cli_alert_warning("Nothing validated :(")

} else {
  cli::cli_progress_message("Reading files...")
  validated <- validated |>
  mutate(data=map(value,~ data.table::fread(.x,header = FALSE))) |>
  unnest(data) |>
  separate_wider_delim(V1,delim = ".", names = c("drop1",
                                                 "drop2",
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
    select(time,table,freq,adjustment,ref_area) |>
    distinct() |>
  group_by(table,freq,adjustment,ref_area) |>
  arrange(time,.by_group = TRUE) |>
  slice_head(n = 1)

  cli::cli_progress_message("Creating file...")



    openxlsx::write.xlsx(validated,
                         file = paste0(output_sel,"/validation_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"),
                         overwrite = TRUE,
                         asTable = TRUE)

    cli::cli_alert_success(paste0("File created in ",output_sel,"/validation", as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"))


}
options(warn = 0)
return(validated)
}
