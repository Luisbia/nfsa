#' @title Generate NFSA Transmission Report
#'
#' @description This function generates a report of NFSA data transmissions received, filtering by a minimum timestamp and outputting the results to an Excel file.
#'
#' @param time_min A character string representing the minimum date and time for filtering received files (e.g., "2025-10-20").  Files received before this time will be excluded.
#' @param time_max A character string representing the maximum date and time for filtering received files, defaults to current date).  Files received after this time will be excluded.
#' @param output_sel A character string specifying the directory where the output Excel file should be saved. Defaults to `"output/logs"`.
#' @param recursive A logical value. If `TRUE`, the function will recursively search for files in subdirectories within the specified path. Defaults to `FALSE`.
#'
#' @return This function does not return a value, instead outputs an Excel file with the transmission report.
#'
#'
#' @examples
#' \dontrun{
#' # Generate a transmission report for files received since "2025-11-01" and save to default location
#' nfsa_transmissions(time_min = "2025-11-01")
#'
#' # Generate a report, save to alternative location and search in subdirectories
#' nfsa_transmissions(time_min = "2025-10-25", output_sel = "my_output_folder", recursive = TRUE)
#' }
#' @export
nfsa_transmissions <- function(time_min,
                               time_max = lubridate::ymd(lubridate::today()),
                               input_sel = "M:/nas/incoming_SDMX_files/",
                               output_sel = here::here("output", "logs"),
                               recursive = FALSE){
  library(tidyverse)
  library(cli)
  library(here)

  cli::cli_inform("Looking for files in the server...")
  received <- list.files(path = input_sel,
                         pattern = "NASEC_",
                         full.names = TRUE,
                         recursive = recursive) |>
    as_tibble()

  cli::cli_inform("Processing info...")

  received <- received |>
    mutate(received = lubridate::ymd_hms(file.mtime(value))) |>
    filter(received >= time_min,
           received <= time_max) |>
    mutate(value = str_remove_all(value,input_sel)) |>
    separate_wider_delim(value,
                         delim = "_",
                         names = c("NASEC", "table", "freq", "ref_area","year", "quarter","version"),
                         cols_remove = FALSE) |>
    mutate(version = stringr::str_remove_all(version,".xml"),
           period = paste0(year,"-Q", str_sub(quarter,4,4)),
           period = str_replace_all(period,"Q0", "A")) |>
    select(ref_area,file=value,table, period,version,received)



  openxlsx::write.xlsx(received,
                       file = paste0(output_sel,"/received_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"),
                       overwrite = TRUE,
                       asTable = TRUE)

  cli::cli_alert_success(paste0("File created in ",output_sel,"/received", as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"))

  return(received)
}

