#' @title Check NFSA Loading Logs
#'
#' @description This function scans NFSA loading logs, extracts relevant information,
#' and saves it to an Excel file.
#'
#' @param time_min A date specifying the minimum time for log files to be considered.
#'   Defaults to the beginning of the current day.
#' @param time_max A character string representing the maximum date and time for filtering received files, defaults to current date).  Files received after this time will be excluded.
#' @param output_sel Path to the directory where the output Excel file will be saved.
#'   Defaults to "output/logs" within the project directory (using `here::here()`).
#' @param recursive Logical. Should the function search for log files recursively within subdirectories?
#'   Defaults to `FALSE`.
#'
#' @return Invisibly returns a tibble containing the processed log data if any files were loaded, otherwise
#'   invisibly returns `NULL`. An Excel file containing the processed data is saved in the
#'   `output_sel` directory.
#'
#' @examples
#' \dontrun{
#' # Check logs loaded since yesterday
#' nfsa_loaded(time_min = Sys.Date() - 1)
#'
#' # Check logs loaded since a specific date
#' nfsa_loaded(time_min = "2025-10-01")
#'
#' # Check logs recursively within subdirectories
#' nfsa_loaded(time_min = "2020-01-01",recursive = TRUE)
#' }
#' @export
nfsa_loaded <- function(time_min,
                        time_max = lubridate::ymd(lubridate::today()),
                        output_sel = here("output", "logs"),
                        recursive = FALSE){

  library(tidyverse)
  library(cli)
  library(here)

  read_loaded_log <- function(file){
    tmp <- read_lines(file) |>
      enframe() |>
      select(-name) |>
      filter(str_detect(value, "nameproc =")|
               str_detect(value, "InputFile =")|
               str_detect(value, "CheckFile =")|
               str_detect(value, "TypeOfFile =")|
               str_detect(value, "TypeOfSer =")|
               str_detect(value, "tableL =")|
               str_detect(value, "freqL =")|
               str_detect(value, "adjustL =")|
               str_detect(value, "countryL =")|
               str_detect(value, "CP_Area =")|
               str_detect(value, "Ref_Sector =")|
               str_detect(value, "Sto =")|
               str_detect(value, "instr_assetsL =")|
               str_detect(value, "unitL =")|
               str_detect(value, "nb_series =")|
               str_detect(value, "feedRC="))
    return(tmp)
  }

  cli::cli_inform("Collecting files from server...")

  loaded <- list.files("I:/econ/NFSA/LOG/",
                       pattern = ".*txt$",
                       full.names = TRUE,
                       recursive = recursive) |>
    as_tibble()


  if (nrow(loaded) == 0) {
    cli::cli_alert_success("No files loaded on the period selected!")

  } else {
  cli::cli_inform("Processing files...")

    loaded<- loaded |>
      mutate(loaded = lubridate::ymd_hm(str_sub(value,-14,-5))) |>
      filter(loaded >= time_min,
             loaded <= time_max) |>
      rename(file = value) |>
      mutate(data = map(file,read_loaded_log)) |>
      unnest(cols=c(data)) |>
      mutate(value = str_remove_all(value,"0=OK 2=Warning 4=Rejection 5=Error"),
             value = str_replace_all(value, "feedRC=", "feedRC = "),
             value = str_replace_all(value, "\\(\\)","")) |>
      #filter(!str_detect(value, "feedRC = 4 ")) |>
      separate_wider_delim(cols = value,
                           delim = " = ",
                           names = c("field", "result")) |>
      distinct() |>
      pivot_wider(names_from = field,
                  values_from = result) |>
      janitor::clean_names() |>
      select(country =country_l,
             loaded,
             input_file,
             table =table_l,
             type_of_series=type_of_ser,
             check_file,
             type_of_file,
             freq = freq_l,
             adjustment =adjust_l,
             sto,
             cp_area,
             ref_sector,
             instr_asset = instr_assets_l,
             unit = unit_l,
             nb_series,
             result = feed_rc) |>
      mutate(result = case_when(result == "0 " ~ "OK",
                                result == "2 " ~ "Warning",
                                result == "4 " ~ "Rejection",
                                result == "5 " ~ "Error"))




    openxlsx::write.xlsx(loaded,
                         file = here(paste0(output_sel,"/loaded_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx")),
                         overwrite = TRUE,
                         asTable = TRUE)

    cli::cli_alert_success(paste0("File created in output/logs/loaded_","_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"))
  }

  return(loaded)
}
