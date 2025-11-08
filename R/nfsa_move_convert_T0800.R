#' Convert SDMX data to Parquet format for NASEC_T08000_A data.
#'
#' This function reads SDMX files, converts them to Parquet format, and saves them to a specified directory.
#'
#' @param country A character string specifying the country code.
#' @param year A character string specifying the year e.g.,(2024_0000 for 2024)
#' @param input_sel A character string specifying the input directory where the SDMX files are located.
#'   Defaults to "M:/nas/Incoming_SDMX_files/".
#' @param output_sel A character string specifying the output directory where the Parquet files should be saved.
#'   Defaults to "data/a/new" relative to the project root using `here::here()`.
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
#' output_sel = "C:/users/biedmlu/data/a/new/")
#'
nfsa_move_convert_T0800 <- function(country,
                           year,
                           input_sel = "M:/nas/Incoming_SDMX_files/",
                           output_sel = here("data","a","new")){
  options(warn=-1)

  library(tidyverse)
  library(readsdmx)
  library(cli)
  library(arrow)
  library(here)
  cli::cli_inform("Looking for files in the server...")

  file <- list.files(path  = input_sel,
                     pattern = paste0("NASEC_T0800_A_",country,"_",year),
                     full.names = TRUE) |>
    as_tibble() |>
    mutate(version = as.numeric(str_extract(value, "(?<=_A_..............).{4}"))) |>
    filter(version == max(version)) |>
    pull(value)

  files_have <- list.files(path  = output_sel,
                           pattern = paste0("NASEC_T0800_A_",country,"_",year),
                           recursive = FALSE) |>
    as_tibble() |>
    mutate(version = as.numeric(str_extract(value, "(?<=_A_..............).{4}"))) |>
    filter(version == max(version)) |>
    mutate(value = str_replace_all(value,".parquet", ".xml"),
           value= paste0(input_sel, value)) |>
    pull(value)

  if(length(files_have) ==0) {files_have <-"no file"}

  if(length(file) ==0) {print(paste0("No T0800 file for ", country))
  } else {

    if (file == files_have){
      print(paste0("No new T0800 file for ", country))
    } else{

cli::cli_inform("Converting files...")

      if(length(file) ==1){
        file_name <- stringr::str_sub(file,
                                      nchar(file)-35,
                                      nchar(file))

        file_name = stringr::str_replace(file_name,
                                         ".xml",
                                         ".parquet")

        dat <- file |>
          read_sdmx() |>
          janitor::clean_names() |>
          mutate(received = file.mtime(file),
                 version = str_extract(file,"(?<=_A_...).{15}"))

        if("embargo_date" %in% names(dat)){

          dat <- dat |>
            select(received, version, table_identifier, freq,adjustment,ref_area,counterpart_area,
                   ref_sector,counterpart_sector, consolidation, accounting_entry,sto,
                   instr_asset, maturity, expenditure,unit_measure,currency_denom,
                   valuation, prices, transformation, cust_breakdown, time_period,
                   obs_value, obs_status, conf_status, embargo_date) |>
            mutate(obs_value = as.numeric(obs_value),
                   embargo_date = as.character(embargo_date))

        } else {
          dat <- dat |>
            select(received,version,table_identifier, freq,adjustment,ref_area,counterpart_area,
                   ref_sector,counterpart_sector, consolidation, accounting_entry,sto,
                   instr_asset, maturity, expenditure,unit_measure,currency_denom,
                   valuation, prices, transformation, cust_breakdown, time_period,
                   obs_value, obs_status, conf_status) |>
            mutate(obs_value = as.numeric(obs_value),
                   embargo_date = "NA")
        }

        options(warn=0)
        arrow::write_parquet(dat,
                             paste0(output_sel,"/",file_name))
        print(file_name)
      }
    }
  }
}



