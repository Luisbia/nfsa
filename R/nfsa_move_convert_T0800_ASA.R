#' Convert and move T0800 files to parquet format from ASA folder.
#'
#' This function converts T0800 XML files for a specified country to parquet format,
#'  cleans the data, and saves it to a specified output directory.  It handles variations
#'  in the file structure, including the presence or absence of an embargo date.
#'
#' @param country A character string specifying the country code for the T0800 files.
#' @param input_sel A character string specifying the path to the directory containing the
#'   original XML files. Defaults to "M:/nas/ASA10/Production/2024/(1) ASA/(1_1) Original transmission/(1_1_1) Original xml files/".
#' @param output_sel A character string specifying the path to the directory where the
#'   converted parquet files will be saved. Defaults to here("data","a","new").
#'
#' @return None. This function saves parquet files to the specified output directory.
#'
#' @examples
#' \dontrun{
#' nfsa_move_convert_T0800_ASA10(country = "BE")
#' }
#'
#' @export
nfsa_move_convert_T0800_ASA10 <- function(country,
                                    input_sel = "M:/nas/ASA10/Production/2024/(1) ASA/(1_1) Original transmission/(1_1_1) Original xml files/",
                                    output_sel = here("data","a","new")){

  options(warn=-1)

  library(tidyverse)
  library(readsdmx)
  library(cli)
  library(arrow)
  library(here)


  files <- list.files(path  = input_sel,
                     pattern = paste0("NASEC_T0800_A_",country),
                     full.names = TRUE) |>
    as_tibble() |>
    mutate(version = as.numeric(str_extract(value, "(?<=_A_..............).{4}"))) |>
    pull(value)


  if(length(files) ==0) {print(paste0("No T0800 file for ", country))
  } else {

        cli::cli_inform("Converting files...")

    move_convert <- function(file, output_path) {

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
  walk(files, ~ move_convert(file = .x,
                             output_path = output_sel))
  }


