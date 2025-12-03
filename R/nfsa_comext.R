#' @title Process and export Eurostat Comext data
#'
#' @description This function retrieves, processes, and exports Eurostat Comext
#'   data for a specified area (EA20 or EU27) and quarter.
#'
#' @param area A string indicating the geographical area ("EA20" or "EU27").
#' @param quarter A string representing the target quarter in the format "YYYYQ#".
#' @param output_sel A string specifying the directory where the output CSV file should be saved.
#' @param input_sel A string specifying the directory where the additional files are saved. Defaults to
#'   "M:/nas/QSA10/Production/2025Q2/(1) QSA/(1_1) Original transmission/(1_1_1) Original xml files/EA early/Comext/".
#'   It is important that the area has the right associated file. For example, "EA20_lines to add.csv" for area EA20 and "EU27_lines to add.csv" for area EU27.
#'
#' @return A data frame containing the processed Comext data. The data frame is
#'   also written to a CSV file in the specified output directory.
#'
#' @examples
#' \dontrun{
#' # Example usage for EA20 in 2023 Q4
#' nfsa_comext(area = "EA20", quarter = "2023Q4", output_sel = "path/to/output/")
#'
#' # Example usage for EU27 in 2023 Q4
#' nfsa_comext(area = "EU27", quarter = "2023Q4", output_sel = "path/to/output/")
#' }
#'
#' @export
nfsa_comext <- function(area,
                        quarter,
                        output_sel,
                        input_sel = "M:/nas/QSA10/Production/2025Q2/(1) QSA/(1_1) Original transmission/(1_1_1) Original xml files/EA early/Comext/"){

library(tidyverse)
library(restatapi)

if(area == "EA20"){

    ea20_raw <- get_eurostat_data("ext_st_eabec",
                                 filters = list(indic_et = "TRD_VAL",
                                                partner = "EA20",
                                                bclas_bec = c("CAP", "CONS", "INT", "TOTAL"),
                                                stk_flow = c("EXP", "IMP")),
                                 stringsAsFactors = FALSE)

  ea20 <- ea20_raw |>
    mutate(year = str_sub(time,1,4),
           quarter = case_when(str_sub(time,6,7) %in% c("01", "02","03") ~ "Q1",
                               str_sub(time,6,7) %in% c("04", "05","06") ~ "Q2",
                               str_sub(time,6,7) %in% c("07", "08","09") ~ "Q3",
                               str_sub(time,6,7) %in% c("10", "11","12") ~ "Q4"))

  check <- ea20 |>
    group_by(year,quarter,bclas_bec,stk_flow) |>
    tally() |>
    filter(n<3)

  if (nrow(check) > 0) {

    cli::cli_abort("A month is still missing to do the calculation of the quarter")
  } else {

    ea20 <- ea20 |>
    unite("time",c(year,quarter),sep="") |>
    filter(time <= quarter)|>
    summarise(values = sum(values)*1000000, .by = -c(values)) |>
    select(REPORTER = geo,
           PARTNER = partner,
           PRODUCT = bclas_bec,
           FLOW = stk_flow,
           PERIOD = time,
           INDICATORS = indic_et,
           INDICATOR_VALUE = values) |>
    mutate(REPORTER = "EA20",
           PARTNER = "EA20_INTRA",
           PRODUCT = if_else(PRODUCT != "TOTAL", paste0("/", PRODUCT),PRODUCT),
           FLOW = if_else(FLOW == "IMP",1,2),
           INDICATORS = "VALUE_IN_EUROS")

  ## load and bind 1999Q1-2001Q4
  ea20_to_add <- readr::read_csv(paste0(input_sel,"/EA20_lines to add.csv"),
                                 show_col_types = FALSE)

  ea20 <- bind_rows(ea20_to_add,ea20)



  ## Write the file
  write_csv(ea20,paste0(output_sel, "EA20_", quarter,"_comext.csv") )

  cli::cli_alert_success(paste0("File created in ",output_sel, "EA20_", quarter,"_comext.csv"))

  return(ea20)
}  }

  if(area == "EU27"){

    eu27_raw <- get_eurostat_data("ext_st_eu27_2020bec",
                                  filters = list(indic_et = "TRD_VAL",
                                                 partner = "EU27_2020",
                                                 bclas_bec = c("CAP", "CONS", "INT", "TOTAL"),
                                                 stk_flow = c("EXP", "IMP")),
                                  stringsAsFactors = FALSE)

    ## clean data ----
    eu27 <- eu27_raw |>
      mutate(year = str_sub(time,1,4),
             quarter = case_when(str_sub(time,6,7) %in% c("01", "02","03") ~ "Q1",
                                 str_sub(time,6,7) %in% c("04", "05","06") ~ "Q2",
                                 str_sub(time,6,7) %in% c("07", "08","09") ~ "Q3",
                                 str_sub(time,6,7) %in% c("10", "11","12") ~ "Q4"))

    check <- eu27 |>
      group_by(year,quarter,bclas_bec,stk_flow) |>
      tally() |>
      filter(n < 3)

    if (nrow(check) > 0) {

      cli::cli_abort("A month is still missing to do the calculation of the quarter")
    } else {
      eu27 <- eu27 |>
      unite("time",c(year,quarter),sep="") |>
      filter(time <= quarter)|>
      summarise(values = sum(values)*1000000, .by = -c(values)) |>
      select(REPORTER = geo,
             PARTNER = partner,
             PRODUCT = bclas_bec,
             FLOW = stk_flow,
             PERIOD = time,
             INDICATORS = indic_et,
             INDICATOR_VALUE = values) |>
      mutate(REPORTER = "EU27",
             PARTNER = "EU27_2020_INTRA",
             PRODUCT = if_else(PRODUCT != "TOTAL", paste0("/", PRODUCT),PRODUCT),
             FLOW = if_else(FLOW == "IMP",1,2),
             INDICATORS = "VALUE_IN_EUROS")

    ## load and bind 1999Q1-2001Q4
    eu27_to_add <- readr::read_csv(paste0(input_sel,"EU27_lines to add.csv"))

    eu27 <- bind_rows(eu27_to_add,eu27)


    ## Write the file
    write_csv(eu27,paste0(output_sel, "EU27_", quarter,"_comext.csv") )

    cli::cli_alert_success(paste0("File created in ",output_sel, "EU27_", quarter,"_comext.csv"))

    return(eu27)
    }
  }
}

