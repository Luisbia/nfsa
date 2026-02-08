#' @title NFSA Completeness Aggregation
#'
#' @description This function checks the completeness of NFSA data for a given country for Table 801,
#'   comparing it against predefined data requirements. It identifies missing data points and
#'   exports them to an Excel file.
#'
#' @param country A character vector specifying the country(ies) codes (e.g., "BE").
#' @param file If a file should be written. FALSE by default which will open a temporary Excel file.
#' @param output_sel A character string specifying the output directory for the completeness report.
#'   Defaults to `here::here("output", "completeness")`.
#'
#' @return A data frame containing the missing data points. Returns an empty dataframe if data requirements are fulfilled.
#' @examples
#' \dontrun{
#' # Check completeness for Belgium (BE) for table T0801 and save the report to the default output directory
#' missing_data <- nfsa_completeness_aggregation(country = "BE")
#' }
#'
#' @export
nfsa_completeness_aggregation <- function(country,
                                          file = FALSE,
                                          output_sel = here::here("output", "completeness")) {
  library(tidyverse)
  library(openxlsx)


  lookup <- nfsa::nfsa_sto_lookup

  # Initialize dat_missing to avoid scope issues
  dat_missing <- NULL


  lookup <- nfsa::nfsa_sto_lookup

  small <- c("BG", "EE", "HR", "CY", "LT", "LV", "LU", "MT", "SI", "SK")
  big <- c("AT", "BE", "CZ", "DE", "DK","EL", "ES", "FI", "FR", "HU", "IE", "IT",
           "NL", "PL", "PT", "RO", "SE")

  requirements <- "M:/nas/Rprod/assets/completeness_aggregation.xlsx"
  req <- readxl::read_xlsx(requirements)

  req_time <- nfsa_get_data (country = c("FR","SE"), table = "T0801",
                             filters = "sto == 'B1GQ'") |>
    select(time_period) |>
    filter(time_period >= "1999-Q1") |>
    arrange()

  req_small <- req |>
    dplyr::filter(countries == "ALL") |>
    dplyr::select(-countries)  %>%
    tidyr::crossing(.,data.frame(ref_area = small))

  req_big <- req |>
    dplyr::select(-countries)  %>%
    tidyr::crossing(.,data.frame(ref_area = big))

  req <- dplyr::bind_rows(req_small, req_big) |>
    dplyr::filter(ref_area %in% country)%>%
    tidyr::crossing(., req_time) |>
    select(ref_area,id,pub,time_period)

  rm(req_small,req_big, req_time)



  dat <- nfsa_get_data(country = country,complete = TRUE) |>
    select(ref_area, id, time_period, obs_value,obs_status) |>
    nfsa::nfsa_separate_id() |>
    filter(time_period >= "1999-Q1",
           ref_sector %in% c("S1", "S1N", "S11", "S12", "S13", "S1M", "S2")) |>
    nfsa_unite_id() %>%
    left_join(req,., by = join_by(ref_area,id, time_period))

  dat_missing <- dat |>
    filter(is.na(obs_value))


  if (nrow(dat_missing) == 0) {
    cli::cli_inform(paste0("Data requirements for ",table, " are fulfilled" ))
  } else if (file == FALSE) {


    dat_missing <- dat_missing |>
      mutate(
        from = min(time_period),
        to = max(max(time_period))
      ) |>
      select(-time_period) |>
      distinct() |>
      rename(missing_series = id) |>
      select(ref_area,missing_series,from,to,obs_status,pub) |>
      arrange(ref_area)

    nfsa::nfsa_to_excel(dat_missing)

  } else {

    write.xlsx(dat_missing,
               asTable = TRUE,
               file = paste0(output_sel, "/completeness_aggregation_",
                             table, "_", as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"),
               overwrite = TRUE
    )

    cli::cli_alert_success(paste0("File created in ", output_sel, "/completeness_aggregation_",
                                  table, "_", as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"))
  }
  return(dat_missing)
}



