#' @title Check NFSA Completeness Against Regulation for Table 0801SA
#'
#' @description This function checks the completeness of NFSA (National Financial Statistics Authority)
#' data against predefined regulatory requirements. It identifies missing data points
#' for a specified country and table, and generates an Excel file detailing these
#' gaps if any exist.
#'
#' @param country A character vector specifying the countries for which to check data completeness.
#' @param file If a file should be written. FALSE by default which will open a temporary Excel file.
#' @param output_sel A file path specifying the directory where the output Excel file should be saved. Defaults to "output/completeness" inside the project.
#'
#' @return Invisible. The function's primary side effect is to produce an Excel file when data is missing.
#'   It also prints messages to the console indicating whether the data requirements are fulfilled or
#'   where the output file is saved.
#'
#' @examples
#' \dontrun{
#' # Check completeness for Germany save the output in "my_output" directory
#' nfsa_completeness_regulation_T0801SA(country = "DE",output_sel = "my_output")

#' }
#' @export
nfsa_completeness_regulation_T0801SA <- function(country,
                                                 file = FALSE,
                                                 output_sel = here::here("output", "completeness")) {
  library(tidyverse)
  lookup <- nfsa::nfsa_sto_lookup
  small <- c("BG", "EE", "HR", "CY", "LT", "LV", "LU", "MT", "SI", "SK")
  big <- c("AT", "BE", "CZ", "DE", "DK","EL", "ES", "FI", "FR", "HU", "IE", "IT",
           "NL", "PL", "PT", "RO", "SE")
  requirements <- "M:/nas/Rprod/assets/completeness_T0801SA.xlsx"
  req <- readxl::read_xlsx(requirements)
  req_time <- nfsa_get_data (country = "FR", table = "T0801SA",
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
    select(ref_area,id,time_period)

  rm(req_small,req_big, req_time)

  dat <- nfsa_get_data(country = country, table = "T0801SA", complete = TRUE) |>
    dplyr::select(ref_area, id, time_period,obs_value,obs_status) |>
    nfsa::nfsa_separate_id() |>
    dplyr::filter(time_period >= "1999-Q1",
                  ref_sector %in% c("S1", "S1N", "S11", "S12", "S13", "S1M", "S2")) |>
    nfsa_unite_id() %>%
    left_join(req,., by = join_by(ref_area,id, time_period))


  dat_missing <- dat |>
    dplyr::filter(is.na(obs_value)| obs_status == "M")|>
    select(ref_area,id, time_period,obs_status) |>
    mutate(
      from = min(time_period),
      to = max(max(time_period))
    ) |>
    select(-time_period) |>
    distinct() |>
    rename(missing_series = id) |>
    mutate(from = str_replace_all(from, c(
      "-01-01" = "-Q1",
      "-04-01" = "-Q2",
      "-07-01" = "-Q3",
      "-10-01" = "-Q4"
    ))) |>
    mutate(to = str_replace_all(to, c(
      "-01-01" = "-Q1",
      "-04-01" = "-Q2",
      "-07-01" = "-Q3",
      "-10-01" = "-Q4"
    ))) |>
    select(ref_area,missing_series,from,to,obs_status) |>
    arrange(ref_area)


  if (nrow(dat_missing) == 0) {
    cli::cli_inform(paste0("Data requirements for T0801SA are fulfilled" ))
  } else if (file == FALSE) {

    nfsa::nfsa_to_excel(dat_missing)

  } else {
    openxlsx::write.xlsx(dat_missing,
                         asTable = TRUE,
                         file = paste0(output_sel, "/completeness_regulation_T0801SA_",
                                       as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"),
                         overwrite = TRUE
    )

    cli::cli_alert_success(paste0("File created in ", output_sel, "/completeness_regulation_T0801SA_"
                                  ,as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"))
  }


  return(dat_missing)
}
