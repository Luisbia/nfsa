#' @title Check NFSA Completeness Against Regulation for Table 0801SA
#'
#' @description This function checks the completeness of NFSA (National Financial Statistics Authority)
#' data against predefined regulatory requirements. It identifies missing data points
#' for a specified country and table, and generates an Excel file detailing these
#' gaps if any exist.
#'
#' @param country A character vector specifying the countries for which to check data completeness.
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
nfsa_completeness_regulation_T0801SA <- function(country  ,
                                         output_sel = here::here("output", "completeness")) {
  lookup <- nfsa::nfsa_sto_lookup
  small <- c("BG", "EE", "HR", "CY", "LT", "LV", "LU", "MT", "SI", "SK")
  big <- c("AT", "BE", "CZ", "DE", "DK","EL", "ES", "FI", "FR", "HU", "IE", "IT",
           "NL", "PL", "PT", "RO", "SE")
    requirements <- here::here("assets", "completeness_T0801SA.xlsx")
    req <- readxl::read_xlsx(requirements)
    req_time <- list.files(path = "M:/nas/Rprod/data/q/new/sca/",
                           pattern = ".parquet",
                           full.names = TRUE) |>
      arrow::open_dataset() |>
      dplyr::select(time_period) |>
      dplyr::filter(time_period>= "1999-Q1") |>
      dplyr::distinct() |>
      dplyr::collect()

    req_small <- req |>
      dplyr::filter(countries == "ALL") |>
      dplyr::select(-countries)  %>%
      tidyr::crossing(.,data.frame(ref_area = small))

    req_big <- req |>
      dplyr::select(-countries)  %>%
      tidyr::crossing(.,data.frame(ref_area = big))

    req <- dplyr::bind_rows(req_small, req_big) |>
      dplyr::filter(ref_area %in% country)%>%
      tidyr::crossing(., req_time)

    rm(req_small,req_big, req_time)

    dat <- list.files(path = "M:/nas/Rprod/data/q/new/sca/",
                      pattern = ".parquet",
                      full.names = TRUE) |>
      tibble::as_tibble() |>
      dplyr::mutate(
        version = as.numeric(stringr::str_extract(value, "(?<=_Q_..............).{4}")),
        country_sel = stringr::str_extract(value, "(?<=_Q_)..")
      ) |>
      dplyr::filter(country_sel %in% country) |>
      dplyr::group_by(country_sel) |>
      dplyr::arrange(version) |>
      dplyr::slice_tail(n = 1) |>
      dplyr::pull(value) |>
      arrow::open_dataset() |>
      dplyr::select(-received, -embargo_date,-version) |>
      dplyr::filter(time_period >= "1999-Q1",
             ref_sector %in% c("S1", "S1N", "S11", "S12", "S13", "S1M")) |>
      dplyr::collect()  %>%
      dplyr::left_join(req,.,by = dplyr::join_by(table_identifier, freq,
                                   adjustment, counterpart_area, ref_sector,
                                   counterpart_sector, consolidation, accounting_entry,
                                   sto, instr_asset, maturity, expenditure,
                                   unit_measure, currency_denom, valuation, prices,
                                   transformation, cust_breakdown, ref_area,time_period)) %>%
      dplyr::select(ref_area,table_identifier,id,time_period,obs_value,obs_status) |>
      nfsa_separate_id()

    dat_missing <- dat |>
      dplyr::filter(is.na(obs_value))|>
      dplyr::select(-obs_value)


    if (nrow(dat_missing) == 0) {
      cli::cli_inform(paste0("Data requirements for T0801SA are fulfilled" ))
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

    nfsa::nfsa_to_excel(dat_missing)
  return(dat_missing)
}
