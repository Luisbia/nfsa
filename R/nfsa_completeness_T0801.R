#' @title Check NFSA Completeness Against Regulation for Table T0801
#'
#' @description This function checks the completeness of NFSA data against predefined regulatory requirements.
#'
#' @param country A character vector specifying the countries for which to check data completeness.
#' @param end_quarter Last quarter that should have been reported (e.g.: "2026-Q3")
#' @param requirements A link to the file to be checked, by default `"M:/nas/Rprod/assets/completeness_T0801.xlsx"`.
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
#' @import dplyr
#' @import janitor
#' @import tidyr
#' @import readxl
#' @export
nfsa_completeness_T0801 <- function(data,
                                    end_quarter,
                                    requirements = "M:/nas/Rprod/assets/completeness_T0801.xlsx") {

  countries <- unique(data$ref_area)

  lookup <- nfsa::nfsa_sto_lookup
  small_eur <- c("BG", "CY", "EE", "HR", "MT", "LT", "LU", "LV", "SI", "SK")
  small_non_eur <- c("AL", "RS", "ME", "MK", "IS")
  big_eur <- c("AT", "BE", "DE", "EL", "ES", "FI", "FR", "IE", "IT", "NL", "PT")
  big_non_eur <- c("CZ", "DK", "HU", "PL", "RO", "SE", "TR", "CH", "NO")
  req <-  readxl::read_xlsx(requirements, sheet = "regulation") |>
    janitor::clean_names() |>
    dplyr::left_join(y = nfsa::nfsa_sto_lookup,by = join_by(counterpart_area,
                                                     ref_sector, counterpart_sector, consolidation,
                                                     accounting_entry, sto, instr_asset, unit_measure,
                                                     prices)) |>
    dplyr::select(ref_area,id)
  max_year <- as.numeric(stringr::str_sub(end_quarter, 1, 4))
  max_quarter <- as.numeric(stringr::str_sub(end_quarter, 7))

  time_period <- tidyr::expand_grid(
    year = 1999:max_year,
    quarter = 1:4
  ) |>
    dplyr::filter(
      year < max_year |
        (year == max_year & quarter <= max_quarter)
    ) |>
    dplyr::mutate(time_period = paste0(year, "-Q", quarter)) |>
    dplyr::pull(time_period)


  req_small_eur <- req |>
    dplyr::filter(ref_area == "small_eur") |>
    dplyr::select(-ref_area)  %>%
    tidyr::crossing(.,data.frame(ref_area = small_eur))

  req_small_non_eur <- req |>
    dplyr::filter(ref_area == "small_non_eur") |>
    dplyr::select(-ref_area)  %>%
    tidyr::crossing(.,data.frame(ref_area = small_non_eur))

  req_big_eur <- req |>
    dplyr::filter(ref_area == "big_eur") |>
    dplyr::select(-ref_area)  %>%
    tidyr::crossing(.,data.frame(ref_area = big_eur))

  req_big_non_eur <- req |>
    dplyr::filter(ref_area == "big_non_eur") |>
    dplyr::select(-ref_area)  %>%
    tidyr::crossing(.,data.frame(ref_area = big_non_eur))

  req <- dplyr::bind_rows(req_small_eur, req_small_non_eur,req_big_eur, req_big_non_eur) |>
    dplyr::filter(ref_area %in% countries)%>%
    tidyr::crossing(., time_period) |>
    dplyr::select(ref_area,id,time_period)



  rm(req_small_eur,req_small_non_eur,req_big_eur,req_big_non_eur, time_period)

  dat <- data |>
    nfsa::nfsa_separate_id() |>
    dplyr::filter(time_period >= "1999-Q1",
                  ref_sector %in% c("S1", "S1N", "S11", "S12", "S13", "S1M", "S2")) |>
    nfsa::nfsa_unite_id() %>%
    dplyr::left_join(req,., by = join_by(ref_area,id, time_period))


  dat_missing <- dat |>
    dplyr::filter(is.na(obs_value))|>
    dplyr::select(ref_area,id, time_period)


  derogations <- readxl::read_xlsx(requirements, sheet = "derogations")

  dat_missing <- dplyr::anti_join(dat_missing,derogations, by = join_by(ref_area,id,time_period))

  if (nrow(dat_missing) == 0) {
    cli::cli_inform(paste0("Data requirements for T0801 are fulfilled" ))
  } else  {

    dat_missing <- dat_missing |>
      dplyr::mutate(
        from = min(time_period),
        to = max(max(time_period))
      ) |>
      dplyr::select(-time_period) |>
      dplyr::distinct() |>
      dplyr::rename(missing_series = id) |>
      dplyr::select(ref_area,missing_series,from,to) |>
      dplyr::arrange(ref_area)

  return(dat_missing)
  }
}
