#' @title Check NFSA Completeness Against Regulation for Table 0801SA
#'
#' @description This function checks the completeness of NFSA data against predefined regulatory requirements.
#'
#' @param data Data frame containing `ref_area`, `id`, `time_period`, and
#'   `obs_value` for the countries to check.
#' @param end_quarter Last quarter that should have been reported (e.g.: "2026-Q3")
#' @param requirements A link to the file to be checked, by default `"M:/nas/Rprod/assets/completeness_T0801SA.xlsx"`.
#'
#' @return Invisible. The function's primary side effect is to produce an Excel file when data is missing.
#'   It also prints messages to the console indicating whether the data requirements are fulfilled or
#'   where the output file is saved.
#'
#' @examples
#' \dontrun{
#' # Check completeness for Germany save the output in "my_output" directory
#' nfsa_completeness_T0801SA(data = dat, end_quarter = "2026-Q3")

#' }
#' @importFrom cli cli_inform
#' @importFrom dplyr arrange bind_rows distinct filter join_by left_join mutate pull rename select
#' @importFrom magrittr %>%
#' @importFrom readxl read_xlsx
#' @importFrom stringr str_sub
#' @importFrom tidyr crossing expand_grid
#' @export
nfsa_completeness_T0801SA <- function(data,
                                      end_quarter,
                                      requirements = "M:/nas/Rprod/assets/completeness_T0801SA.xlsx") {

  country <- unique(data$ref_area)

  lookup <- nfsa::nfsa_sto_lookup
  small <- c("BG", "EE", "HR", "CY", "LT", "LV", "LU", "MT", "SI", "SK")
  big <- c("AT", "BE", "CZ", "DE", "DK","EL", "ES", "FI", "FR", "HU", "IE", "IT",
           "NL", "PL", "PT", "RO", "SE")
  req <- readxl::read_xlsx(requirements)
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

  req_small <- req |>
    dplyr::filter(countries == "ALL") |>
    dplyr::select(-countries)  %>%
    tidyr::crossing(.,data.frame(ref_area = small))

  req_big <- req |>
    dplyr::select(-countries)  %>%
    tidyr::crossing(.,data.frame(ref_area = big))

  req <- dplyr::bind_rows(req_small, req_big) |>
    dplyr::filter(ref_area %in% country)%>%
    tidyr::crossing(., time_period) |>
    dplyr::select(ref_area,id,time_period)

  rm(req_small,req_big, time_period)

  dat <- data |>
    nfsa::nfsa_separate_id() |>
    dplyr::filter(time_period >= "1999-Q1",
                  ref_sector %in% c("S1", "S1N", "S11", "S12", "S13", "S1M", "S2")) |>
    nfsa_unite_id() %>%
    dplyr::left_join(req,., by = join_by(ref_area,id, time_period))


  dat_missing <- dat |>
    dplyr::filter(is.na(obs_value))|>
    dplyr::select(ref_area,id, time_period)

  if (nrow(dat_missing) == 0) {
    cli::cli_inform(paste0("Data requirements for T0801SA are fulfilled" ))
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

