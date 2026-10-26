#' @title NFSA Completeness Aggregation
#'
#' @description This function checks the completeness of NFSA data for a given country for Table 801,
#'   comparing it against predefined data requirements. .
#'
#' @param data A data frame containing `ref_area`, `id`,
#'   `time_period` and `obs_value`.
#' @param end_quarter Last quarter that should have been reported (e.g.: "2026-Q3")
#' @param requirements A link to the file to be checked, by default `""M:/nas/Rprod/assets/completeness_aggregation.xlsx"`.

#'
#' @return A data frame containing the missing data points. Returns an empty dataframe if data requirements are fulfilled.
#' @examples
#' \dontrun{
#' missing_data <- nfsa_completeness_aggregation(dat)
#' }
#'
#' @export
nfsa_completeness_aggregation <- function(data,
                                          end_quarter,
                                          requirements = "M:/nas/Rprod/assets/completeness_aggregation.xlsx") {

  country <- unique(dat$ref_area)
  lookup <- nfsa::nfsa_sto_lookup

  # Initialize dat_missing to avoid scope issues
  dat_missing <- NULL


  lookup <- nfsa::nfsa_sto_lookup

  smallEA <- c("BG", "EE", "HR", "CY", "LT", "LV", "LU", "MT", "SI", "SK")
  bigEA <- c("AT", "BE", "DE", "EL", "ES", "FI", "FR", "IE", "IT", "NL", "PT")
  bigEU <- c("CZ", "DK", "HU", "PL", "RO", "SE")


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

  req_smallEA <- req |>
    dplyr::filter(smallEA == "TRUE") |>
    dplyr::select(id)  %>%
    tidyr::crossing(.,data.frame(ref_area = smallEA))

  req_bigEA <- req |>
    dplyr::filter(bigEA == "TRUE") |>
    dplyr::select(id)  %>%
    tidyr::crossing(.,data.frame(ref_area = bigEA))

  req_bigEU <- req |>
    dplyr::filter(bigEU == "TRUE") |>
    dplyr::select(id)  %>%
    tidyr::crossing(.,data.frame(ref_area = bigEU))

  req <- dplyr::bind_rows(req_smallEA, req_bigEA, req_bigEU) |>
    dplyr::filter(ref_area %in% country)%>%
    tidyr::crossing(., time_period) |>
    select(ref_area,id,time_period)

  rm(req_smallEA,req_bigEA, req_bigEU, time_period)



  dat <- data |>
    nfsa::nfsa_separate_id() |>
    filter(time_period >= "1999-Q1",
           ref_sector %in% c("S1", "S1N", "S11", "S12", "S13", "S1M", "S2")) |>
    nfsa_unite_id() %>%
    left_join(req,., by = join_by(ref_area,id, time_period))

  dat_missing <- dat |>
    filter(is.na(obs_value))


  if (nrow(dat_missing) == 0) {
    cli::cli_inform(paste0("Data requirements are fulfilled" ))
  } else {


    dat_missing <- dat_missing |>
      mutate(
        from = min(time_period),
        to = max(max(time_period)),.by = c(id)
      ) |>
      select(-time_period) |>
      distinct() |>
      rename(missing_series = id) |>
      select(ref_area,missing_series,from,to) |>
      arrange(ref_area)

  return(dat_missing)
  }
}


