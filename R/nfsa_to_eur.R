#' @title Convert to Euro
#'
#' @description Converts data to Euro using exchange rates from Eurostat.
#'   Automatically detects whether to use Euro-fixed or non-Euro conversion
#'   based on the countries present in the ref_area column. Handles mixed datasets
#'   containing both types of countries.
#'
#' @param data A data frame containing financial data, with columns `ref_area` (country code) and `time_period` (time period).
#' @param freq A character string indicating the frequency of the data ("Q" for quarterly, "A" for annual). Defaults to "Q".
#'
#' @return A data frame with an additional `obs_value` column, where the original values have been converted to Euro.
#'
#' @examples
#' \dontrun{
#' # Convert to Euro (quarterly data) - automatically detects conversion type
#' data_eur <- nfsa_to_eur(data = your_data, freq = "Q")
#'
#' # Convert to Euro (annual data)
#' data_eur <- nfsa_to_eur(data = your_data, freq = "A")
#' }
#' @export
nfsa_to_eur <- function(data, freq = "Q"){

  # Define non-Euro countries (those needing noneuro conversion)
  noneuro_countries <- c("CZ", "DK", "HU", "PL", "RO", "SE",
                         "CH", "IS", "NO", "AL", "MK", "RS", "TR")

  # Identify which countries are present in the data
  countries_in_data <- unique(data$ref_area)

  # Determine which countries need which conversion
  noneuro_in_data <- intersect(countries_in_data, noneuro_countries)
  eurofixed_in_data <- setdiff(countries_in_data, noneuro_countries)

  # Initialize result
  result_list <- list()

  # Process Euro-fixed countries if present
  if(length(eurofixed_in_data) > 0){

    data_eurofixed <- data |>
      dplyr::filter(ref_area %in% eurofixed_in_data)

    if(freq == "Q"){
      erate <- restatapi::get_eurostat_data("ert_bil_conv_q",
                                            filters = list(statinfo = "AVG"),
                                            stringsAsFactors = FALSE) |>
        dplyr::select(ref_area = geo, time_period = time, erate = values) |>
        tibble::as_tibble()

      tmp <- data_eurofixed |>
        dplyr::filter(stringr::str_detect(id, ".PS|.HW"))

      data_eur_fixed <- dplyr::left_join(data_eurofixed, erate, by = dplyr::join_by(ref_area, time_period)) |>
        dplyr::mutate(obs_value = round(obs_value/erate,3)) |>
        dplyr::select(-erate) |>
        dplyr::filter(!stringr::str_detect(id, ".PS|.HW"))

      data_eur_fixed <- dplyr::bind_rows(data_eur_fixed, tmp)

    } else if (freq == "A"){
      erate <- restatapi::get_eurostat_data("ert_bil_conv_a",
                                            filters = list(statinfo = "AVG"),
                                            stringsAsFactors = FALSE) |>
        dplyr::select(ref_area = geo, time_period = time, erate = values)|>
        tibble::as_tibble()

      tmp <- data_eurofixed |>
        dplyr::filter(stringr::str_detect(id, ".PS|.HW"))

      data_eur_fixed <- dplyr::left_join(data_eurofixed, erate, by = dplyr::join_by(ref_area, time_period)) |>
        dplyr::mutate(obs_value = round(obs_value/erate,3)) |>
        dplyr::select(-erate) |>
        dplyr::filter(!stringr::str_detect(id, ".PS|.HW"))

      data_eur_fixed <- dplyr::bind_rows(data_eur_fixed, tmp)
    }

    result_list <- c(result_list, list(data_eur_fixed))
  }

  # Process non-Euro countries if present
  if(length(noneuro_in_data) > 0){

    data_noneuro <- data |>
      dplyr::filter(ref_area %in% noneuro_in_data)

    if(freq == "Q"){
      erate <- restatapi::get_eurostat_data("ert_bil_eur_q",
                                            filters = list(statinfo = "AVG",
                                                           currency = c("CZK","DKK","HUF", "PLN", "RON", "SEK",
                                                                        "CHF", "ISK", "NOK", "ALL", "MKD", "RSD", "TRY")),
                                            stringsAsFactors = FALSE) |>
        dplyr::mutate(ref_area = dplyr::case_when(currency == "CZK" ~ "CZ",
                                                  currency == "DKK" ~ "DK",
                                                  currency == "HUF" ~ "HU",
                                                  currency == "PLN" ~ "PL",
                                                  currency == "RON" ~ "RO",
                                                  currency == "SEK" ~ "SE",
                                                  currency == "CHF" ~ "CH",
                                                  currency == "ISK" ~ "IS",
                                                  currency == "NOK" ~ "NO",
                                                  currency == "ALL" ~ "AL",
                                                  currency == "MKD" ~ "MK",
                                                  currency == "RSD" ~ "RS",
                                                  currency == "TRY" ~ "TR")) |>
        dplyr::select(ref_area, time_period = time, erate = values) |>
        tibble::as_tibble()

      tmp <- data_noneuro |>
        dplyr::filter(stringr::str_detect(id, ".PS|.HW"))

      data_eur_noneuro <- dplyr::left_join(data_noneuro, erate, by = dplyr::join_by(ref_area, time_period)) |>
        dplyr::mutate(obs_value = round(obs_value/erate,3)) |>
        dplyr::select(-erate) |>
        dplyr::filter(!stringr::str_detect(id, ".PS|.HW"))

      data_eur_noneuro <- dplyr::bind_rows(data_eur_noneuro, tmp)

    } else if (freq == "A"){
      erate <- restatapi::get_eurostat_data("ert_bil_eur_a",
                                            filters = list(statinfo = "AVG",
                                                           currency = c("CZK","DKK","HUF", "PLN", "RON", "SEK",
                                                                        "CHF", "ISK", "NOK", "ALL", "MKD", "RSD", "TRY")),
                                            stringsAsFactors = FALSE) |>
        dplyr::mutate(ref_area = dplyr::case_when(currency == "CZK" ~ "CZ",
                                                  currency == "DKK" ~ "DK",
                                                  currency == "HUF" ~ "HU",
                                                  currency == "PLN" ~ "PL",
                                                  currency == "RON" ~ "RO",
                                                  currency == "SEK" ~ "SE",
                                                  currency == "CHF" ~ "CH",
                                                  currency == "ISK" ~ "IS",
                                                  currency == "NOK" ~ "NO",
                                                  currency == "ALL" ~ "AL",
                                                  currency == "MKD" ~ "MK",
                                                  currency == "RSD" ~ "RS",
                                                  currency == "TRY" ~ "TR")) |>
        dplyr::select(ref_area, time_period = time, erate = values)|>
        tibble::as_tibble()

      tmp <- data_noneuro |>
        dplyr::filter(stringr::str_detect(id, ".PS|.HW"))

      data_eur_noneuro <- dplyr::left_join(data_noneuro, erate, by = dplyr::join_by(ref_area, time_period)) |>
        dplyr::mutate(obs_value = round(obs_value/erate,3)) |>
        dplyr::select(-erate) |>
        dplyr::filter(!stringr::str_detect(id, ".PS|.HW"))

      data_eur_noneuro <- dplyr::bind_rows(data_eur_noneuro, tmp)
    }

    result_list <- c(result_list, list(data_eur_noneuro))
  }

  # Combine all results
  data_eur <- dplyr::bind_rows(result_list)

  return(data_eur)
}

tmp <- nfsa::nfsa_get_data(country = c("IT", "PL"))

tmp1 <- nfsa_to_eur(tmp)
