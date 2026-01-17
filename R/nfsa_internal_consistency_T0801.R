#' Performs internal consistency checks on NFSA data.
#'
#' This function takes an NFSA dataset and performs a series of internal
#' consistency checks based on predefined rules. It identifies discrepancies
#' in the data based on a specified threshold and outputs the results to an
#' Excel file.  The checks include uses vs resources, S1 vs sum of sub-sectors,
#' subitems vs total, and balancing items.
#'
#' @param dataset A data frame containing the NFSA data. This data frame
#'   should include columns for `sto` (Statistical Object), `ref_sector`
#'   (Reference Sector), `accounting_entry`, `obs_value` (Observed Value),
#'   `ref_area` (Reference Area), and `time_period`.
#' @param output_sel (optional) A character string specifying the directory where
#'   the output Excel file will be saved. Defaults to `"output/internal"`
#'   within the project directory (using `here::here()`).
#' @param threshold (optional) A numeric value specifying the threshold for
#'   identifying discrepancies.  Differences greater than this threshold
#'   (in absolute value) will be flagged. Defaults to 2.
#' @param rounding (optional) An integer specifying the number of decimal places
#'   to round the `obs_value` and check calculations to.  Defaults to 1.
#'
#' @return None. The function writes the results to an Excel file.
#'
#' @details
#' The function performs the following main categories of checks:
#'
#' \itemize{
#'   \item{\strong{Uses vs Resources (UR)}: Checks for consistency between
#'         uses and resources for various statistical objects and reference
#'         sectors.}
#'   \item{\strong{S1 vs Sum of Sub-sectors (S1SS)}: Verifies that the
#'         value for sector S1 is consistent with the sum of its sub-sectors.}
#'   \item{\strong{Subitems vs Total (SIT)}: Checks if total values are
#'         equal to the sum of their subitems.}
#'   \item{\strong{Balancing Items (BI)}: Performs checks to ensure that
#'         balancing items are consistent with other related variables.}
#' }
#'
#' The specific checks performed within each category depend on the
#' `sto` and `ref_sector` values present in the dataset.  The function
#' dynamically adjusts the checks based on the available data.
#'
#' The output Excel file contains a separate sheet for each consistency
#' check that identified discrepancies exceeding the specified threshold. Each
#' sheet includes the relevant columns from the input dataset, as well as
#' the calculated check value and the difference between expected and
#' actual values. "NaN" values are used in the output to represent
#' missing data to avoid errors in Excel.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' nfsa_internal_consistency_T0801(my_dataset, output_sel = "my_output_folder", threshold = 5)
#' }
#'
#' @export
nfsa_internal_consistency_T0801 <- function(dataset,
                                            output_sel = here::here("output", "internal"),
                                            threshold = 1,
                                            rounding = 1) {


  lookup <- nfsa::nfsa_sto_lookup

  data <- dataset |>
    dplyr::mutate(obs_value = janitor::round_half_up(obs_value, rounding)) |>
    nfsa::nfsa_separate_id()



  # Uses vs Resources-----------------------------------------------------------
  ## UR01-----------------------------------------------------------------------

  ur01 <- data |>
    dplyr::filter(
      sto %in% c(
        "D1", "D4", "D41", "D4N","D41G","D42", "D421","D422", "D43", "D44",
        "D45",  "D5", "D6", "D61","D62", "D7", "D71", "D72", "D7N",
        "D74", "D75", "D8", "D9", "D9N","D91", "D92", "D99"
      ),
      ref_sector %in% c("S1", "S2")
    ) |>
    tidyr::pivot_wider(
      names_from = c(ref_sector, accounting_entry),
      values_from = obs_value,
      names_sep = "."
    )

  if(ncol(ur01) == 7){
    ur01 <- ur01 |>
    dplyr::select(ref_area, sto, time_period, S1.D, S2.C, S1.C, S2.D) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `S1.D + S2.C` = sum(c(S1.D, S2.C), na.rm = TRUE),
      `S1.C + S2.D` = sum(c(S1.C, S2.D), na.rm = TRUE),
      check = round(`S1.D + S2.C` - `S1.C + S2.D`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold) |>
    dplyr::filter(if_all(c(S1.D,S1.C), ~ !is.na(.x )))
  } else {
    rm(ur01)
  }

  ## UR02---------------------------------------------------------------------

  ur02 <- data |>
    dplyr::filter(
      sto %in% c(
        "D2", "D21", "D29"
      ),
      ref_sector %in% c("S1", "S2")
    ) |>
    tidyr::pivot_wider(
      names_from = c(ref_sector, accounting_entry),
      values_from = obs_value,
      names_sep = "."
    )

  if(ncol(ur02) == 6){
    ur02 <- ur02 |>
    dplyr::select(ref_area, sto, time_period, S1.D, S1.C, S2.D) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `S1.C + S2.D` = sum(c(S1.C, S2.D), na.rm = TRUE),
      check = round(S1.D - `S1.C + S2.D`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)
  } else {
    rm(ur02)
  }
  ## UR03---------------------------------------------------------------------

  ur03 <- data |>
    dplyr::filter(
      sto %in% c(
        "D3", "D31", "D39"
      ),
      ref_sector %in% c("S1", "S2")
    ) |>
    tidyr::pivot_wider(
      names_from = c(ref_sector, accounting_entry),
      values_from = obs_value,
      names_sep = "."
    )

  if(ncol(ur03) == 6){
    ur03 <- ur03 |>
    dplyr::select(ref_area, sto, time_period, S1.D, S2.C, S1.C) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `S1.D + S2.C` = sum(c(S1.D, S2.C), na.rm = TRUE),
      check = round(S1.C - `S1.D + S2.C`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)
  } else {
    rm(ur03)
  }
  ## UR04---------------------------------------------------------------------

  ur04 <- data |>
    dplyr::filter(
      sto %in% c(
        "D63", "P51C"
      ),
      ref_sector %in% c("S1")
    ) |>
    tidyr::pivot_wider(
      names_from = c(ref_sector, accounting_entry),
      values_from = obs_value,
      names_sep = "."
    )

  if(ncol(ur04) == 5){
    ur04 <- ur04 |>
    dplyr::select(ref_area, sto, time_period, S1.D, S1.C) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `S1.D - S1.C` = sum(c(S1.D, -S1.C), na.rm = TRUE),
    ) |>
    dplyr::filter(abs(`S1.D - S1.C`) > threshold)
  } else {
    rm(ur04)
  }
  ## UR05---------------------------------------------------------------------

  ur05 <- data |>
    dplyr::filter(
      sto %in% c(
        "D43", "D74", "D76"
      ),
      ref_sector %in% c("S1", "S2")
    ) |>
    tidyr::pivot_wider(
      names_from = c(ref_sector, accounting_entry),
      values_from = obs_value,
      names_sep = "."
    )

  if(ncol(ur05) == 7){
    ur05 <- ur05 |>
    dplyr::select(ref_area, sto, time_period, S1.D, S2.D) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `S1.D - S2.D` = sum(c(S1.D, -S2.D), na.rm = TRUE),
    ) |>
    dplyr::filter(abs(`S1.D - S2.D`) > threshold)
  } else {
    rm(ur05)
  }

  ## UR06---------------------------------------------------------------------

  ur06 <- data |>
    dplyr::filter(
      sto %in% c(
        "NP"
      ),
      ref_sector %in% c("S1", "S2")
    ) |>
    tidyr::pivot_wider(
      names_from = c(ref_sector, accounting_entry),
      values_from = obs_value,
      names_sep = "."
    )

  if(ncol(ur06) == 5){
    ur06 <- ur06 |>
    dplyr::select(ref_area, sto, time_period, S1.D, S2.C) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `S1.D + S2.C` = sum(c(S1.D, S2.C), na.rm = TRUE),
    ) |>
    dplyr::filter(abs(`S1.D + S2.C`) > threshold)
  } else {
    rm(ur06)
  }

  ## UR07---------------------------------------------------------------------

  ur07 <- data |>
    dplyr::filter(
      sto %in% c(
        "B9", "B9X9F"
      ),
      ref_sector %in% c("S1", "S2")
    ) |>
    tidyr::pivot_wider(
      names_from = c(ref_sector),
      values_from = obs_value,
      names_sep = "."
    )

  if(ncol(ur07) == 6){
    ur07 <- ur07 |>
    dplyr::select(ref_area, sto, time_period, S1, S2) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `S1 + S2` = sum(c(S1, S2), na.rm = TRUE),
    ) |>
    dplyr::filter(abs(`S1 + S2`) > threshold)
  } else {
    rm(ur07)
  }
  ## UR08---------------------------------------------------------------------

  ur08 <- data |>
    dplyr::filter(
      sto %in% c(
        "D43", "D74"
      ),
      ref_sector %in% c("S1", "S2")
    ) |>
    tidyr::pivot_wider(
      names_from = c(ref_sector,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    )

  if(ncol(ur08) == 7){
    ur08 <- ur08 |>
    dplyr::select(ref_area, sto, time_period, S1.C, S2.C) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `S1.C - S2.C` = sum(c(S1.C, -S2.C), na.rm = TRUE),
    ) |>
    dplyr::filter(abs(`S1.C - S2.C`) > threshold)
  } else {
    rm(ur08)
  }

  # S1 vs Sum of Sub-sectors--------------------------------------------------
  ## S1SS01-------------------------------------------------------------------
  s1ss01 <- data |>
    dplyr::filter(ref_sector %in% c("S1", "S11", "S12", "S13", "S1M", "S1N")) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    dplyr::filter(sto %in% c(
      "D2.D", "D3.C"
    )) |>
    tidyr::pivot_wider(
      names_from = ref_sector,
      values_from = obs_value
    )

  if(ncol(s1ss01) == 9){
    s1ss01 <- s1ss01 |>
      dplyr::select(ref_area, sto, time_period, S1N, S11, S12, S13, S1M, S1) |>
    dplyr::rowwise() |>
    dplyr::mutate(`S1N + S11 + S12 + S13 + S1M` = sum(c(S1N, S11, S12, S13, S1M), na.rm = TRUE)) |>
    dplyr::mutate(check = round(S1 - `S1N + S11 + S12 + S13 + S1M`, rounding)) |>
    dplyr::filter(abs(check) > threshold) |>
    dplyr::filter(if_all(c(S11,S12,S1M), ~ !is.na(.x )))
  } else {
    rm(s1ss01)
  }
  ## S1SS02-------------------------------------------------------------------
  s1ss02 <- data |>
    dplyr::filter(ref_sector %in% c("S1", "S11", "S12", "S13", "S1M")) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    dplyr::filter(sto %in% c(
      "P2.D", "P5.D", "P51G.D", "P5M.D", "D1.D", "D29.D", "D4.D", "D41.D", "D4N.D","D45.D", "D41G.D",
      "D5.D", "D6.D", "D62.D", "D7.D", "D71.D", "D7N.D","D75.D", "D8.D", "D9.D", "D9.N","D99.D",
      "P51.C", "NP.D", "P1.C", "D39.C", "D4.C", "D41.C", "D4N.C","D42.C", "D43.C", "D44.C", "D45.C",
      "D41G.C", "D6.C", "D61.C", "D7.C", "D7N.C","D72.C", "D75.C", "D9.C", "D9N.C","D92.C", "D99.C",
      "P51C.C", "B2A3G.B", "B4G.B", "B5G.B", "B6G.B", "B8G.B", "B101.B", "B9.B", "B9X9F", "EMP.PS", "EMP.HW"
    )) |>
    tidyr::pivot_wider(
      names_from = ref_sector,
      values_from = obs_value
    )

  if(ncol(s1ss02) == 8){
    s1ss02 <- s1ss02 |>
    dplyr::select(ref_area, sto, time_period, S11, S12, S13, S1M, S1) |>
    dplyr::rowwise() |>
    dplyr::mutate(`S11 + S12 + S13 + S1M` = sum(c(S11, S12, S13, S1M), na.rm = TRUE)) |>
    dplyr::mutate(check = round(S1 - `S11 + S12 + S13 + S1M`, rounding)) |>
    dplyr::filter(abs(check) > threshold) |>
    dplyr::filter(if_all(c(S11,S12, S1M), ~ !is.na(.x )))
  } else {
    rm(s1ss02)
  }
  ## S1SS03-------------------------------------------------------------------
    s1ss03 <- data |>
      dplyr::filter(ref_sector %in% c("S1", "S11", "S12","S1M")) |>
      unite("sto", c(sto, accounting_entry), sep = ".") |>
      dplyr::filter(sto %in% c(
        "D43.D", "D91.D"
      )) |>
      tidyr::pivot_wider(
        names_from = ref_sector,
        values_from = obs_value
      )

  if(ncol(s1ss03) == 7){
    s1ss03 <- s1ss03 |>
      dplyr::select(ref_area, sto, time_period, S11, S12, S1M, S1) |>
      dplyr::rowwise() |>
      dplyr::mutate(`S11 + S12 + S1M` = sum(c(S11, S12, S1M), na.rm = TRUE)) |>
      dplyr::mutate(check = round(S1 - `S11 + S12 + S1M`, rounding)) |>
      dplyr::filter(abs(check) > threshold) |>
      dplyr::filter(if_all(c(S11,S12, S1M), ~ !is.na(.x )))
  } else {
    rm(s1ss03)
  }
  ## S1SS04-------------------------------------------------------------------


    s1ss04 <- data |>
      dplyr::filter(ref_sector %in% c("S1", "S11", "S12","S13")) |>
      unite("sto", c(sto, accounting_entry), sep = ".") |>
      dplyr::filter(sto %in% c(
        "D42.D", "D44.D"
      )) |>
      tidyr::pivot_wider(
        names_from = ref_sector,
        values_from = obs_value
      )

  if(ncol(s1ss04) == 7){
    s1ss04 <- s1ss04 |>
      dplyr::select(ref_area, sto, time_period, S11, S12, S13, S1) |>
      dplyr::rowwise() |>
      dplyr::mutate(`S11 + S12 + S13` = sum(c(S11, S12, S13), na.rm = TRUE)) |>
      dplyr::mutate(check = round(S1 - `S11 + S12 + S13`, rounding)) |>
      dplyr::filter(abs(check) > threshold)
  } else {
    rm(s1ss04)
  }
  ## S1SS05-------------------------------------------------------------------
    s1ss05 <- data |>
      dplyr::filter(ref_sector %in% c("S1", "S11", "S12")) |>
      unite("sto", c(sto, accounting_entry), sep = ".") |>
      dplyr::filter(sto %in% c(
        "D43.D"
      )) |>
      tidyr::pivot_wider(
        names_from = ref_sector,
        values_from = obs_value
      )

  if(ncol(s1ss05) == 6){
    s1ss05 <- s1ss05 |>
      dplyr::select(ref_area, sto, time_period, S11, S12,  S1) |>
      dplyr::rowwise() |>
      dplyr::mutate(`S11 + S12` = sum(c(S11, S12), na.rm = TRUE)) |>
      dplyr::mutate(check = round(S1 - `S11 + S12`, rounding)) |>
      dplyr::filter(abs(check) > threshold) |>
      dplyr::filter(if_all(c(S11,S12), ~ !is.na(.x )))
  } else {
    rm(s1ss05)
  }

  ## S1SS06-------------------------------------------------------------------
    s1ss06 <- data |>
      dplyr::filter(ref_sector %in% c("S1", "S12", "S13")) |>
      unite("sto", c(sto, accounting_entry), sep = ".") |>
      dplyr::filter(sto %in% c(
        "D72.D", "D71.C"
      )) |>
      tidyr::pivot_wider(
        names_from = ref_sector,
        values_from = obs_value
      )

  if(ncol(s1ss06) == 6){
    s1ss06 <- s1ss06 |>
      dplyr::select(ref_area, sto, time_period, S12, S13,  S1) |>
      dplyr::rowwise() |>
      dplyr::mutate(`S12 + S13` = sum(c(S12, S13), na.rm = TRUE)) |>
      dplyr::mutate(check = round(S1 - `S12 + S13`, rounding)) |>
      dplyr::filter(abs(check) > threshold)|>
      dplyr::filter(if_all(c(S12), ~ !is.na(.x )))
  } else {
    rm(s1ss06)
  }

  ## S1SS07-------------------------------------------------------------------
  s1ss07 <- data |>
    dplyr::filter(ref_sector %in% c("S1",  "S13")) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    dplyr::filter(sto %in% c(
      "P32.D", "D3.D", "D31.D", "D39.D", "D74.D", "D74_4Y.D", "D76.D", "D92.D", "D2.C", "D21.C",
      "D211.C", "D212.C", "D214.C", "D29.C", "D5.C", "D51.C", "D59.C", "D74.C", "D91.C"
    )) |>
    tidyr::pivot_wider(
      names_from = ref_sector,
      values_from = obs_value
    )

  if(ncol(s1ss07) == 5){
    s1ss07 <- s1ss07 |>
    dplyr::select(ref_area, sto, time_period, S13,  S1) |>
    dplyr::rowwise() |>
    dplyr::mutate(check = round(S1 - S13, rounding)) |>
    dplyr::filter(abs(check) > threshold)
  } else {
    rm(s1ss07)
  }

  ## S1SS08-------------------------------------------------------------------

    s1ss08 <- data |>
      dplyr::filter(ref_sector %in% c("S1",  "S13", "S1M")) |>
      unite("sto", c(sto, accounting_entry), sep = ".") |>
      dplyr::filter(sto %in% c(
        "P3.D", "P31.D", "D63.D", "D631.D", "D632.D", "P13.C"
      )) |>
      tidyr::pivot_wider(
        names_from = ref_sector,
        values_from = obs_value
      )

  if(ncol(s1ss08) == 6){
    s1ss08 <- s1ss08 |>
      dplyr::select(ref_area, sto, time_period, S13, S1M,  S1) |>
      dplyr::rowwise() |>
      dplyr::mutate(`S13 + S1M` = sum(c(S13, S1M), na.rm = TRUE)) |>
      dplyr::mutate(check = round(S1 - `S13 + S1M`, rounding)) |>
      dplyr::filter(abs(check) > threshold) |>
      dplyr::filter(if_all(c(S1M), ~ !is.na(.x )))

  } else {
    rm(s1ss08)
  }
  ## S1SS09-------------------------------------------------------------------
    s1ss09 <- data |>
      dplyr::filter(ref_sector %in% c("S1",  "S1M")) |>
      unite("sto", c(sto, accounting_entry), sep = ".") |>
      dplyr::filter(sto %in% c(
        "D61.D", "D1.C", "D62.C", "D63.C",  "D8.C", "B3G.B"
      )) |>
      tidyr::pivot_wider(
        names_from = ref_sector,
        values_from = obs_value
      )

  if(ncol(s1ss09) == 5){
    s1ss09 <- s1ss09 |>
      dplyr::select(ref_area, sto, time_period, S1M,  S1) |>
      dplyr::rowwise() |>
      dplyr::mutate(check = round(S1 - S1M, rounding)) |>
      dplyr::filter(abs(check) > threshold)|>
      dplyr::filter(if_all(c(S1M), ~ !is.na(.x )))
} else {
  rm(s1ss09)
}

  ## S1SS10-------------------------------------------------------------------
  s1ss10 <- data |>
    dplyr::filter(ref_sector %in% c("S1",  "S1N")) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    dplyr::filter(sto %in% c(
      "D21.D", "D31.C", "D21X31.C"
    )) |>
    tidyr::pivot_wider(
      names_from = ref_sector,
      values_from = obs_value
    )

  if(ncol(s1ss10) == 5){
    s1ss10 <- s1ss10 |>
    dplyr::select(ref_area, sto, time_period, S1N,  S1) |>
    dplyr::rowwise() |>
    dplyr::mutate(check = round(S1 - S1N, rounding)) |>
    dplyr::filter(abs(check) > threshold)
  } else {
    rm(s1ss10)
  }
  ## S1SS11-------------------------------------------------------------------

    s1ss11 <- data |>
      dplyr::filter(ref_sector %in% c("S1M",  "S14")) |>
      unite("sto", c(sto, accounting_entry), sep = ".") |>
      dplyr::filter(sto %in% c(
        "B3G.B", "D1.C", "D11.C", "D12.C", "D61.D", "D611.D", "D612.D", "D613.D", "D614.D", "D61SC.D",
        "D62.C", "D63.C", "D631.C", "D632.C", "D8.C"
      )) |>
      tidyr::pivot_wider(
        names_from = ref_sector,
        values_from = obs_value
      )

  if(ncol(s1ss11) == 5){
    s1ss11 <- s1ss11 |>
      dplyr::select(ref_area, sto, time_period, S1M,  S14) |>
      dplyr::rowwise() |>
      dplyr::mutate(check = round(S1M - S14, rounding)) |>
      dplyr::filter(abs(check) > threshold)
  } else {
    rm(s1ss11)
  }
  ## S1SS12-------------------------------------------------------------------

    s1ss12 <- data |>
      dplyr::filter(ref_sector %in% c("S1M",  "S15")) |>
      unite("sto", c(sto, accounting_entry), sep = ".") |>
      dplyr::filter(sto %in% c(
        "D63.D", "D631.D", "D632.D", "P13.C"
      )) |>
      tidyr::pivot_wider(
        names_from = ref_sector,
        values_from = obs_value
      )

  if(ncol(s1ss12) == 5){
    s1ss12 <- s1ss12 |>
      dplyr::select(ref_area, sto, time_period, S1M,  S15) |>
      dplyr::rowwise() |>
      dplyr::mutate(check = round(S1M - S15, rounding)) |>
      dplyr::filter(abs(check) > threshold)
  } else {
    rm(s1ss12)
  }

  ## S1SS13-------------------------------------------------------------------

    s1ss13 <- data |>
      dplyr::filter(ref_sector %in% c("S1M",  "S14", "S15")) |>
      unite("sto", c(sto, accounting_entry), sep = ".") |>
      dplyr::filter(sto %in% c(
        "P2.D", "P3.D", "P31.D", "P5.D", "P51G.D", "P51G_N111G.D","P51G_N112G.D",
        "P51G_N1121G.D", "P51G_N1122G.D", "P52.D", "P53.D", "D1.D", "D11.D", "D12.D",
        "D2.D", "D29.D", "D4.D", "D4N.D","D41.D", "D43.D", "D44.D", "D441.D","D442.D", "D443.D",
        "D45.D", "D41G.D", "D5.D", "D51.D", "D59.D", "D6.D", "D62.D", "D7.D", "D7N.D", "D71.D",
        "D75.D", "D8.D", "D9.D", "D9N.D","D91.D", "D99.D", "P51C.D", "NP.D", "P1.C", "P11.C",
        "P12.C", "D3.C", "D39.C", "D4.C", "D41.C", "D4N.C", "D42.C", "D421.C", "D422.C", "D43.C",
        "D44.C", "D441.C", "D442.C", "D443.C", "D45.C", "D41G.C", "D6.C", "D61.C", "D611.C",
        "D612.C", "D613.C", "D614.C", "D61SC.C", "D7.C", "D7N.C","D72.C", "D75.C", "D9.C", "D9N.C", "D92.C",
        "D99.C", "P51C.C", "B2A3G.B", "B2G.B", "B4G.B", "B5G.B", "B6G.B", "B7G.B", "B8G.B",
        "B101.B", "B9.B", "B9X9F._Z", "B1G.B", "B1N.B", "EMP.PS", "EMP.HW"
      )) |>
      tidyr::pivot_wider(
        names_from = ref_sector,
        values_from = obs_value
      )

  if(ncol(s1ss13) == 6){
    s1ss13 <- s1ss13 |>
      dplyr::select(ref_area, sto, time_period, S14, S15,  S1M) |>
      dplyr::rowwise() |>
      dplyr::mutate(`S14 + S15` = sum(c(S14, S15), na.rm = TRUE)) |>
      dplyr::mutate(check = round(S1M - `S14 + S15`, rounding)) |>
      dplyr::filter(abs(check) > threshold)|>
      dplyr::filter(if_all(c(S14,S15), ~ !is.na(.x )))
  } else {
    rm(s1ss13)
  }

  # S1SS14 to 19 are for voluntary subsectors
  ## S1SS20-------------------------------------------------------------------

    s1ss20 <- data |>
      unite("sto", c(ref_sector,sto, accounting_entry), sep = ".") |>
      dplyr::filter(sto %in% c(
        "S1.B1GQ.B", "S1N.B1G.B","S11.B1G.B","S12.B1G.B","S13.B1G.B","S1M.B1G.B"
      )) |>
      tidyr::pivot_wider(
        names_from = sto,
        values_from = obs_value
      )

  if(ncol(s1ss20) == 8){
    s1ss20 <- s1ss20 |>
      dplyr::select(ref_area,time_period,S1N.B1G.B,S11.B1G.B,S12.B1G.B,S13.B1G.B,S1M.B1G.B, S1.B1GQ.B ) |>
      dplyr::rowwise() |>
      dplyr::mutate(`sum_B1G` = sum(c(S1N.B1G.B,S11.B1G.B,S12.B1G.B,S13.B1G.B,S1M.B1G.B), na.rm = TRUE)) |>
      dplyr::mutate(check = round(S1.B1GQ.B - `sum_B1G`, rounding)) |>
      dplyr::filter(abs(check) > threshold) |>
      dplyr::filter(if_all(c(S11.B1G.B,S12.B1G.B), ~ !is.na(.x )))
  } else {
    rm(s1ss20)
  }
  # SubItems vs Total-----------------------------------------------------------------------------

  ## SIT01----------------------------------------------------------------------------------------

  sit01 <- data |>
    dplyr::filter(
      ref_sector %in% c("S1", "S13"),
      sto %in% c("P3", "P31", "P32"),
      accounting_entry == "D"
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(
      names_from = sto,
      values_from = obs_value
    )

  if(ncol(sit01) == 6){
    sit01 <- sit01 |>
    dplyr::select(ref_area, ref_sector, time_period, P31.D, P32.D, P3.D) |>
    dplyr::rowwise() |>
    dplyr::mutate(`P31.D + P32.D` = sum(c(P31.D,P32.D), na.rm = TRUE)) |>
    dplyr::mutate(check = round(P3.D - `P31.D + P32.D`, rounding)) |>
    dplyr::filter(abs(check) > threshold)
    } else {
      rm(sit01)
    }

  ## SIT02----------------------------------------------------------------------------------------
    sit02 <- data |>
      dplyr::filter(
        ref_sector %in% c("S1M", "S14", "S15"),
        sto %in% c("P3", "P31"),
        accounting_entry == "D"
      ) |>
      unite("sto", c(sto, accounting_entry), sep = ".") |>
      tidyr::pivot_wider(
        names_from = sto,
        values_from = obs_value
      )

  if(ncol(sit02) == 5){
    sit02 <- sit02 |>
      dplyr::select(ref_area, ref_sector, time_period, P3.D, P31.D) |>
      dplyr::rowwise() |>
      dplyr::mutate(check = round(P3.D - P31.D, rounding)) |>
      dplyr::filter(abs(check) > threshold)
  } else {
    rm(sit02)
  }


  ## SIT03----------------------------------------------------------------------------------------

  sit03 <- data |>
    dplyr::filter(
      sto %in% c("P5", "P51G", "P5M"),
      accounting_entry == "D"
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(
      names_from = sto,
      values_from = obs_value
    )

  if(ncol(sit03) == 6){
    sit03 <- sit03 |>
    dplyr::select(ref_area, ref_sector, time_period, P51G.D, P5M.D, P5.D) |>
    dplyr::rowwise() |>
    dplyr::mutate(`P51G.D + P5M.D` = sum(P51G.D,P5M.D, na.rm = TRUE),
           check = round(P5.D - `P51G.D + P5M.D`, rounding)) |>
    dplyr::filter(abs(check) > threshold)|>
      dplyr::filter(if_all(c(P51G.D,P5M.D), ~ !is.na(.x )))
  } else {
    rm(sit03)
  }
  ## SIT06----------------------------------------------------------------------------------------
  sit06 <- data |>
    dplyr::filter(
      sto %in% c("D2", "D21", "D29")
    ) |>
    tidyr::pivot_wider(
      names_from = sto,
      values_from = obs_value
    )

  if(ncol(sit06) == 7){
    sit06 <- sit06 |>
    dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D21, D29, D2) |>
    dplyr::rowwise() |>
    dplyr::mutate(`D21 + D29` = sum(D21,D29, na.rm = TRUE),
           check = round(D2 - `D21 + D29`, rounding)) |>
    dplyr::filter(abs(check) > threshold)

    } else {
    rm(sit06)
  }
  ## SIT07----------------------------------------------------------------------------------------

  sit07 <- data |>
    dplyr::filter(
      sto %in% c("D2", "D21"),
      ref_sector =="S1N",
      accounting_entry == "D"
    ) |>
    tidyr::pivot_wider(
      names_from = sto,
      values_from = obs_value
    )

  if(ncol(sit07) == 6){
    sit07 <- sit07 |>
    dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D2, D21) |>
    dplyr::rowwise() |>
    dplyr::mutate(check = round(D2 - D21, rounding)) |>
    dplyr::filter(abs(check) > threshold)
} else {
  rm(sit07)
}
  ## SIT08----------------------------------------------------------------------------------------

  sit08 <- data |>
    dplyr::filter(
      sto %in% c("D2", "D29"),
      !ref_sector %in% c("S1","S1N", "S2"),
      accounting_entry == "D"
    ) |>
    tidyr::pivot_wider(
      names_from = sto,
      values_from = obs_value
    )
  if(ncol(sit08) == 6){
    sit08 <- sit08 |>

  dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D2, D29) |>
    dplyr::rowwise() |>
    dplyr::mutate(check = round(D2 - D29, rounding)) |>
    dplyr::filter(abs(check) > threshold)
  } else {
    rm(sit08)
  }
  ## SIT09----------------------------------------------------------------------------------------

  sit09 <- data |>
    dplyr::filter(
      sto %in% c("D3", "D31", "D39"),
      ref_sector %in% c("S1","S2", "S13"),
    ) |>
    tidyr::pivot_wider(
      names_from = sto,
      values_from = obs_value
    )

  if(ncol(sit09) == 7){
    sit09 <- sit09 |>
    dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D31, D39, D3) |>
    dplyr::rowwise() |>
    dplyr::mutate(`D31 + D39` = sum(c(D31,D39), na.rm = TRUE),
           check = round(D3 - `D31 + D39`, rounding)) |>
    dplyr::filter(abs(check) > threshold)
  } else {
    rm(sit09)
  }
  ## SIT14----------------------------------------------------------------------------------------

  sit14 <- data |>
    dplyr::filter(
      sto %in% c("D4", "D41", "D4N")
    ) |>
    tidyr::pivot_wider(
      names_from = sto,
      values_from = obs_value
    )

  if(ncol(sit14) == 7){
    sit14 <- sit14 |>
    dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D41, D4N, D4) |>
    dplyr::rowwise() |>
    dplyr::mutate(`D41 + D4N` = sum(c(D41, D4N), na.rm = TRUE),
           check = round(D4 - `D41 + D4N`, rounding)) |>
    dplyr::filter(abs(check) > threshold)

  } else {
    rm(sit14)
  }

  ## SIT18----------------------------------------------------------------------------------------

  sit18 <- data |>
    dplyr::filter(
      sto %in% c("D6", "D61", "D62", "D63"),
      ref_sector !="S2"
    ) |>
    tidyr::pivot_wider(
      names_from = sto,
      values_from = obs_value
    )

  if(ncol(sit18) == 8){
    sit18 <- sit18 |>
  dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D61, D62, D63, D6) |>
    dplyr::rowwise() |>
    dplyr::mutate(`D61 + D62 + D63` = sum(c(D61, D62, D63), na.rm = TRUE),
           check = round(D6 - `D61 + D62 + D63`, rounding)) |>
    dplyr::filter(abs(check) > threshold)
  } else {
    rm(sit18)
  }

  ## SIT19----------------------------------------------------------------------------------------

  sit19 <- data |>
    dplyr::filter(
      sto %in% c("D6", "D62"),
      ref_sector %in% c("S11", "S12"),
      accounting_entry == "D"
    ) |>
    tidyr::pivot_wider(
      names_from = sto,
      values_from = obs_value
    )

  if(ncol(sit19) == 6){
    sit19 <- sit19 |>
    dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D6, D62) |>
    dplyr::rowwise() |>
    dplyr::mutate(check = round(D6 - D62, rounding)) |>
    dplyr::filter(abs(check) > threshold)
  } else {
    rm(sit19)
  }
  ## SIT20----------------------------------------------------------------------------------------

  sit20 <- data |>
    dplyr::filter(
      sto %in% c("D6", "D62", "D63"),
      ref_sector %in% c("S13"),
      accounting_entry == "D"
    ) |>
    tidyr::pivot_wider(
      names_from = sto,
      values_from = obs_value
    )

  if(ncol(sit20) == 7){
    sit20 <- sit20 |>
  dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D62, D63, D6) |>
    dplyr::rowwise() |>
    dplyr::mutate(`D62 + D63` = sum(c(D62, D63), na.rm = TRUE),
           check = round(D6 - `D62 + D63`, rounding)) |>
    dplyr::filter(abs(check) > threshold)

  } else {
    rm(sit20)
  }
  ## SIT21----------------------------------------------------------------------------------------

  sit21 <- data |>
    dplyr::filter(
      sto %in% c("D6", "D61", "D62"),
      ref_sector %in% c("S2")
    ) |>
    tidyr::pivot_wider(
      names_from = sto,
      values_from = obs_value
    )

  if(ncol(sit21) == 7){
    sit21 <- sit21 |>
    dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D61, D62, D6) |>
    dplyr::rowwise() |>
    dplyr::mutate(`D61 + D62` = sum(c(D61, D62), na.rm = TRUE),
           check = round(D6 - `D61 + D62`, rounding)) |>
    dplyr::filter(abs(check) > threshold)
  } else {
    rm(sit21)
  }

  ## SIT23----------------------------------------------------------------------------------------

  sit23 <- data |>
    dplyr::filter(
      sto %in% c("D63", "D631", "D632"),
    ) |>
    tidyr::pivot_wider(
      names_from = sto,
      values_from = obs_value
    )

  if(ncol(sit23) == 7){
    sit23 <- sit23 |>
    dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D631, D632, D63) |>
    dplyr::rowwise() |>
    dplyr::mutate(`D631 + D632` = sum(c(D631, D632), na.rm = TRUE),
           check = round(D63 - `D631 + D632`, rounding)) |>
    dplyr::filter(abs(check) > threshold) |>
    dplyr::filter(if_all(c(D631,D632), ~ !is.na(.x )))
  } else {
    rm(sit23)
  }

  ## SIT28----------------------------------------------------------------------------------------

  ## SIT28----------------------------------------------------------------------------------------

    sit28 <- data |>
      dplyr::filter(
        sto %in% c("D74", "D74_4Y"),
        ref_sector %in% c("S1", "S13"),
        accounting_entry == "D"
      ) |>
      tidyr::pivot_wider(
        names_from = sto,
        values_from = obs_value
      )


  if(ncol(sit28) == 6){
    sit28 <- sit28 |>
      dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D74, D74_4Y) |>
      dplyr::filter(D74 < D74_4Y)
  } else {
    rm(sit28)
  }


  ## SIT31----------------------------------------------------------------------------------------

  sit31 <- data |>
    dplyr::filter(
      sto %in% c("P6", "P61",  "P62"),
      ref_sector %in% c("S2"),
      accounting_entry == "D"
    ) |>
    tidyr::pivot_wider(
      names_from = sto,
      values_from = obs_value
    )

  if(ncol(sit31) == 7){
    sit31 <- sit31 |>
    dplyr::select(ref_area, ref_sector, accounting_entry,time_period, P61,  P62, P6) |>
    dplyr::rowwise() |>
    dplyr::mutate(`P61 + P62` = sum(c(P61,P62), na.rm = TRUE),
           check = round(P6 - `P61 + P62`, rounding)) |>
    dplyr::filter(abs(check) > threshold)
  } else {
    rm(sit31)
  }
  ## SIT32----------------------------------------------------------------------------------------

  sit32 <- data |>
    dplyr::filter(
      sto %in% c("P62", "P62F"),
      ref_sector %in% c("S2"),
      accounting_entry == "D"
    ) |>
    tidyr::pivot_wider(
      names_from = sto,
      values_from = obs_value
    )

  if(ncol(sit32) == 6){
    sit32 <- sit32 |>
    dplyr::select(ref_area, ref_sector, accounting_entry,time_period, P62, P62F) |>
    dplyr::filter(P62F > P62)
  } else {
    rm(sit32)
  }

  ## SIT34----------------------------------------------------------------------------------------

  sit34 <- data |>
    dplyr::filter(
      sto %in% c("D3", "D31"),
      ref_sector %in% c("S1N"),
      accounting_entry == "C"
    ) |>
    tidyr::pivot_wider(
      names_from = sto,
      values_from = obs_value
    )

  if(ncol(sit34) == 6){
    sit34 <- sit34 |>
    dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D3, D31) |>
    dplyr::rowwise() |>
    dplyr::mutate(`check` = sum(c(D3, -D31), na.rm = TRUE)) |>
    dplyr::filter(abs(check) > threshold)
  } else {
    rm(sit34)
  }

  ## SIT35----------------------------------------------------------------------------------------

  sit35 <- data |>
    dplyr::filter(
      sto %in% c("D3", "D39"),
      !ref_sector %in% c("S1", "S2", "S1N"),
      accounting_entry == "C"
    ) |>
    tidyr::pivot_wider(
      names_from = sto,
      values_from = obs_value
    )

  if(ncol(sit35) == 6){
    sit35 <- sit35 |>
    dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D3, D39) |>
    dplyr::rowwise() |>
    dplyr::mutate(`check` = sum(c(D3, -D39), na.rm = TRUE)) |>
    dplyr::filter(abs(check) > threshold)
  } else {
    rm(sit35)
  }
  ## SIT36----------------------------------------------------------------------------------------

  sit36 <- data |>
    dplyr::filter(
      sto %in% c("D6", "D61"),
      !ref_sector %in% c("S1", "S2", "S14", "S1M"),
      accounting_entry == "C"
    ) |>
    tidyr::pivot_wider(
      names_from = sto,
      values_from = obs_value
    )

  if(ncol(sit36) == 6){
    sit36 <- sit36 |>
    dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D6, D61) |>
    dplyr::rowwise() |>
    dplyr::mutate(`check` = sum(c(D6, -D61), na.rm = TRUE)) |>
    dplyr::filter(abs(check) > threshold)
  } else {
    rm(sit36)
  }

  ## SIT41----------------------------------------------------------------------------------------

  sit41 <- data |>
    dplyr::filter(
      sto %in% c("P7", "P71", "P72"),
      ref_sector %in% c("S2"),
      accounting_entry == "C"
    ) |>
    tidyr::pivot_wider(
      names_from = sto,
      values_from = obs_value
    )

  if(ncol(sit41) == 6){
    sit41 <- sit41 |>
    dplyr::select(ref_area, ref_sector, accounting_entry,time_period, P7, P71, P72) |>
    dplyr::rowwise() |>
    dplyr::mutate(`P71 + P72` = sum(c(P71, P72), na.rm = TRUE),
           check = round(P7 - `P71 + P72`, rounding)) |>
    dplyr::filter(abs(check) > threshold)
  } else {
    rm(sit41)
  }
  ## SIT42----------------------------------------------------------------------------------------

  sit42 <- data |>
    dplyr::filter(
      sto %in% c("P72", "P72F"),
      ref_sector %in% c("S2"),
      accounting_entry == "C"
    ) |>
    tidyr::pivot_wider(
      names_from = sto,
      values_from = obs_value
    )

  if(ncol(sit42) == 6){
    sit42 <- sit42 |>
    dplyr::select(ref_area, ref_sector, accounting_entry,time_period, P72, P72F) |>
    dplyr::filter(P72F > P72)
  } else {
    rm(sit42)
  }
  ## SIT43----------------------------------------------------------------------------------------

    sit43 <- data |>
      dplyr::filter(
        sto %in% c("D43", "D43_I9", "D43_J9")
      ) |>
      tidyr::pivot_wider(
        names_from = sto,
        values_from = obs_value
      )

  if(ncol(sit43) == 7){
    sit43 <- sit43 |>
      dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D43, D43_I9, D43_J9) |>
      dplyr::rowwise() |>
      dplyr::mutate(`D43_I9 + D43_J9` = sum(c(D43_I9, D43_J9), na.rm = TRUE),
             check = round(D43 - `D43_I9 + D43_J9`, rounding)) |>
      dplyr::filter(abs(check) > threshold) |>
      dplyr::filter(if_all(c(D43_I9,D43_J9), ~ !is.na(.x )))

  } else {
    rm(sit43)
  }
  ## SIT44----------------------------------------------------------------------------------------

    sit44 <- data |>
      dplyr::filter(
        sto %in% c("D43", "D43_B6", "D43_D6")
      ) |>
      tidyr::pivot_wider(
        names_from = sto,
        values_from = obs_value
      )

  if(ncol(sit44) == 7){
    sit44 <- sit44 |>
    dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D43, D43_B6, D43_D6) |>
      dplyr::rowwise() |>
      dplyr::mutate(`D43_B6 + D43_D6` = sum(c(D43_B6, D43_D6), na.rm = TRUE),
             check = round(D43 - `D43_B6 + D43_D6`, rounding)) |>
      dplyr::filter(abs(check) > threshold) |>
      dplyr::filter(if_all(c(D43_B6,D43_D6), ~ !is.na(.x )))

  } else {
    rm(sit44)
  }
  ## SIT45----------------------------------------------------------------------------------------
  sit45 <- data |>
    dplyr::filter(
      sto %in% c("D4N", "D42", "D43", "D44", "D45")
    ) |>
    tidyr::pivot_wider(
      names_from = sto,
      values_from = obs_value
    )

  if(ncol(sit45) == 9){
    sit45 <- sit45 |>
    dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D42, D43, D44, D45, D4N) |>
    dplyr::rowwise() |>
    dplyr::mutate(`D4N_calc` = sum(c(D42, D43, D44, D45), na.rm = TRUE),
           check = round(D4N - `D4N_calc`, rounding)) |>
    dplyr::filter(abs(check) > threshold)

  } else {
    rm(sit45)
  }
  ## SIT46----------------------------------------------------------------------------------------
  sit46 <- data |>
    dplyr::filter(
      sto %in% c("D4N", "D42",  "D44", "D45"),
      ref_sector == "S13",
      accounting_entry == "D"
    ) |>
    tidyr::pivot_wider(
      names_from = sto,
      values_from = obs_value
    )

  if(ncol(sit46) == 8){
  sit46 <- sit46 |>
    dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D42,  D44, D45, D4N) |>
    dplyr::rowwise() |>
    dplyr::mutate(`D4N_calc` = sum(c(D42, D44, D45), na.rm = TRUE),
           check = round(D4N - `D4N_calc`, rounding)) |>
    dplyr::filter(abs(check) > threshold)

    } else {
    rm(sit46)
  }

  ## SIT47----------------------------------------------------------------------------------------
  sit47 <- data |>
    dplyr::filter(
      sto %in% c("D4N", "D45"),
      ref_sector %in% c("S1M", "S14", "S15"),
      accounting_entry == "D"
    ) |>
    tidyr::pivot_wider(
      names_from = sto,
      values_from = obs_value
    )

  if(ncol(sit47) == 6){
    sit47 <- sit47 |>
    dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D45, D4N) |>
    dplyr::mutate(check = round(D4N - D45, rounding)) |>
    dplyr::filter(abs(check) > threshold)
  } else {
    rm(sit47)
  }

  ## SIT49----------------------------------------------------------------------------------------
  sit49 <- data |>
    dplyr::filter(
      sto %in% c("D7N", "D71", "D72", "D7")
    ) |>
    tidyr::pivot_wider(
      names_from = sto,
      values_from = obs_value
    )

  if(ncol(sit49) == 8){
    sit49 <- sit49 |>
    dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D71, D72, D7N,D7) |>
    dplyr::rowwise() |>
    dplyr::mutate(`D7_calc` = sum(c(D71, D72, D7N), na.rm = TRUE),
           check = round(D7 - `D7_calc`, rounding)) |>
    dplyr::filter(abs(check) > threshold)
  } else {
    rm(sit49)
  }
  ## SIT50----------------------------------------------------------------------------------------

    sit50 <- data |>
      dplyr::filter(
        sto %in% c("D7N", "D71", "D7"),
        accounting_entry == "D",
        ref_sector %in% c("S11", "S1M", "S14", "S15")
      ) |>
      tidyr::pivot_wider(
        names_from = sto,
        values_from = obs_value
      )

  if(ncol(sit50) == 7){
    sit50 <- sit50 |>
      dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D71, D7N,D7) |>
      dplyr::rowwise() |>
      dplyr::mutate(`D71 +D7N` = sum(c(D71,  D7N), na.rm = TRUE),
             check = round(D7 - `D71 +D7N`, rounding)) |>
      dplyr::filter(abs(check) > threshold)
} else {
  rm(sit50)
}

  ## SIT51----------------------------------------------------------------------------------------

    sit51 <- data |>
      dplyr::filter(
        sto %in% c("D7N", "D74", "D75", "D76"),
        accounting_entry == "D",
        ref_sector %in% c("S1", "S13", "S2")
      ) |>
      tidyr::pivot_wider(
        names_from = sto,
        values_from = obs_value
      )

  if(ncol(sit51) == 8){
    sit51 <- sit51 |>
      dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D74,  D75, D76, D7N) |>
      dplyr::rowwise() |>
      dplyr::mutate(`D7N_cal` = sum(c(D74,  D75, D76), na.rm = TRUE),
             check = round(D7N - `D7N_cal`, rounding)) |>
      dplyr::filter(abs(check) > threshold) |>
      dplyr::filter(if_all(c(D74,D75,D76), ~ !is.na(.x )))

  } else {
    rm(sit51)
  }

  ## SIT52----------------------------------------------------------------------------------------
    sit52 <- data |>
      dplyr::filter(
        sto %in% c("D7N", "D75"),
        ref_sector %in% c("S11", "S12", "S1M", "S14", "S15")
      ) |>
      tidyr::pivot_wider(
        names_from = sto,
        values_from = obs_value
      )

  if(ncol(sit52) == 6){
    sit52 <- sit52 |>
      dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D75,  D7N) |>
      dplyr::rowwise() |>
      dplyr::mutate(check = round(D7N - D75, rounding)) |>
      dplyr::filter(abs(check) > threshold)

  } else {
    rm(sit52)
  }
  ## SIT53----------------------------------------------------------------------------------------

  sit53 <- data |>
    dplyr::filter(
      sto %in% c("D7N", "D74", "D75"),
      accounting_entry == "C",
      ref_sector %in% c("S1", "S13", "S2")
    ) |>
    tidyr::pivot_wider(
      names_from = sto,
      values_from = obs_value
    )

  if(ncol(sit53) == 7){
    sit53 <- sit53 |>
    dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D74,  D75, D7N) |>
    dplyr::rowwise() |>
    dplyr::mutate(`D7N_cal` = sum(c(D74,  D75), na.rm = TRUE),
           check = round(D7N - `D7N_cal`, rounding)) |>
    dplyr::filter(abs(check) > threshold) |>
      dplyr::filter(if_all(c(D74,D75), ~ !is.na(.x )))
  } else {
    rm(sit53)
  }
  ## SIT54----------------------------------------------------------------------------------------

    sit54 <- data |>
      dplyr::filter(
        sto %in% c("D9", "D91", "D9N")
      ) |>
      tidyr::pivot_wider(
        names_from = sto,
        values_from = obs_value
      )

  if(ncol(sit54) == 7){
    sit54 <- sit54 |>
      dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D91,  D9N, D9) |>
      dplyr::rowwise() |>
      dplyr::mutate(`D91 +D9N` = sum(c(D91,  D9N), na.rm = TRUE),
             check = round(D9 - `D91 +D9N`, rounding)) |>
      dplyr::filter(abs(check) > threshold)
  } else {
    rm(sit54)
  }
  ## SIT55----------------------------------------------------------------------------------------

    sit55 <- data |>
      dplyr::filter(
        sto %in% c("D9", "D9N"),
        !ref_sector %in% c("S1", "S2", "S13"),
        accounting_entry == "C"
      ) |>
      tidyr::pivot_wider(
        names_from = sto,
        values_from = obs_value
      )

  if(ncol(sit55) == 6){
    sit55 <- sit55 |>
      dplyr::select(ref_area, ref_sector, accounting_entry,time_period,  D9N, D9) |>
      dplyr::mutate(check = round(D9 - D9N, rounding)) |>
      dplyr::filter(abs(check) > threshold)
  } else {
    rm(sit55)
  }

  ## SIT56----------------------------------------------------------------------------------------

  sit56 <- data |>
    dplyr::filter(
      sto %in% c("D9N", "D92", "D99")
    ) |>
    tidyr::pivot_wider(
      names_from = sto,
      values_from = obs_value
    )

  if(ncol(sit56) == 7){
    sit56 <- sit56 |>
    dplyr::select(ref_area, ref_sector, accounting_entry,time_period,  D92, D99, D9N) |>
    dplyr::rowwise() |>
    dplyr::mutate(`D92 +D99` = sum(c(D92,  D99), na.rm = TRUE),
           check = round(D9N - `D92 +D99`, rounding)) |>
    dplyr::filter(abs(check) > threshold)|>
    dplyr::filter(if_all(c(D92,D99), ~ !is.na(.x )))
  } else {
    rm(sit56)
  }

  ## SIT57----------------------------------------------------------------------------------------

    sit57 <- data |>
      dplyr::filter(
        sto %in% c("D9N", "D99"),
        accounting_entry == "D",
        !ref_sector %in% c("S13", "S2", "S1")
      ) |>
      tidyr::pivot_wider(
        names_from = sto,
        values_from = obs_value
      )

  if(ncol(sit57) == 6){
    sit57 <- sit57 |>
      dplyr::select(ref_area, ref_sector, accounting_entry,time_period,  D9N, D99) |>
      dplyr::mutate(check = round(D9N - D99, rounding)) |>
      dplyr::filter(abs(check) > threshold)
  } else {
    rm(sit57)
  }
  ## SIT58----------------------------------------------------------------------------------------

    sit58 <- data |>
      dplyr::filter(
        sto %in% c("D7", "D72", "D7N"),
        accounting_entry == "C",
        ref_sector %in% c("S11", "S1M", "S14", "S15")
      ) |>
      tidyr::pivot_wider(
        names_from = sto,
        values_from = obs_value
      )

  if(ncol(sit58) == 7){
    sit58 <- sit58 |>
      dplyr::select(ref_area, ref_sector, accounting_entry,time_period,  D72, D7N, D7) |>
      dplyr::rowwise() |>
      dplyr::mutate(`D7N +D72` = sum(c(D7N,  D72), na.rm = TRUE),
             check = round(D7 - `D7N +D72`, rounding)) |>
      dplyr::filter(abs(check) > threshold)

  } else {
    rm(sit58)
  }

  # Balancing items-------------------------------------------------------------------------------

  ## BI01-----------------------------------------------------------------------------------------

    BI01 <- data |>
      dplyr::filter(
        ref_sector == "S1",
        sto %in% c("P1", "P2", "B1GQ", "D21X31")
      ) |>
      unite("sto", c(sto, accounting_entry), sep = ".") |>
      tidyr::pivot_wider(names_from = sto,
                  values_from = obs_value)

  if(ncol(BI01) == 7){
    BI01 <- BI01 |>
      dplyr::select(ref_area, ref_sector, time_period, P1.C, P2.D, D21X31.C, B1GQ.B ) |>
      dplyr::rowwise() |>
      dplyr::mutate(
        `P1.C - P2.D + D21X31.C` = P1.C - P2.D + D21X31.C,
        check = round(B1GQ.B - `P1.C - P2.D + D21X31.C`, rounding)
      ) |>
      dplyr::filter(abs(check) > threshold)

  } else {
    rm(BI01)
  }
  ## BI02-----------------------------------------------------------------------------------------

  BI02 <- data |>
    dplyr::filter(
      ref_sector %in% c("S1", "S1N"),
      sto %in% c("D21", "D31", "D21X31")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                values_from = obs_value)

  if(ncol(BI02) == 8){
    BI02 <- BI02 |>
    dplyr::select(ref_area, ref_sector, time_period, D21X31.C, D21.D, D31.C) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `D21.D - D31.C` = D21.D - D31.C,
      check = round(D21X31.C - `D21.D - D31.C`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)
  } else {
    rm(BI02)
  }

  ## BI03-----------------------------------------------------------------------------------------

  BI03 <- data |>
    dplyr::filter(
      ref_sector == "S1N",
      sto %in% c("B1G", "D21X31"),
      accounting_entry != "D"
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                values_from = obs_value)

  if(ncol(BI03) == 5){
    BI03 <- BI03 |>
    dplyr::select(ref_area, ref_sector, time_period, B1G.B, D21X31.C) |>
    dplyr::rowwise() |>
    dplyr::mutate(check = round(B1G.B - D21X31.C, rounding)) |>
    dplyr::filter(abs(check) > threshold)
  } else {
    rm(BI03)
  }

  ## BI04-----------------------------------------------------------------------------------------
  BI04 <- data |>
    dplyr::filter(
      sto %in% c("B1G", "P1", "P2")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                values_from = obs_value)

  if(ncol(BI04) == 6){
    BI04 <- BI04 |>
    dplyr::select(ref_area, ref_sector, time_period, B1G.B, P1.C, P2.D) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `P1.C - P2.D` = P1.C - P2.D,
      check = round(B1G.B - `P1.C - P2.D`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)
  } else {
    rm(BI04)
  }

  ## BI05-----------------------------------------------------------------------------------------

  BI05 <- data |>
    dplyr::filter(
      ref_sector == "S1",
      sto %in% c("B1GQ", "B1NQ", "P51C"),
      accounting_entry != "C"
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                values_from = obs_value)

  if(ncol(BI05) == 6){
    BI05 <- BI05 |>
    dplyr::select(ref_area, ref_sector, time_period, B1NQ.B, B1GQ.B, P51C.D) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `B1GQ.B - P51C.D` = B1GQ.B - P51C.D,
      check = round(B1NQ.B - `B1GQ.B - P51C.D`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)
  } else {
    rm(BI05)
  }


  ## BI07-----------------------------------------------------------------------------------------
  BI07 <- data |>
    dplyr::filter(
      ref_sector != "S1N",
      sto %in% c("B1G", "B1N", "P51C"),
      accounting_entry != "D"
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                values_from = obs_value)

  if(ncol(BI07) == 6){
    BI07 <- BI07 |>
    dplyr::select(ref_area, ref_sector, time_period, B1N.B, P51C.C,B1G.B) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `B1N.B + P51C.C` = B1N.B + P51C.C,
      check = round(B1G.B - `B1N.B + P51C.C`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)
  } else {
    rm(BI07)
  }

  ## BI08-----------------------------------------------------------------------------------------
  BI08 <- data |>
    dplyr::filter(
      ref_sector %in% c("S1","S2"),
      sto %in% c("B1GQ", "P3", "P5", "P6", "P7")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    dplyr::select(-ref_sector) |>
    tidyr::pivot_wider(names_from = sto,
                values_from = obs_value)

  if(ncol(BI08) == 7){
    BI08 <- BI08 |>
    dplyr::select(ref_area,  time_period, P3.D, P5.D,P6.D,P7.C,B1GQ.B) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `B1GQ_cal` = P3.D + P5.D + P6.D - P7.C,
      check = round(B1GQ.B - `B1GQ_cal`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)
  } else {
    rm(BI08)
  }

  ## BI09-----------------------------------------------------------------------------------------
  BI09 <- data |>
    dplyr::filter(
      ref_sector %in% c("S1"),
      sto %in% c("B1GQ", "B2A3G", "D1", "D2", "D3")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                values_from = obs_value)

  if(ncol(BI09) == 11){
    BI09 <- BI09 |>
    dplyr::select(ref_area, ref_sector, time_period, B2A3G.B, D1.D,D3.C,D2.D,,B1GQ.B) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `B1GQ_cal` = B2A3G.B + D1.D + D2.D - D3.C,
      check = round(B1GQ.B - `B1GQ_cal`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)
  } else {
    rm(BI09)
  }

  ## BI10-----------------------------------------------------------------------------------------
  BI10 <- data |>
    dplyr::filter(
      ref_sector != "S1",
      sto %in% c("B1G", "B2A3G", "D1", "D2", "D3")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                values_from = obs_value)

  if(ncol(BI10) == 11){
    BI10 <- BI10 |>
    dplyr::select(ref_area, ref_sector, time_period, B2A3G.B, D1.D,D3.C,D2.D,,B1G.B) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `B1G_cal` = B2A3G.B + D1.D + D2.D - D3.C,
      check = round(B1G.B - `B1G_cal`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)

  } else {
    rm(BI10)
  }
  ## BI13-----------------------------------------------------------------------------------------

    BI13 <- data |>
      dplyr::filter(
        ref_sector %in% c("S11","S12"),
        sto %in% c("B4G", "B2A3G", "D4","D41", "D44", "D45"),
      ) |>
      unite("sto", c(sto, accounting_entry), sep = ".") |>
      tidyr::pivot_wider(names_from = sto,
                  values_from = obs_value)

  if(ncol(BI13) == 13){
    BI13 <- BI13 |>
      dplyr::select(ref_area, ref_sector, time_period, B2A3G.B, D4.C,D41.D,D44.D,D45.D, B4G.B) |>
      dplyr::rowwise() |>
      dplyr::mutate(
        `B4G_cal` = sum(c(B2A3G.B,D4.C,-D41.D,-D44.D,-D45.D),na.rm = TRUE),
        check = round(B4G.B - `B4G_cal`, rounding)
      ) |>
      dplyr::filter(abs(check) > threshold)
  } else {
    rm(BI13)
  }
  ## BI14-----------------------------------------------------------------------------------------

    BI14 <- data |>
      dplyr::filter(
        ref_sector %in% c("S1"),
        sto %in% c("B5G", "B2A3G", "D1","D2", "D3", "D4"),
      ) |>
      unite("sto", c(sto, accounting_entry), sep = ".") |>
      tidyr::pivot_wider(names_from = sto,
                  values_from = obs_value)

  if(ncol(BI14) == 13){
    BI14 <- BI14 |>
      dplyr::select(ref_area, ref_sector, time_period, B2A3G.B, D1.C,D2.C,D3.D,D4.C, D4.D,B5G.B) |>
      dplyr::rowwise() |>
      dplyr::mutate(
        `B5G_cal` = sum(c(B2A3G.B,D1.C,D2.C,-D3.D,D4.C,-D4.D),na.rm = TRUE),
        check = round(B5G.B - `B5G_cal`, rounding)
      ) |>
      dplyr::filter(abs(check) > threshold) |>
      dplyr::filter(if_all(c(D4.C,D4.D), ~ !is.na(.x )))
  } else {
    rm(BI14)
  }

  ## BI15-----------------------------------------------------------------------------------------
    BI15 <- data |>
      dplyr::filter(
        !ref_sector %in% c("S1", "S13", "S14", "S1M"),
        sto %in% c("B5G", "B2A3G",  "D4"),
      ) |>
      unite("sto", c(sto, accounting_entry), sep = ".") |>
      tidyr::pivot_wider(names_from = sto,
                  values_from = obs_value)

  if(ncol(BI15) == 7){
    BI15 <- BI15 |>
      dplyr::select(ref_area, ref_sector, time_period, B2A3G.B, D4.C,D4.D, B5G.B) |>
      dplyr::rowwise() |>
      dplyr::mutate(
        `B5G_cal` = sum(c(B2A3G.B,D4.C,-D4.D),na.rm = TRUE),
        check = round(B5G.B - `B5G_cal`, rounding)
      ) |>
      dplyr::filter(abs(check) > threshold)
  } else {
    rm(BI15)
  }
  ## BI16-----------------------------------------------------------------------------------------
  BI16 <- data |>
    dplyr::filter(
      ref_sector %in% c("S13"),
      sto %in% c("B5G", "B2A3G", "D2", "D3" , "D4")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                values_from = obs_value)

  if(ncol(BI16) == 11){
    BI16 <- BI16 |>
    dplyr::select(ref_area, ref_sector, time_period, B2A3G.B, D2.C,D3.D,D4.C,D4.D, B5G.B) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `B5G_cal` = sum(c(B2A3G.B,D2.C,-D3.D,D4.C,-D4.D),na.rm = TRUE),
      check = round(B5G.B - `B5G_cal`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)
  } else {
    rm(BI16)
  }
  ## BI17-----------------------------------------------------------------------------------------

    BI17 <- data |>
      dplyr::filter(
        ref_sector %in% c("S1M", "S14", "S15"),
        sto %in% c("B5G", "B2A3G", "D1",  "D4")
      ) |>
      unite("sto", c(sto, accounting_entry), sep = ".") |>
      tidyr::pivot_wider(names_from = sto,
                  values_from = obs_value)

  if(ncol(BI17) == 9){
    BI17 <- BI17 |>
      dplyr::select(ref_area, ref_sector, time_period, B2A3G.B, D1.C,D4.C,D4.D, B5G.B) |>
      dplyr::rowwise() |>
      dplyr::mutate(
        `B5G_cal` = sum(c(B2A3G.B,D1.C,D4.C,-D4.D),na.rm = TRUE),
        check = round(B5G.B - `B5G_cal`, rounding)
      ) |>
      dplyr::filter(abs(check) > threshold)
  } else {
    rm(BI17)
  }

  ## BI18-----------------------------------------------------------------------------------------

    BI18 <- data |>
      dplyr::filter(
        ref_sector %in% c("S1", "S2"),
        sto %in% c("B5G", "B1GQ", "D1",  "D2", "D3", "D4")
      ) |>
      unite("sto", c(ref_sector,sto, accounting_entry), sep = ".") |>
      tidyr::pivot_wider(names_from = sto,
                  values_from = obs_value)

  if(ncol(BI18) == 18){
    BI18 <- BI18 |>
      dplyr::select(ref_area, time_period, S1.B1GQ.B,S2.D1.D,S2.D1.C,S2.D2.D,S2.D3.C,S2.D4.D,S2.D4.C,S1.B5G.B) |>
      dplyr::rowwise() |>
      dplyr::mutate(
        `B5G_cal` = sum(c(S1.B1GQ.B,-S2.D1.D,S2.D1.C,-S2.D2.D,S2.D3.C,-S2.D4.D,S2.D4.C),na.rm = TRUE),
        check = round(S1.B5G.B - `B5G_cal`, rounding)
      ) |>
      dplyr::filter(abs(check) > threshold)
  } else {
    rm(BI18)
  }

  ## BI19-----------------------------------------------------------------------------------------

    BI19 <- data |>
      dplyr::filter(
        ref_sector %in% c("S1"),
        sto %in% c("B5G", "D5", "D61","D62", "D7", "B6G")
      ) |>
      unite("sto", c(sto, accounting_entry), sep = ".") |>
      tidyr::pivot_wider(names_from = sto,
                  values_from = obs_value)

  if(ncol(BI19) == 13){
    BI19 <- BI19 |>
      dplyr::select(ref_area, ref_sector, time_period, B5G.B,D5.C,D5.D,D61.C,D61.D,D62.C,D62.D,D7.C,D7.D,B6G.B) |>
      dplyr::rowwise() |>
      dplyr::mutate(
        `B6G_cal` = sum(c(B5G.B,D5.C,-D5.D,D61.C,-D61.D,D62.C,-D62.D,D7.C,-D7.D),na.rm = TRUE),
        check = round(B6G.B - `B6G_cal`, rounding)
      ) |>
      dplyr::filter(abs(check) > threshold) |>
      dplyr::filter(if_all(c(D61.D,D62.C), ~ !is.na(.x )))
  } else {
    rm(BI19)
  }
  ## BI20-----------------------------------------------------------------------------------------

    BI20 <- data |>
      dplyr::filter(
        ref_sector %in% c("S11", "S12", "S15"),
        sto %in% c("B5G", "D5", "D61","D62", "D7", "B6G")
      ) |>
      unite("sto", c(sto, accounting_entry), sep = ".") |>
      tidyr::pivot_wider(names_from = sto,
                  values_from = obs_value)

  if(ncol(BI20) == 12){
    BI20 <- BI20 |>
      dplyr::select(ref_area, ref_sector, time_period, B5G.B,D5.D,D61.C,D62.D,D7.C,D7.D,B6G.B) |>
      dplyr::rowwise() |>
      dplyr::mutate(
        `B6G_cal` = sum(c(B5G.B,-D5.D,D61.C,-D62.D,D7.C,-D7.D),na.rm = TRUE),
        check = round(B6G.B - `B6G_cal`, rounding)
      ) |>
      dplyr::filter(abs(check) > threshold)
  } else {
    rm(BI20)
  }

  ## BI21-----------------------------------------------------------------------------------------
  BI21 <- data |>
    dplyr::filter(
      ref_sector %in% c("S13"),
      sto %in% c("B5G", "D5", "D61","D62", "D7", "B6G")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                values_from = obs_value)

  if(ncol(BI21) == 11){
    BI21 <- BI21 |>
    dplyr::select(ref_area, ref_sector, time_period, B5G.B,D5.C,D5.D,D61.C,D62.D,D7.C,D7.D,B6G.B) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `B6G_cal` = sum(c(B5G.B,D5.C,-D5.D,D61.C,-D62.D,D7.C,-D7.D),na.rm = TRUE),
      check = round(B6G.B - `B6G_cal`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)
  } else {
    rm(BI21)
  }

  ## BI22-----------------------------------------------------------------------------------------

    BI22 <- data |>
      dplyr::filter(
        ref_sector %in% c("S14", "S1M"),
        sto %in% c("B5G", "D5", "D61","D62", "D7", "B6G")
      ) |>
      unite("sto", c(sto, accounting_entry), sep = ".") |>
      tidyr::pivot_wider(names_from = sto,
                  values_from = obs_value)

  if(ncol(BI22) == 12){
    BI22 <- BI22 |>
      dplyr::select(ref_area, ref_sector, time_period, B5G.B,D5.D,D5.D,D61.C,D61.D,D62.C,D62.D,D7.C,D7.D,B6G.B) |>
      dplyr::rowwise() |>
      dplyr::mutate(
        `B6G_cal` = sum(c(B5G.B,-D5.D,D61.C,-D61.D,D62.C,-D62.D,D7.C,-D7.D),na.rm = TRUE),
        check = round(B6G.B - `B6G_cal`, rounding)
      ) |>
      dplyr::filter(abs(check) > threshold)
  } else {
    rm(BI22)
  }


  ## BI23-----------------------------------------------------------------------------------------

  BI23 <- data |>
    dplyr::filter(
      ref_sector %in% c("S1", "S2"),
      sto %in% c("B6G", "B5G","D5","D61","D62","D7")
    ) |>
    unite("sto", c(ref_sector,sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                values_from = obs_value)

  if(ncol(BI23) == 20){
    BI23 <- BI23 |>
    dplyr::select(ref_area,  time_period, S1.B5G.B,S2.D5.D,S2.D5.C,S2.D61.D,S2.D61.C,S2.D62.D,S2.D62.C,S2.D7.D,S2.D7.C, S1.B6G.B) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `B6G_cal` = sum(c(S1.B5G.B,-S2.D5.D,S2.D5.C,-S2.D61.D,S2.D61.C,-S2.D62.D,S2.D62.C,-S2.D7.D,S2.D7.C),na.rm = TRUE),
      check = round(S1.B6G.B - `B6G_cal`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)
  } else {
    rm(BI23)
  }

  ## BI24-----------------------------------------------------------------------------------------

    BI24 <- data |>
      dplyr::filter(
        ref_sector %in% c("S1", "S1M", "S14", "S15"),
        sto %in% c("B7G", "B6G", "D63")
      ) |>
      unite("sto", c(sto, accounting_entry), sep = ".") |>
      tidyr::pivot_wider(names_from = sto,
                  values_from = obs_value)

  if(ncol(BI24) == 7){
    BI24 <- BI24 |>
      dplyr::select(ref_area, ref_sector, time_period, B6G.B,D63.C,D63.D,B7G.B) |>
      dplyr::rowwise() |>
      dplyr::mutate(
        `B7G_cal` = sum(c(B6G.B,D63.C,-D63.D),na.rm = TRUE),
        check = round(B7G.B - `B7G_cal`, rounding)
      ) |>
      dplyr::filter(abs(check) > threshold)
  } else {
    rm(BI24)
  }

  ## BI26-----------------------------------------------------------------------------------------
  BI26 <- data |>
    dplyr::filter(
      ref_sector %in% c("S13", "S15"),
      sto %in% c("B7G", "B6G", "D63"),
      accounting_entry != "C"
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                values_from = obs_value)

  if(ncol(BI26) == 6){
    BI26 <- BI26 |>
    dplyr::select(ref_area, ref_sector, time_period, B6G.B,D63.D,B7G.B) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `B7G_cal` = sum(c(B6G.B,-D63.D),na.rm = TRUE),
      check = round(B7G.B - `B7G_cal`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)
  } else {
    rm(BI26)
  }

  ## BI28-----------------------------------------------------------------------------------------
  BI28 <- data |>
    dplyr::filter(
      ref_sector %in% c("S1", "S1M", "S14"),
      sto %in% c("B8G", "B6G", "D8", "P3")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                values_from = obs_value)

  if(ncol(BI28) == 8){
    BI28 <- BI28 |>
    dplyr::select(ref_area, ref_sector, time_period, B6G.B,D8.C,D8.D,P3.D,B8G.B) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `B8G_cal` = sum(c(B6G.B,D8.C,-D8.D,-P3.D),na.rm = TRUE),
      check = round(B8G.B - `B8G_cal`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)
  } else {
    rm(BI28)
  }

  ## BI29-----------------------------------------------------------------------------------------

    BI29 <- data |>
      dplyr::filter(
        ref_sector %in% c("S11", "S12"),
        sto %in% c("B8G", "B6G", "D8")
      ) |>
      unite("sto", c(sto, accounting_entry), sep = ".") |>
      tidyr::pivot_wider(names_from = sto,
                  values_from = obs_value)

  if(ncol(BI29) == 6){
    BI29 <- BI29 |>
      dplyr::select(ref_area, ref_sector, time_period, B6G.B,D8.D,B8G.B) |>
      dplyr::rowwise() |>
      dplyr::mutate(
        `B6G.B - D8.D` = sum(c(B6G.B,-D8.D),na.rm = TRUE),
        check = round(B8G.B - `B6G.B - D8.D`, rounding)
      ) |>
      dplyr::filter(abs(check) > threshold)
  } else {
    rm(BI29)
  }
  ## BI30-----------------------------------------------------------------------------------------
  BI30 <- data |>
    dplyr::filter(
      ref_sector %in% c("S13", "S15"),
      sto %in% c("B8G", "B6G", "D8", "P3"),
      accounting_entry != "C"
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                values_from = obs_value)

  if(ncol(BI30) == 7){
    BI30 <- BI30 |>
    dplyr::select(ref_area, ref_sector, time_period, B6G.B,D8.D,P3.D,B8G.B) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `B6G.B - D8.D - P3.D` = sum(c(B6G.B,-D8.D, -P3.D),na.rm = TRUE),
      check = round(B8G.B - `B6G.B - D8.D - P3.D`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)
  } else {
    rm(BI30)
  }
  ## BI31-----------------------------------------------------------------------------------------

    BI31 <- data |>
      dplyr::filter(
        ref_sector %in% c("S1", "S2"),
        sto %in% c("B8G", "B6G", "D8", "P3")
      ) |>
      unite("sto", c(ref_sector,sto, accounting_entry), sep = ".") |>
      tidyr::pivot_wider(names_from = sto,
                  values_from = obs_value)

  if(ncol(BI31) == 9){
    BI31 <- BI31 |>
      dplyr::select(ref_area, time_period, S1.B6G.B,S2.D8.D,S2.D8.C,S1.P3.D,S1.B8G.B) |>
      dplyr::rowwise() |>
      dplyr::mutate(
        `B8G_calc` = sum(c(S1.B6G.B,-S2.D8.D,S2.D8.C,-S1.P3.D),na.rm = TRUE),
        check = round(S1.B8G.B - `B8G_calc`, rounding)
      ) |>
      dplyr::filter(abs(check) > threshold)
  } else {
    rm(BI31)
  }

  ## BI32-----------------------------------------------------------------------------------------
  BI32 <- data |>
    dplyr::filter(
      !ref_sector %in% c("S2"),
      sto %in% c("B8G", "B101", "D9", "P51C")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                values_from = obs_value)

  if(ncol(BI32) == 9){
    BI32 <- BI32 |>
    dplyr::select(ref_area, ref_sector, time_period, B8G.B,D9.C,D9.D,P51C.C,B101.B) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `B101_calc` = sum(c(B8G.B,D9.C,-D9.D,-P51C.C),na.rm = TRUE),
      check = round(B101.B - `B101_calc`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold) |>
    dplyr::filter(if_all(c(D9.C,D9.D), ~ !is.na(.x )))
  } else {
    rm(BI32)
  }


  ## BI33-----------------------------------------------------------------------------------------
  BI33 <- data |>
    dplyr::filter(
      ref_sector %in% c("S2"),
      sto %in% c("B101", "D9", "B12")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                values_from = obs_value)

  if(ncol(BI33) == 7){
    BI33 <- BI33 |>
    dplyr::select(ref_area, ref_sector, time_period, B12.B,D9.C,D9.D,B101.B) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `B101_calc` = sum(c(B12.B,D9.D,-D9.C),na.rm = TRUE),
      check = round(B101.B - `B101_calc`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)
  } else {
    rm(BI33)
  }

  ## BI34-----------------------------------------------------------------------------------------
  BI34 <- data |>
    dplyr::filter(
      ref_sector %in% c("S1","S2"),
      sto %in% c("B101", "B8G","D9", "P51C")
    ) |>
    unite("sto", c(ref_sector,sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                values_from = obs_value)

  if(ncol(BI34) == 11){
    BI34 <- BI34 |>
    dplyr::select(ref_area,  time_period, S1.B8G.B,S2.D9.D,S2.D9.C,S1.P51C.C,S1.B101.B) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `B101_calc` = sum(c(S1.B8G.B,-S2.D9.D,S2.D9.C,-S1.P51C.C),na.rm = TRUE),
      check = round(S1.B101.B - `B101_calc`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)
  } else {
    rm(BI34)
  }

  ## BI35-----------------------------------------------------------------------------------------
  BI35 <- data |>
    dplyr::filter(
      !ref_sector %in% c("S2"),
      sto %in% c("B9", "B8G", "D9", "P5", "NP")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                values_from = obs_value)

  if(ncol(BI35) == 9){
    BI35 <- BI35 |>
    dplyr::select(ref_area, ref_sector, time_period, B8G.B,D9.C,D9.D,P5.D,NP.D,B9.B) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `B9_calc` = sum(c(B8G.B,D9.C,-D9.D,-P5.D,-NP.D),na.rm = TRUE),
      check = round(B9.B - `B9_calc`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)|>
      dplyr::filter(if_all(c(D9.C,D9.D), ~ !is.na(.x )))
  } else {
    rm(BI35)
  }

  ## BI36-----------------------------------------------------------------------------------------
  BI36 <- data |>
    dplyr::filter(
      ref_sector %in% c("S2"),
      sto %in% c("B9", "B12", "D9", "NP")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                values_from = obs_value)

  if(ncol(BI36) == 8){
    BI36 <- BI36 |>
    dplyr::select(ref_area, ref_sector, time_period, B12.B,D9.C,D9.D,NP.C,B9.B) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `B9_calc` = sum(c(B12.B,D9.D,-D9.C,-NP.C),na.rm = TRUE),
      check = round(B9.B - `B9_calc`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)
  } else {
    rm(BI36)
  }

  ## BI37-----------------------------------------------------------------------------------------
  BI37 <- data |>
    dplyr::filter(
      ref_sector %in% c("S13"),
      sto %in% c("B9", "OTR", "OTE")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                values_from = obs_value)

  if(ncol(BI37) == 6){
    BI37 <- BI37 |>
    dplyr::select(ref_area, ref_sector, time_period, OTR.C, OTE.D,B9.B) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `OTR.C - OTE.D` = sum(c(OTR.C,-OTE.D),na.rm = TRUE),
      check = round(B9.B - `OTR.C - OTE.D`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)
  } else {
    rm(BI37)
  }
  ## BI38-----------------------------------------------------------------------------------------
  BI38 <- data |>
    dplyr::filter(
      !ref_sector %in% c("S2"),
      sto %in% c("B9", "B101", "P51C", "P5", "NP")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                values_from = obs_value)

  if(ncol(BI38) == 9){
    BI38 <- BI38 |>
    dplyr::select(ref_area, ref_sector, time_period, B101.B, P51C.C, P5.D,NP.D,B9.B) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `B9_calc` = sum(c(B101.B,P51C.C,-P5.D,-NP.D),na.rm = TRUE),
      check = round(B9.B - `B9_calc`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)
  } else {
    rm(BI38)
  }
  ## BI39-----------------------------------------------------------------------------------------
  BI39 <- data |>
    dplyr::filter(
      ref_sector %in% c("S2"),
      sto %in% c("B9", "B101", "NP")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                values_from = obs_value)

  if(ncol(BI39) == 6){
    BI39 <- BI39 |>
    dplyr::select(ref_area, ref_sector, time_period, B101.B, NP.C,B9.B) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `B101.B - NP.C` = sum(c(B101.B,-NP.C),na.rm = TRUE),
      check = round(B9.B - `B101.B - NP.C`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)
  } else {
    rm(BI39)
  }

  ## BI40-----------------------------------------------------------------------------------------
  BI40 <- data |>
    dplyr::filter(
      ref_sector %in% c("S1","S2"),
      sto %in% c("B9", "B8G", "D9", "P5", "NP")
    ) |>
    unite("sto", c(ref_sector,sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                values_from = obs_value)

  if(ncol(BI40) == 12){
    BI40 <- BI40 |>
    dplyr::select(ref_area,  time_period, S1.B8G.B,S2.D9.D,S2.D9.C,S1.P5.D,S1.NP.D, S1.B9.B) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `B9_calc` = sum(c(S1.B8G.B,-S2.D9.D,S2.D9.C,-S1.P5.D,-S1.NP.D),na.rm = TRUE),
      check = round(S1.B9.B - `B9_calc`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)
  } else {
    rm(BI40)
  }
  ## BI41-----------------------------------------------------------------------------------------
  BI41 <- data |>
    dplyr::filter(
      ref_sector %in% c("S1", "S2"),
      sto %in% c("B101", "P5", "P51C")
    ) |>
    unite("sto", c(ref_sector,sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                values_from = obs_value)

  if(ncol(BI41) == 7){
    BI41 <- BI41 |>
    dplyr::select(ref_area, time_period, S2.B101.B, S1.P5.D, S1.P51C.C, S1.B101.B) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `S1.B101.B_cal` = sum(c(-S2.B101.B,S1.P5.D,-S1.P51C.C),na.rm = TRUE),
      check = round(S1.B101.B - `S1.B101.B_cal`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)
  } else {
    rm(BI41)
  }
  ## BI42-----------------------------------------------------------------------------------------
  BI42 <- data |>
    dplyr::filter(
      ref_sector %in% c("S2"),
      sto %in% c("B11", "P6", "P7")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                values_from = obs_value)

  if(ncol(BI42) == 6){
    BI42 <- BI42 |>
    dplyr::select(ref_area, time_period, P7.C,P6.D,B11.B) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `P7.C - P6.D` = sum(c(P7.C,-P6.D),na.rm = TRUE),
      check = round(B11.B - `P7.C - P6.D`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)
  } else {
    rm(BI42)
  }

  ## BI43-----------------------------------------------------------------------------------------
    BI43 <- data |>
      dplyr::filter(
        ref_sector %in% c("S2"),
        sto %in% c("B12","B11", "D1", "D2", "D3", "D4", "D5", "D61", "D62", "D7", "D8" )
      ) |>
      unite("sto", c(sto, accounting_entry), sep = ".") |>
      tidyr::pivot_wider(names_from = sto,
                  values_from = obs_value)

  if(ncol(BI43) == 21){
    BI43 <- BI43 |>
      dplyr::select(ref_area, time_period, B11.B,,D1.D,D1.C,D2.D,D3.C,D4.D,D4.C,D5.D,D5.C,D61.D,D61.C,D62.D,D62.C,D7.D,D7.C,D8.D,D8.C, B12.B) |>
      dplyr::rowwise() |>
      dplyr::mutate(
        `B12.B_calc` = sum(c(B11.B,D1.D,-D1.C,D2.D,-D3.C,D4.D,-D4.C,D5.D,-D5.C,D61.D,-D61.C,D62.D,-D62.C,D7.D,-D7.C,D8.D,-D8.C),na.rm = TRUE),
        check = round(B12.B - `B12.B_calc`, rounding)
      ) |>
      dplyr::filter(abs(check) > threshold)
  } else {
    rm(BI43)
  }

  ## BI44-----------------------------------------------------------------------------------------
  BI44 <- data |>
    dplyr::filter(
      ref_sector %in% c("S13"),
      sto %in% c("OTR", "P1O", "D2", "D39", "D4", "D5", "D61", "D7", "D9" )
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                values_from = obs_value)

  if(ncol(BI44) == 18){
    BI44 <- BI44 |>
    dplyr::select(ref_area, time_period, P1O.C,D2.C,D39.C,D4.C,D5.C,D61.C,D7.C,D9.C,OTR.C) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `OTR.C_calc` = sum(c(P1O.C,D2.C,D39.C,D4.C,D5.C,D61.C,D7.C,D9.C),na.rm = TRUE),
      check = round(OTR.C - `OTR.C_calc`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)
  } else {
    rm(BI44)
  }

  ## BI45-----------------------------------------------------------------------------------------
  BI45 <- data |>
    dplyr::filter(
      ref_sector %in% c("S13"),
      sto %in% c("OTE","P2","P5","D1","D29","D3","D4","D5","D62","D632","D7","D8","D9","NP")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                values_from = obs_value)

  if(ncol(BI45) == 23){
    BI45 <- BI45 |>
    dplyr::select(ref_area, time_period, P2.D,P5.D,D1.D,D29.D,D3.D,D4.D,D5.D,D62.D,D632.D,D7.D,D8.D,D9.D,NP.D,OTE.D) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `OTE.D_calc` = sum(c(P2.D,P5.D,D1.D,D29.D,D3.D,D4.D,D5.D,D62.D,D632.D,D7.D,D8.D,D9.D,NP.D),na.rm = TRUE),
      check = round(OTE.D - `OTE.D_calc`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)
  } else {
    rm(BI45)
  }
  ## BI46-----------------------------------------------------------------------------------------
  BI46 <- data |>
    dplyr::filter(
      ref_sector %in% c("S13"),
      sto %in% c("P31", "D63")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                values_from = obs_value)

  if(ncol(BI46) == 5){
    BI46 <- BI46 |>
    dplyr::select(ref_area, time_period, P31.D,D63.D) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      check = round(P31.D - `D63.D`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)
  } else {
    rm(BI46)
  }

  ## BI47-----------------------------------------------------------------------------------------
  BI47 <- data |>
    dplyr::filter(
      ref_sector %in% c("S13"),
      sto %in% c("P3", "P1", "P1O", "D632")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                values_from = obs_value)

  if(ncol(BI47) == 7){
    BI47 <- BI47 |>
    dplyr::select(ref_area, time_period, P1.C,P1O.C,D632.D,P3.D) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `P3.D_calc` = sum(c(P1.C,-P1O.C,D632.D),na.rm = TRUE),
      check = round(P3.D - `P3.D_calc`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)
  } else {
    rm(BI47)
  }

  list_ur <- mget(ls(pattern = "ur")) |>
    keep(~ any(nrow(.x) > 0)) |>
    map(~ dplyr::mutate(.x, across(everything(), ~ replace(.x, is.na(.x), "NaN"))))

  list_s1ss <- mget(ls(pattern = "s1ss")) |>
    keep(~ any(nrow(.x) > 0)) |>
    map(~ dplyr::mutate(.x, across(everything(), ~ replace(.x, is.na(.x), "NaN"))))

  list_sit <- mget(ls(pattern = "sit")) |>
    keep(~ any(nrow(.x) > 0)) |>
    map(~ dplyr::mutate(.x, across(everything(), ~ replace(.x, is.na(.x), "NaN"))))

  list_BI <- mget(ls(pattern = "BI")) |>
    keep(~ any(nrow(.x) > 0)) |>
    map(~ dplyr::mutate(.x, across(everything(), ~ replace(.x, is.na(.x), "NaN"))))

  list_ir <- c(list_ur, list_s1ss, list_sit, list_BI)

  if(length(list_ir) == 0){

    cli::cli_inform("All consistent!")
  } else {
  openxlsx::write.xlsx(list_ir,
                       file = paste0(paste0(output_sel, "/", as.character(format(Sys.time(), "%Y%m%d_%H%M%S")), ".xlsx")),
                       asTable = TRUE,
                       overwrite = TRUE
  )

  cli::cli_alert_success(paste0("File created at: ", output_sel, "/", as.character(format(Sys.time(), "%Y%m%d_%H%M%S")), ".xlsx"))
  }
}

