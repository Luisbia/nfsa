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
#' nfsa_internal_consistency_T0800(my_dataset, output_sel = "my_output_folder", threshold = 5)
#' }
#'
#' @export
nfsa_internal_consistency_T0801 <- function(dataset,
                                            output_sel = here::here("output", "internal"),
                                            threshold = 0,
                                            rounding = 2) {

  library(tidyverse)
  library(arrow)
  library(here)
  library(janitor)
  library(openxlsx)

  lookup <- nfsa::nfsa_sto_lookup

   data <- dataset |>
    mutate(obs_value = round_half_up(obs_value, rounding)) |>
    nfsa::nfsa_separate_id()



  # Uses vs Resources-----------------------------------------------------------
  ## UR01-----------------------------------------------------------------------

  ur01 <- data |>
    filter(
      sto %in% c(
        "D1", "D4", "D41", "D4N","D41G","D42", "D421","D422", "D43", "D44",
        "D45",  "D5", "D6", "D61","D62", "D7", "D71", "D72", "D7N",
        "D74", "D75", "D8", "D9", "D9N","D91", "D92", "D99"
      ),
      ref_sector %in% c("S1", "S2")
    ) |>
    pivot_wider(
      names_from = c(ref_sector, accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    select(ref_area, sto, time_period, S1.D, S2.C, S1.C, S2.D) |>
    rowwise() |>
    mutate(
      `S1.D + S2.C` = sum(c(S1.D, S2.C), na.rm = TRUE),
      `S1.C + S2.D` = sum(c(S1.C, S2.D), na.rm = TRUE),
      check = round(`S1.D + S2.C` - `S1.C + S2.D`, rounding)
    ) |>
    filter(abs(check) > threshold)

  ## UR02---------------------------------------------------------------------

  ur02 <- data |>
    filter(
      sto %in% c(
        "D2", "D21", "D29"
      ),
      ref_sector %in% c("S1", "S2")
    ) |>
    pivot_wider(
      names_from = c(ref_sector, accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    select(ref_area, sto, time_period, S1.D, S1.C, S2.D) |>
    rowwise() |>
    mutate(
      `S1.C + S2.D` = sum(c(S1.C, S2.D), na.rm = TRUE),
      check = round(S1.D - `S1.C + S2.D`, rounding)
    ) |>
    filter(abs(check) > threshold)

  ## UR03---------------------------------------------------------------------

  ur03 <- data |>
    filter(
      sto %in% c(
        "D3", "D31", "D39"
      ),
      ref_sector %in% c("S1", "S2")
    ) |>
    pivot_wider(
      names_from = c(ref_sector, accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    select(ref_area, sto, time_period, S1.D, S2.C, S1.C) |>
    rowwise() |>
    mutate(
      `S1.D + S2.C` = sum(c(S1.D, S2.C), na.rm = TRUE),
      check = round(S1.C - `S1.D + S2.C`, rounding)
    ) |>
    filter(abs(check) > threshold)

  ## UR04---------------------------------------------------------------------

  ur04 <- data |>
    filter(
      sto %in% c(
        "D63", "P51C"
      ),
      ref_sector %in% c("S1")
    ) |>
    pivot_wider(
      names_from = c(ref_sector, accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    select(ref_area, sto, time_period, S1.D, S1.C) |>
    rowwise() |>
    mutate(
      `S1.D - S1.C` = sum(c(S1.D, -S1.C), na.rm = TRUE),
    ) |>
    filter(abs(`S1.D - S1.C`) > threshold)

  ## UR05---------------------------------------------------------------------

  ur05 <- data |>
    filter(
      sto %in% c(
        "D43", "D74", "D76"
      ),
      ref_sector %in% c("S1", "S2")
    ) |>
    pivot_wider(
      names_from = c(ref_sector, accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    select(ref_area, sto, time_period, S1.D, S2.D) |>
    rowwise() |>
    mutate(
      `S1.D - S2.D` = sum(c(S1.D, -S2.D), na.rm = TRUE),
    ) |>
    filter(abs(`S1.D - S2.D`) > threshold)

  ## UR06---------------------------------------------------------------------

  ur06 <- data |>
    filter(
      sto %in% c(
        "NP"
      ),
      ref_sector %in% c("S1", "S2")
    ) |>
    pivot_wider(
      names_from = c(ref_sector, accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    select(ref_area, sto, time_period, S1.D, S2.C) |>
    rowwise() |>
    mutate(
      `S1.D + S2.C` = sum(c(S1.D, S2.C), na.rm = TRUE),
    ) |>
    filter(abs(`S1.D + S2.C`) > threshold)

  ## UR07---------------------------------------------------------------------

  ur07 <- data |>
    filter(
      sto %in% c(
        "B9", "B9X9F"
      ),
      ref_sector %in% c("S1", "S2")
    ) |>
    pivot_wider(
      names_from = c(ref_sector),
      values_from = obs_value,
      names_sep = "."
    ) |>
    select(ref_area, sto, time_period, S1, S2) |>
    rowwise() |>
    mutate(
      `S1 + S2` = sum(c(S1, S2), na.rm = TRUE),
    ) |>
    filter(abs(`S1 + S2`) > threshold)

  ## UR08---------------------------------------------------------------------

  ur08 <- data |>
    filter(
      sto %in% c(
        "D43", "D74"
      ),
      ref_sector %in% c("S1", "S2")
    ) |>
    pivot_wider(
      names_from = c(ref_sector,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    select(ref_area, sto, time_period, S1.C, S2.C) |>
    rowwise() |>
    mutate(
      `S1.C - S2.C` = sum(c(S1.C, -S2.C), na.rm = TRUE),
    ) |>
    filter(abs(`S1.C - S2.C`) > threshold)


  # S1 vs Sum of Sub-sectors--------------------------------------------------
  ## S1SS01-------------------------------------------------------------------
  s1ss01 <- data |>
    filter(ref_sector %in% c("S1", "S11", "S12", "S13", "S1M", "S1N")) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    filter(sto %in% c(
      "D2.D", "D3.C"
    )) |>
    pivot_wider(
      names_from = ref_sector,
      values_from = obs_value
    ) |>
    select(ref_area, sto, time_period, S1N, S11, S12, S13, S1M, S1) |>
    rowwise() |>
    mutate(`S1N + S11 + S12 + S13 + S1M` = sum(c(S1N, S11, S12, S13, S1M), na.rm = TRUE)) |>
    mutate(check = round(S1 - `S1N + S11 + S12 + S13 + S1M`, rounding)) |>
    filter(abs(check) > threshold) |>
    filter(if_all(c(S11,S12,S1M), ~ !is.na(.x )))

  ## S1SS02-------------------------------------------------------------------
  s1ss02 <- data |>
    filter(ref_sector %in% c("S1", "S11", "S12", "S13", "S1M")) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    filter(sto %in% c(
      "P2.D", "P5.D", "P51G.D", "P5M.D", "D1.D", "D29.D", "D4.D", "D41.D", "D4N.D","D45.D", "D41G.D",
      "D5.D", "D6.D", "D62.D", "D7.D", "D71.D", "D7N.D","D75.D", "D8.D", "D9.D", "D9.N","D99.D",
      "P51.C", "NP.D", "P1.C", "D39.C", "D4.C", "D41.C", "D4N.C","D42.C", "D43.C", "D44.C", "D45.C",
      "D41G.C", "D6.C", "D61.C", "D7.C", "D7N.C","D72.C", "D75.C", "D9.C", "D9N.C","D92.C", "D99.C",
      "P51C.C", "B2A3G.B", "B4G.B", "B5G.B", "B6G.B", "B8G.B", "B101.B", "B9.B", "B9X9F", "EMP.PS", "EMP.HW"
    )) |>
    pivot_wider(
      names_from = ref_sector,
      values_from = obs_value
    ) |>
    select(ref_area, sto, time_period, S11, S12, S13, S1M, S1) |>
    rowwise() |>
    mutate(`S11 + S12 + S13 + S1M` = sum(c(S11, S12, S13, S1M), na.rm = TRUE)) |>
    mutate(check = round(S1 - `S11 + S12 + S13 + S1M`, rounding)) |>
    filter(abs(check) > threshold) |>
    filter(if_all(c(S11,S12, S1M), ~ !is.na(.x )))

  ## S1SS03-------------------------------------------------------------------
  check <- data |>
      filter(ref_sector == "S11",
             sto %in% c("D43", "D91"),
             accounting_entry == "D")

  if(nrow(check) > 0){
    s1ss03 <- data |>
    filter(ref_sector %in% c("S1", "S11", "S12","S1M")) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    filter(sto %in% c(
      "D43.D", "D91.D"
    )) |>
    pivot_wider(
      names_from = ref_sector,
      values_from = obs_value
    ) |>
    select(ref_area, sto, time_period, S11, S12, S1M, S1) |>
    rowwise() |>
    mutate(`S11 + S12 + S1M` = sum(c(S11, S12, S1M), na.rm = TRUE)) |>
    mutate(check = round(S1 - `S11 + S12 + S1M`, rounding)) |>
    filter(abs(check) > threshold) |>
      filter(if_all(c(S11,S12, S1M), ~ !is.na(.x )))
  }
  rm(check)

    ## S1SS04-------------------------------------------------------------------
  check <- data |>
    filter(ref_sector == "S11",
           sto %in% c("D42", "D44"),
           accounting_entry == "D")

  if(nrow(check) > 0){
  s1ss04 <- data |>
    filter(ref_sector %in% c("S1", "S11", "S12","S13")) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    filter(sto %in% c(
      "D42.D", "D44.D"
    )) |>
    pivot_wider(
      names_from = ref_sector,
      values_from = obs_value
    ) |>
    select(ref_area, sto, time_period, S11, S12, S13, S1) |>
    rowwise() |>
    mutate(`S11 + S12 + S13` = sum(c(S11, S12, S13), na.rm = TRUE)) |>
    mutate(check = round(S1 - `S11 + S12 + S13`, rounding)) |>
    filter(abs(check) > threshold)}
  rm(check)

  ## S1SS05-------------------------------------------------------------------
  check <- data |>
    filter(ref_sector == "S11",
           sto %in% c("D43"),
           accounting_entry == "D")

  if(nrow(check) > 0){
    s1ss05 <- data |>
    filter(ref_sector %in% c("S1", "S11", "S12")) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    filter(sto %in% c(
      "D43.D"
    )) |>
    pivot_wider(
      names_from = ref_sector,
      values_from = obs_value
    ) |>
    select(ref_area, sto, time_period, S11, S12,  S1) |>
    rowwise() |>
    mutate(`S11 + S12` = sum(c(S11, S12), na.rm = TRUE)) |>
    mutate(check = round(S1 - `S11 + S12`, rounding)) |>
    filter(abs(check) > threshold) |>
      filter(if_all(c(S11,S12), ~ !is.na(.x )))
  }
  rm(check)

  ## S1SS06-------------------------------------------------------------------
  check <- data |>
    filter(ref_sector == "S12",
           sto %in% c("D72", "D71"))

  if(nrow(check) > 0){
  s1ss06 <- data |>
    filter(ref_sector %in% c("S1", "S12", "S13")) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    filter(sto %in% c(
      "D72.D", "D71.C"
    )) |>
    pivot_wider(
      names_from = ref_sector,
      values_from = obs_value
    ) |>
    select(ref_area, sto, time_period, S12, S13,  S1) |>
    rowwise() |>
    mutate(`S12 + S13` = sum(c(S12, S13), na.rm = TRUE)) |>
    mutate(check = round(S1 - `S12 + S13`, rounding)) |>
    filter(abs(check) > threshold)|>
    filter(if_all(c(S12), ~ !is.na(.x )))}
  rm(check)

  ## S1SS07-------------------------------------------------------------------
  s1ss07 <- data |>
    filter(ref_sector %in% c("S1",  "S13")) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    filter(sto %in% c(
      "P32.D", "D3.D", "D31.D", "D39.D", "D74.D", "D74_4Y.D", "D76.D", "D92.D", "D2.C", "D21.C",
      "D211.C", "D212.C", "D214.C", "D29.C", "D5.C", "D51.C", "D59.C", "D74.C", "D91.C"
    )) |>
    pivot_wider(
      names_from = ref_sector,
      values_from = obs_value
    ) |>
    select(ref_area, sto, time_period, S13,  S1) |>
    rowwise() |>
    mutate(check = round(S1 - S13, rounding)) |>
    filter(abs(check) > threshold)

  ## S1SS08-------------------------------------------------------------------
  check <- data |>
    filter(ref_sector == "S1M",
           sto %in% c("P3", "P31"))

  if(nrow(check) > 0){
  s1ss08 <- data |>
    filter(ref_sector %in% c("S1",  "S13", "S1M")) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    filter(sto %in% c(
      "P3.D", "P31.D", "D63.D", "D631.D", "D632.D", "P13.C"
    )) |>
    pivot_wider(
      names_from = ref_sector,
      values_from = obs_value
    ) |>
    select(ref_area, sto, time_period, S13, S1M,  S1) |>
    rowwise() |>
    mutate(`S13 + S1M` = sum(c(S13, S1M), na.rm = TRUE)) |>
    mutate(check = round(S1 - `S13 + S1M`, rounding)) |>
    filter(abs(check) > threshold) |>
    filter(if_all(c(S1M), ~ !is.na(.x )))}
  rm(check)

  ## S1SS09-------------------------------------------------------------------
  check <- data |>
    filter(ref_sector == "S1M",
           sto %in% c("D61", "D62"))
  if(nrow(check) > 0){
  s1ss09 <- data |>
    filter(ref_sector %in% c("S1",  "S1M")) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    filter(sto %in% c(
      "D61.D", "D1.C", "D62.C", "D63.C",  "D8.C", "B3G.B"
    )) |>
    pivot_wider(
      names_from = ref_sector,
      values_from = obs_value
    ) |>
    select(ref_area, sto, time_period, S1M,  S1) |>
    rowwise() |>
    mutate(check = round(S1 - S1M, rounding)) |>
    filter(abs(check) > threshold)|>
    filter(if_all(c(S1M), ~ !is.na(.x )))}
rm(check)

  ## S1SS10-------------------------------------------------------------------
  s1ss10 <- data |>
    filter(ref_sector %in% c("S1",  "S1N")) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    filter(sto %in% c(
      "D21.D", "D31.C", "D21X31.C"
    )) |>
    pivot_wider(
      names_from = ref_sector,
      values_from = obs_value
    ) |>
    select(ref_area, sto, time_period, S1N,  S1) |>
    rowwise() |>
    mutate(check = round(S1 - S1N, rounding)) |>
    filter(abs(check) > threshold)

  ## S1SS11-------------------------------------------------------------------
check <- data |>
  filter(ref_sector == "S14",
         sto %in% c("D1"))
if(nrow(check) > 0){
s1ss11 <- data |>
    filter(ref_sector %in% c("S1M",  "S14")) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    filter(sto %in% c(
      "B3G.B", "D1.C", "D11.C", "D12.C", "D61.D", "D611.D", "D612.D", "D613.D", "D614.D", "D61SC.D",
      "D62.C", "D63.C", "D631.C", "D632.C", "D8.C"
    )) |>
    pivot_wider(
      names_from = ref_sector,
      values_from = obs_value
    ) |>
    select(ref_area, sto, time_period, S1M,  S14) |>
    rowwise() |>
    mutate(check = round(S1M - S14, rounding)) |>
    filter(abs(check) > threshold)}
rm(check)

  ## S1SS12-------------------------------------------------------------------
check <- data |>
  filter(ref_sector == "S15",
         sto %in% c("D63"),
         accounting_entry == "D")
if(nrow(check) > 0){
s1ss12 <- data |>
    filter(ref_sector %in% c("S1M",  "S15")) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    filter(sto %in% c(
      "D63.D", "D631.D", "D632.D", "P13.C"
    )) |>
    pivot_wider(
      names_from = ref_sector,
      values_from = obs_value
    ) |>
    select(ref_area, sto, time_period, S1M,  S15) |>
    rowwise() |>
    mutate(check = round(S1M - S15, rounding)) |>
    filter(abs(check) > threshold)}
rm(check)

  ## S1SS13-------------------------------------------------------------------
check <- data |>
  filter(ref_sector == "S15",
         sto %in% c("P3"),
         accounting_entry == "D")
if(nrow(check) > 0){
s1ss13 <- data |>
    filter(ref_sector %in% c("S1M",  "S14", "S15")) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    filter(sto %in% c(
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
    pivot_wider(
      names_from = ref_sector,
      values_from = obs_value
    ) |>
    select(ref_area, sto, time_period, S14, S15,  S1M) |>
    rowwise() |>
    mutate(`S14 + S15` = sum(c(S14, S15), na.rm = TRUE)) |>
    mutate(check = round(S1M - `S14 + S15`, rounding)) |>
    filter(abs(check) > threshold)|>
  filter(if_all(c(S14,S15), ~ !is.na(.x )))}
rm(check)

  # S1SS14 to 19 are for voluntary subsectors
  ## S1SS20-------------------------------------------------------------------
check <- data |>
  filter(ref_sector == "S11",
         sto %in% c("B1G"),
         accounting_entry == "B")
if(nrow(check) > 0){
s1ss20 <- data |>
    unite("sto", c(ref_sector,sto, accounting_entry), sep = ".") |>
    filter(sto %in% c(
      "S1.B1GQ.B", "S1N.B1G.B","S11.B1G.B","S12.B1G.B","S13.B1G.B","S1M.B1G.B"
    )) |>
    pivot_wider(
      names_from = sto,
      values_from = obs_value
    ) |>
    select(ref_area,time_period,S1N.B1G.B,S11.B1G.B,S12.B1G.B,S13.B1G.B,S1M.B1G.B, S1.B1GQ.B ) |>
    rowwise() |>
    mutate(`sum_B1G` = sum(c(S1N.B1G.B,S11.B1G.B,S12.B1G.B,S13.B1G.B,S1M.B1G.B), na.rm = TRUE)) |>
    mutate(check = round(S1.B1GQ.B - `sum_B1G`, rounding)) |>
    filter(abs(check) > threshold) |>
  filter(if_all(c(S11.B1G.B,S12.B1G.B), ~ !is.na(.x )))}
rm(check)

  # SubItems vs Total-----------------------------------------------------------------------------

  ## SIT01----------------------------------------------------------------------------------------

  sit01 <- data |>
    filter(
      ref_sector %in% c("S1", "S13"),
      sto %in% c("P3", "P31", "P32"),
      accounting_entry == "D"
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(
      names_from = sto,
      values_from = obs_value
    ) |>
    select(ref_area, ref_sector, time_period, P31.D, P32.D, P3.D) |>
    rowwise() |>
    mutate(`P31.D + P32.D` = sum(c(P31.D,P32.D), na.rm = TRUE)) |>
    mutate(check = round(P3.D - `P31.D + P32.D`, rounding)) |>
    filter(abs(check) > threshold)

  ## SIT02----------------------------------------------------------------------------------------
check <- data |>
  filter(ref_sector == "S14",
         sto %in% c("P31"),
         accounting_entry == "B")
if(nrow(check) > 0){
  sit02 <- data |>
    filter(
      ref_sector %in% c("S1M", "S14", "S15"),
      sto %in% c("P3", "P31"),
      accounting_entry == "D"
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(
      names_from = sto,
      values_from = obs_value
    ) |>
    select(ref_area, ref_sector, time_period, P3.D, P31.D) |>
    rowwise() |>
    mutate(check = round(P3.D - P31.D, rounding)) |>
    filter(abs(check) > threshold)}
rm(check)


  ## SIT04----------------------------------------------------------------------------------------

  sit03 <- data |>
    filter(
      sto %in% c("P5", "P51G", "P5M"),
      accounting_entry == "D"
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(
      names_from = sto,
      values_from = obs_value
    ) |>
    select(ref_area, ref_sector, time_period, P51G.D, P5M.D, P5.D) |>
    rowwise() |>
    mutate(`P51G.D + P5M.D` = sum(P51G.D,P5M.D, na.rm = TRUE),
           check = round(P5.D - `P51G.D + P5M.D`, rounding)) |>
    filter(abs(check) > threshold)

    ## SIT06----------------------------------------------------------------------------------------

  sit06 <- data |>
    filter(
      sto %in% c("D2", "D21", "D29")
    ) |>
    pivot_wider(
      names_from = sto,
      values_from = obs_value
    ) |>
    select(ref_area, ref_sector, accounting_entry,time_period, D21, D29, D2) |>
    rowwise() |>
    mutate(`D21 + D29` = sum(D21,D29, na.rm = TRUE),
           check = round(D2 - `D21 + D29`, rounding)) |>
    filter(abs(check) > threshold)

  ## SIT07----------------------------------------------------------------------------------------

  sit07 <- data |>
    filter(
      sto %in% c("D2", "D21"),
      ref_sector =="S1N",
      accounting_entry == "D"
    ) |>
    pivot_wider(
      names_from = sto,
      values_from = obs_value
    ) |>
    select(ref_area, ref_sector, accounting_entry,time_period, D2, D21) |>
    rowwise() |>
    mutate(check = round(D2 - D21, rounding)) |>
    filter(abs(check) > threshold)

  ## SIT08----------------------------------------------------------------------------------------

  sit08 <- data |>
    filter(
      sto %in% c("D2", "D29"),
      !ref_sector %in% c("S1","S1N", "S2"),
      accounting_entry == "D"
    ) |>
    pivot_wider(
      names_from = sto,
      values_from = obs_value
    ) |>
    select(ref_area, ref_sector, accounting_entry,time_period, D2, D29) |>
    rowwise() |>
    mutate(check = round(D2 - D29, rounding)) |>
    filter(abs(check) > threshold)

  ## SIT09----------------------------------------------------------------------------------------

  sit09 <- data |>
    filter(
      sto %in% c("D3", "D31", "D39"),
      ref_sector %in% c("S1","S2", "S13"),
    ) |>
    pivot_wider(
      names_from = sto,
      values_from = obs_value
    ) |>
    select(ref_area, ref_sector, accounting_entry,time_period, D31, D39, D3) |>
    rowwise() |>
    mutate(`D31 + D39` = sum(c(D31,D39), na.rm = TRUE),
           check = round(D3 - `D31 + D39`, rounding)) |>
    filter(abs(check) > threshold)

  # ## SIT10----------------------------------------------------------------------------------------
  #
  # sit10 <- data |>
  #   filter(
  #     sto %in% c("D4", "D41", "D42", "D43", "D44", "D45"),
  #   ) |>
  #   pivot_wider(
  #     names_from = sto,
  #     values_from = obs_value
  #   ) |>
  #   select(ref_area, ref_sector, accounting_entry,time_period, D41, D42, D43, D44, D45, D4) |>
  #   rowwise() |>
  #   mutate(`D41 + D42 + D43 + D44 + D45` = sum(c(D41, D42, D43, D44, D45), na.rm = TRUE),
  #          check = round(D4 - `D41 + D42 + D43 + D44 + D45`, rounding)) |>
  #   filter(abs(check) > threshold)
  #
  # ## SIT11----------------------------------------------------------------------------------------
  #
  # sit11 <- data |>
  #   filter(
  #     sto %in% c("D4", "D41", "D42",  "D44", "D45"),
  #     ref_sector == "S13",
  #     accounting_entry == "D"
  #   ) |>
  #   pivot_wider(
  #     names_from = sto,
  #     values_from = obs_value
  #   ) |>
  #   select(ref_area, ref_sector, accounting_entry,time_period, D41, D42, D44, D45, D4) |>
  #   rowwise() |>
  #   mutate(`D41 + D42 + D44 + D45` = sum(c(D41, D42,  D44, D45), na.rm = TRUE),
  #          check = round(D4 - `D41 + D42 + D44 + D45`, rounding)) |>
  #   filter(abs(check) > threshold)
  #
  # ## SIT12----------------------------------------------------------------------------------------
  #
  # sit12 <- data |>
  #   filter(
  #     sto %in% c("D4", "D41", "D43",  "D44", "D45"),
  #     ref_sector %in% c("S1M", "S14", "S15"),
  #     accounting_entry == "D"
  #   ) |>
  #   pivot_wider(
  #     names_from = sto,
  #     values_from = obs_value
  #   ) |>
  #   select(ref_area, ref_sector, accounting_entry,time_period, D41, D43, D44, D45, D4) |>
  #   rowwise() |>
  #   mutate(`D41 + D43 + D44 + D45` = sum(c(D41, D43,  D44, D45), na.rm = TRUE),
  #          check = round(D4 - `D41 + D43 + D44 + D45`, rounding)) |>
  #   filter(abs(check) > threshold)
  #
  # ## SIT15----------------------------------------------------------------------------------------
  #
  # sit15 <- data |>
  #   filter(
  #     sto %in% c("D42", "D421", "D422")
  #   ) |>
  #   pivot_wider(
  #     names_from = sto,
  #     values_from = obs_value
  #   ) |>
  #   select(ref_area, ref_sector, accounting_entry,time_period, D42, D421, D422) |>
  #   rowwise() |>
  #   mutate(`D421 + D422` = sum(c(D421, D422), na.rm = TRUE),
  #          check = round(D42 - `D421 + D422`, rounding)) |>
  #   filter(abs(check) > threshold)
  #
  ## SIT16----------------------------------------------------------------------------------------

  # sit16 <- data |>
  #   filter(
  #     sto %in% c("D44", "D441", "D442", "D443")
  #   ) |>
  #   pivot_wider(
  #     names_from = sto,
  #     values_from = obs_value
  #   ) |>
  #   select(ref_area, ref_sector, accounting_entry,time_period, D44, D441, D442, D443) |>
  #   rowwise() |>
  #   mutate(`D441 + D442 + D443` = sum(c(D441, D442, D443), na.rm = TRUE),
  #          check = round(D44 - `D441 + D442 + D443`, rounding)) |>
  #   filter(abs(check) > threshold)

## SIT14----------------------------------------------------------------------------------------

sit14 <- data |>
  filter(
    sto %in% c("D4", "D41", "D4N")
  ) |>
  pivot_wider(
    names_from = sto,
    values_from = obs_value
  ) |>
  select(ref_area, ref_sector, accounting_entry,time_period, D41, D4N, D4) |>
  rowwise() |>
  mutate(`D41 + D4N` = sum(c(D41, D4N), na.rm = TRUE),
         check = round(D4 - `D41 + D4N`, rounding)) |>
  filter(abs(check) > threshold)
  # ## SIT17----------------------------------------------------------------------------------------
  #
  # sit17 <- data |>
  #   filter(
  #     sto %in% c("D5", "D51", "D59")
  #   ) |>
  #   pivot_wider(
  #     names_from = sto,
  #     values_from = obs_value
  #   ) |>
  #   select(ref_area, ref_sector, accounting_entry,time_period, D51, D59, D5) |>
  #   rowwise() |>
  #   mutate(`D51 + D59` = sum(c(D51, D59), na.rm = TRUE),
  #          check = round(D5 - `D51 + D59`, rounding)) |>
  #   filter(abs(check) > threshold)
  #
  ## SIT18----------------------------------------------------------------------------------------

  sit18 <- data |>
    filter(
      sto %in% c("D6", "D61", "D62", "D63"),
      ref_sector !="S2"
    ) |>
    pivot_wider(
      names_from = sto,
      values_from = obs_value
    ) |>
    select(ref_area, ref_sector, accounting_entry,time_period, D61, D62, D63, D6) |>
    rowwise() |>
    mutate(`D61 + D62 + D63` = sum(c(D61, D62, D63), na.rm = TRUE),
           check = round(D6 - `D61 + D62 + D63`, rounding)) |>
    filter(abs(check) > threshold)

  ## SIT19----------------------------------------------------------------------------------------

  sit19 <- data |>
    filter(
      sto %in% c("D6", "D62"),
      ref_sector %in% c("S11", "S12"),
      accounting_entry == "D"
    ) |>
    pivot_wider(
      names_from = sto,
      values_from = obs_value
    ) |>
    select(ref_area, ref_sector, accounting_entry,time_period, D6, D62) |>
    rowwise() |>
    mutate(check = round(D6 - D62, rounding)) |>
    filter(abs(check) > threshold)

  ## SIT20----------------------------------------------------------------------------------------

  sit20 <- data |>
    filter(
      sto %in% c("D6", "D62", "D63"),
      ref_sector %in% c("S13"),
      accounting_entry == "D"
    ) |>
    pivot_wider(
      names_from = sto,
      values_from = obs_value
    ) |>
    select(ref_area, ref_sector, accounting_entry,time_period, D62, D63, D6) |>
    rowwise() |>
    mutate(`D62 + D63` = sum(c(D62, D63), na.rm = TRUE),
           check = round(D6 - `D62 + D63`, rounding)) |>
    filter(abs(check) > threshold)


  ## SIT21----------------------------------------------------------------------------------------

  sit21 <- data |>
    filter(
      sto %in% c("D6", "D61", "D62"),
      ref_sector %in% c("S2")
    ) |>
    pivot_wider(
      names_from = sto,
      values_from = obs_value
    ) |>
    select(ref_area, ref_sector, accounting_entry,time_period, D61, D62, D6) |>
    rowwise() |>
    mutate(`D61 + D62` = sum(c(D61, D62), na.rm = TRUE),
           check = round(D6 - `D61 + D62`, rounding)) |>
    filter(abs(check) > threshold)

  ## SIT23----------------------------------------------------------------------------------------

  sit23 <- data |>
    filter(
      sto %in% c("D63", "D631", "D632"),
    ) |>
    pivot_wider(
      names_from = sto,
      values_from = obs_value
    ) |>
    select(ref_area, ref_sector, accounting_entry,time_period, D631, D632, D63) |>
    rowwise() |>
    mutate(`D631 + D632` = sum(c(D631, D632), na.rm = TRUE),
           check = round(D63 - `D631 + D632`, rounding)) |>
    filter(abs(check) > threshold) |>
    filter(if_all(c(D631,D632), ~ !is.na(.x )))


  ## SIT28----------------------------------------------------------------------------------------

  sit28 <- data |>
    filter(
      sto %in% c("D74", "D74_4Y"),
      ref_sector %in% c("S1", "S13"),
      accounting_entry == "D"
    ) |>
    pivot_wider(
      names_from = sto,
      values_from = obs_value
    ) |>
    select(ref_area, ref_sector, accounting_entry,time_period, D74, D74_4Y) |>
    filter(D74 < D74_4Y)


  ## SIT31----------------------------------------------------------------------------------------

  sit31 <- data |>
    filter(
      sto %in% c("P6", "P61",  "P62"),
      ref_sector %in% c("S2"),
      accounting_entry == "D"
    ) |>
    pivot_wider(
      names_from = sto,
      values_from = obs_value
    ) |>
    select(ref_area, ref_sector, accounting_entry,time_period, P61,  P62, P6) |>
    rowwise() |>
    mutate(`P61 + P62` = sum(c(P61,P62), na.rm = TRUE),
           check = round(P6 - `P61 + P62`, rounding)) |>
    filter(abs(check) > threshold)

  ## SIT32----------------------------------------------------------------------------------------

  sit32 <- data |>
    filter(
      sto %in% c("P62", "P62F"),
      ref_sector %in% c("S2"),
      accounting_entry == "D"
    ) |>
    pivot_wider(
      names_from = sto,
      values_from = obs_value
    ) |>
    select(ref_area, ref_sector, accounting_entry,time_period, P62, P62F) |>
    filter(P62F > P62)


  ## SIT34----------------------------------------------------------------------------------------

  sit34 <- data |>
    filter(
      sto %in% c("D3", "D31"),
      ref_sector %in% c("S1N"),
      accounting_entry == "C"
    ) |>
    pivot_wider(
      names_from = sto,
      values_from = obs_value
    ) |>
    select(ref_area, ref_sector, accounting_entry,time_period, D3, D31) |>
    rowwise() |>
    mutate(`check` = sum(c(D3, -D31), na.rm = TRUE)) |>
    filter(abs(check) > threshold)

  ## SIT35----------------------------------------------------------------------------------------

  sit35 <- data |>
    filter(
      sto %in% c("D3", "D39"),
      !ref_sector %in% c("S1", "S2", "S1N"),
      accounting_entry == "C"
    ) |>
    pivot_wider(
      names_from = sto,
      values_from = obs_value
    ) |>
    select(ref_area, ref_sector, accounting_entry,time_period, D3, D39) |>
    rowwise() |>
    mutate(`check` = sum(c(D3, -D39), na.rm = TRUE)) |>
    filter(abs(check) > threshold)

  ## SIT36----------------------------------------------------------------------------------------

  sit36 <- data |>
    filter(
      sto %in% c("D6", "D61"),
      !ref_sector %in% c("S1", "S2", "S14", "S1M"),
      accounting_entry == "C"
    ) |>
    pivot_wider(
      names_from = sto,
      values_from = obs_value
    ) |>
    select(ref_area, ref_sector, accounting_entry,time_period, D6, D61) |>
    rowwise() |>
    mutate(`check` = sum(c(D6, -D61), na.rm = TRUE)) |>
    filter(abs(check) > threshold)


  ## SIT41----------------------------------------------------------------------------------------

  sit41 <- data |>
    filter(
      sto %in% c("P7", "P71", "P72"),
      ref_sector %in% c("S2"),
      accounting_entry == "C"
    ) |>
    pivot_wider(
      names_from = sto,
      values_from = obs_value
    ) |>
    select(ref_area, ref_sector, accounting_entry,time_period, P7, P71, P72) |>
    rowwise() |>
    mutate(`P71 + P72` = sum(c(P71, P72), na.rm = TRUE),
           check = round(P7 - `P71 + P72`, rounding)) |>
    filter(abs(check) > threshold)

  ## SIT42----------------------------------------------------------------------------------------

  sit42 <- data |>
    filter(
      sto %in% c("P72", "P72F"),
      ref_sector %in% c("S2"),
      accounting_entry == "C"
    ) |>
    pivot_wider(
      names_from = sto,
      values_from = obs_value
    ) |>
    select(ref_area, ref_sector, accounting_entry,time_period, P72, P72F) |>
    filter(P72F > P72)

  ## SIT43----------------------------------------------------------------------------------------

check <- data |>
  filter(sto == "D43_I9")

if(nrow(check) > 0){
  sit43 <- data |>
    filter(
      sto %in% c("D43", "D43_I9", "D43_J9")
    ) |>
    pivot_wider(
      names_from = sto,
      values_from = obs_value
    ) |>
    select(ref_area, ref_sector, accounting_entry,time_period, D43, D43_I9, D43_J9) |>
    rowwise() |>
    mutate(`D43_I9 + D43_J9` = sum(c(D43_I9, D43_J9), na.rm = TRUE),
           check = round(D43 - `D43_I9 + D43_J9`, rounding)) |>
    filter(abs(check) > threshold) |>
    filter(if_all(c(D43_I9,D43_J9), ~ !is.na(.x )))}
rm(check)

  ## SIT44----------------------------------------------------------------------------------------
check <- data |>
  filter(sto == "D43_B6")

if(nrow(check) > 0){
  sit44 <- data |>
    filter(
      sto %in% c("D43", "D43_B6", "D43_D6")
    ) |>
    pivot_wider(
      names_from = sto,
      values_from = obs_value
    ) |>
    select(ref_area, ref_sector, accounting_entry,time_period, D43, D43_B6, D43_D6) |>
    rowwise() |>
    mutate(`D43_B6 + D43_D6` = sum(c(D43_B6, D43_D6), na.rm = TRUE),
           check = round(D43 - `D43_B6 + D43_D6`, rounding)) |>
    filter(abs(check) > threshold) |>
    filter(if_all(c(D43_B6,D43_D6), ~ !is.na(.x )))}
rm(check)

## SIT45----------------------------------------------------------------------------------------
  sit45 <- data |>
    filter(
      sto %in% c("D4N", "D42", "D43", "D44", "D45")
    ) |>
    pivot_wider(
      names_from = sto,
      values_from = obs_value
    ) |>
    select(ref_area, ref_sector, accounting_entry,time_period, D42, D43, D44, D45, D4N) |>
    rowwise() |>
    mutate(`D4N_calc` = sum(c(D42, D43, D44, D45), na.rm = TRUE),
           check = round(D4N - `D4N_calc`, rounding)) |>
    filter(abs(check) > threshold)

## SIT46----------------------------------------------------------------------------------------
sit46 <- data |>
  filter(
    sto %in% c("D4N", "D42",  "D44", "D45"),
    ref_sector == "S13",
    accounting_entry == "D"
  ) |>
  pivot_wider(
    names_from = sto,
    values_from = obs_value
  ) |>
  select(ref_area, ref_sector, accounting_entry,time_period, D42,  D44, D45, D4N) |>
  rowwise() |>
  mutate(`D4N_calc` = sum(c(D42, D44, D45), na.rm = TRUE),
         check = round(D4N - `D4N_calc`, rounding)) |>
  filter(abs(check) > threshold)

## SIT47----------------------------------------------------------------------------------------
sit47 <- data |>
  filter(
    sto %in% c("D4N", "D45"),
    ref_sector %in% c("S1M", "S14", "S15"),
    accounting_entry == "D"
  ) |>
  pivot_wider(
    names_from = sto,
    values_from = obs_value
  ) |>
  select(ref_area, ref_sector, accounting_entry,time_period, D45, D4N) |>
  mutate(check = round(D4N - D45, rounding)) |>
  filter(abs(check) > threshold)

## SIT49----------------------------------------------------------------------------------------
sit49 <- data |>
  filter(
    sto %in% c("D7N", "D71", "D72", "D7")
  ) |>
  pivot_wider(
    names_from = sto,
    values_from = obs_value
  ) |>
  select(ref_area, ref_sector, accounting_entry,time_period, D71, D72, D7N,D7) |>
  rowwise() |>
  mutate(`D7_calc` = sum(c(D71, D72, D7N), na.rm = TRUE),
         check = round(D7 - `D7_calc`, rounding)) |>
  filter(abs(check) > threshold)

## SIT50----------------------------------------------------------------------------------------
check <- data |>
  filter(ref_sector == "S11", sto == "D71", accounting_entry == "D")

if(nrow(check) > 0){
sit50 <- data |>
  filter(
    sto %in% c("D7N", "D71", "D7"),
    accounting_entry == "D",
    ref_sector %in% c("S11", "S1M", "S14", "S15")
  ) |>
  pivot_wider(
    names_from = sto,
    values_from = obs_value
  ) |>
  select(ref_area, ref_sector, accounting_entry,time_period, D71, D7N,D7) |>
  rowwise() |>
  mutate(`D71 +D7N` = sum(c(D71,  D7N), na.rm = TRUE),
         check = round(D7 - `D71 +D7N`, rounding)) |>
  filter(abs(check) > threshold) }
rm(check)

## SIT51----------------------------------------------------------------------------------------
  sit51 <- data |>
    filter(
      sto %in% c("D7N", "D74", "D75", "D76"),
      accounting_entry == "D",
      ref_sector %in% c("S1", "S13", "S2")
    ) |>
    pivot_wider(
      names_from = sto,
      values_from = obs_value
    ) |>
    select(ref_area, ref_sector, accounting_entry,time_period, D74,  D75, D76, D7N) |>
    rowwise() |>
    mutate(`D7N_cal` = sum(c(D74,  D75, D76), na.rm = TRUE),
           check = round(D7N - `D7N_cal`, rounding)) |>
    filter(abs(check) > threshold)

## SIT52----------------------------------------------------------------------------------------
check <- data |>
  filter(ref_sector == "S11", sto == "D75")

if(nrow(check) > 0){
sit52 <- data |>
  filter(
    sto %in% c("D7N", "D75"),
    ref_sector %in% c("S11", "S12", "S1M", "S14", "S15")
  ) |>
  pivot_wider(
    names_from = sto,
    values_from = obs_value
  ) |>
  select(ref_area, ref_sector, accounting_entry,time_period, D75,  D7N) |>
  rowwise() |>
  mutate(check = round(D7N - D75, rounding)) |>
  filter(abs(check) > threshold) }
rm(check)

## SIT53----------------------------------------------------------------------------------------
sit53 <- data |>
  filter(
    sto %in% c("D7N", "D74", "D75"),
    accounting_entry == "C",
    ref_sector %in% c("S1", "S13", "S2")
  ) |>
  pivot_wider(
    names_from = sto,
    values_from = obs_value
  ) |>
  select(ref_area, ref_sector, accounting_entry,time_period, D74,  D75, D7N) |>
  rowwise() |>
  mutate(`D7N_cal` = sum(c(D74,  D75), na.rm = TRUE),
         check = round(D7N - `D7N_cal`, rounding)) |>
  filter(abs(check) > threshold)

## SIT54----------------------------------------------------------------------------------------
check <- data |>
  filter(sto == "D91")

if(nrow(check) > 0){
sit54 <- data |>
  filter(
    sto %in% c("D9", "D91", "D9N")
  ) |>
  pivot_wider(
    names_from = sto,
    values_from = obs_value
  ) |>
  select(ref_area, ref_sector, accounting_entry,time_period, D91,  D9N, D9) |>
  rowwise() |>
  mutate(`D91 +D9N` = sum(c(D91,  D9N), na.rm = TRUE),
         check = round(D9 - `D91 +D9N`, rounding)) |>
  filter(abs(check) > threshold) }
rm(check)

## SIT55----------------------------------------------------------------------------------------
check <- data |>
  filter(sto == "D9N")

if(nrow(check) > 0){
  sit55 <- data |>
    filter(
      sto %in% c("D9", "D9N"),
      ref_sector != "S2"
    ) |>
    pivot_wider(
      names_from = sto,
      values_from = obs_value
    ) |>
    select(ref_area, ref_sector, accounting_entry,time_period,  D9N, D9) |>
    mutate(check = round(D9 - D9N, rounding)) |>
    filter(abs(check) > threshold) }
rm(check)

## SIT56----------------------------------------------------------------------------------------

  sit56 <- data |>
    filter(
      sto %in% c("D9N", "D92", "D99")
    ) |>
    pivot_wider(
      names_from = sto,
      values_from = obs_value
    ) |>
    select(ref_area, ref_sector, accounting_entry,time_period,  D92, D99, D9N) |>
  rowwise() |>
    mutate(`D92 +D99` = sum(c(D92,  D99), na.rm = TRUE),
           check = round(D9N - `D92 +D99`, rounding)) |>
    filter(abs(check) > threshold)

## SIT57----------------------------------------------------------------------------------------
check <- data |>
  filter(sto == "D99", ref_sector == "S1")

if(nrow(check) > 0){

sit57 <- data |>
  filter(
    sto %in% c("D9N", "D99"),
    accounting_entry == "D",
    !ref_sector %in% c("S13", "S2")
  ) |>
  pivot_wider(
    names_from = sto,
    values_from = obs_value
  ) |>
  select(ref_area, ref_sector, accounting_entry,time_period,  D9N, D99) |>
  mutate(check = round(D9N - D99, rounding)) |>
  filter(abs(check) > threshold) }
rm(check)

## SIT58----------------------------------------------------------------------------------------

check <- data |>
  filter(sto == "D72", ref_sector == "S11")

if(nrow(check) > 0){
sit58 <- data |>
  filter(
    sto %in% c("D7", "D72", "D7N"),
    accounting_entry == "C",
    ref_sector %in% c("S11", "S1M", "S14", "S15")
  ) |>
  pivot_wider(
    names_from = sto,
    values_from = obs_value
  ) |>
  select(ref_area, ref_sector, accounting_entry,time_period,  D72, D7N, D7) |>
  rowwise() |>
  mutate(`D7N +D72` = sum(c(D7N,  D72), na.rm = TRUE),
         check = round(D7 - `D7N +D72`, rounding)) |>
  filter(abs(check) > threshold)}
rm(check)

# Balancing items-------------------------------------------------------------------------------

  ## BI01-----------------------------------------------------------------------------------------
  BI01 <- data |>
    filter(
      ref_sector == "S1",
      sto %in% c("P1", "P2", "B1GQ", "D21X31")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto,
                values_from = obs_value) |>
    select(ref_area, ref_sector, time_period, P1.C, P2.D, D21X31.C, B1GQ.B ) |>
    rowwise() |>
    mutate(
      `P1.C - P2.D + D21X31.C` = P1.C - P2.D + D21X31.C,
      check = round(B1GQ.B - `P1.C - P2.D + D21X31.C`, rounding)
    ) |>
    filter(abs(check) > threshold)

  ## BI02-----------------------------------------------------------------------------------------

  BI02 <- data |>
    filter(
      ref_sector %in% c("S1", "S1N"),
      sto %in% c("D21", "D31", "D21X31")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto,
                values_from = obs_value) |>
    select(ref_area, ref_sector, time_period, D21X31.C, D21.D, D31.C) |>
    rowwise() |>
    mutate(
      `D21.D - D31.C` = D21.D - D31.C,
      check = round(D21X31.C - `D21.D - D31.C`, rounding)
    ) |>
    filter(abs(check) > threshold)

  ## BI03-----------------------------------------------------------------------------------------

  BI03 <- data |>
    filter(
      ref_sector == "S1N",
      sto %in% c("B1G", "D21X31"),
      accounting_entry != "D"
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto,
                values_from = obs_value) |>
    select(ref_area, ref_sector, time_period, B1G.B, D21X31.C) |>
    rowwise() |>
    mutate(check = round(B1G.B - D21X31.C, rounding)) |>
    filter(abs(check) > threshold)

  ## BI04-----------------------------------------------------------------------------------------
  BI04 <- data |>
    filter(
      sto %in% c("B1G", "P1", "P2")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto,
                values_from = obs_value) |>
    select(ref_area, ref_sector, time_period, B1G.B, P1.C, P2.D) |>
    rowwise() |>
    mutate(
      `P1.C - P2.D` = P1.C - P2.D,
      check = round(B1G.B - `P1.C - P2.D`, rounding)
    ) |>
    filter(abs(check) > threshold)

  ## BI05-----------------------------------------------------------------------------------------

  BI05 <- data |>
    filter(
      ref_sector == "S1",
      sto %in% c("B1GQ", "B1NQ", "P51C"),
      accounting_entry != "C"
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto,
                values_from = obs_value) |>
    select(ref_area, ref_sector, time_period, B1NQ.B, B1GQ.B, P51C.D) |>
    rowwise() |>
    mutate(
      `B1GQ.B - P51C.D` = B1GQ.B - P51C.D,
      check = round(B1NQ.B - `B1GQ.B - P51C.D`, rounding)
    ) |>
    filter(abs(check) > threshold)


  ## BI07-----------------------------------------------------------------------------------------
  BI07 <- data |>
    filter(
      ref_sector != "S1N",
      sto %in% c("B1G", "B1N", "P51C"),
      accounting_entry != "D"
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto,
                values_from = obs_value) |>
    select(ref_area, ref_sector, time_period, B1N.B, P51C.C,B1G.B) |>
    rowwise() |>
    mutate(
      `B1N.B + P51C.C` = B1N.B + P51C.C,
      check = round(B1G.B - `B1N.B + P51C.C`, rounding)
    ) |>
    filter(abs(check) > threshold)

  ## BI08-----------------------------------------------------------------------------------------
  BI08 <- data |>
    filter(
      ref_sector %in% c("S1","S2"),
      sto %in% c("B1GQ", "P3", "P5", "P6", "P7")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    select(-ref_sector) |>
    pivot_wider(names_from = sto,
                values_from = obs_value) |>
    select(ref_area,  time_period, P3.D, P5.D,P6.D,P7.C,B1GQ.B) |>
    rowwise() |>
    mutate(
      `B1GQ_cal` = P3.D + P5.D + P6.D - P7.C,
      check = round(B1GQ.B - `B1GQ_cal`, rounding)
    ) |>
    filter(abs(check) > threshold)

  ## BI09-----------------------------------------------------------------------------------------
  BI09 <- data |>
    filter(
      ref_sector %in% c("S1"),
      sto %in% c("B1GQ", "B2A3G", "D1", "D2", "D3")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto,
                values_from = obs_value) |>
    select(ref_area, ref_sector, time_period, B2A3G.B, D1.D,D3.C,D2.D,,B1GQ.B) |>
    rowwise() |>
    mutate(
      `B1GQ_cal` = B2A3G.B + D1.D + D2.D - D3.C,
      check = round(B1GQ.B - `B1GQ_cal`, rounding)
    ) |>
    filter(abs(check) > threshold)

  ## BI10-----------------------------------------------------------------------------------------
  BI10 <- data |>
    filter(
      ref_sector != "S1",
      sto %in% c("B1G", "B2A3G", "D1", "D2", "D3")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto,
                values_from = obs_value) |>
    select(ref_area, ref_sector, time_period, B2A3G.B, D1.D,D3.C,D2.D,,B1G.B) |>
    rowwise() |>
    mutate(
      `B1G_cal` = B2A3G.B + D1.D + D2.D - D3.C,
      check = round(B1G.B - `B1G_cal`, rounding)
    ) |>
    filter(abs(check) > threshold)


  ## BI13-----------------------------------------------------------------------------------------
  check <- data |>
  filter(ref_sector %in% c("S11", "S12"), sto == "B4G")
if(nrow(check)>0){

  BI13 <- data |>
    filter(
      ref_sector %in% c("S11","S12"),
      sto %in% c("B4G", "B2A3G", "D4","D41", "D44", "D45"),
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto,
                values_from = obs_value) |>
    select(ref_area, ref_sector, time_period, B2A3G.B, D4.C,D41.D,D44.D,D45.D, B4G.B) |>
    rowwise() |>
    mutate(
      `B4G_cal` = sum(c(B2A3G.B,D4.C,-D41.D,-D44.D,-D45.D),na.rm = TRUE),
      check = round(B4G.B - `B4G_cal`, rounding)
    ) |>
    filter(abs(check) > threshold)}
rm(check)

  ## BI14-----------------------------------------------------------------------------------------
check <- data |>
  filter(ref_sector %in% c("S1"), sto == "D4")
if(nrow(check)>0){

BI14 <- data |>
    filter(
      ref_sector %in% c("S1"),
      sto %in% c("B5G", "B2A3G", "D1","D2", "D3", "D4"),
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto,
                values_from = obs_value) |>
    select(ref_area, ref_sector, time_period, B2A3G.B, D1.C,D2.C,D3.D,D4.C, D4.D,B5G.B) |>
    rowwise() |>
    mutate(
      `B5G_cal` = sum(c(B2A3G.B,D1.C,D2.C,-D3.D,D4.C,-D4.D),na.rm = TRUE),
      check = round(B5G.B - `B5G_cal`, rounding)
    ) |>
    filter(abs(check) > threshold)}
rm(check)

  ## BI15-----------------------------------------------------------------------------------------
check <- data |>
  filter(ref_sector %in% c("S11"), sto == "B5G")
if(nrow(check)>0){
BI15 <- data |>
    filter(
      !ref_sector %in% c("S1", "S13", "S14", "S1M"),
      sto %in% c("B5G", "B2A3G",  "D4"),
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto,
                values_from = obs_value) |>
    select(ref_area, ref_sector, time_period, B2A3G.B, D4.C,D4.D, B5G.B) |>
    rowwise() |>
    mutate(
      `B5G_cal` = sum(c(B2A3G.B,D4.C,-D4.D),na.rm = TRUE),
      check = round(B5G.B - `B5G_cal`, rounding)
    ) |>
    filter(abs(check) > threshold)}
rm(check)

  ## BI16-----------------------------------------------------------------------------------------
  BI16 <- data |>
    filter(
      ref_sector %in% c("S13"),
      sto %in% c("B5G", "B2A3G", "D2", "D3" , "D4")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto,
                values_from = obs_value) |>
    select(ref_area, ref_sector, time_period, B2A3G.B, D2.C,D3.D,D4.C,D4.D, B5G.B) |>
    rowwise() |>
    mutate(
      `B5G_cal` = sum(c(B2A3G.B,D2.C,-D3.D,D4.C,-D4.D),na.rm = TRUE),
      check = round(B5G.B - `B5G_cal`, rounding)
    ) |>
    filter(abs(check) > threshold)

  ## BI17-----------------------------------------------------------------------------------------

check <- data |>
  filter(ref_sector %in% c("S1M"), sto == "D4")
if(nrow(check)>0){
 BI17 <- data |>
    filter(
      ref_sector %in% c("S1M", "S14", "S15"),
      sto %in% c("B5G", "B2A3G", "D1",  "D4")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto,
                values_from = obs_value) |>
    select(ref_area, ref_sector, time_period, B2A3G.B, D1.C,D4.C,D4.D, B5G.B) |>
    rowwise() |>
    mutate(
      `B5G_cal` = sum(c(B2A3G.B,D1.C,D4.C,-D4.D),na.rm = TRUE),
      check = round(B5G.B - `B5G_cal`, rounding)
    ) |>
    filter(abs(check) > threshold)}
rm(check)

## BI18-----------------------------------------------------------------------------------------

check <- data |>
  filter(ref_sector %in% c("S1M"), sto == "D4")
if(nrow(check)>0){
  BI18 <- data |>
    filter(
      ref_sector %in% c("S1", "S2"),
      sto %in% c("B5G", "B1GQ", "D1",  "D2", "D3", "D4")
    ) |>
    unite("sto", c(ref_sector,sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto,
                values_from = obs_value) |>
    select(ref_area, time_period, S1.B1GQ.B,S2.D1.D,S2.D1.C,S2.D2.D,S2.D3.C,S2.D4.D,S2.D4.C,S1.B5G.B) |>
    rowwise() |>
    mutate(
      `B5G_cal` = sum(c(S1.B1GQ.B,-S2.D1.D,S2.D1.C,-S2.D2.D,S2.D3.C,-S2.D4.D,S2.D4.C),na.rm = TRUE),
      check = round(S1.B5G.B - `B5G_cal`, rounding)
    ) |>
    filter(abs(check) > threshold)}
rm(check)

  ## BI19-----------------------------------------------------------------------------------------
check <- data |>
  filter(ref_sector %in% c("S1"), sto == "D5", accounting_entry == "D")
if(nrow(check)>0){

BI19 <- data |>
    filter(
      ref_sector %in% c("S1"),
      sto %in% c("B5G", "D5", "D61","D62", "D7", "B6G")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto,
                values_from = obs_value) |>
    select(ref_area, ref_sector, time_period, B5G.B,D5.C,D5.D,D61.C,D61.D,D62.C,D62.D,D7.C,D7.D,B6G.B) |>
    rowwise() |>
    mutate(
      `B6G_cal` = sum(c(B5G.B,D5.C,-D5.D,D61.C,-D61.D,D62.C,-D62.D,D7.C,-D7.D),na.rm = TRUE),
      check = round(B6G.B - `B6G_cal`, rounding)
    ) |>
    filter(abs(check) > threshold)}
rm(check)

  ## BI20-----------------------------------------------------------------------------------------
check <- data |>
  filter(ref_sector %in% c("S11"), sto == "B5G")
if(nrow(check)>0){

BI20 <- data |>
    filter(
      ref_sector %in% c("S11", "S12", "S15"),
      sto %in% c("B5G", "D5", "D61","D62", "D7", "B6G")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto,
                values_from = obs_value) |>
    select(ref_area, ref_sector, time_period, B5G.B,D5.D,D61.C,D62.D,D7.C,D7.D,B6G.B) |>
    rowwise() |>
    mutate(
      `B6G_cal` = sum(c(B5G.B,-D5.D,D61.C,-D62.D,D7.C,-D7.D),na.rm = TRUE),
      check = round(B6G.B - `B6G_cal`, rounding)
    ) |>
    filter(abs(check) > threshold)}
rm(check)

  ## BI21-----------------------------------------------------------------------------------------
  BI21 <- data |>
    filter(
      ref_sector %in% c("S13"),
      sto %in% c("B5G", "D5", "D61","D62", "D7", "B6G")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto,
                values_from = obs_value) |>
    select(ref_area, ref_sector, time_period, B5G.B,D5.C,D5.D,D61.C,D62.D,D7.C,D7.D,B6G.B) |>
    rowwise() |>
    mutate(
      `B6G_cal` = sum(c(B5G.B,D5.C,-D5.D,D61.C,-D62.D,D7.C,-D7.D),na.rm = TRUE),
      check = round(B6G.B - `B6G_cal`, rounding)
    ) |>
    filter(abs(check) > threshold)

  ## BI22-----------------------------------------------------------------------------------------
check <- data |>
  filter(ref_sector %in% c("S1M"), sto == "B5G")
if(nrow(check)>0){
BI22 <- data |>
    filter(
      ref_sector %in% c("S14", "S1M"),
      sto %in% c("B5G", "D5", "D61","D62", "D7", "B6G")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto,
                values_from = obs_value) |>
    select(ref_area, ref_sector, time_period, B5G.B,D5.C,D5.D,D61.C,D62.D,D7.C,D7.D,B6G.B) |>
    rowwise() |>
    mutate(
      `B6G_cal` = sum(c(B5G.B,D5.C,-D5.D,D61.C,-D62.D,D7.C,-D7.D),na.rm = TRUE),
      check = round(B6G.B - `B6G_cal`, rounding)
    ) |>
    filter(abs(check) > threshold)}
rm(check)

## BI23-----------------------------------------------------------------------------------------

  BI23 <- data |>
    filter(
      ref_sector %in% c("S1", "S2"),
      sto %in% c("B6G", "B5G","D5","D61","D62","D7")
    ) |>
    unite("sto", c(ref_sector,sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto,
                values_from = obs_value) |>
    select(ref_area,  time_period, S1.B5G.B,S2.D5.D,S2.D5.C,S2.D61.D,S2.D61.C,S2.D62.D,S2.D62.C,S2.D7.D,S2.D7.C, S1.B6G.B) |>
    rowwise() |>
    mutate(
      `B6G_cal` = sum(c(S1.B5G.B,-S2.D5.D,S2.D5.C,-S2.D61.D,S2.D61.C,-S2.D62.D,S2.D62.C,-S2.D7.D,S2.D7.C),na.rm = TRUE),
      check = round(S1.B6G.B - `B6G_cal`, rounding)
    ) |>
    filter(abs(check) > threshold)

  ## BI24-----------------------------------------------------------------------------------------
check <- data |>
  filter(ref_sector %in% c("S1"), sto == "D63", accounting_entry == "C")
if(nrow(check)>0){
 BI24 <- data |>
    filter(
      ref_sector %in% c("S1", "S1M", "S14", "S15"),
      sto %in% c("B7G", "B6G", "D63")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto,
                values_from = obs_value) |>
    select(ref_area, ref_sector, time_period, B6G.B,D63.C,D63.D,B7G.B) |>
    rowwise() |>
    mutate(
      `B7G_cal` = sum(c(B6G.B,D63.C,-D63.D),na.rm = TRUE),
      check = round(B7G.B - `B7G_cal`, rounding)
    ) |>
    filter(abs(check) > threshold)}
rm(check)

  ## BI25-----------------------------------------------------------------------------------------
  # B7G does not exist
  # BI25 <- data |>
  #   filter(
  #     ref_sector %in% c("S11", "S12"),
  #     sto %in% c("B7G", "B6G")
  #   ) |>
  #   unite("sto", c(sto, accounting_entry), sep = ".") |>
  #   pivot_wider(names_from = sto,
  #               values_from = obs_value) |>
  #   select(ref_area, ref_sector, time_period, B6G.B,B7G.B) |>
  #   rowwise() |>
  #   mutate(
  #     check = round(B7G.B - B6G.B, rounding)
  #   ) |>
  #   filter(abs(check) > threshold)
  #

  ## BI26-----------------------------------------------------------------------------------------
  BI26 <- data |>
    filter(
      ref_sector %in% c("S13", "S15"),
      sto %in% c("B7G", "B6G", "D63"),
      accounting_entry != "C"
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto,
                values_from = obs_value) |>
    select(ref_area, ref_sector, time_period, B6G.B,D63.D,B7G.B) |>
    rowwise() |>
    mutate(
      `B7G_cal` = sum(c(B6G.B,-D63.D),na.rm = TRUE),
      check = round(B7G.B - `B7G_cal`, rounding)
    ) |>
    filter(abs(check) > threshold)


  ## BI28-----------------------------------------------------------------------------------------
  BI28 <- data |>
    filter(
      ref_sector %in% c("S1", "S1M", "S14"),
      sto %in% c("B8G", "B6G", "D8", "P3")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto,
                values_from = obs_value) |>
    select(ref_area, ref_sector, time_period, B6G.B,D8.C,D8.D,P3.D,B8G.B) |>
    rowwise() |>
    mutate(
      `B8G_cal` = sum(c(B6G.B,D8.C,-D8.D,-P3.D),na.rm = TRUE),
      check = round(B8G.B - `B8G_cal`, rounding)
    ) |>
    filter(abs(check) > threshold)

  ## BI29-----------------------------------------------------------------------------------------
check <- data |>
  filter(ref_sector %in% c("S11"), sto == "B6G", accounting_entry == "B")
if(nrow(check)>0){

 BI29 <- data |>
    filter(
      ref_sector %in% c("S11", "S12"),
      sto %in% c("B8G", "B6G", "D8")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto,
                values_from = obs_value) |>
    select(ref_area, ref_sector, time_period, B6G.B,D8.D,B8G.B) |>
    rowwise() |>
    mutate(
      `B6G.B - D8.D` = sum(c(B6G.B,-D8.D),na.rm = TRUE),
      check = round(B8G.B - `B6G.B - D8.D`, rounding)
    ) |>
    filter(abs(check) > threshold)}
rm(check)

  ## BI30-----------------------------------------------------------------------------------------
  BI30 <- data |>
    filter(
      ref_sector %in% c("S13", "S15"),
      sto %in% c("B8G", "B6G", "D8", "P3"),
      accounting_entry != "C"
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto,
                values_from = obs_value) |>
    select(ref_area, ref_sector, time_period, B6G.B,D8.D,P3.D,B8G.B) |>
    rowwise() |>
    mutate(
      `B6G.B - D8.D - P3.D` = sum(c(B6G.B,-D8.D, -P3.D),na.rm = TRUE),
      check = round(B8G.B - `B6G.B - D8.D - P3.D`, rounding)
    ) |>
    filter(abs(check) > threshold)

## BI31-----------------------------------------------------------------------------------------
check <- data |>
  filter(ref_sector %in% c("S2"), sto == "D8")
if(nrow(check)>0){

BI31 <- data |>
  filter(
    ref_sector %in% c("S1", "S2"),
    sto %in% c("B8G", "B6G", "D8", "P3"),
    accounting_entry != "C"
  ) |>
  unite("sto", c(ref_sector,sto, accounting_entry), sep = ".") |>
  pivot_wider(names_from = sto,
              values_from = obs_value) |>
  select(ref_area, time_period, S1.B6G.B,S2.D8.D,S2.D8.C,S1.P3.D,S1.B8G.B) |>
  rowwise() |>
  mutate(
    `B8G_cal` = sum(c(S1.B6G.B,-S2.D8.D,S2.D8.C,-S1.P3.D),na.rm = TRUE),
    check = round(B8G.B - `B6G_calc`, rounding)
  ) |>
  filter(abs(check) > threshold)}
rm(check)

  ## BI32-----------------------------------------------------------------------------------------
  BI32 <- data |>
    filter(
      !ref_sector %in% c("S1", "S2"),
      sto %in% c("B8G", "B101", "D9", "P51C")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto,
                values_from = obs_value) |>
    select(ref_area, ref_sector, time_period, B8G.B,D9.C,D9.D,P51C.C,B101.B) |>
    rowwise() |>
    mutate(
      `B101_calc` = sum(c(B8G.B,D9.C,-D9.D,-P51C.C),na.rm = TRUE),
      check = round(B101.B - `B101_calc`, rounding)
    ) |>
    filter(abs(check) > threshold)

  ## BI33-----------------------------------------------------------------------------------------
  BI33 <- data |>
    filter(
      ref_sector %in% c("S2"),
      sto %in% c("B101", "D9", "B12")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto,
                values_from = obs_value) |>
    select(ref_area, ref_sector, time_period, B12.B,D9.C,D9.D,B101.B) |>
    rowwise() |>
    mutate(
      `B101_calc` = sum(c(B12.B,D9.D,-D9.C),na.rm = TRUE),
      check = round(B101.B - `B101_calc`, rounding)
    ) |>
    filter(abs(check) > threshold)

  ## BI35-----------------------------------------------------------------------------------------
  BI35 <- data |>
    filter(
      !ref_sector %in% c("S2"),
      sto %in% c("B9", "B8G", "D9", "P5", "NP")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto,
                values_from = obs_value) |>
    select(ref_area, ref_sector, time_period, B8G.B,D9.C,D9.D,P5.D,NP.D,B9.B) |>
    rowwise() |>
    mutate(
      `B9_calc` = sum(c(B8G.B,D9.C,-D9.D,-P5.D,-NP.D),na.rm = TRUE),
      check = round(B9.B - `B9_calc`, rounding)
    ) |>
    filter(abs(check) > threshold)

  ## BI36-----------------------------------------------------------------------------------------
  BI36 <- data |>
    filter(
      ref_sector %in% c("S2"),
      sto %in% c("B9", "B12", "D9", "NP")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto,
                values_from = obs_value) |>
    select(ref_area, ref_sector, time_period, B12.B,D9.C,D9.D,NP.C,B9.B) |>
    rowwise() |>
    mutate(
      `B9_calc` = sum(c(B12.B,D9.D,-D9.C,-NP.C),na.rm = TRUE),
      check = round(B9.B - `B9_calc`, rounding)
    ) |>
    filter(abs(check) > threshold)

  ## BI37-----------------------------------------------------------------------------------------
  BI37 <- data |>
    filter(
      ref_sector %in% c("S13"),
      sto %in% c("B9", "OTR", "OTE")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto,
                values_from = obs_value) |>
    select(ref_area, ref_sector, time_period, OTR.C, OTE.D,B9.B) |>
    rowwise() |>
    mutate(
      `OTR.C - OTE.D` = sum(c(OTR.C,-OTE.D),na.rm = TRUE),
      check = round(B9.B - `OTR.C - OTE.D`, rounding)
    ) |>
    filter(abs(check) > threshold)

  ## BI38-----------------------------------------------------------------------------------------
  BI38 <- data |>
    filter(
      !ref_sector %in% c("S2"),
      sto %in% c("B9", "B101", "P51C", "P5", "NP")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto,
                values_from = obs_value) |>
    select(ref_area, ref_sector, time_period, B101.B, P51C.C, P5.D,NP.D,B9.B) |>
    rowwise() |>
    mutate(
      `B9_calc` = sum(c(B101.B,P51C.C,-P5.D,-NP.D),na.rm = TRUE),
      check = round(B9.B - `B9_calc`, rounding)
    ) |>
    filter(abs(check) > threshold)

  ## BI39-----------------------------------------------------------------------------------------
  BI39 <- data |>
    filter(
      ref_sector %in% c("S2"),
      sto %in% c("B9", "B101", "NP")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto,
                values_from = obs_value) |>
    select(ref_area, ref_sector, time_period, B101.B, NP.C,B9.B) |>
    rowwise() |>
    mutate(
      `B101.B - NP.C` = sum(c(B101.B,-NP.C),na.rm = TRUE),
      check = round(B9.B - `B101.B - NP.C`, rounding)
    ) |>
    filter(abs(check) > threshold)

## BI40-----------------------------------------------------------------------------------------
BI40 <- data |>
  filter(
    ref_sector %in% c("S1","S2"),
    sto %in% c("B9", "B8G", "D9", "P5", "NP")
  ) |>
  unite("sto", c(ref_sector,sto, accounting_entry), sep = ".") |>
  pivot_wider(names_from = sto,
              values_from = obs_value) |>
  select(ref_area,  time_period, S1.B8G.B,S2.D9.D,S2.D9.C,S1.P5.D,S1.NP.D, S1.B9.B) |>
  rowwise() |>
  mutate(
    `B9_calc` = sum(c(S1.B8G.B,-S2.D9.D,S2.D9.C,-S1.P5.D,-S1.NP.D),na.rm = TRUE),
    check = round(S1.B9.B - `B9_calc`, rounding)
  ) |>
  filter(abs(check) > threshold)

  ## BI41-----------------------------------------------------------------------------------------
  BI41 <- data |>
    filter(
      ref_sector %in% c("S1", "S2"),
      sto %in% c("B101", "P5", "P51C")
    ) |>
    unite("sto", c(ref_sector,sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto,
                values_from = obs_value) |>
    select(ref_area, time_period, S2.B101.B, S1.P5.D, S1.P51C.C, S1.B101.B) |>
    rowwise() |>
    mutate(
      `S1.B101.B_cal` = sum(c(-S2.B101.B,S1.P5.D,-S1.P51C.C),na.rm = TRUE),
      check = round(S1.B101.B - `S1.B101.B_cal`, rounding)
    ) |>
    filter(abs(check) > threshold)

  ## BI42-----------------------------------------------------------------------------------------
  BI42 <- data |>
    filter(
      ref_sector %in% c("S2"),
      sto %in% c("B11", "P6", "P7")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto,
                values_from = obs_value) |>
    select(ref_area, time_period, P7.C,P6.D,B11.B) |>
    rowwise() |>
    mutate(
      `P7.C - P6.D` = sum(c(P7.C,-P6.D),na.rm = TRUE),
      check = round(B11.B - `P7.C - P6.D`, rounding)
    ) |>
    filter(abs(check) > threshold)

  ## BI43-----------------------------------------------------------------------------------------
 check <- data |>
  filter(sto == "D8", ref_sector == "S2")

if(nrow(check)>0){
 BI43 <- data |>
    filter(
      ref_sector %in% c("S2"),
      sto %in% c("B12","B11", "D1", "D2", "D3", "D4", "D5", "D61", "D62", "D7", "D8" )
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto,
                values_from = obs_value) |>
    select(ref_area, time_period, B11.B,,D1.D,D1.C,D2.D,D3.C,D4.D,D4.C,D5.D,D5.C,D61.D,D61.C,D62.D,D62.C,D7.D,D7.C,D8.D,D8.C, B12.B) |>
    rowwise() |>
    mutate(
      `B12.B_calc` = sum(c(B11.B,D1.D,-D1.C,D2.D,-D3.C,D4.D,-D4.C,D5.D,-D5.C,D61.D,-D61.C,D62.D,-D62.C,D7.D,-D7.C,D8.D,-D8.C),na.rm = TRUE),
      check = round(B12.B - `B12.B_calc`, rounding)
    ) |>
    filter(abs(check) > threshold)}
rm(check)

  ## BI44-----------------------------------------------------------------------------------------
  BI44 <- data |>
    filter(
      ref_sector %in% c("S13"),
      sto %in% c("OTR", "P1O", "D2", "D39", "D4", "D5", "D61", "D7", "D9" )
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto,
                values_from = obs_value) |>
    select(ref_area, time_period, P1O.C,D2.C,D39.C,D4.C,D5.C,D61.C,D7.C,D9.C,OTR.C) |>
    rowwise() |>
    mutate(
      `OTR.C_calc` = sum(c(P1O.C,D2.C,D39.C,D4.C,D5.C,D61.C,D7.C,D9.C),na.rm = TRUE),
      check = round(OTR.C - `OTR.C_calc`, rounding)
    ) |>
    filter(abs(check) > threshold)

  ## BI45-----------------------------------------------------------------------------------------
  BI45 <- data |>
    filter(
      ref_sector %in% c("S13"),
      sto %in% c("OTE","P2","P5","D1","D29","D3","D4","D5","D62","D632","D7","D8","D9","NP")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto,
                values_from = obs_value) |>
    select(ref_area, time_period, P2.D,P5.D,D1.D,D29.D,D3.D,D4.D,D5.D,D62.D,D632.D,D7.D,D8.D,D9.D,NP.D,OTE.D) |>
    rowwise() |>
    mutate(
      `OTE.D_calc` = sum(c(P2.D,P5.D,D1.D,D29.D,D3.D,D4.D,D5.D,D62.D,D632.D,D7.D,D8.D,D9.D,NP.D),na.rm = TRUE),
      check = round(OTE.D - `OTE.D_calc`, rounding)
    ) |>
    filter(abs(check) > threshold)

  ## BI46-----------------------------------------------------------------------------------------
  BI46 <- data |>
    filter(
      ref_sector %in% c("S13"),
      sto %in% c("P31", "D63")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto,
                values_from = obs_value) |>
    select(ref_area, time_period, P31.D,D63.D) |>
    rowwise() |>
    mutate(
      check = round(P31.D - `D63.D`, rounding)
    ) |>
    filter(abs(check) > threshold)

  ## BI47-----------------------------------------------------------------------------------------
  BI47 <- data |>
    filter(
      ref_sector %in% c("S13"),
      sto %in% c("P3", "P1", "P1O", "D632")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto,
                values_from = obs_value) |>
    select(ref_area, time_period, P1.C,P1O.C,D632.D,P3.D) |>
    rowwise() |>
    mutate(
      `P3.D_calc` = sum(c(P1.C,-P1O.C,D632.D),na.rm = TRUE),
      check = round(P3.D - `P3.D_calc`, rounding)
    ) |>
    filter(abs(check) > threshold)

  list_ur <- mget(ls(pattern = "ur")) |>
    keep(~ any(nrow(.x) > 0)) |>
    map(~ mutate(.x, across(everything(), ~ replace(.x, is.na(.x), "NaN"))))

  list_s1ss <- mget(ls(pattern = "s1ss")) |>
    keep(~ any(nrow(.x) > 0)) |>
    map(~ mutate(.x, across(everything(), ~ replace(.x, is.na(.x), "NaN"))))

  list_sit <- mget(ls(pattern = "sit")) |>
    keep(~ any(nrow(.x) > 0)) |>
    map(~ mutate(.x, across(everything(), ~ replace(.x, is.na(.x), "NaN"))))

  list_BI <- mget(ls(pattern = "BI")) |>
    keep(~ any(nrow(.x) > 0)) |>
    map(~ mutate(.x, across(everything(), ~ replace(.x, is.na(.x), "NaN"))))

  list_ir <- c(list_ur, list_s1ss, list_sit, list_BI)


  openxlsx::write.xlsx(list_ir,
                       file = paste0(paste0(output_sel, "/", as.character(format(Sys.time(), "%Y%m%d_%H%M%S")), ".xlsx")),
                       asTable = TRUE,
                       overwrite = TRUE
  )

  cli::cli_alert_success(paste0("File created at: ", output_sel, "/", as.character(format(Sys.time(), "%Y%m%d_%H%M%S")), ".xlsx"))
}
