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
nfsa_internal_consistency_T0800 <- function(dataset,
                                      output_sel = here::here("output", "internal"),
                                      threshold = 2,
                                      rounding = 1) {

  library(tidyverse)
  library(arrow)
  library(here)
  library(janitor)
  library(openxlsx)

  lookup <- nfsa::nfsa_sto_lookup

  data <- dataset |>
    mutate(obs_value = round_half_up(obs_value, rounding)) |>
    nfsa::nfsa_separate_id()



  # Uses vs Resources----
  ## UR01----

  ur01 <- data |>
    filter(
      sto %in% c(
        "D1", "D11", "D12", "D4", "D41", "D4N", "D42", "D422", "D43", "D44",
        "D441", "D442", "D443", "D45", "D41G", "D5", "D51", "D59", "D6",
        "D611", "D612", "D613", "D614","D61SC","D61", "D62", "D7", "D71", "D72",
        "D7N", "D74", "D75", "D8", "D9", "D91", "D9N", "D92", "D99"
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

  ## UR02----
  ur02 <- data |>
    filter(
      sto %in% c("D2", "D21", "D29"),
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
      check = round(`S1.D` - `S1.C + S2.D`, rounding)
    ) |>
    filter(abs(check) > threshold)

  ## UR03----
  ur03 <- data |>
    filter(
      sto %in% c("D3", "D31", "D39"),
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
      check = round(`S1.D + S2.C` - S1.C, rounding)
    ) |>
    filter(abs(check) > threshold)


  ## UR04----
  ur04 <- data |>
    filter(
      sto %in% c("D63", "D631", "D632", "P51C"),
      ref_sector %in% c("S1")
    ) |>
    pivot_wider(
      names_from = c(ref_sector, accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    select(ref_area, sto, time_period, S1.D, S1.C) |>
    rowwise() |>
    mutate(check = round(S1.D - S1.C, rounding)) |>
    filter(abs(check) > threshold)

  ## UR05----
  ur05 <- data |>
    filter(
      sto %in% c("D43", "D74", "D76"),
      ref_sector %in% c("S1", "S2"),
      accounting_entry == "D"
    ) |>
    pivot_wider(
      names_from = c(ref_sector, accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    select(ref_area, sto, time_period, S1.D, S2.D) |>
    rowwise() |>
    mutate(check = round(S1.D - S2.D, rounding)) |>
    filter(abs(check) > threshold)


  ## UR06----
  ur06 <- data |>
    filter(
      sto %in% c("NP"),
      ref_sector %in% c("S1", "S2")
    ) |>
    pivot_wider(
      names_from = c(ref_sector, accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    select(ref_area, sto, time_period, S1.D, S2.C) |>
    rowwise() |>
    mutate(check = round(S1.D + S2.C, rounding)) |>
    filter(abs(check) > threshold)


  ## UR07----
  ur07 <- data |>
    filter(
      sto %in% c("B9", "B9X9F"),
      ref_sector %in% c("S1", "S2")
    ) |>
    pivot_wider(
      names_from = ref_sector,
      values_from = obs_value,
      names_sep = "."
    ) |>
    select(ref_area, sto, time_period, S1, S2) |>
    rowwise() |>
    mutate(check = round(S1 + S2, rounding)) |>
    filter(abs(check) > threshold)

  ## UR08----
  ur08 <- data |>
    filter(
      sto %in% c("D12", "D611", "D612"),
      ref_sector %in% c("S1", "S2")
    ) |>
    pivot_wider(
      names_from = c(ref_sector,sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    select(ref_area,time_period, S1.D12.D,S2.D12.C,S1.D611.C, S1.D612.C, S2.D611.D, S2.D612.D) |>
    rowwise() |>
    mutate(`S1.D12.D + S2.D12.C` = round(S1.D12.D + S2.D12.C, rounding),
           `S1.D611.C + S1.D612.C + S2.D611.D + S2.D612.D` = round(S1.D611.C + S1.D612.C + S2.D611.D + S2.D612.D, rounding)) |>
    mutate(check = `S1.D12.D + S2.D12.C` - `S1.D611.C + S1.D612.C + S2.D611.D + S2.D612.D`) |>
    filter(abs(check) > threshold)

  ## UR09----
  ur09 <- data |>
    filter(
      sto %in% c("D442", "D614"),
      ref_sector %in% c("S1", "S2")
    ) |>
    pivot_wider(
      names_from = c(ref_sector,sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    select(ref_area,time_period, S1.D442.D,S2.D442.C,S1.D614.C,S2.D614.D) |>
    rowwise() |>
    mutate(`S1.D442.D+S2.D442.C` = round(S1.D442.D+S2.D442.C, rounding),
           `S1.D614.C+S2.D614.D` = round(S1.D614.C+S2.D614.D, rounding)) |>
    filter(`S1.D442.D+S2.D442.C` > `S1.D614.C+S2.D614.D`)

  # S1 vs Sum of Sub-sectors----
  ## S1SS01----

  s1ss01 <- data |>
    filter(ref_sector %in% c("S1", "S11", "S12", "S13", "S1M")) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    filter(sto %in% c(
      "P2.D", "P5.D", "P51G.D", "P5M.D", "P52.D", "P53.D", "D1.D", "D11.D", "D12.D", "D29.D", "D4.D",
      "D41.D", "D4N.D", "D44.D", "D441.D", "D442.D","D443.D", "D45.D", "D41G.D", "D5.D", "D51.D","D59.D", "D6.D", "D62.D",
      "D7.D", "D71.D", "D7N.D", "D75.D", "D8.D", "D9.D", "D9N.D",
      "D99.D", "P51C.D", "NP.D", "P1.C", "P11.C", "P12.C", "D39.C", "D4.C", "D41.C",
      "D4N.C", "D42.C", "D43.C", "D44.C", "D441.C", "D442.C", "D443.C", "D45.C", "D41G.C", "D6.C",
      "D61.C", "D611.C", "D612.C", "D613.C", "D614.C", "D61SC.C", "D7.C", "D72.C", "D7N.C", "D75.C", "D9.C", "D9N.C",
      "D92.C", "D99.C", "P51C.C", "B2A3G.B", "B4G.B", "B5G.B", "B6G.B",
      "B8G.B", "B101.B", "B9.B", "B9FX9._Z", "EMP.PS", "EMP.HW"
    )) |>
    pivot_wider(
      names_from = ref_sector,
      values_from = obs_value
    ) |>
    select(ref_area, sto, time_period, S1, S11, S12, S13, S1M) |>
    rowwise() |>
    mutate(`S11 + S12 + S13 + S1M` = sum(c(S11, S12, S13, S1M), na.rm = TRUE)) |>
    mutate(check = round(S1 - `S11 + S12 + S13 + S1M`, rounding)) |>
    filter(abs(check) > threshold)

  ## S1SS02----

  s1ss02 <- data |>
    filter(ref_sector %in% c("S1", "S13", "S1M")) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    filter(sto %in% c("P3.D", "P31.D","D63.D", "D631.D", "D632.D", "P13.C")) |>
    pivot_wider(
      names_from = ref_sector,
      values_from = obs_value
    ) |>
    select(ref_area, sto, time_period, S1, S13, S1M) |>
    rowwise() |>
    mutate(`S13 + S1M` = sum(c(S13, S1M), na.rm = TRUE)) |>
    mutate(check = round(S1 - `S13 + S1M`, rounding)) |>
    filter(abs(check) > threshold)

  ## S1SS03----

  s1ss03 <- data |>
    filter(ref_sector %in% c("S1", "S13")) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    filter(sto %in% c(
      "P32.D", "D3.D", "D31.D", "D39.D",  "D74.D", "D74_4Y", "D76.D", "D92.D",
      "D2.C", "D21.C", "D211.C", "D212.C", "D.214.C", "D29.C", "D5.C", "D51.C", "D59.C", "D74.C", "D91.C"
    )) |>
    pivot_wider(
      names_from = ref_sector,
      values_from = obs_value
    ) |>
    select(ref_area, sto, time_period, S1, S13) |>
    mutate(check = round(S1 - S13, rounding)) |>
    filter(abs(check) > threshold)

  ## S1SS04----

  s1ss04 <- data |>
    filter(ref_sector %in% c("S1", "S1N", "S11", "S12", "S13", "S1M")) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    filter(sto %in% c("D2.D", "D3.C")) |>
    pivot_wider(
      names_from = ref_sector,
      values_from = obs_value
    ) |>
    select(ref_area, sto, time_period, S1, S1N, S11, S12, S13, S1M) |>
    rowwise() |>
    mutate(`S1N + S11 + S12 + S13 + S1M` = sum(c(S1N, S11, S12, S13, S1M), na.rm = TRUE)) |>
    mutate(check = round(S1 - `S1N + S11 + S12 + S13 + S1M`, rounding)) |>
    filter(abs(check) > threshold)

  ## S1SS05----

  s1ss05 <- data |>
    filter(ref_sector %in% c("S1", "S1N")) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    filter(sto %in% c("D21.D", "D31.C", "D21X31.C")) |>
    pivot_wider(
      names_from = ref_sector,
      values_from = obs_value
    ) |>
    select(ref_area, sto, time_period, S1, S1N) |>
    mutate(check = round(S1 - S1N, rounding)) |>
    filter(abs(check) > threshold)

  ## S1SS06----

  s1ss06 <- data |>
    filter(ref_sector %in% c("S1", "S1M")) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    filter(sto %in% c("D61.D", "D611.D", "D612.D", "D613.D", "D614.D", "D61SC.D", "D1.C", "D11.C", "D12.C", "D62.C", "D63.C", "D631.C", "D632.C",  "D8.C", "B3G.B")) |>
    pivot_wider(
      names_from = ref_sector,
      values_from = obs_value
    ) |>
    select(ref_area, sto, time_period, S1, S1M) |>
    mutate(check = round(S1 - S1M, rounding)) |>
    filter(abs(check) > threshold)

  ## S1SS07----
  check <- data |>
    filter(ref_sector == "S11", accounting_entry == "D", sto == "D42")

  if(nrow(check)>0){
    s1ss07 <- data |>
      filter(ref_sector %in% c("S1", "S11", "S12")) |>
      unite("sto", c(sto, accounting_entry), sep = ".") |>
      filter(sto %in% c("D42.D", "D421.D", "D422.D", "D43.D")) |>
      pivot_wider(
        names_from = ref_sector,
        values_from = obs_value
      ) |>
      select(ref_area, sto, time_period, S1, S11, S12) |>
      rowwise() |>
      mutate(`S11 + S12` = sum(c(S11, S12), na.rm = TRUE)) |>
      filter(`S11 + S12` != 0) |>
      mutate(check = round(S1 - `S11 + S12`, rounding)) |>
      filter(abs(check) > threshold)}
  rm(check)

  ## S1SS08----
  check <- data |>
    filter(ref_sector == "S11", accounting_entry == "D", sto == "D44")

  if(nrow(check)>0){
    s1ss08 <- data |>
      filter(ref_sector %in% c("S1", "S11", "S12", "S13")) |>
      unite("sto", c(sto, accounting_entry), sep = ".") |>
      filter(sto %in% c("D44.D", "D441.D", "D442.D", "D443.D" )) |># different in T0801, includes S1M
      pivot_wider(
        names_from = ref_sector,
        values_from = obs_value
      ) |>
      select(ref_area, sto, time_period, S1, S11, S12, S13) |>
      rowwise() |>
      mutate(`S11 + S12 + S13` = sum(c(S11, S12, S13), na.rm = TRUE)) |>
      filter(`S11 + S12 + S13` != 0) |>
      mutate(check = round(S1 - `S11 + S12 + S13`, rounding)) |>
      filter(abs(check) > threshold)}
  rm(check)

  ## S1SS09----
  check <- data |>
    filter(ref_sector == "S11", accounting_entry == "D", sto == "D72")

  if(nrow(check)>0){
    s1ss09 <- data |>
      filter(ref_sector %in% c("S1", "S12", "S13")) |>
      unite("sto", c(sto, accounting_entry), sep = ".") |>
      filter(sto %in% c("D72.D", "D71.C")) |>
      pivot_wider(
        names_from = ref_sector,
        values_from = obs_value
      ) |>
      select(ref_area, sto, time_period, S1, S12, S13) |>
      rowwise() |>
      mutate(`S12 + S13` = sum(c(S12, S13), na.rm = TRUE)) |>
      filter(`S12 + S13` != 0) |>
      mutate(check = round(S1 - `S12 + S13`, rounding)) |>
      filter(abs(check) > threshold)}
  rm(check)

  ## S1SS10----
  check <- data |>
    filter(ref_sector == "S11", accounting_entry == "D", sto == "D91")
  if(nrow(check)>0){
    s1ss10 <- data |>
      filter(ref_sector %in% c("S1", "S11", "S12", "S1M")) |>
      unite("sto", c(sto, accounting_entry), sep = ".") |>
      filter(sto %in% c("D91.D")) |>
      pivot_wider(
        names_from = ref_sector,
        values_from = obs_value
      ) |>
      select(ref_area, sto, time_period, S1, S11, S12, S1M) |>
      rowwise() |>
      mutate(`S11 + S12 + S1M` = sum(c(S11, S12, S1M), na.rm = TRUE)) |>
      filter(`S11 + S12 + S1M` != 0) |>
      mutate(check = round(S1 - `S11 + S12 + S1M`, rounding)) |>
      filter(abs(check) > threshold)}
  rm(check)


  ## S1SS11----
  check <- data |>
    filter(ref_sector == "S11", accounting_entry == "B", sto == "B1G")

  if(nrow(check)>0){
    s1ss11 <- data |>
      unite("sto", c(ref_sector, sto, accounting_entry), sep = ".") |>
      filter(sto %in% c("S1.B1GQ.B", "S1N.B1G.B", "S11.B1G.B", "S12.B1G.B", "S13.B1G.B", "S1M.B1G.B")) |>
      pivot_wider(
        names_from = sto,
        values_from = obs_value
      ) |>
      select(ref_area, time_period, S1.B1GQ.B, S1N.B1G.B, S11.B1G.B, S12.B1G.B, S13.B1G.B, S1M.B1G.B) |>
      rowwise() |>
      mutate(
        `S1N.B1G.B+S11.B1G.B+S12.B1G.B+S13.B1G.B+S1M.B1G.B` =
          sum(c(S1N.B1G.B, S11.B1G.B, S12.B1G.B, S13.B1G.B, S1M.B1G.B), na.rm = TRUE)
      ) |>
      mutate(check = round(S1.B1GQ.B - `S1N.B1G.B+S11.B1G.B+S12.B1G.B+S13.B1G.B+S1M.B1G.B`, rounding)) |>
      filter(abs(check) > threshold)}
  rm(check)

  ## S1SS12----
  if ("S14" %in% unique(data$ref_sector)) {
    s1ss12 <- data |>
      filter(ref_sector %in% c("S1M", "S14", "S15")) |>
      unite("sto", c(sto, accounting_entry), sep = ".") |>
      filter(sto %in% c(
        "P5.D", "P51G.D", "P5M.D", "D1.D", "D2.D", "D29.D", "D4.D", "D41.D",
        "D4N.D", "D45.D", "D41G.D", "D5.D", "D6.D", "D61.D", "D62.D", "D63.D",
        "D7.D", "D71.D", "D7N.D", "D75.D", "D8.D", "D9.D", "D91.D", "D9N.D",
        "D99.D", "P51C.D", "NP.D", "P1.C", "D1.C", "D3.C", "D39.C", "D4.C",
        "D41.C", "D4N.C", "D42.C", "D43.C", "D44.C", "D45.C", "D41G.C", "D6.C",
        "D61.C", "D62.C", "D63.C", "D7.C", "D72.C", "D7N.C", "D75.C", "D8.C",
        "D9.C", "D9N.C", "D92.C", "D99.C", "P51C.C", "B2A3G.B", "B3G.B", "B4G.B",
        "B5G.B", "B6G.B", "B8G.B", "B101.B", "B9.B", "B9FX9._Z", "B1G.B",
        "B1N.B", "B7G.B", "EMP.PS", "EMP.HW"
      )) |>
      pivot_wider(
        names_from = ref_sector,
        values_from = obs_value
      ) |>
      select(ref_area, sto, time_period, S1M, S14, S15) |>
      rowwise() |>
      mutate(`S14 + S15` = sum(c(S14, S15), na.rm = TRUE)) |>
      filter(`S14 + S15` != 0) |>
      mutate(check = round(S1M - `S14 + S15`, rounding)) |>
      filter(abs(check) > threshold)
  }
  ## No S11 breakdowns---


  ## S1SS13----
  if ("S12K" %in% unique(data$ref_sector)) {
    s1ss13 <- data |>
      filter(ref_sector %in% c("S12", "S12K", "S12P", "S12Q")) |>
      unite("sto", c(sto, accounting_entry), sep = ".") |>
      filter(sto %in% c(
        "P2.D", "P5.D", "P51G.D", "P5M.D", "D1.D", "D2.D", "D29.D",
        "D4.D", "D41.D", "D4N.D", "D42.D", "D43.D", "D44.D", "D45.D",
        "D41G.D", "D5.D", "D6.D", "D62.D", "D7.D", "D71.D", "D72.D",
        "D7N.D", "D75.D", "D8.D", "D9.D", "D91.D", "D9N.D", "D99.D",
        "P51C.D", "NP.D", "P1.C", "D3.C", "D39.C", "D4.C", "D41.C",
        "D4N.C", "D42.C", "D43.C", "D44.C", "D45.C", "D41G.D", "D6.C",
        "D61.C", "D7.C", "D71.C", "D72.C", "D7N.C", "D75.C", "D9.C",
        "D9N.C", "D92.C", "D99.C", "P51C.C", "B2A3BG.B", "B4G.B", "B5G.B",
        "B6G.B", "B8G.B", "B101.B", "B9.B", "B9FX9._Z", "B1G.B",
        "D43_U2.D", "D43_U4.D", "D43_B0.D", "D43_D0.D", "D43_U2.D",
        "D43_U4.C", "D43_U4.C", "D43_B0.C", "D43_D0.C", "D43_U2.C",
        "B1N.B"
      )) |>
      pivot_wider(
        names_from = ref_sector,
        values_from = obs_value
      ) |>
      select(ref_area, sto, time_period, S12, S12K, S12P, S12Q) |>
      rowwise() |>
      mutate(`S12K + S12P + S12Q` = sum(c(S12K, S12P, S12Q), na.rm = TRUE)) |>
      filter(`S12K + S12P + S12Q` != 0) |>
      mutate(check = round(S12 - `S12K + S12P + S12Q`, rounding)) |>
      filter(abs(check) > threshold)
  }
  # SubItems vs Total----

  ## SIT01----

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
    select(ref_area, ref_sector, time_period, P3.D, P31.D, P32.D) |>
    rowwise() |>
    mutate(`P31.D + P32.D` = sum(c(P31.D, P32.D), na.rm = TRUE)) |>
    filter(`P31.D + P32.D` != 0) |>
    mutate(check = round(P3.D - `P31.D + P32.D`, rounding)) |>
    filter(abs(check) > threshold)

  ## SIT02----

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
    filter(P3.D + P31.D != 0) |>
    filter(abs(check) > threshold)

  ## SIT03----
  sit03 <- data |>
    filter(
      ref_sector %in% c(
        "S1", "S11", "S11DO", "S11001", "S110011", "S11002",
        "S110021", "S11003", "S12", "S12K", "S12P", "S12Q",
        "S12001", "S120011", "S12002", "S120021", "S12003",
        "S13", "S1M", "S14", "S15"
      ),
      sto %in% c("P5", "P51G", "P5M"),
      accounting_entry == "D"
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(
      names_from = sto,
      values_from = obs_value
    ) |>
    select(ref_area, ref_sector, time_period, P5.D, P51G.D, P5M.D) |>
    rowwise() |>
    mutate(`P51G.D + P5M.D` = sum(c(P51G.D, P5M.D), na.rm = TRUE)) |>
    filter(`P51G.D + P5M.D` != 0) |>
    mutate(check = round(P5.D - `P51G.D + P5M.D`, rounding)) |>
    filter(abs(check) > threshold)

  ## SIT04----

  sit04 <- data |>
    filter(
      ref_sector %in% c("S1", "S2"),
      sto %in% c("D2", "D21", "D29"),
      accounting_entry == "D"
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(
      names_from = sto,
      values_from = obs_value
    ) |>
    select(ref_area, ref_sector, time_period, D2.D, D21.D, D29.D) |>
    rowwise() |>
    mutate(`D21.D + D29.D` = sum(c(D21.D, D29.D), na.rm = TRUE)) |>
    filter(`D21.D + D29.D` != 0) |>
    mutate(check = round(D2.D - `D21.D + D29.D`, rounding)) |>
    filter(abs(check) > threshold)

  ## SIT05----

  sit05 <- data |>
    filter(
      ref_sector %in% c("S1N"),
      sto %in% c("D2", "D21"),
      accounting_entry == "D"
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(
      names_from = sto,
      values_from = obs_value
    ) |>
    select(ref_area, ref_sector, time_period, D2.D, D21.D) |>
    rowwise() |>
    filter(D2.D + D21.D != 0) |>
    mutate(check = round(D2.D - D21.D, rounding)) |>
    filter(abs(check) > threshold)

  ## SIT06----
  sit06 <- data |>
    filter(
      ref_sector %in% c("S11", "S12", "S13", "S1M", "S14", "S15"),
      sto %in% c("D2", "D29"),
      accounting_entry == "D"
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(
      names_from = sto,
      values_from = obs_value
    ) |>
    select(ref_area, ref_sector, time_period, D2.D, D29.D) |>
    rowwise() |>
    filter(D2.D + D29.D != 0) |>
    mutate(check = round(D2.D - D29.D, rounding)) |>
    filter(abs(check) > threshold)

  ## SIT07----

  sit07 <- data |>
    filter(
      ref_sector %in% c("S1", "S13"),
      sto %in% c("D3", "D31", "D39"),
      accounting_entry == "D"
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto, values_from = obs_value) |>
    select(ref_area, ref_sector, time_period, D3.D, D31.D, D39.D) |>
    rowwise() |>
    mutate(`D31.D + D39.D` = sum(c(D31.D, D39.D), na.rm = TRUE)) |>
    filter(`D31.D + D39.D` != 0) |>
    mutate(check = round(D3.D - `D31.D + D39.D`, rounding)) |>
    filter(abs(check) > threshold)

  ## SIT08----

  sit08 <- data |>
    filter(
      ref_sector %in% c("S1", "S2"),
      sto %in% c("D3", "D31", "D39"),
      accounting_entry == "C"
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto, values_from = obs_value) |>
    select(ref_area, ref_sector, time_period, D3.C, D31.C, D39.C) |>
    rowwise() |>
    mutate(`D31.C + D39.C` = sum(c(D31.C, D39.C), na.rm = TRUE)) |>
    filter(`D31.C + D39.C` != 0) |>
    mutate(check = round(D3.C - `D31.C + D39.C`, rounding)) |>
    filter(abs(check) > threshold)

  ## SIT09 ----

  sit09 <- data |>
    filter(
      ref_sector == "S1N",
      sto %in% c("D3", "D31"),
      accounting_entry == "C"
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(
      names_from = sto,
      values_from = obs_value
    ) |>
    select(ref_area, ref_sector, time_period, D3.C, D31.C) |>
    rowwise() |>
    filter(D3.C + D31.C != 0) |>
    mutate(check = round(D3.C - D31.C, rounding)) |>
    filter(abs(check) > threshold)

  ## SIT10 ----
  sit10 <- data |>
    filter(
      ref_sector %in% c(
        "S1", "S11", "S11DO", "S11001", "S110011", "S11002", "S110021", "S11003",
        "S12", "S12K", "S12P", "S12Q", "S12001", "S120011", "S12002", "S120021",
        "S12003", "S13", "S1M", "S14", "S15", "S2"
      ),
      sto %in% c("D4", "D41", "D4N", "D42", "D43", "D44", "D45"),
      accounting_entry == "D"
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto, values_from = obs_value) |>
    rowwise() |>
    mutate(
      sum_D42_D45 = sum(c_across(c("D42.D", "D43.D", "D44.D", "D45.D")), na.rm = TRUE),
      sum_D41_D42_D45 = sum(c_across("D41.D"), na.rm = TRUE) + sum_D42_D45,
      check1 = round(D4.D - sum_D41_D42_D45, rounding),
      check2 = round(D4N.D - sum_D42_D45, rounding),
      check3 = round(check1 - check2, rounding)
    ) |>
    filter(
      abs(check1) > threshold |
        abs(check2) > threshold |
        abs(check3) > threshold
    )

  ## SIT11 ----
  sit11 <- data |>
    filter(
      ref_sector %in% c(
        "S1", "S11", "S11DO", "S11001", "S110011", "S11002", "S110021", "S11003",
        "S12", "S12K", "S12P", "S12Q", "S12001", "S120011", "S12002", "S120021",
        "S12003", "S13", "S1M", "S14", "S15", "S2"
      ),
      sto %in% c("D4", "D41", "D4N", "D42", "D43", "D44", "D45"),
      accounting_entry == "C"
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto, values_from = obs_value) |>
    rowwise() |>
    mutate(
      sum_D42_D45 = sum(c_across(c("D42.C", "D43.C", "D44.C", "D45.C")), na.rm = TRUE),
      sum_D41_D42_D45 = sum(c_across("D41.C"), na.rm = TRUE) + sum_D42_D45,
      check1 = round(D4.C - sum_D41_D42_D45, rounding),
      check2 = round(D4N.C - sum_D42_D45, rounding),
      check3 = round(check1 - check2, rounding)
    ) |>
    filter(
      abs(check1) > threshold |
        abs(check2) > threshold |
        abs(check3) > threshold
    )


  ## SIT12----

  check <- data |>
    filter(ref_sector == "S11", accounting_entry == "D", sto == "D44")

  if(nrow(check)>0){

    sit12 <- data |>
      filter(
        ref_sector %in% c(
          "S1", "S11", "S11DO", "S11001", "S110011", "S11002",
          "S110021", "S11003", "S12", "S12K", "S12P", "S12Q",
          "S12001", "S120011", "S12002", "S120021", "S12003"
        ),
        sto %in% c("D4N", "D42", "D43", "D44", "D45"),
        accounting_entry == "D"
      ) |>
      unite("sto", c(sto, accounting_entry), sep = ".") |>
      pivot_wider(names_from = sto, values_from = obs_value) |>
      select(ref_area, ref_sector, time_period, D4N.D, D42.D, D43.D, D44.D, D45.D) |>
      rowwise() |>
      filter(D4N.D + D42.D + D43.D + D44.D + D45.D != 0) |>
      mutate(`D42.D + D43.D + D44.D + D45.D` = sum(c(D42.D, D43.D, D44.D, D45.D), na.rm = TRUE)) |>
      mutate(check = round(D4N.D - `D42.D + D43.D + D44.D + D45.D`, rounding)) |>
      filter(abs(check) > threshold)}

  rm(check)

  ## SIT13----
  sit13 <- data |>
    filter(
      ref_sector %in% c("S13"),
      accounting_entry == "D"
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto, values_from = obs_value) |>
    select(ref_area, ref_sector, time_period, D4N.D, D44.D, D45.D) |>
    rowwise() |>
    mutate(`D44.D + D45.D` = sum(c(D44.D, D45.D), na.rm = TRUE)) |>
    filter(`D44.D + D45.D` != 0) |>
    mutate(check = round(D4N.D - `D44.D + D45.D`, rounding)) |>
    filter(abs(check) > threshold)

  ## SIT14----
  check <- data |>
    filter(ref_sector == "S1M", accounting_entry == "D", sto == "D45")

  if(nrow(check)>0){
    sit14 <- data |>
      filter(
        ref_sector %in% c("S1M", "S14", "S15"),
        sto %in% c("D4N", "D45"),
        accounting_entry == "D"
      ) |>
      unite("sto", c(sto, accounting_entry), sep = ".") |>
      pivot_wider(names_from = sto, values_from = obs_value) |>
      select(ref_area, ref_sector, time_period, D4N.D, D45.D) |>
      rowwise() |>
      mutate(check = round(D4N.D - D45.D, rounding)) |>
      filter(D45.D != 0) |>
      filter(abs(check) > threshold)}
  rm(check)

  ## SIT15----
  sit15 <- data |>
    filter(
      ref_sector %in% c(
        "S1", "S11", "S11DO", "S11001", "S110011", "S11002",
        "S110021", "S11003", "S12", "S12K", "S12P", "S12Q", "S12DO",
        "S12001", "S120011", "S12002", "S120021", "S12003", "S13",
        "S1M", "S14", "S15"
      ),
      sto %in% c("D4N", "D42", "D43", "D44", "D45"),
      accounting_entry == "C"
    ) %>%
    unite("sto", c(sto, accounting_entry), sep = ".") %>%
    pivot_wider(names_from = sto, values_from = obs_value) %>%
    select(ref_area, ref_sector, time_period, D4N.C, D42.C, D43.C, D44.C, D45.C) %>%
    rowwise() %>%
    filter(D4N.C + D42.C + D43.C + D44.C + D45.C != 0) %>%
    mutate(`D42.C + D43.C + D44.C + D45.C` = sum(c(D42.C, D43.C, D44.C, D45.C), na.rm = TRUE)) %>%
    mutate(check = round(D4N.C - `D42.C + D43.C + D44.C + D45.C`, rounding)) %>%
    filter(abs(check) > threshold)

  ## SIT16----
  sit16 <- data |>
    filter(
      ref_sector == "S2",
      sto %in% c("D4N", "D42", "D43", "D44"),
      accounting_entry == "C"
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto, values_from = obs_value) |>
    select(ref_area, ref_sector, time_period, D4N.C, D42.C, D43.C, D44.C) |>
    rowwise() |>
    mutate(D4N.C = D42.C + D43.C + D44.C) |>
    mutate(check = round(D4N.C - (D42.C + D43.C + D44.C), rounding)) |>
    filter(D4N.C + D42.C + D43.C + D44.C != 0) |>
    filter(abs(check) > threshold)


  ## SIT17----
  check <- data |>
    filter(ref_sector == "S1M", accounting_entry == "C", sto == "D6")

  if(nrow(check)>0){
    sit17 <- data |>
      filter(
        ref_sector %in% c("S1", "S1M", "S14", "S15"),
        sto %in% c("D6", "D61", "D62", "D63"),
        accounting_entry == "C"
      ) |>
      unite("sto", c(sto, accounting_entry), sep = ".") |>
      pivot_wider(names_from = sto, values_from = obs_value) |>
      select(ref_area, ref_sector, time_period, D6.C, D61.C, D62.C, D63.C) |>
      rowwise() |>
      mutate(D6.C = D61.C + D62.C + D63.C) |>
      mutate(check = round(D6.C - (D61.C + D62.C + D63.C), rounding)) |>
      filter(D6.C + D61.C + D62.C + D63.C != 0) |>
      filter(abs(check) > threshold)}
  rm(check)

  ## SIT18----

  sit18 <- data |>
    filter(
      ref_sector %in% c(
        "S1", "S11", "S11DO", "S11001", "S110011",
        "S11002", "S110021", "S11003", "S12", "S12K",
        "S12P", "S12Q", "S12DO", "S12001", "S120011",
        "S12002", "S120021", "S12003", "S13", "S15"
      ),
      sto %in% c("D6", "D61"),
      accounting_entry == "C"
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto, values_from = obs_value) |>
    select(ref_area, ref_sector, time_period, D6.C, D61.C) |>
    rowwise() |>
    mutate(D6.C = D61.C) |>
    mutate(check = round(D6.C - D61.C, rounding)) |>
    filter(D6.C + D61.C != 0) |>
    filter(abs(check) > threshold)

  ## SIT19----
  check <- data |>
    filter(ref_sector == "S1M", accounting_entry == "D", sto == "D6")

  if(nrow(check)>0){
    sit19 <- data |>
      filter(
        ref_sector %in% c("S1", "S1M", "S14", "S15"),
        sto %in% c("D6", "D61", "D62", "D63"),
        accounting_entry == "D"
      ) |>
      unite("sto", c(sto, accounting_entry), sep = ".") |>
      pivot_wider(names_from = sto, values_from = obs_value) |>
      select(ref_area, ref_sector, time_period, D6.D, D61.D, D62.D, D63.D) |>
      rowwise() |>
      filter(D6.D + D61.D + D62.D + D63.D != 0) |>
      mutate(`D61.D + D62.D + D63.D` = sum(c(D61.D, D62.D, D63.D), na.rm = TRUE)) |>
      mutate(check = round(D6.D - `D61.D + D62.D + D63.D`, rounding)) |>
      filter(abs(check) > threshold)}
  rm(check)


  ## SIT20----
  check <- data |>
    filter(ref_sector == "S11", accounting_entry == "D", sto == "D6")

  if(nrow(check)>0){
    sit20 <- data |>
      filter(
        ref_sector %in% c(
          "S11", "S11DO", "S11001", "S110011", "S11002", "S110021",
          "S11003", "S12", "S12K", "S12P", "S12Q", "S12DO", "S12001",
          "S120011", "S12002", "S120021", "S12003"
        ),
        sto %in% c("D6", "D62"),
        accounting_entry == "D"
      ) |>
      unite("sto", c(sto, accounting_entry), sep = ".") |>
      pivot_wider(names_from = sto, values_from = obs_value) |>
      select(ref_area, ref_sector, time_period, D6.D, D62.D) |>
      rowwise() |>
      filter(D6.D + D62.D != 0) |>
      mutate(check = round(D6.D - D62.D, rounding)) |>
      filter(abs(check) > threshold)}
  rm(check)

  ## SIT21----

  sit21 <- data |>
    filter(
      ref_sector %in% c("S13", "S15"),
      sto %in% c("D6", "D62", "D63"),
      accounting_entry == "D"
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto, values_from = obs_value) |>
    select(ref_area, ref_sector, time_period, D6.D, D62.D, D63.D) |>
    rowwise() |>
    filter(D6.D + D62.D + D63.D != 0) |>
    mutate(`D62.D + D63.D` = sum(c(D62.D, D63.D), na.rm = TRUE)) |>
    mutate(check = round(D6.D - `D62.D + D63.D`, rounding)) |>
    filter(abs(check) > threshold)

  ## SIT22----

  sit22 <- data |>
    filter(
      ref_sector == "S2",
      sto %in% c("D6", "D61", "D62"),
      accounting_entry == "C"
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto, values_from = obs_value) |>
    select(ref_area, ref_sector, time_period, D6.C, D61.C, D62.C) |>
    rowwise() |>
    filter(D6.C + D61.C + D62.C != 0) |>
    mutate(`D61.C + D62.C` = sum(c(D61.C, D62.C), na.rm = TRUE)) |>
    mutate(check = round(D6.C - `D61.C + D62.C`, rounding)) |>
    filter(abs(check) > threshold)

  ## SIT23----

  sit23 <- data |>
    filter(
      ref_sector == "S2",
      sto %in% c("D6", "D61", "D62"),
      accounting_entry == "D"
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto, values_from = obs_value) |>
    select(ref_area, ref_sector, time_period, D6.D, D61.D, D62.D) |>
    rowwise() |>
    mutate(D6.D = D61.D + D62.D) |>
    mutate(check = round(D6.D - (D61.D + D62.D), rounding)) |>
    filter(D6.D + D61.D + D62.D != 0) |>
    ungroup() |>
    filter(abs(check) > threshold)

  ## SIT24----
  sit24 <- data |>
    filter(
      ref_sector %in% c(
        "S1", "S12", "S12K", "S12P", "S12Q", "S12DO",
        "S12001", "S120011", "S12002", "S120021", "S12003",
        "S13", "S2"
      ),
      sto %in% c("D7", "D71", "D72", "D7N"),
      accounting_entry == "D"
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto, values_from = obs_value) |>
    select(ref_area, ref_sector, time_period, D7.D, D71.D, D72.D, D7N.D) |>
    rowwise() |>
    filter(D7.D + D71.D + D72.D + D7N.D != 0) |>
    mutate(`D71.D + D72.D + D7N.D` = sum(c(D71.D, D72.D, D7N.D), na.rm = TRUE)) |>
    mutate(check = round(D7.D - `D71.D + D72.D + D7N.D`, rounding)) |>
    filter(abs(check) > threshold)

  ## SIT25----

  sit25 <- data |>
    filter(
      ref_sector %in% c(
        "S1", "S12", "S12K", "S12P", "S12Q", "S12DO",
        "S12001", "S120011", "S12002", "S120021",
        "S12003", "S13", "S2"
      ),
      sto %in% c("D7", "D71", "D72", "D7N"),
      accounting_entry == "C"
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto, values_from = obs_value) |>
    select(ref_area, ref_sector, time_period, D7.C, D71.C, D72.C, D7N.C) |>
    rowwise() |>
    mutate(D7.C = D71.C + D72.C + D7N.C) |>
    mutate(check = round(D7.C - (D71.C + D72.C + D7N.C), rounding)) |>
    filter(D7.C + D71.C + D72.C + D7N.C != 0) |>
    ungroup() |>
    filter(abs(check) > threshold)

  ## SIT26----
  sit26 <- data |>
    filter(
      ref_sector %in% c("S1", "S13", "S2"),
      sto %in% c("D7N", "D74", "D75", "D76"),
      accounting_entry == "D"
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto, values_from = obs_value) |>
    select(ref_area, ref_sector, time_period, D7N.D, D74.D, D75.D, D76.D) |>
    rowwise() |>
    mutate(D7N.D = D74.D + D75.D + D76.D) |>
    mutate(check = round(D7N.D - (D74.D + D75.D + D76.D), rounding)) |>
    filter(D7N.D + D74.D + D75.D + D76.D != 0) |>
    ungroup() |>
    filter(abs(check) > threshold)

  ## SIT27----
  check <- data |>
    filter(ref_sector == "S11", accounting_entry == "D", sto == "D75")

  if(nrow(check)>0){
    sit27 <- data |>
      filter(
        ref_sector %in% c(
          "S11", "S11DO", "S11001", "S110011", "S11002",
          "S110021", "S11003", "S12", "S12K", "S12P", "S12Q",
          "S12DO", "S12001", "S120011", "S12002", "S120021",
          "S12003", "S1M", "S14", "S15"
        ),
        sto %in% c("D7N", "D75"),
        accounting_entry == "D"
      ) |>
      unite("sto", c(sto, accounting_entry), sep = ".") |>
      pivot_wider(names_from = sto, values_from = obs_value) |>
      select(ref_area, ref_sector, time_period, D7N.D, D75.D) |>
      rowwise() |>
      mutate(D7N.D = D75.D) |>
      mutate(check = round(D7N.D - D75.D, rounding)) |>
      filter(D7N.D + D75.D != 0) |>
      ungroup() |>
      filter(abs(check) > threshold)}
  rm(check)

  ## SIT28----

  sit28 <- data |>
    filter(
      ref_sector %in% c("S1", "S13", "S2"),
      sto %in% c("D7N", "D74", "D75"),
      accounting_entry == "C"
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto, values_from = obs_value) |>
    select(ref_area, ref_sector, time_period, D7N.C, D74.C, D75.C) |>
    rowwise() |>
    filter(D7N.C + D74.C + D75.C != 0) |>
    mutate(`D74.C + D75.C` = sum(c(D74.C, D75.C), na.rm = TRUE)) |>
    mutate(check = round(D7N.C - `D74.C + D75.C`, rounding)) |>
    filter(abs(check) > threshold)


  ## SIT29----

  sit29 <- data |>
    filter(
      ref_sector %in% c("S1", "S13"),
      sto %in% c("D74", "D74_4Y"),
      accounting_entry == "D"
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto, values_from = obs_value) |>
    select(ref_area, ref_sector, time_period, D74.D, D74_4Y.D) |>
    rowwise() |>
    filter(!is.na(D74.D) & !is.na(D74_4Y.D)) |>
    filter(D74.D < D74_4Y.D)


  ## SIT30----
  check <- data |>
    filter(ref_sector == "S1M", accounting_entry == "D", sto == "D91")

  if(nrow(check)>0){
    sit30 <- data |>
      filter(
        ref_sector %in% c(
          "S1", "S11", "S11DO", "S11001", "S110011", "S11002",
          "S110021", "S11003", "S12", "S12K", "S12P", "S12Q",
          "S12DO", "S12001", "S120011", "S12002", "S120021",
          "S12003", "S1M", "S14", "S15", "S2"
        ),
        sto %in% c("D9", "D91", "D9N"),
        accounting_entry == "D"
      ) |>
      unite("sto", c(sto, accounting_entry), sep = ".") |>
      pivot_wider(names_from = sto, values_from = obs_value) |>
      select(ref_area, ref_sector, time_period, D9.D, D91.D, D9N.D) |>
      rowwise() |>
      filter(D9.D + D91.D + D9N.D != 0) |>
      mutate(`D91.D + D9N.D` = sum(c(D91.D, D9N.D), na.rm = TRUE)) |>
      mutate(check = round(D9.D - `D91.D + D9N.D`, rounding)) |>
      filter(abs(check) > threshold)}
  rm(check)

  ## SIT31----
  sit31 <- data |>
    filter(
      ref_sector == "S13",
      sto %in% c("D9", "D9N"),
      accounting_entry == "D"
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto, values_from = obs_value) |>
    select(ref_area, ref_sector, time_period, D9.D, D9N.D) |>
    rowwise() |>
    filter(D9.D + D9N.D != 0) |>
    mutate(check = round(D9.D - D9N.D, rounding)) |>
    filter(abs(check) > threshold)

  ## SIT32----
  check <- data |>
    filter(ref_sector == "S1", accounting_entry == "C", sto == "D91")

  if(nrow(check)>0){
    sit32 <- data |>
      filter(
        ref_sector %in% c("S1", "S13", "S2"),
        sto %in% c("D9", "D91", "D9N"),
        accounting_entry == "C"
      ) |>
      unite("sto", c(sto, accounting_entry), sep = ".") |>
      pivot_wider(names_from = sto, values_from = obs_value) |>
      select(ref_area, ref_sector, time_period, D9.C, D91.C, D9N.C) |>
      rowwise() |>
      filter(D9.C + D91.C + D9N.C != 0) |>
      mutate(`D91.C + D9N.C` = sum(c(D91.C, D9N.C), na.rm = TRUE)) |>
      mutate(check = round(D9.C - `D91.C + D9N.C`, rounding)) |>
      filter(abs(check) > threshold)}
  rm(check)

  ## SIT33----
  check <- data |>
    filter(ref_sector == "S1M", accounting_entry == "D", sto == "D99")

  if(nrow(check)>0){
    sit33 <- data |>
      filter(
        ref_sector %in% c(
          "S11", "S11DO", "S11001", "S110011", "S11002",
          "S110021", "S11003", "S12", "S12K", "S12P", "S12Q",
          "S12DO", "S12001", "S120011", "S12002", "S120021",
          "S12003", "S1M", "S14", "S15"
        ),
        sto %in% c("D9N", "D99"),
        accounting_entry == "D"
      ) |>
      unite("sto", c(sto, accounting_entry), sep = ".") |>
      pivot_wider(names_from = sto, values_from = obs_value) |>
      select(ref_area, ref_sector, time_period, D9N.D, D99.D) |>
      rowwise() |>
      filter(D9N.D + D99.D != 0) |>
      mutate(check = round(D9N.D - D99.D, rounding)) |>
      filter(abs(check) > threshold)}
  rm(check)


  ## SIT34----
  sit34 <- data |>
    filter(
      ref_sector %in% c("S1", "S2", "S13"),
      sto %in% c("D9N", "D92", "D99"),
      accounting_entry == "D"
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto, values_from = obs_value) |>
    select(ref_area, ref_sector, time_period, D9N.D, D92.D, D99.D) |>
    rowwise() |>
    filter(D9N.D + D92.D + D99.D != 0) |>
    mutate(`D92.D + D99.D` = sum(c(D92.D, D99.D), na.rm = TRUE)) |>
    mutate(check = round(D9N.D - `D92.D + D99.D`, rounding)) |>
    filter(abs(check) > threshold)


  ## SIT35----

  sit35 <- data |>
    filter(
      ref_sector %in% c(
        "S1", "S11", "S11DO", "S11001", "S110011", "S11002",
        "S110021", "S11003", "S12", "S12K", "S12P", "S12Q",
        "S12DO", "S12001", "S120011", "S12002", "S120021",
        "S12003", "S1M", "S14", "S15", "S2"
      ),
      sto %in% c("D9N", "D92", "D99"),
      accounting_entry == "C"
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(
      names_from = sto,
      values_from = obs_value
    ) |>
    select(ref_area, ref_sector, time_period, D9N.C, D92.C, D99.C) |>
    rowwise() |>
    filter(D9N.C + D92.C + D99.C != 0) |>
    mutate(`D92.C + D99.C` = sum(c(D92.C, D99.C), na.rm = TRUE)) |>
    mutate(check = round(D9N.C - `D92.C + D99.C`, rounding)) |>
    filter(abs(check) > threshold)

  ## SIT36----

  sit36 <- data |>
    filter(
      ref_sector == "S2",
      sto %in% c("P7", "P71", "P72"),
      accounting_entry == "C"
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto, values_from = obs_value) |>
    select(ref_area, ref_sector, time_period, P7.C, P71.C, P72.C) |>
    rowwise() |>
    filter(!is.na(P7.C) | !is.na(P71.C) | !is.na(P72.C)) |>
    mutate(`P71.C + P72.C` = sum(c(P71.C, P72.C), na.rm = TRUE)) |>
    mutate(check = round(P7.C - `P71.C + P72.C`, rounding)) |>
    filter(abs(check) > threshold)

  ## SIT37----
  if ("P72F" %in% unique(data$sto)) {
    sit37 <- data |>
      filter(
        ref_sector == "S2",
        sto %in% c("P72", "P72F"),
        accounting_entry == "C"
      ) |>
      unite("sto", c(sto, accounting_entry), sep = ".") |>
      pivot_wider(names_from = sto, values_from = obs_value) |>
      select(ref_area, ref_sector, time_period, P72.C, P72F.C) |>
      rowwise() |>
      filter(!is.na(P72.C) & !is.na(P72F.C)) |>
      filter(P72.C < P72F.C)
  }
  ## SIT38----
  if ("P62F" %in% unique(data$sto)) {
    sit38 <- data |>
      filter(
        ref_sector == "S2",
        sto %in% c("P62", "P62F"),
        accounting_entry == "D"
      ) %>%
      unite("sto", c(sto, accounting_entry), sep = ".") %>%
      pivot_wider(names_from = sto, values_from = obs_value) %>%
      select(ref_area, ref_sector, time_period, P62.D, P62F.D) %>%
      rowwise() %>%
      filter(!is.na(P62.D) & !is.na(P62F.D)) %>%
      filter(P62.D < P62F.D)
  }
  ## SIT39----
  if ("D43_I9" %in% unique(data$sto)) {
    sit39 <- data |>
      filter(
        ref_sector %in% c("S11", "S12"),
        sto %in% c("D43", "D43_I9", "D43_J9"),
        accounting_entry == "D"
      ) |>
      unite("sto", c(sto, accounting_entry), sep = ".") |>
      pivot_wider(names_from = sto, values_from = obs_value) |>
      select(ref_area, ref_sector, time_period, D43_I9.D, D43_J9.D, D43.D) |>
      rowwise() |>
      filter(!is.na(D43.D) | !is.na(D43_I9.D) | !is.na(D43_J9.D)) |>
      mutate(`D43_I9.D + D43_J9.D` = sum(c(D43_I9.D, D43_J9.D), na.rm = TRUE)) |>
      mutate(check = round(D43.D - `D43_I9.D + D43_J9.D`, rounding)) |>
      filter(abs(check) > threshold)
  }


  ## SIT40----
  if ("D43_B6" %in% unique(data$sto)) {
    sit40 <- data |>
      filter(
        ref_sector %in% c("S11", "S12"),
        sto %in% c("D43", "D43_B6", "D43_D6"),
        accounting_entry == "D"
      ) |>
      unite("sto", c(sto, accounting_entry), sep = ".") |>
      pivot_wider(names_from = sto, values_from = obs_value) |>
      select(ref_area, ref_sector, time_period, D43_B6.D, D43_D6.D, D43.D) |>
      rowwise() |>
      filter(!is.na(D43.D) | !is.na(D43_B6.D) | !is.na(D43_D6.D)) |>
      mutate(`D43_B6.D + D43_D6.D` = sum(c(D43_B6.D, D43_D6.D), na.rm = TRUE)) |>
      mutate(check = round(D43.D - `D43_B6.D + D43_D6.D`, rounding)) |>
      filter(abs(check) > threshold)
  }
  # Balancing items----

  ## BI01----

  check <- data |>
    filter(sto == "P1" & ref_sector == "S1")

  if (nrow(check)>0){
    BI01 <- data |>
      filter(
        ref_sector == "S1",
        (sto == "B1GQ" & accounting_entry == "B") |
          (sto == "P1" & accounting_entry == "C") |
          (sto == "P2" & accounting_entry == "D") |
          (sto == "D21X31" & accounting_entry == "C")
      ) |>
      unite("sto", c(sto, accounting_entry), sep = ".") |>
      pivot_wider(names_from = sto, values_from = obs_value) |>
      select(ref_area, ref_sector, time_period, B1GQ.B, P1.C, P2.D, D21X31.C) |>
      rowwise() |>
      filter(!is.na(B1GQ.B) | !is.na(P1.C) | !is.na(P2.D) | !is.na(D21X31.C)) |>
      mutate(
        `P1.C - P2.D + D21X31.C` = P1.C - P2.D + D21X31.C,
        check = round(B1GQ.B - `P1.C - P2.D + D21X31.C`, rounding)
      ) |>
      filter(abs(check) > threshold)
  }
  rm(check)
  ## BI02----

  BI02 <- data |>
    filter(
      ref_sector %in% c("S1", "S1N"),
      (sto == "D21X31" & accounting_entry == "C") |
        (sto == "D21" & accounting_entry == "D") |
        (sto == "D31" & accounting_entry == "C")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto, values_from = obs_value) |>
    select(ref_area, ref_sector, time_period, D21X31.C, D21.D, D31.C) |>
    rowwise() |>
    filter(!is.na(D21X31.C) | !is.na(D21.D) | !is.na(D31.C)) |>
    mutate(
      `D21.D - D31.C` = D21.D - D31.C,
      check = round(D21X31.C - `D21.D - D31.C`, rounding)
    ) |>
    filter(abs(check) > threshold)

  ## BI03----

  BI03 <- data |>
    filter(
      ref_sector == "S1N",
      (sto == "B1G" & accounting_entry == "B") |
        (sto == "D21X31" & accounting_entry == "C")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto, values_from = obs_value) |>
    select(ref_area, ref_sector, time_period, B1G.B, D21X31.C) |>
    rowwise() |>
    filter(!is.na(B1G.B) | !is.na(D21X31.C)) |>
    filter(round(B1G.B - D21X31.C, rounding) > threshold |
             round(B1G.B - D21X31.C, rounding) < -threshold)


  ## BI04----

  BI04 <- data |>
    filter(
      ref_sector %in% c(
        "S11", "S11DO", "S11001", "S110011", "S11002", "S110021", "S11003",
        "S12", "S12K", "S12P", "S12Q", "S12DO", "S12001", "S120011", "S12002",
        "S120021", "S12003", "S13", "S1M", "S14", "S15"
      ),
      (sto == "B1G" & accounting_entry == "B") |
        (sto == "P1" & accounting_entry == "C") |
        (sto == "P2" & accounting_entry == "D")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto, values_from = obs_value) |>
    select(ref_area, ref_sector, time_period, B1G.B, P1.C, P2.D) |>
    rowwise() |>
    mutate(
      `P1.C - P2.D` = P1.C - P2.D,
      check = round(B1G.B - `P1.C - P2.D`, rounding)
    ) |>
    filter(abs(check) > threshold)

  ## BI05----

  BI05 <- data |>
    filter(
      ref_sector == "S1",
      (sto == "B1NQ" & accounting_entry == "B") |
        (sto == "B1GQ" & accounting_entry == "B") |
        (sto == "P51C" & accounting_entry == "C")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto, values_from = obs_value) |>
    select(ref_area, ref_sector, time_period, B1NQ.B, B1GQ.B, P51C.C) |>
    rowwise() |>
    mutate(
      `B1GQ.B - P51C.C` = B1GQ.B - P51C.C,
      check = round(B1NQ.B - `B1GQ.B - P51C.C`, rounding)
    ) |>
    filter(abs(check) > threshold)

  ## BI06----

  BI06 <- data |>
    filter(
      ref_sector %in% c(
        "S11", "S11DO", "S11001", "S110011", "S11002", "S110021", "S11003",
        "S12", "S12K", "S12P", "S12Q", "S12DO", "S12001", "S120011",
        "S12002", "S120021", "S12003", "S13", "S1M", "S14", "S15"
      ),
      (sto == "B1N" & accounting_entry == "B") |
        (sto == "B1G" & accounting_entry == "B") |
        (sto == "P51C" & accounting_entry == "C")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto, values_from = obs_value) |>
    select(ref_area, ref_sector, time_period, B1G.B, P51C.C,B1N.B) |>
    rowwise() |>
    mutate(
      `B1G.B - P51C.C` = B1G.B - P51C.C,
      check = round(B1N.B - `B1G.B - P51C.C`, rounding)
    ) |>
    filter(abs(check) > threshold)

  ## BI07----

  BI07 <- data |>
    filter(
      ref_sector %in% c("S1", "S2"),
      (sto == "P3" & accounting_entry == "D") |
        (sto == "P5" & accounting_entry == "D") |
        (sto == "P6" & accounting_entry == "D") |
        (sto == "P7" & accounting_entry == "C") |
        (sto == "B1GQ" & accounting_entry == "B")
    ) |>
    unite("sto", c(ref_sector, sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto, values_from = obs_value) |>
    select(
      ref_area, time_period,
      S1.P3.D, S1.P5.D, S2.P6.D, S2.P7.C,S1.B1GQ.B
    ) |>
    rowwise() |>
    mutate(
      S1.B1GQ.B.calc = S1.P3.D + S1.P5.D + S2.P6.D - S2.P7.C,
      check = round(S1.B1GQ.B - S1.B1GQ.B.calc, rounding)
    ) |>
    filter(abs(check) > threshold)

  ## BI08----

  BI08 <- data |>
    filter(
      ref_sector %in% c(
        "S1", "S11", "S11DO", "S11001", "S110011", "S11002",
        "S110021", "S11003", "S12", "S12K", "S12P", "S12Q",
        "S12DO", "S12001", "S120011", "S12002", "S120021",
        "S12003", "S13", "S1M", "S14", "S15"
      ),
      (sto == "B2A3G" & accounting_entry == "B") |
        (sto == "B1GQ" & accounting_entry == "B") |
        (sto == "D3" & accounting_entry == "C") |
        (sto == "D1" & accounting_entry == "D") |
        (sto == "D2" & accounting_entry == "D")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto, values_from = obs_value) |>
    select(ref_area, ref_sector, time_period, B1GQ.B, D3.C, D1.D, D2.D,B2A3G.B) |>
    rowwise() |>
    mutate(
      B2A3G.B.calc = B1GQ.B + D3.C - D1.D - D2.D,
      check = round(B2A3G.B - B2A3G.B.calc, rounding)
    ) |>
    filter(abs(check) > threshold)

  ## BI09----

  BI09 <- data |>
    filter(
      ref_sector %in% c("S11", "S12"),
      (sto == "B4G" & accounting_entry == "B") |
        (sto == "B2A3G" & accounting_entry == "B") |
        (sto == "D4" & accounting_entry == "C") |
        (sto == "D41" & accounting_entry == "D") |
        (sto == "D44" & accounting_entry == "D") |
        (sto == "D45" & accounting_entry == "D")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto, values_from = obs_value) |>
    select(
      ref_area, ref_sector, time_period,
      B2A3G.B, D4.C, D41.D, D44.D, D45.D,B4G.B
    ) |>
    rowwise() |>
    mutate(
      B4G.B.calc = B2A3G.B + D4.C - D41.D - D44.D - D45.D,
      check = round(B4G.B - B4G.B.calc, rounding)
    ) |>
    filter(abs(check) > threshold)

  ## BI10----

  BI10 <- data |>
    filter(
      ref_sector == "S1",
      (sto == "B5G" & accounting_entry == "B") |
        (sto == "B2A3G" & accounting_entry == "B") |
        (sto == "D1" & accounting_entry == "C") |
        (sto == "D2" & accounting_entry == "C") |
        (sto == "D3" & accounting_entry == "D") |
        (sto == "D4" & accounting_entry %in% c("C", "D"))
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto, values_from = obs_value) |>
    select(
      ref_area, ref_sector, time_period,
      B2A3G.B, D1.C, D2.C, D3.D, D4.C, D4.D,B5G.B
    ) |>
    rowwise() |>
    mutate(
      B5G.B.calc = B2A3G.B + D1.C + D2.C - D3.D + D4.C - D4.D,
      check = round(B5G.B - B5G.B.calc, rounding)
    ) |>
    filter(abs(check) > threshold)


  ## BI11----
  BI11 <- data |>
    filter(
      ref_sector %in% c(
        "S11", "S11DO", "S11001", "S110011", "S11002", "S110021", "S11003",
        "S12", "S12K", "S12P", "S12Q", "S12DO", "S12001", "S120011", "S12002",
        "S120021", "S12003", "S15"
      ),
      (sto == "B5G" & accounting_entry == "B") |
        (sto == "B2A3G" & accounting_entry == "B") |
        (sto == "D4" & accounting_entry %in% c("C", "D"))
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto, values_from = obs_value) |>
    select(ref_area, ref_sector, time_period,  B2A3G.B, D4.C, D4.D,B5G.B) |>
    rowwise() |>
    mutate(
      B5G.B.calc  = B2A3G.B + D4.C - D4.D,
      check = round(B5G.B - B5G.B.calc, rounding)
    ) |>
    filter(abs(check) > threshold)

  ## BI12----
  BI12 <- data |>
    filter(
      ref_sector == "S13",
      (sto == "B5G" & accounting_entry == "B") |
        (sto == "B2A3G" & accounting_entry == "B") |
        (sto == "D2" & accounting_entry == "C") |
        (sto == "D3" & accounting_entry == "D") |
        (sto == "D4" & accounting_entry %in% c("C", "D"))
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto, values_from = obs_value) |>
    select(ref_area, ref_sector, time_period, B2A3G.B, D2.C, D3.D, D4.C, D4.D,B5G.B) |>
    rowwise() |>
    mutate(
      B5G.B.calc  = B2A3G.B + D2.C - D3.D + D4.C - D4.D,
      check = round(B5G.B - B5G.B.calc, rounding)
    ) |>
    filter(abs(check) > threshold)

  ## BI13----
  BI13 <- data |>
    filter(
      ref_sector %in% c("S1M", "S14", "S15"),
      (sto == "B5G" & accounting_entry == "B") |
        (sto == "B2A3G" & accounting_entry == "B") |
        (sto == "D1" & accounting_entry == "C") |
        (sto == "D4" & accounting_entry %in% c("C", "D"))
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto, values_from = obs_value) |>
    select(ref_area, ref_sector, time_period, B2A3G.B, D1.C, D4.C, D4.D,B5G.B) |>
    rowwise() |>
    mutate(
      B5G.B.calc  = B2A3G.B + D1.C + D4.C - D4.D,
      check = round(B5G.B - B5G.B.calc, rounding)
    ) |>
    filter(abs(check) > threshold)

  ## BI14----
  BI14 <- data |>
    filter(
      ref_sector %in% c("S1", "S2"),
      (sto == "B1GQ" & accounting_entry == "B") |
        (sto == "B5G" & accounting_entry == "B") |
        (sto == "D1" & accounting_entry %in% c("D", "C")) |
        (sto == "D2" & accounting_entry == "D") |
        (sto == "D3" & accounting_entry == "C") |
        (sto == "D4" & accounting_entry %in% c("D", "C"))
    ) |>
    unite("sto", c(ref_sector, sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto, values_from = obs_value) |>
    select(
      ref_area, time_period,
      S1.B1GQ.B,
      S2.D1.D, S2.D1.C, S2.D2.D, S2.D3.C, S2.D4.D, S2.D4.C,S1.B5G.B
    ) |>
    rowwise() |>
    mutate(
      S1.B5G.B.calc = S1.B1GQ.B - S2.D1.D + S2.D1.C - S2.D2.D +
        S2.D3.C - S2.D4.D + S2.D4.C,
      check = round(S1.B5G.B - S1.B5G.B.calc, rounding)
    ) |>
    filter(abs(check) > threshold)

  ## BI15----
  BI15 <- data |>
    filter(
      ref_sector == "S1",
      (sto == "B6G" & accounting_entry == "B") |
        (sto == "B5G" & accounting_entry == "B") |
        (sto == "D5" & accounting_entry %in% c("C", "D")) |
        (sto == "D61" & accounting_entry %in% c("C", "D")) |
        (sto == "D62" & accounting_entry %in% c("C", "D")) |
        (sto == "D7" & accounting_entry %in% c("C", "D"))
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto, values_from = obs_value) |>
    select(
      ref_area, ref_sector, time_period,
      B5G.B, D5.C, D5.D, D61.C, D61.D, D62.C, D62.D, D7.C, D7.D, B6G.B
    ) |>
    rowwise() |>
    mutate(
      B6G.B.calc = B5G.B + D5.C - D5.D + D61.C - D61.D +
        D62.C - D62.D + D7.C - D7.D,
      check = round(B6G.B - B6G.B.calc, rounding)
    ) |>
    filter(abs(check) > threshold)

  ## BI16----
  BI16 <- data |>
    filter(
      ref_sector %in% c(
        "S11", "S11DO", "S11001", "S110011", "S11002", "S110021", "S11003",
        "S12", "S12K", "S12P", "S12Q", "S12DO", "S12001", "S120011", "S12002",
        "S120021", "S12003", "S15"
      ),
      (sto == "B6G" & accounting_entry == "B") |
        (sto == "B5G" & accounting_entry == "B") |
        (sto == "D5" & accounting_entry == "D") |
        (sto == "D61" & accounting_entry == "C") |
        (sto == "D62" & accounting_entry == "D") |
        (sto == "D7" & accounting_entry %in% c("C", "D"))
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto, values_from = obs_value) |>
    select(
      ref_area, ref_sector, time_period,
      B5G.B, D5.D, D61.C, D62.D, D7.C, D7.D,B6G.B
    ) |>
    rowwise() |>
    mutate(
      B6G.B.calc  = B5G.B - D5.D + D61.C - D62.D + D7.C - D7.D,
      check = round(B6G.B - B6G.B.calc, rounding)
    ) |>
    filter(abs(check) > threshold)

  ## BI17----
  BI17 <- data |>
    filter(
      ref_sector == "S13",
      (sto == "B6G" & accounting_entry == "B") |
        (sto == "B5G" & accounting_entry == "B") |
        (sto == "D5" & accounting_entry %in% c("C", "D")) |
        (sto == "D61" & accounting_entry == "C") |
        (sto == "D62" & accounting_entry == "D") |
        (sto == "D7" & accounting_entry %in% c("C", "D"))
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto, values_from = obs_value) |>
    select(
      ref_area, ref_sector, time_period,
      B5G.B, D5.C, D5.D, D61.C, D62.D, D7.C, D7.D, B6G.B
    ) |>
    rowwise() |>
    mutate(
      B6G.B.calc  = B5G.B + D5.C - D5.D + D61.C - D62.D + D7.C - D7.D,
      check = round(B6G.B - B6G.B.calc, rounding)
    ) |>
    filter(abs(check) > threshold)

  ## BI18----
  BI18 <- data |>
    filter(
      ref_sector %in% c("S1M", "S14", "S15"),
      (sto == "B6G" & accounting_entry == "B") |
        (sto == "B5G" & accounting_entry == "B") |
        (sto == "D5" & accounting_entry == "D") |
        (sto == "D61" & accounting_entry %in% c("C", "D")) |
        (sto == "D62" & accounting_entry %in% c("C", "D")) |
        (sto == "D7" & accounting_entry %in% c("C", "D"))
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto, values_from = obs_value) |>
    select(
      ref_area, ref_sector, time_period,
      B5G.B, D5.D, D61.C, D61.D, D62.C, D62.D, D7.C, D7.D, B6G.B
    ) |>
    rowwise() |>
    mutate(
      B6G.B.calc  = B5G.B - D5.D + D61.C - D61.D + D62.C - D62.D + D7.C - D7.D,
      check = round(B6G.B - B6G.B.calc, rounding)
    ) |>
    filter(abs(check) > threshold)

  ## BI19----
  BI19 <- data |>
    filter(
      ref_sector %in% c("S1", "S2"),
      (sto == "B5G" & accounting_entry == "B") |
        (sto == "B6G" & accounting_entry == "B") |
        (sto == "D5" & accounting_entry %in% c("D", "C")) |
        (sto == "D61" & accounting_entry %in% c("D", "C")) |
        (sto == "D62" & accounting_entry %in% c("D", "C")) |
        (sto == "D7" & accounting_entry %in% c("D", "C"))
    ) |>
    unite("sto", c(ref_sector, sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto, values_from = obs_value) |>
    select(
      ref_area, time_period,
      S1.B5G.B,
      S2.D5.D, S2.D5.C,
      S2.D61.D, S2.D61.C,
      S2.D62.D, S2.D62.C,
      S2.D7.D, S2.D7.C,S1.B6G.B
    ) |>
    rowwise() |>
    mutate(
      S1.B6G.B.calc = S1.B5G.B - S2.D5.D + S2.D5.C -
        S2.D61.D + S2.D61.C -
        S2.D62.D + S2.D62.C -
        S2.D7.D + S2.D7.C,
      check = round(S1.B6G.B - S1.B6G.B, rounding)
    ) |>
    filter(abs(check) > threshold)

  ## BI20----
  BI20 <- data |>
    filter(
      ref_sector %in% c("S13", "S15"),
      (sto == "B7G" & accounting_entry == "B") |
        (sto == "B6G" & accounting_entry == "B") |
        (sto == "D63" & accounting_entry == "D")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto, values_from = obs_value) |>
    select(ref_area, ref_sector, time_period,  B6G.B, D63.D,B7G.B) |>
    rowwise() |>
    mutate(
      B7G.B.calc  = B6G.B - D63.D,
      check = round(B7G.B - B7G.B.calc, rounding)
    ) |>
    filter(abs(check) > threshold)

  ## BI21----
  BI21 <- data |>
    filter(
      ref_sector %in% c("S1M", "S14", "S15"),
      (sto == "B7G" & accounting_entry == "B") |
        (sto == "B6G" & accounting_entry == "B") |
        (sto == "D63" & accounting_entry %in% c("C", "D"))
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto, values_from = obs_value) |>
    select(ref_area, ref_sector, time_period, B6G.B, D63.C, D63.D,B7G.B) |>
    rowwise() |>
    mutate(
      B7G.B.calc  = B6G.B + D63.C - D63.D,
      check = round(B7G.B - B7G.B.calc, rounding)
    ) |>
    filter(abs(check) > threshold)

  ## BI22----
  BI22 <- data |>
    filter(
      ref_sector %in% c("S1", "S1M", "S14", "S15"),
      (sto == "B8G" & accounting_entry == "B") |
        (sto == "B6G" & accounting_entry == "B") |
        (sto == "D8" & accounting_entry %in% c("C", "D")) |
        (sto == "P3" & accounting_entry == "D")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto, values_from = obs_value) |>
    select(ref_area, ref_sector, time_period, B6G.B, D8.C, D8.D, P3.D, B8G.B ) |>
    rowwise() |>
    mutate(
      B8G.B.calc  = B6G.B + D8.C - D8.D - P3.D,
      check = round(B8G.B - B8G.B.calc, rounding)
    ) |>
    filter(abs(check) > threshold)

  ## BI23----
  BI23 <- data |>
    filter(
      ref_sector %in% c("S1", "S2"),
      (sto == "B8G" & accounting_entry == "B") |
        (sto == "B6G" & accounting_entry == "B") |
        (sto == "D8" & accounting_entry %in% c("C", "D")) |
        (sto == "P3" & accounting_entry == "D")
    ) |>
    unite("sto", c(ref_sector, sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto, values_from = obs_value) |>
    select(
      ref_area, time_period,
      S2.D8.D, S2.D8.C, S1.P3.D, S1.B8G.B,S1.B6G.B
    ) |>
    rowwise() |>

    mutate(
      S1.B6G.B.calc  = S1.B6G.B - S2.D8.D + S2.D8.C - S1.P3.D,
      check = round(S1.B8G.B - S1.B6G.B.calc, rounding)
    ) |>
    filter(abs(check) > threshold)

  ## BI24----
  BI24 <- data |>
    filter(
      ref_sector %in% c(
        "S1", "S11", "S11DO", "S11001", "S110011", "S11002", "S110021", "S11003",
        "S12", "S12K", "S12P", "S12Q", "S12DO", "S12001", "S120011", "S12002",
        "S120021", "S12003", "S13", "S1M", "S14", "S15"
      ),
      (sto == "B101" & accounting_entry == "B") |
        (sto == "B8G" & accounting_entry == "B") |
        (sto == "D9" & accounting_entry %in% c("C", "D")) |
        (sto == "P51C" & accounting_entry == "C")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto, values_from = obs_value) |>
    select(
      ref_area, ref_sector, time_period,
      B8G.B, D9.C, D9.D, P51C.C,B101.B
    ) |>
    rowwise() |>
    mutate(
      B101.B.calc  = B8G.B + D9.C - D9.D - P51C.C,
      check = round(B101.B - B101.B.calc, rounding)
    ) |>
    filter(abs(check) > threshold)

  ## BI25----
  BI25 <- data |>
    filter(
      ref_sector == "S2",
      (sto == "B101" & accounting_entry == "B") |
        (sto == "B12" & accounting_entry == "B") |
        (sto == "D9" & accounting_entry %in% c("C", "D"))
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto, values_from = obs_value) |>
    select(ref_area, ref_sector, time_period, B12.B, D9.D, D9.C,B101.B) |>
    rowwise() |>
    mutate(
      B101.B.calc  = B12.B + D9.D - D9.C,
      check = round(B101.B - B101.B.calc, rounding)
    ) |>
    filter(abs(check) > threshold)

  ## BI26----
  BI26 <- data |>
    filter(
      ref_sector %in% c("S1", "S2"),
      (sto == "B101" & accounting_entry == "B") |
        (sto == "B8G" & accounting_entry == "B") |
        (sto == "D9" & accounting_entry %in% c("D", "C")) |
        (sto == "P51C" & accounting_entry == "C")
    ) |>
    unite("sto", c(ref_sector, sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto, values_from = obs_value) |>
    select(
      ref_area, time_period, S1.B8G.B, S2.D9.D, S2.D9.C, S1.P51C.C, S1.B101.B
    ) |>
    rowwise() |>

    mutate(
      S1.B101.B.calc  = S1.B8G.B - S2.D9.D + S2.D9.C - S1.P51C.C,
      check = round(S1.B101.B - S1.B101.B.calc, rounding)
    ) |>
    filter(abs(check) > threshold)

  ## BI27----
  BI27 <- data |>
    filter(
      ref_sector %in% c(
        "S1", "S11", "S11DO", "S11001", "S110011", "S11002", "S110021", "S11003",
        "S12", "S12K", "S12P", "S12Q", "S12DO", "S12001", "S120011", "S12002",
        "S120021", "S12003", "S13", "S1M", "S14", "S15"
      ),
      (sto == "B9" & accounting_entry == "B") |
        (sto == "B8G" & accounting_entry == "B") |
        (sto == "D9" & accounting_entry %in% c("C", "D")) |
        (sto == "P5" & accounting_entry == "D") |
        (sto == "NP" & accounting_entry == "D")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto, values_from = obs_value) |>
    select(
      ref_area, ref_sector, time_period,
      B8G.B, D9.C, D9.D, P5.D, NP.D, B9.B
    ) |>
    rowwise() |>
    mutate(
      B9.B.calc  = B8G.B + D9.C - D9.D - P5.D - NP.D,
      check = round(B9.B - B9.B.calc, rounding)
    ) |>
    filter(abs(check) > threshold)

  ## BI28----
  BI28 <- data |>
    filter(
      ref_sector == "S2",
      (sto == "B9" & accounting_entry == "B") |
        (sto == "B12" & accounting_entry == "B") |
        (sto == "D9" & accounting_entry %in% c("C", "D")) |
        (sto == "NP" & accounting_entry == "C")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto, values_from = obs_value) |>
    select(
      ref_area, ref_sector, time_period,
      B12.B, D9.D, D9.C, NP.C,B9.B
    ) |>
    rowwise() |>
    mutate(
      B9.B.calc  = B12.B + D9.D - D9.C - NP.C,
      check = round(B9.B - B9.B.calc, rounding)
    ) |>
    filter(abs(check) > threshold)

  ## BI29----
  BI29 <- data |>
    filter(
      ref_sector %in% c("S1", "S2"),
      (sto == "B9" & accounting_entry == "B") |
        (sto == "B8G" & accounting_entry == "B") |
        (sto == "D9" & accounting_entry %in% c("D", "C")) |
        (sto == "P5" & accounting_entry == "D") |
        (sto == "NP" & accounting_entry == "D")
    ) |>
    unite("sto", c(ref_sector, sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto, values_from = obs_value) |>
    select(
      ref_area, time_period, S1.B8G.B, S2.D9.D, S2.D9.C, S1.P5.D, S1.NP.D,S1.B9.B
    ) |>
    rowwise() |>
    mutate(
      S1.B9.B.calc  = S1.B8G.B - S2.D9.D + S2.D9.C - S1.P5.D - S1.NP.D,
      check = round(S1.B9.B - S1.B9.B.calc, rounding)
    ) |>
    filter(abs(check) > threshold)

  ## BI30----
  BI30 <- data |>
    filter(
      ref_sector %in% c(
        "S11", "S11DO", "S11001", "S110011", "S11002", "S110021", "S11003",
        "S12", "S12K", "S12P", "S12Q", "S12DO", "S12001", "S120011", "S12002",
        "S120021", "S12003", "S13", "S1M", "S14", "S15"
      ),
      (sto == "B9" & accounting_entry == "B") |
        (sto == "B101" & accounting_entry == "B") |
        (sto == "P51C" & accounting_entry == "C") |
        (sto == "P5" & accounting_entry == "D") |
        (sto == "NP" & accounting_entry == "D")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto, values_from = obs_value) |>
    select(
      ref_area, ref_sector, time_period,
      B101.B, P51C.C, P5.D, NP.D,B9.B
    ) |>
    rowwise() |>
    mutate(
      B9.B.calc  = B101.B + P51C.C - P5.D - NP.D,
      check = round(B9.B - B9.B.calc, rounding)
    ) |>
    filter(abs(check) > threshold)

  ## BI31----
  BI31 <- data |>
    filter(
      ref_sector == "S2",
      (sto == "B9" & accounting_entry == "B") |
        (sto == "B101" & accounting_entry == "B") |
        (sto == "NP" & accounting_entry == "C")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto, values_from = obs_value) |>
    select(ref_area, ref_sector, time_period, B101.B, NP.C,B9.B) |>
    rowwise() |>
    mutate(
      B9.B.calc  = B101.B - NP.C,
      check = round(B9.B - B9.B.calc, rounding)
    ) |>
    filter(abs(check) > threshold)

  ## BI32----
  BI32 <- data |>
    filter(
      ref_sector %in% c("S1", "S2"),
      (sto == "B101" & accounting_entry == "B") |
        (sto == "P5" & accounting_entry == "D") |
        (sto == "P51C" & accounting_entry == "C") |
        (sto == "B101" & ref_sector == "S2" & accounting_entry == "B")
    ) |>
    unite("sto", c(ref_sector, sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto, values_from = obs_value) |>
    select(
      ref_area, time_period, S2.B101.B, S1.P5.D, S1.P51C.C,S1.B101.B
    ) |>
    rowwise() |>
    mutate(
      S1.B101.B.calc  = -S2.B101.B + S1.P5.D - S1.P51C.C,
      check = round(S1.B101.B - S1.B101.B.calc, rounding)
    ) |>
    filter(abs(check) > threshold)

  ## BI33----
  BI33 <- data |>
    filter(
      ref_sector == "S2",
      (sto == "B11" & accounting_entry == "B") |
        (sto == "P7" & accounting_entry == "C") |
        (sto == "P6" & accounting_entry == "D")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto, values_from = obs_value) |>
    select(ref_area, ref_sector, time_period,  P7.C, P6.D,B11.B) |>
    rowwise() |>
    mutate(
      B11.B.calc  = P7.C - P6.D,
      check = round(B11.B - B11.B.calc, rounding)
    ) |>
    filter(abs(check) > threshold)
  ## BI34----
  BI34 <- data |>
    filter(
      ref_sector == "S2",
      (sto == "B12" & accounting_entry == "B") |
        (sto == "B11" & accounting_entry == "B") |
        (sto == "D1" & accounting_entry %in% c("C", "D")) |
        (sto == "D2" & accounting_entry == "D") |
        (sto == "D3" & accounting_entry == "C") |
        (sto == "D4" & accounting_entry %in% c("C", "D")) |
        (sto == "D5" & accounting_entry %in% c("C", "D")) |
        (sto == "D61" & accounting_entry %in% c("C", "D")) |
        (sto == "D62" & accounting_entry %in% c("C", "D")) |
        (sto == "D7" & accounting_entry %in% c("C", "D")) |
        (sto == "D8" & accounting_entry %in% c("C", "D"))
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto, values_from = obs_value) |>
    select(
      ref_area, ref_sector, time_period,
      B11.B, D1.C, D1.D, D2.D, D3.C, D4.C, D4.D,
      D5.C, D5.D, D61.C, D61.D, D62.C, D62.D, D7.C, D7.D, D8.C, D8.D,B12.B,
    ) |>
    rowwise() |>
    mutate(
      B12.B.calc = B11.B + D1.D - D1.C + D2.D - D3.C + D4.D - D4.C +
        D5.D - D5.C + D61.D - D61.C + D62.D - D62.C +
        D7.D - D7.C + D8.D - D8.C,
      check = round(B12.B - B12.B.calc, rounding)
    ) |>
    filter(abs(check) > threshold)

  ## BI35----
  BI35 <- data |>
    filter(
      ref_sector == "S13",
      (sto == "OTR" & accounting_entry == "C") |
        (sto == "P1O" & accounting_entry == "C") |
        (sto %in% c("D2", "D39", "D4", "D5", "D61", "D7", "D9") & accounting_entry == "C")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto, values_from = obs_value) |>
    select(
      ref_area, ref_sector, time_period,
      P1O.C, D2.C, D39.C, D4.C, D5.C, D61.C, D7.C, D9.C,OTR.C
    ) |>
    rowwise() |>
    mutate(
      OTR_calc  = P1O.C + D2.C + D39.C + D4.C + D5.C + D61.C + D7.C + D9.C,
      check = round(OTR.C - OTR_calc, rounding)
    ) |>
    filter(abs(check) > threshold)

  ## BI36----
  BI36 <- data |>
    filter(
      ref_sector == "S13",
      (sto == "OTE" & accounting_entry == "D") |
        (sto %in% c(
          "P2", "P5", "D1", "D29", "D3", "D4", "D5", "D62", "D632",
          "D7", "D8", "D9", "NP"
        ) & accounting_entry == "D")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto, values_from = obs_value) |>
    select(
      ref_area, ref_sector, time_period,
      P2.D, P5.D, D1.D, D29.D, D3.D, D4.D, D5.D,
      D62.D, D632.D, D7.D, D8.D, D9.D, NP.D,OTE.D
    ) |>
    rowwise() |>
    mutate(
      OTE_calc = P2.D + P5.D + D1.D + D29.D + D3.D + D4.D + D5.D +
        D62.D + D632.D + D7.D + D8.D + D9.D + NP.D,
      check = round(OTE.D - OTE_calc, rounding)
    ) |>
    filter(abs(check) > threshold)

  ## BI37----
  BI37 <- data |>
    filter(
      ref_sector == "S13",
      (sto == "P31" & accounting_entry == "D") |
        (sto == "D63" & accounting_entry == "D")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto, values_from = obs_value) |>
    select(ref_area, ref_sector, time_period, P31.D, D63.D) |>
    mutate(
      `P31.D - D63.D`  = P31.D - D63.D) |>
    filter(abs(`P31.D - D63.D`) > threshold)

  ## BI38----
  BI38 <- data |>
    filter(
      ref_sector == "S13",
      (sto == "P3" & accounting_entry == "D") |
        (sto == "P1" & accounting_entry == "C") |
        (sto == "P1O" & accounting_entry == "C") |
        (sto == "D632" & accounting_entry == "D")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(names_from = sto, values_from = obs_value) |>
    select(ref_area, ref_sector, time_period, P3.D, P1.C, P1O.C, D632.D) |>
    rowwise() |>
    #    filter(!is.na(P3.D) | !is.na(P1.C) | !is.na(P1O.C) | !is.na(D632.D)) |>
    mutate(
      `P1.C - P1O.C + D632.D`  = P1.C - P1O.C + D632.D,
      check = round(P3.D - `P1.C - P1O.C + D632.D`, rounding)
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



