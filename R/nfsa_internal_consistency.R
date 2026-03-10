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
#' @param clean_NA (optional) A logical value. If `TRUE` it removes the accounting checks for which
#'   there are no values for one (or several) columns.
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
nfsa_internal_consistency <- function(dataset,
                                            output_sel = here::here("output", "internal"),
                                            threshold = 1,
                                            rounding = 1,
                                            clean_NA = TRUE) {
  
  
  library(tidyverse)
  library(arrow)
  library(here)
  library(janitor)
  library(openxlsx)
  
  
  # Helper: Ensures specific columns exist to prevent "Column not found" crashes
  # We fill with NA as requested, not 0.
  ensure_cols <- function(df, cols) {
    add <- setdiff(cols, names(df))
    if (length(add) > 0) df[add] <- NA
    return(df)
  }
  
  data <- dataset |>
    mutate(obs_value = round_half_up(obs_value, rounding)) |>
    nfsa::nfsa_separate_id()
  
  # List to store results for Excel export
  check_results <- list()
  
  # Uses vs Resources-----------------------------------------------------------
  ## UR01-----------------------------------------------------------------------
  # --- 1. Uses vs Resources (UR) ---
  
  # UR01: Standard U/R check
  needed_ur01 <- c("ref_area", "sto", "time_period", "S1.D", "S2.C", "S1.C", "S2.D")
  ur01 <- data |>
    filter(
      sto %in% c(
        "D1", "D4", "D41", "D4N", "D41G", "D42", "D421", "D422", "D43", "D44",
        "D45", "D5", "D6", "D61", "D62", "D7", "D71", "D72", "D7N",
        "D74", "D75", "D8", "D9", "D9N", "D91", "D92", "D99"
      ),
      ref_sector %in% c("S1", "S2")
    ) |>
    pivot_wider(
      names_from = c(ref_sector, accounting_entry),
      values_from = obs_value, names_sep = "."
    ) |>
    ensure_cols(needed_ur01) |>
    select(ref_area, sto, time_period, S1.D, S2.C, S1.C, S2.D) |>
    mutate(
      `S1.D + S2.C` = rowSums(across(c(S1.D, S2.C)), na.rm = TRUE),
      `S1.C + S2.D` = rowSums(across(c(S1.C, S2.D)), na.rm = TRUE),
      `S1.D + S2.C - S1.C - S2.D` = round(`S1.D + S2.C` - `S1.C + S2.D`, rounding)
    ) |>
    filter(abs(`S1.D + S2.C - S1.C - S2.D`) > threshold)
  
  if (clean_NA == TRUE) {
    ur01 <- ur01 |>
      filter(if_all(c("S1.D", "S2.C", "S1.C", "S2.D"), ~ !is.na(.x)))
  }
  
  if (nrow(ur01) > 0) check_results$UR01 <- ur01
  
  # UR02: D2 items
  needed_ur02 <- c("ref_area", "sto", "time_period", "S1.D", "S1.C", "S2.D")
  ur02 <- data |>
    filter(sto %in% c("D2", "D21", "D29"), ref_sector %in% c("S1", "S2")) |>
    pivot_wider(
      names_from = c(ref_sector, accounting_entry),
      values_from = obs_value, names_sep = "."
    ) |>
    ensure_cols(needed_ur02) |>
    select(ref_area, sto, time_period, S1.D, S1.C, S2.D) |>
    mutate(
      `S1.C + S2.D` = rowSums(across(c(S1.C, S2.D)), na.rm = TRUE),
      `S1.D - S1.C + S2.D` = round(S1.D - `S1.C + S2.D`, rounding)
    ) |>
    filter(abs(`S1.D - S1.C + S2.D`) > threshold)
  
  if (clean_NA == TRUE) {
    ur02 <- ur02 |>
      filter(if_all(c("S2.D"), ~ !is.na(.x)))
  }
  
  if (nrow(ur02) > 0) check_results$UR02 <- ur02
  
  ## UR03---------------------------------------------------------------------
  needed_ur03 <- c("ref_area", "sto", "time_period", "S1.D", "S2.C", "S1.C")
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
    ensure_cols(needed_ur03) |>
    select(ref_area, sto, time_period, S1.D, S2.C, S1.C) |>
    mutate(
      `S1.D + S2.C` = rowSums(across(c(S1.D, S2.C)), na.rm = TRUE),
      check = round(S1.C - `S1.D + S2.C`, rounding)
    ) |>
    filter(abs(check) > threshold)
  
  if (clean_NA == TRUE) {
    ur03 <- ur03 |>
      filter(if_all(c("S1.D", "S1.C", "S2.C"), ~ !is.na(.x)))
  }
  
  if (nrow(ur03) > 0) check_results$UR03 <- ur03
  
  ## UR04---------------------------------------------------------------------
  needed_ur04 <- c("ref_area", "sto", "time_period", "S1.D", "S1.C")
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
    ensure_cols(needed_ur04) |>
    select(ref_area, sto, time_period, S1.D, S1.C) |>
    mutate(
      `S1.D - S1.C` = rowSums(across(c(S1.D)), na.rm = TRUE) -
        rowSums(across(c(S1.C)), na.rm = TRUE)
    ) |>
    filter(abs(round(`S1.D - S1.C`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    ur04 <- ur04 |>
      filter(if_all(c("S1.D", "S1.C"), ~ !is.na(.x)))
  }
  
  if (nrow(ur04) > 0) check_results$UR04 <- ur04
  
  
  ## UR05---------------------------------------------------------------------
  needed_ur05 <- c("ref_area", "sto", "time_period", "S1.D", "S2.D")
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
    ensure_cols(needed_ur05) |>
    select(ref_area, sto, time_period, S1.D, S2.D) |>
    mutate(
      `S1.D - S2.D` = rowSums(across(c(S1.D)), na.rm = TRUE) -
        rowSums(across(c(S2.D)), na.rm = TRUE)
    ) |>
    filter(abs(round(`S1.D - S2.D`, rounding)) > threshold)
  
  if (nrow(ur05) > 0) check_results$UR05 <- ur05
  
  
  ## UR06---------------------------------------------------------------------
  needed_ur06 <- c("ref_area", "sto", "time_period", "S1.D", "S2.C")
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
    ensure_cols(needed_ur06) |>
    select(ref_area, sto, time_period, S1.D, S2.C) |>
    mutate(
      `S1.D + S2.C` = rowSums(across(c(S1.D, S2.C)), na.rm = TRUE)
    ) |>
    filter(abs(round(`S1.D + S2.C`, rounding)) > threshold)
  
  
  if (nrow(ur06) > 0) check_results$UR06 <- ur06
  
  ## UR07---------------------------------------------------------------------
  needed_ur07 <- c("ref_area", "sto", "time_period", "S1", "S2")
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
    ensure_cols(needed_ur07) |>
    select(ref_area, sto, time_period, S1, S2) |>
    mutate(
      `S1 + S2` = rowSums(across(c(S1, S2)), na.rm = TRUE)
    ) |>
    filter(abs(round(`S1 + S2`, rounding)) > threshold)
  
  if (nrow(ur07) > 0) check_results$UR07 <- ur07
  
  
  ## UR08---------------------------------------------------------------------
  needed_ur08 <- c("ref_area", "sto", "time_period", "S1.C", "S2.C")
  ur08 <- data |>
    filter(
      sto %in% c(
        "D43", "D74"
      ),
      ref_sector %in% c("S1", "S2")
    ) |>
    pivot_wider(
      names_from = c(ref_sector, accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_ur08) |>
    select(ref_area, sto, time_period, S1.C, S2.C) |>
    mutate(
      `S1.C - S2.C` = rowSums(across(c(S1.C)), na.rm = TRUE) -
        rowSums(across(c(S2.C)), na.rm = TRUE)
    ) |>
    filter(abs(round(`S1.C - S2.C`, rounding)) > threshold)
  
  if (nrow(ur08) > 0) check_results$UR08 <- ur08
  
  
  # S1 vs Sum of Sub-sectors--------------------------------------------------
  ## S1SS01-------------------------------------------------------------------
  
  needed_s1ss01 <- c("ref_area", "sto", "time_period", "S1N", "S11", "S12", "S13", "S1M", "S1")
  
  s1ss01 <- data |>
    dplyr::filter(ref_sector %in% c("S1", "S11", "S12", "S13", "S1M", "S1N")) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    dplyr::filter(sto %in% c(
      "D2.D", "D3.C"
    )) |>
    pivot_wider(
      names_from = c(ref_sector),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_s1ss01) |>
    select(ref_area, sto, time_period, S1N, S11, S12, S13, S1M, S1) |>
    mutate(
      `S1N + S11 + S12 + S13 + S1M` = rowSums(across(c(S1N, S11, S12, S13, S1M)), na.rm = TRUE),
      `S1 - S1N - S11 - S12 - S13 - S1M` = S1 - `S1N + S11 + S12 + S13 + S1M`
    ) |>
    filter(abs(round(`S1 - S1N - S11 - S12 - S13 - S1M`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    s1ss01 <- s1ss01 |>
      filter(if_all(c("S11", "S12", "S1M"), ~ !is.na(.x)))
  }
  
  if (nrow(s1ss01) > 0) check_results$S1SS01 <- s1ss01
  ## S1SS02-------------------------------------------------------------------
  
  needed_s1ss02 <- c("ref_area", "sto", "time_period", "S11", "S12", "S13", "S1M", "S1")
  
  s1ss02 <- data |>
    dplyr::filter(ref_sector %in% c("S1", "S11", "S12", "S13", "S1M")) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    dplyr::filter(sto %in% c(
      "P2.D", "P5.D", "P51G.D", "P5M.D", "D1.D", "D29.D", "D4.D", "D41.D", "D4N.D", "D45.D", "D41G.D",
      "D5.D", "D6.D", "D62.D", "D7.D", "D71.D", "D7N.D", "D75.D", "D8.D", "D9.D", "D9.N", "D99.D",
      "P51.C", "NP.D", "P1.C", "D39.C", "D4.C", "D41.C", "D4N.C", "D42.C", "D43.C", "D44.C", "D45.C",
      "D41G.C", "D6.C", "D61.C", "D7.C", "D7N.C", "D72.C", "D75.C", "D9.C", "D9N.C", "D92.C", "D99.C",
      "P51C.C", "B2A3G.B", "B4G.B", "B5G.B", "B6G.B", "B8G.B", "B101.B", "B9.B", "B9X9F", "EMP.PS", "EMP.HW"
    )) |>
    pivot_wider(
      names_from = c(ref_sector),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_s1ss02) |>
    select(ref_area, sto, time_period, S11, S12, S13, S1M, S1) |>
    mutate(
      `S11 + S12 + S13 + S1M` = rowSums(across(c(S11, S12, S13, S1M)), na.rm = TRUE),
      `S1 - S11 - S12 - S13 - S1M` = S1 - `S11 + S12 + S13 + S1M`
    ) |>
    filter(abs(round(`S1 - S11 - S12 - S13 - S1M`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    s1ss02 <- s1ss02 |>
      filter(if_all(c("S11", "S12", "S1M"), ~ !is.na(.x)))
  }
  
  if (nrow(s1ss02) > 0) check_results$S1SS02 <- s1ss02
  ## S1SS03-------------------------------------------------------------------
  
  needed_s1ss03 <- c("ref_area", "sto", "time_period", "S11", "S12", "S1M", "S1")
  
  s1ss03 <- data |>
    dplyr::filter(ref_sector %in% c("S1", "S11", "S12", "S1M")) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    dplyr::filter(sto %in% c("D43.D", "D91.D")) |>
    pivot_wider(
      names_from = c(ref_sector),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_s1ss03) |>
    select(ref_area, sto, time_period, S11, S12,S1M, S1) |>
    mutate(
      `S11 + S12 + S1M` = rowSums(across(c(S11, S12,  S1M)), na.rm = TRUE),
      `S1 - S11 - S12 - S1M` = S1 - `S11 + S12 + S1M`
    ) |>
    filter(abs(round(`S1 - S11 - S12 - S1M`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    s1ss03 <- s1ss03 |>
      filter(if_all(c("S11", "S12", "S1M"), ~ !is.na(.x)))
  }
  
  if (nrow(s1ss03) > 0) check_results$S1SS03 <- s1ss03
  
  ## S1SS04-------------------------------------------------------------------
  
  needed_s1ss04 <- c("ref_area", "sto", "time_period", "S11", "S12", "S13", "S1")
  
  s1ss04 <- data |>
    dplyr::filter(ref_sector %in% c("S1", "S11", "S12", "S13")) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    dplyr::filter(sto %in% c("D42.D", "D44.D")) |>
    pivot_wider(
      names_from = c(ref_sector),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_s1ss04) |>
    select(ref_area, sto, time_period, S11, S12, S13, S1) |>
    mutate(
      `S11 + S12 + S13` = rowSums(across(c(S11, S12,  S13)), na.rm = TRUE),
      `S1 - S11 - S12 - S13` = S1 - `S11 + S12 + S13`
    ) |>
    filter(abs(round(`S1 - S11 - S12 - S13`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    s1ss04 <- s1ss04 |>
      filter(if_all(c("S11", "S12", "S13"), ~ !is.na(.x)))
  }
  
  if (nrow(s1ss04) > 0) check_results$S1SS04 <- s1ss04
  ## S1SS05-------------------------------------------------------------------
  
  needed_s1ss05 <- c("ref_area", "sto", "time_period", "S11", "S12", "S1")
  
  s1ss05 <- data |>
    dplyr::filter(ref_sector %in% c("S1", "S11", "S12")) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    dplyr::filter(sto %in% c("D43.D")) |>
    pivot_wider(
      names_from = c(ref_sector),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_s1ss05) |>
    select(ref_area, sto, time_period, S11, S12, S1) |>
    mutate(
      `S11 + S12` = rowSums(across(c(S11, S12)), na.rm = TRUE),
      `S1 - S11 - S12` = S1 - `S11 + S12`
    ) |>
    filter(abs(round(`S1 - S11 - S12`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    s1ss05 <- s1ss05 |>
      filter(if_all(c("S11", "S12"), ~ !is.na(.x)))
  }
  
  if (nrow(s1ss05) > 0) check_results$S1SS05 <- s1ss05
  ## S1SS06-------------------------------------------------------------------
  
  needed_s1ss06 <- c("ref_area", "sto", "time_period", "S13", "S12", "S1")
  
  s1ss06 <- data |>
    dplyr::filter(ref_sector %in% c("S1", "S12", "S13")) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    dplyr::filter(sto %in% c("D72.D", "D71.C")) |>
    pivot_wider(
      names_from = c(ref_sector),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_s1ss06) |>
    select(ref_area, sto, time_period, S12, S13, S1) |>
    mutate(
      `S12 + S13` = rowSums(across(c(S12, S13)), na.rm = TRUE),
      `S1 - S12 - S13` = S1 - `S12 + S13`
    ) |>
    filter(abs(round(`S1 - S12 - S13`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    s1ss06 <- s1ss06 |>
      filter(if_all(c("S12"), ~ !is.na(.x)))
  }
  
  if (nrow(s1ss06) > 0) check_results$S1SS06 <- s1ss06
  ## S1SS07-------------------------------------------------------------------
  
  needed_s1ss07 <- c("ref_area", "sto", "time_period", "S13", "S1")
  
  s1ss07 <- data |>
    dplyr::filter(ref_sector %in% c("S1", "S13")) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    dplyr::filter(sto %in% c( "P32.D", "D3.D", "D31.D", "D39.D", "D74.D", "D74_4Y.D", "D76.D", "D92.D", "D2.C", "D21.C",
                              "D211.C", "D212.C", "D214.C", "D29.C", "D5.C", "D51.C", "D59.C", "D74.C", "D91.C")) |>
    pivot_wider(
      names_from = c(ref_sector),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_s1ss07) |>
    select(ref_area, sto, time_period, S13, S1) |>
    mutate(
      `S1 - S13` = S1 - S13
    ) |>
    filter(abs(round(`S1 - S13`, rounding)) > threshold)
  
  if (nrow(s1ss07) > 0) check_results$S1SS07 <- s1ss07
  ## S1SS08-------------------------------------------------------------------
  
  needed_s1ss08 <- c("ref_area", "sto", "time_period", "S13", "S1M", "S1")
  
  s1ss08 <- data |>
    dplyr::filter(ref_sector %in% c("S1", "S13", "S1M")) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    dplyr::filter(sto %in% c("P3.D", "P31.D", "D63.D", "D631.D", "D632.D", "P13.C")) |>
    pivot_wider(
      names_from = c(ref_sector),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_s1ss08) |>
    select(ref_area, sto, time_period, S13, S1M, S1) |>
    mutate(
      `S13 + S1M` = rowSums(across(c(S13, S1M)), na.rm = TRUE),
      `S1 - S13 - S1M` = S1 - `S13 + S1M`
    ) |>
    filter(abs(round(`S1 - S13 - S1M`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    s1ss08 <- s1ss08 |>
      filter(if_all(c("S1M"), ~ !is.na(.x)))
  }
  
  if (nrow(s1ss08) > 0) check_results$S1SS08 <- s1ss08
  ## S1SS09-------------------------------------------------------------------
  
  needed_s1ss09 <- c("ref_area", "sto", "time_period", "S1M", "S1")
  
  s1ss09 <- data |>
    dplyr::filter(ref_sector %in% c("S1", "S1M")) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    dplyr::filter(sto %in% c("D61.D", "D1.C", "D62.C", "D63.C",  "D8.C", "B3G.B")) |>
    pivot_wider(
      names_from = c(ref_sector),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_s1ss09) |>
    select(ref_area, sto, time_period, S1M, S1) |>
    mutate(
      `S1 - S1M` = S1 - `S1M`
    ) |>
    filter(abs(round(`S1 - S1M`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    s1ss09 <- s1ss09 |>
      filter(if_all(c("S1M"), ~ !is.na(.x)))
  }
  
  if (nrow(s1ss09) > 0) check_results$S1SS09 <- s1ss09
  ## S1SS10-------------------------------------------------------------------
  
  needed_s1ss10 <- c("ref_area", "sto", "time_period", "S1N", "S1")
  
  s1ss10 <- data |>
    dplyr::filter(ref_sector %in% c("S1", "S1N")) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    dplyr::filter(sto %in% c( "D21.D", "D31.C", "D21X31.C")) |>
    pivot_wider(
      names_from = c(ref_sector),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_s1ss10) |>
    select(ref_area, sto, time_period, S1N, S1) |>
    mutate(
      `S1 - S1N` = S1 - `S1N`
    ) |>
    filter(abs(round(`S1 - S1N`, rounding)) > threshold)
  
  if (nrow(s1ss10) > 0) check_results$S1SS10 <- s1ss10
  ## S1SS11-------------------------------------------------------------------
  
  needed_s1ss11 <- c("ref_area", "sto", "time_period", "S14", "S1M")
  
  s1ss11 <- data |>
    dplyr::filter(ref_sector %in% c("S14", "S1M")) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    dplyr::filter(sto %in% c(      "B3G.B", "D1.C", "D11.C", "D12.C", "D61.D", "D611.D", "D612.D", "D613.D", "D614.D", "D61SC.D",
                                   "D62.C", "D63.C", "D631.C", "D632.C", "D8.C")) |>
    pivot_wider(
      names_from = c(ref_sector),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_s1ss11) |>
    select(ref_area, sto, time_period, S14, S1M) |>
    mutate(
      `S1M - S14` = S1M - `S14`
    ) |>
    filter(abs(round(`S1M - S14`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    s1ss11 <- s1ss11 |>
      filter(if_all(c("S1M", "S14"), ~ !is.na(.x)))
  }
  
  if (nrow(s1ss11) > 0) check_results$S1SS11 <- s1ss11
  ## S1SS12-------------------------------------------------------------------
  
  needed_s1ss12 <- c("ref_area", "sto", "time_period", "S15", "S1M")
  
  s1ss12 <- data |>
    dplyr::filter(ref_sector %in% c("S15", "S1M")) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    dplyr::filter(sto %in% c("D63.D", "D631.D", "D632.D", "P13.C")) |>
    pivot_wider(
      names_from = c(ref_sector),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_s1ss12) |>
    select(ref_area, sto, time_period, S15, S1M) |>
    mutate(
      `S1M - S15` = S1M - `S15`
    ) |>
    filter(abs(round(`S1M - S15`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    s1ss12 <- s1ss12 |>
      filter(if_all(c("S1M", "S15"), ~ !is.na(.x)))
  }
  if (nrow(s1ss12) > 0) check_results$S1SS12 <- s1ss12
  ## S1SS13-------------------------------------------------------------------
  
  needed_s1ss13 <- c("ref_area", "sto", "time_period", "S14", "S15", "S1M")
  
  s1ss13 <- data |>
    dplyr::filter(ref_sector %in% c("S1M", "S14", "S15")) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    dplyr::filter(sto %in% c(  "P2.D", "P3.D", "P31.D", "P5.D", "P51G.D", "P51G_N111G.D","P51G_N112G.D",
                               "P51G_N1121G.D", "P51G_N1122G.D", "P52.D", "P53.D", "D1.D", "D11.D", "D12.D",
                               "D2.D", "D29.D", "D4.D", "D4N.D","D41.D", "D43.D", "D44.D", "D441.D","D442.D", "D443.D",
                               "D45.D", "D41G.D", "D5.D", "D51.D", "D59.D", "D6.D", "D62.D", "D7.D", "D7N.D", "D71.D",
                               "D75.D", "D8.D", "D9.D", "D9N.D","D91.D", "D99.D", "P51C.D", "NP.D", "P1.C", "P11.C",
                               "P12.C", "D3.C", "D39.C", "D4.C", "D41.C", "D4N.C", "D42.C", "D421.C", "D422.C", "D43.C",
                               "D44.C", "D441.C", "D442.C", "D443.C", "D45.C", "D41G.C", "D6.C", "D61.C", "D611.C",
                               "D612.C", "D613.C", "D614.C", "D61SC.C", "D7.C", "D7N.C","D72.C", "D75.C", "D9.C", "D9N.C", "D92.C",
                               "D99.C", "P51C.C", "B2A3G.B", "B2G.B", "B4G.B", "B5G.B", "B6G.B", "B7G.B", "B8G.B",
                               "B101.B", "B9.B", "B9X9F._Z", "B1G.B", "B1N.B", "EMP.PS", "EMP.HW")) |>
    pivot_wider(
      names_from = c(ref_sector),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_s1ss13) |>
    select(ref_area, sto, time_period, S14, S15, S1M) |>
    mutate(
      `S14 + S15` = rowSums(across(c(S14, S15)), na.rm = TRUE),
      `S1M - S14 - S15` = S1M - `S14 + S15`
    ) |>
    filter(abs(round(`S1M - S14 - S15`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    s1ss13 <- s1ss13 |>
      filter(if_all(c("S14", "S15"), ~ !is.na(.x)))
  }
  if (nrow(s1ss13) > 0) check_results$S1SS13 <- s1ss13
  # I skip the ones we have no data S12003....
  ## S1SS16-------------------------------------------------------------------
  
  needed_s1ss16 <- c("ref_area", "sto", "time_period", "S12K", "S12P", "S12Q", "S12")
  
  s1ss16 <- data |>
    dplyr::filter(ref_sector %in% c("S12K", "S12P", "S12Q", "S12")) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    pivot_wider(
      names_from = c(ref_sector),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_s1ss16) |>
    select(ref_area, sto, time_period, S12K, S12P,S12Q,S12) |>
    mutate(
      `S12K + S12P + S12Q` = rowSums(across(c(S12K, S12P, S12Q)), na.rm = TRUE),
      `S12 - S12K - S12P - S12Q` = S12 - `S12K + S12P + S12Q`
    ) |>
    filter(abs(round(`S12 - S12K - S12P - S12Q`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    s1ss16 <- s1ss16 |>
      filter(if_all(c("S12K", "S12P", "S12Q"), ~ !is.na(.x)))
  }
  
  if (nrow(s1ss16) > 0) check_results$S1SS16 <- s1ss16
  ## S1SS20-------------------------------------------------------------------
  needed_s1ss20 <- c("ref_area", "time_period", "S1.B1GQ.B", "S1N.B1G.B","S11.B1G.B","S12.B1G.B","S13.B1G.B","S1M.B1G.B")
  
  
  s1ss20 <- data |>
    nfsa::nfsa_unite_id() |>
    filter(id %in% c("S1.B1GQ.B", "S1N.B1G.B","S11.B1G.B","S12.B1G.B","S13.B1G.B","S1M.B1G.B")) |>
    pivot_wider(
      names_from = id,
      values_from = obs_value
    ) |>
    ensure_cols(needed_s1ss20) |>
    select(ref_area,time_period, S1N.B1G.B,S11.B1G.B,S12.B1G.B,S13.B1G.B,S1M.B1G.B, S1.B1GQ.B) |>
    mutate(
      `S1.B1GQ.B_cal` = rowSums(across(c(S1N.B1G.B,S11.B1G.B,S12.B1G.B,S13.B1G.B,S1M.B1G.B)), na.rm = TRUE),
      `S1.B1GQ.B - S1.B1GQ.B_cal` = S1.B1GQ.B - S1.B1GQ.B_cal
    ) |>
    filter(abs(round(`S1.B1GQ.B - S1.B1GQ.B_cal`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    s1ss20 <- s1ss20 |>
      filter(if_all(c("S11.B1G.B", "S12.B1G.B", "S1M.B1G.B"), ~ !is.na(.x)))
  }
  if (nrow(s1ss20) > 0) check_results$S1SS20 <- s1ss20
  # SubItems vs Total-----------------------------------------------------------------------------
  
  ## SIT01----------------------------------------------------------------------------------------
  
  needed_sit01 <- c("ref_area", "ref_sector", "time_period", "P31.D", "P32.D","P3.D")
  
  
  sit01 <- data |>
    filter(sto %in% c("P3", "P31", "P32"),
           ref_sector %in% c("S1","S13"),
           accounting_entry == "D") |>
    pivot_wider(
      names_from = c(sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_sit01) |>
    select(ref_area,ref_sector,time_period, P31.D, P32.D,P3.D) |>
    mutate(
      `P31.D + P32.D` = rowSums(across(c(P31.D,P32.D)), na.rm = TRUE),
      `P3.D - P31.D - P32.D` = P3.D - `P31.D + P32.D`
    ) |>
    filter(abs(round(`P3.D - P31.D - P32.D`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    sit01 <- sit01 |>
      filter(if_all(c("P31.D", "P32.D"), ~ !is.na(.x)))
  }
  
  if (nrow(sit01) > 0) check_results$SIT01 <- sit01
  ## SIT02----------------------------------------------------------------------------------------
  
  needed_sit02 <- c("ref_area", "ref_sector", "time_period", "P3.D", "P31.D")
  
  
  sit02 <- data |>
    filter(sto %in% c("P3", "P31"),
           ref_sector %in% c("S1M","S14", "S15"),
           accounting_entry == "D") |>
    pivot_wider(
      names_from = c(sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_sit02) |>
    select(ref_area,ref_sector,time_period, P31.D, P3.D) |>
    mutate(
      `P3.D - P31.D` = rowSums(across(c(P3.D)), na.rm = TRUE) -
        rowSums(across(c(P31.D)), na.rm = TRUE)
    ) |>
    filter(abs(round(`P3.D - P31.D`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    sit02 <- sit02 |>
      filter(if_all(c("P31.D", "P3.D"), ~ !is.na(.x)))
  }
  
  if (nrow(sit02) > 0) check_results$SIT02 <- sit02
  
  ## SIT03----------------------------------------------------------------------------------------
  
  needed_sit03 <- c("ref_area", "ref_sector", "time_period", "P5.D", "P51G.D", "P5M.D")
  
  
  sit03 <- data |>
    filter(sto %in% c("P5", "P51G", "P5M"),
           accounting_entry == "D") |>
    pivot_wider(
      names_from = c(sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_sit03) |>
    select(ref_area,ref_sector,time_period, P51G.D, P5M.D, P5.D) |>
    mutate(
      `P51G.D + P5M.D` = rowSums(across(c(P51G.D,P5M.D)), na.rm = TRUE),
      `P5.D - P51G.D - P5M.D` = P5.D - `P51G.D + P5M.D`
    ) |>
    filter(abs(round(`P5.D - P51G.D - P5M.D`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    sit03 <- sit03 |>
      filter(if_all(c("P51G.D", "P5M.D"), ~ !is.na(.x)))
  }
  if (nrow(sit03) > 0) check_results$SIT03 <- sit03
  
  ## SIT04----------------------------------------------------------------------------------------
  
  needed_sit04 <- c("ref_area", "ref_sector", "time_period", "D2.D", "D21.D", "D29.D")
  
  
  sit04 <- data |>
    filter(sto %in% c("D2", "D21", "D29"),
           accounting_entry == "D",
           ref_sector %in% c("S1", "S2", "S13")) |>
    pivot_wider(
      names_from = c(sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_sit04) |>
    select(ref_area,ref_sector,time_period, D21.D, D29.D, D2.D) |>
    mutate(
      `D21.D + D29.D` = rowSums(across(c(D21.D,D29.D)), na.rm = TRUE),
      `D2.D - D21.D - D29.D` = D2.D - `D21.D + D29.D`
    ) |>
    filter(abs(round(`D2.D - D21.D - D29.D`, rounding)) > threshold)
  
  if (nrow(sit04) > 0) check_results$SIT04 <- sit04
  
  ## SIT05----------------------------------------------------------------------------------------
  
  needed_sit05 <- c("ref_area", "ref_sector", "time_period", "D2.C", "D21.C", "D29.C")
  
  
  sit05 <- data |>
    filter(sto %in% c("D2", "D21", "D29"),
           accounting_entry == "C",
           ref_sector %in% c("S1", "S13")) |>
    pivot_wider(
      names_from = c(sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_sit04) |>
    select(ref_area,ref_sector,time_period, D21.C, D29.C, D2.C) |>
    mutate(
      `D21.C + D29.C` = rowSums(across(c(D21.C,D29.C)), na.rm = TRUE),
      `D2.C - D21.C - D29.C` = D2.C - `D21.C + D29.C`
    ) |>
    filter(abs(round(`D2.C - D21.C - D29.C`, rounding)) > threshold)
  
  if (nrow(sit05) > 0) check_results$SIT05 <- sit05
  
  ## SIT06----------------------------------------------------------------------------------------
  
  needed_sit06 <- c("ref_area", "ref_sector", "time_period", "D2.D", "D21.D")
  
  
  sit06 <- data |>
    filter(sto %in% c("D2", "D21"),
           ref_sector %in% c("S1N"),
           accounting_entry == "D") |>
    pivot_wider(
      names_from = c(sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_sit06) |>
    select(ref_area,ref_sector,time_period, D2.D, D21.D) |>
    mutate(
      `D2.D - D21.D` = rowSums(across(c(D2.D)), na.rm = TRUE) -
        rowSums(across(c(D21.D)), na.rm = TRUE)
    ) |>
    filter(abs(round(`D2.D - D21.D`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    sit06 <- sit06 |>
      filter(if_all(c("D2.D", "D21.D"), ~ !is.na(.x)))
  }
  
  if (nrow(sit06) > 0) check_results$SIT06 <- sit06
  
  ## SIT07----------------------------------------------------------------------------------------
  
  needed_sit07 <- c("ref_area", "ref_sector", "time_period", "D2.D", "D29.D")
  
  
  sit07 <- data |>
    filter(sto %in% c("D2", "D29"),
           !ref_sector %in% c("S1","S1N", "S2"),
           accounting_entry == "D") |>
    pivot_wider(
      names_from = c(sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_sit07) |>
    select(ref_area,ref_sector,time_period, D2.D, D29.D) |>
    mutate(
      `D2.D - D29.D` = rowSums(across(c(D2.D)), na.rm = TRUE) -
        rowSums(across(c(D29.D)), na.rm = TRUE)
    ) |>
    filter(abs(round(`D2.D - D29.D`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    sit07 <- sit07 |>
      filter(if_all(c("D2.D", "D29.D"), ~ !is.na(.x)))
  }
  
  if (nrow(sit07) > 0) check_results$SIT07 <- sit07
  
  ## SIT08----------------------------------------------------------------------------------------
  
  needed_sit08 <- c("ref_area", "ref_sector", "time_period", "D31.D", "D39.D", "D3.D")
  
  
  sit08 <- data |>
    filter(sto %in% c("D3", "D31", "D39"),
           ref_sector %in% c("S1", "S13"),
           accounting_entry == "D") |>
    pivot_wider(
      names_from = c(sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_sit08) |>
    select(ref_area,ref_sector,time_period, D31.D, D39.D, D3.D) |>
    mutate(
      `D31.D + D39.D` = rowSums(across(c(D31.D,D39.D)), na.rm = TRUE),
      `D3.D - D31.D - D39.D` = D3.D - `D31.D + D39.D`
    ) |>
    filter(abs(round(`D3.D - D31.D - D39.D`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    sit08 <- sit08 |>
      filter(if_all(c("D31.D", "D39.D"), ~ !is.na(.x)))
  }
  
  if (nrow(sit08) > 0) check_results$SIT08 <- sit08
  
  
  ## SIT09----------------------------------------------------------------------------------------
  
  needed_sit09 <- c("ref_area", "ref_sector", "time_period", "D31.C", "D39.C", "D3.C")
  
  
  sit09 <- data |>
    filter(sto %in% c("D3", "D31", "D39"),
           ref_sector %in% c("S1", "S2"),
           accounting_entry == "C") |>
    pivot_wider(
      names_from = c(sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_sit09) |>
    select(ref_area,ref_sector,time_period, D31.C, D39.C, D3.C) |>
    mutate(
      `D31.C + D39.C` = rowSums(across(c(D31.C,D39.C)), na.rm = TRUE),
      `D3.C - D31.C - D39.C` = D3.C - `D31.C + D39.C`
    ) |>
    filter(abs(round(`D3.C - D31.C - D39.C`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    sit09 <- sit09 |>
      filter(if_all(c("D31.C", "D39.C"), ~ !is.na(.x)))
  }
  
  if (nrow(sit09) > 0) check_results$SIT09 <- sit09
  
  ## SIT10----------------------------------------------------------------------------------------
  
  needed_sit10 <- c("ref_area", "ref_sector", "time_period", "D41", "D4N", "D4")
  
  
  sit10 <- data |>
    filter(sto %in% c("D4", "D41", "D4N")) |>
    pivot_wider(
      names_from = c(sto),
      values_from = obs_value
    ) |>
    ensure_cols(needed_sit10) |>
    select(ref_area,ref_sector,accounting_entry,time_period, D41, D4N, D4) |>
    mutate(
      `D41 + D4N` = rowSums(across(c(D41,D4N)), na.rm = TRUE),
      `D4 - D41 - D4N` = D4 - `D41 + D4N`
    ) |>
    filter(abs(round(`D4 - D41 - D4N`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    sit10 <- sit10 |>
      filter(if_all(c("D41", "D4N"), ~ !is.na(.x)))
  }
  
  if (nrow(sit10) > 0) check_results$SIT10 <- sit10
  
  ## SIT11----------------------------------------------------------------------------------------
  
  needed_sit11 <- c("ref_area", "ref_sector", "time_period", "D61", "D62", "D63", "D6")
  
  
  sit11 <- data |>
    filter(sto %in% c("D61", "D62", "D63", "D6"),
           ref_sector != "S2") |>
    pivot_wider(
      names_from = c(sto),
      values_from = obs_value
    ) |>
    ensure_cols(needed_sit11) |>
    select(ref_area,ref_sector,accounting_entry,time_period, D61, D62, D63, D6) |>
    mutate(
      `D61 + D62 + D63` = rowSums(across(c(D61,D62, D63)), na.rm = TRUE),
      `D6 - D61 - D62 - D63` = D6 - `D61 + D62 + D63`
    ) |>
    filter(abs(round(`D6 - D61 - D62 - D63`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    sit11 <- sit11 |>
      filter(if_all(c("D61", "D62", "D63"), ~ !is.na(.x)))
  }
  
  if (nrow(sit11) > 0) check_results$SIT11 <- sit11
  
  ## SIT12----------------------------------------------------------------------------------------
  
  needed_sit12 <- c("ref_area", "ref_sector", "time_period", "D6.D", "D62.D")
  
  
  sit12 <- data |>
    filter(sto %in% c("D6", "D62"),
           str_detect(ref_sector,"S11|S12"),
           accounting_entry == "D") |>
    pivot_wider(
      names_from = c(sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_sit12) |>
    select(ref_area,ref_sector,time_period, D6.D, D62.D) |>
    mutate(
      `D6.D - D62.D` = rowSums(across(c(D6.D)), na.rm = TRUE) -
        rowSums(across(c(D62.D)), na.rm = TRUE)
    ) |>
    filter(abs(round(`D6.D - D62.D`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    sit12 <- sit12 |>
      filter(if_all(c("D6.D", "D62.D"), ~ !is.na(.x)))
  }
  
  if (nrow(sit12) > 0) check_results$SIT12 <- sit12
  
  ## SIT13----------------------------------------------------------------------------------------
  
  needed_sit13 <- c("ref_area", "ref_sector", "time_period", "D62.D", "D63.D", "D6.D")
  
  
  sit13 <- data |>
    filter(sto %in% c("D6", "D62", "D63"),
           ref_sector == "S13",
           accounting_entry == "D")|>
    pivot_wider(
      names_from = c(sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_sit13) |>
    select(ref_area,ref_sector,time_period, D62.D, D63.D, D6.D) |>
    mutate(
      `D62.D + D63.D` = rowSums(across(c(D62.D, D63.D)), na.rm = TRUE),
      `D6.D - D62.D - D63.D` = D6.D - `D62.D + D63.D`
    ) |>
    filter(abs(round(`D6.D - D62.D - D63.D`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    sit13 <- sit13 |>
      filter(if_all(c("D62.D", "D63.D"), ~ !is.na(.x)))
  }
  
  if (nrow(sit13) > 0) check_results$SIT13 <- sit13
  
  ## SIT14----------------------------------------------------------------------------------------
  
  needed_sit14 <- c("ref_area", "ref_sector", "time_period", "D61", "D62", "D6")
  
  
  sit14 <- data |>
    filter(sto %in% c("D6", "D61", "D62"),
           ref_sector == "S2")|>
    pivot_wider(
      names_from = c(sto),
      values_from = obs_value
    ) |>
    ensure_cols(needed_sit14) |>
    select(ref_area,ref_sector,accounting_entry,time_period, D61, D62, D6) |>
    mutate(
      `D61 + D62` = rowSums(across(c(D61, D62)), na.rm = TRUE),
      `D6 - D61 - D62` = D6 - `D61 + D62`
    ) |>
    filter(abs(round(`D6 - D61 - D62`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    sit14 <- sit14 |>
      filter(if_all(c("D61", "D62"), ~ !is.na(.x)))
  }
  
  if (nrow(sit14) > 0) check_results$SIT14 <- sit14
  
  ## SIT15----------------------------------------------------------------------------------------
  
  needed_sit15 <- c("ref_area", "ref_sector", "time_period", "D631", "D632", "D63")
  
  
  sit15 <- data |>
    filter(sto %in% c("D63", "D631", "D632"))|>
    pivot_wider(
      names_from = c(sto),
      values_from = obs_value
    ) |>
    ensure_cols(needed_sit15) |>
    select(ref_area,ref_sector,accounting_entry,time_period, D631, D632, D63) |>
    mutate(
      `D631 + D632` = rowSums(across(c(D631, D632)), na.rm = TRUE),
      `D63 - D631 - D632` = D63 - `D631 + D632`
    ) |>
    filter(abs(round(`D63 - D631 - D632`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    sit15 <- sit15 |>
      filter(if_all(c("D631", "D632"), ~ !is.na(.x)))
  }
  
  if (nrow(sit15) > 0) check_results$SIT15 <- sit15
  
  ## SIT16----------------------------------------------------------------------------------------
  
  needed_sit16 <- c("ref_area", "ref_sector", "time_period", "D74.D", "D74_4Y.D")
  
  
  sit16 <- data |>
    filter(sto %in% c("D74", "D74_4Y"),
           accounting_entry == "D",
           ref_sector %in% c("S1", "S13"))|>
    pivot_wider(
      names_from = c(sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_sit16) |>
    select(ref_area,ref_sector,time_period, D74.D, D74_4Y.D) |>
    filter(D74.D < D74_4Y.D)
  
  if (nrow(sit16) > 0) check_results$SIT16 <- sit16
  
  ## SIT17----------------------------------------------------------------------------------------
  
  needed_sit17 <- c("ref_area", "ref_sector", "time_period", "P61.D", "P62.D", "P6.D")
  
  
  sit17 <- data |>
    filter(sto %in% c("P6", "P61", "P62"),
           ref_sector == "S2")|>
    pivot_wider(
      names_from = c(sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_sit17) |>
    select(ref_area,ref_sector,time_period, P61.D, P62.D, P6.D) |>
    mutate(
      `P61.D + P62.D` = rowSums(across(c(P61.D, P62.D)), na.rm = TRUE),
      `P6.D - P61.D - P62.D` = P6.D - `P61.D + P62.D`
    ) |>
    filter(abs(round(`P6.D - P61.D - P62.D`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    sit17 <- sit17 |>
      filter(if_all(c("P61.D", "P62.D"), ~ !is.na(.x)))
  }
  
  if (nrow(sit17) > 0) check_results$SIT17 <- sit17
  
  ## SIT18----------------------------------------------------------------------------------------
  
  needed_sit18 <- c("ref_area", "ref_sector", "time_period", "P62", "P62F")
  
  
  sit18 <- data |>
    filter(sto %in% c("P62", "P62F"))|>
    pivot_wider(
      names_from = c(sto),
      values_from = obs_value
    ) |>
    ensure_cols(needed_sit18) |>
    select(ref_area,ref_sector,accounting_entry,time_period, P62, P62F) |>
    filter(P62 < P62F)
  
  if (nrow(sit18) > 0) check_results$SIT18 <- sit18
  
  ## SIT19----------------------------------------------------------------------------------------
  
  needed_sit19 <- c("ref_area", "ref_sector", "time_period", "D3.C", "D31.C")
  
  
  sit19 <- data |>
    filter(sto %in% c("D3", "D31"),
           ref_sector %in% c("S1N"),
           accounting_entry == "C") |>
    pivot_wider(
      names_from = c(sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_sit19) |>
    select(ref_area,ref_sector,time_period, D3.C, D31.C) |>
    mutate(
      `D3.C - D31.C` = rowSums(across(c(D3.C)), na.rm = TRUE) -
        rowSums(across(c(D31.C)), na.rm = TRUE)
    ) |>
    filter(abs(round(`D3.C - D31.C`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    sit19 <- sit19 |>
      filter(if_all(c("D3.C", "D31.C"), ~ !is.na(.x)))
  }
  
  if (nrow(sit19) > 0) check_results$SIT19 <- sit19
  
  ## SIT20----------------------------------------------------------------------------------------
  
  needed_sit20 <- c("ref_area", "ref_sector", "time_period", "D3.C", "D39.C")
  
  
  sit20 <- data |>
    filter(sto %in% c("D3", "D39"),
           accounting_entry == "C",
           !ref_sector %in% c("S1", "S2", "S1N")) |>
    pivot_wider(
      names_from = c(sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_sit20) |>
    select(ref_area,ref_sector,time_period, D3.C, D39.C) |>
    mutate(
      `D3.C - D39.C` = rowSums(across(c(D3.C)), na.rm = TRUE) -
        rowSums(across(c(D39.C)), na.rm = TRUE)
    ) |>
    filter(abs(round(`D3.C - D39.C`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    sit20 <- sit20 |>
      filter(if_all(c("D3.C", "D39.C"), ~ !is.na(.x)))
  }
  if (nrow(sit20) > 0) check_results$SIT20 <- sit20
  
  ## SIT21----------------------------------------------------------------------------------------
  
  needed_sit21 <- c("ref_area", "ref_sector", "time_period", "D6.C", "D61.C")
  
  
  sit21 <- data |>
    filter(sto %in% c("D6", "D61"),
           accounting_entry == "C",
           !ref_sector %in% c("S1", "S2", "S1N", "S1M", "S14", "S15")) |>
    pivot_wider(
      names_from = c(sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_sit21) |>
    select(ref_area,ref_sector,time_period, D6.C, D61.C) |>
    mutate(
      `D6.C - D61.C` = rowSums(across(c(D6.C)), na.rm = TRUE) -
        rowSums(across(c(D61.C)), na.rm = TRUE)
    ) |>
    filter(abs(round(`D6.C - D61.C`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    sit21 <- sit21 |>
      filter(if_all(c("D6.C", "D61.C"), ~ !is.na(.x)))
  }
  
  if (nrow(sit21) > 0) check_results$SIT21 <- sit21
  
  ## SIT22----------------------------------------------------------------------------------------
  
  needed_sit22 <- c("ref_area", "ref_sector", "time_period", "P71.C", "P72.C", "P7.C")
  
  
  sit22 <- data |>
    filter(sto %in% c("P7", "P71", "P72"),
           ref_sector == "S2")|>
    pivot_wider(
      names_from = c(sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_sit22) |>
    select(ref_area,ref_sector,time_period, P71.C, P72.C, P7.C) |>
    mutate(
      `P71.C + P72.C` = rowSums(across(c(P71.C, P72.C)), na.rm = TRUE),
      `P7.C - P71.C - P72.C` = P7.C - `P71.C + P72.C`
    ) |>
    filter(abs(round(`P7.C - P71.C - P72.C`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    sit22 <- sit22 |>
      filter(if_all(c("P71.C", "P72.C"), ~ !is.na(.x)))
  }
  
  if (nrow(sit22) > 0) check_results$SIT22 <- sit22
  
  ## SIT23----------------------------------------------------------------------------------------
  
  needed_sit23 <- c("ref_area", "ref_sector", "time_period", "P72", "P72F")
  
  
  sit23 <- data |>
    filter(sto %in% c("P72", "P72F"))|>
    pivot_wider(
      names_from = c(sto),
      values_from = obs_value
    ) |>
    ensure_cols(needed_sit23) |>
    select(ref_area,ref_sector,accounting_entry,time_period, P72, P72F) |>
    filter(P72 < P72F)
  
  if (nrow(sit23) > 0) check_results$SIT23 <- sit23
  ## SIT24----------------------------------------------------------------------------------------
  
  needed_sit24 <- c("ref_area", "ref_sector", "time_period", "D43_I9", "D43_J9", "D43")
  
  
  sit24 <- data |>
    filter(sto %in% c("D43", "D43_I9", "D43_J9"))|>
    pivot_wider(
      names_from = c(sto),
      values_from = obs_value
    ) |>
    ensure_cols(needed_sit24) |>
    select(ref_area,ref_sector,accounting_entry,time_period, D43_I9, D43_J9, D43) |>
    mutate(
      `D43_I9 + D43_J9` = rowSums(across(c(D43_I9, D43_J9)), na.rm = TRUE),
      `D43 - D43_I9 - D43_J9` = D43 - `D43_I9 + D43_J9`
    ) |>
    filter(abs(round(`D43 - D43_I9 - D43_J9`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    sit24 <- sit24 |>
      filter(if_all(c("D43_I9", "D43_J9"), ~ !is.na(.x)))
  }
  
  if (nrow(sit24) > 0) check_results$SIT24 <- sit24
  ## SIT25----------------------------------------------------------------------------------------
  
  needed_sit25 <- c("ref_area", "ref_sector", "time_period", "D43_B6", "D43_D6", "D43")
  
  
  sit25 <- data |>
    filter(sto %in% c("D43", "D43_B6", "D43_D6"))|>
    pivot_wider(
      names_from = c(sto),
      values_from = obs_value
    ) |>
    ensure_cols(needed_sit25) |>
    select(ref_area,ref_sector,accounting_entry,time_period, D43_B6, D43_D6, D43) |>
    mutate(
      `D43_B6 + D43_D6` = rowSums(across(c(D43_B6, D43_D6)), na.rm = TRUE),
      `D43 - D43_B6 - D43_D6` = D43 - `D43_B6 + D43_D6`
    ) |>
    filter(abs(round(`D43 - D43_B6 - D43_D6`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    sit25 <- sit25 |>
      filter(if_all(c("D43_B6", "D43_D6"), ~ !is.na(.x)))
  }
  
  if (nrow(sit25) > 0) check_results$SIT25 <- sit25
  
  ## SIT26----------------------------------------------------------------------------------------
  
  needed_sit26 <- c("ref_area", "ref_sector", "time_period", "D42", "D43", "D44", "D45", "D4N")
  
  
  sit26 <- data |>
    filter(sto %in% c("D42", "D43", "D44", "D45", "D4N"))|>
    pivot_wider(
      names_from = c(sto),
      values_from = obs_value
    ) |>
    ensure_cols(needed_sit26) |>
    select(ref_area,ref_sector,accounting_entry,time_period, D42, D43, D44, D45, D4N) |>
    mutate(
      `D42 + D43 + D44 + D45` = rowSums(across(c(D42, D43, D44, D45)), na.rm = TRUE),
      `D4N - D42 - D43 - D44 - D45` = D4N - `D42 + D43 + D44 + D45`
    ) |>
    filter(abs(round(`D4N - D42 - D43 - D44 - D45`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    sit26 <- sit26 |>
      filter(if_all(c("D42", "D43", "D44", "D45", "D4N"), ~ !is.na(.x)))
  }
  
  if (nrow(sit26) > 0) check_results$SIT26 <- sit26
  ## SIT27----------------------------------------------------------------------------------------
  
  needed_sit27 <- c("ref_area", "ref_sector", "time_period", "D4N.D", "D45.D")
  
  
  sit27 <- data |>
    filter(sto %in% c("D45", "D4N"),
           accounting_entry == "D",
           ref_sector %in% c("S1M", "S14", "S15"))|>
    pivot_wider(
      names_from = c(sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_sit27) |>
    select(ref_area,ref_sector,time_period, D4N.D, D45.D) |>
    mutate(
      `D4N.D - D45.D` = rowSums(across(c(D4N.D)), na.rm = TRUE) -
        rowSums(across(c(D45.D)), na.rm = TRUE)
    ) |>
    filter(abs(round(`D4N.D - D45.D`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    sit27 <- sit27 |>
      filter(if_all(c("D4N.D", "D45.D"), ~ !is.na(.x)))
  }
  
  if (nrow(sit27) > 0) check_results$SIT27 <- sit27
  ## SIT28----------------------------------------------------------------------------------------
  
  needed_sit28 <- c("ref_area", "ref_sector", "time_period", "D71", "D72", "D7N", "D7")
  
  
  sit28 <- data |>
    filter(sto %in% c("D71", "D72", "D7N", "D7"))|>
    pivot_wider(
      names_from = c(sto),
      values_from = obs_value
    ) |>
    ensure_cols(needed_sit28) |>
    select(ref_area,ref_sector,accounting_entry,time_period, D71, D72, D7N, D7) |>
    mutate(
      `D71 + D72 + D7N` = rowSums(across(c(D71, D72, D7N)), na.rm = TRUE),
      `D7 - D71 - D72 - D7N` = D7 - `D71 + D72 + D7N`
    ) |>
    filter(abs(round(`D7 - D71 - D72 - D7N`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    sit28 <- sit28 |>
      filter(if_all(c("D71", "D72", "D7N"), ~ !is.na(.x)))
  }
  
  if (nrow(sit28) > 0) check_results$SIT28 <- sit28
  ## SIT29----------------------------------------------------------------------------------------
  
  needed_sit29 <- c("ref_area", "ref_sector", "time_period", "D71.D", "D7N.D", "D7.D")
  
  
  sit29 <- data |>
    filter(sto %in% c("D71","D7N", "D7"),
           accounting_entry == "D",
           ref_sector %in% c("S11", "S1M", "S14", "S15"))|>
    pivot_wider(
      names_from = c(sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_sit29) |>
    select(ref_area,ref_sector,time_period, D71.D, D7N.D, D7.D) |>
    mutate(
      `D71.D + D7N.D` = rowSums(across(c(D71.D, D7N.D)), na.rm = TRUE),
      `D7.D - D71.D - D7N.D` = D7.D - `D71.D + D7N.D`
    ) |>
    filter(abs(round(`D7.D - D71.D - D7N.D`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    sit29 <- sit29 |>
      filter(if_all(c("D71.D","D7N.D"), ~ !is.na(.x)))
  }
  
  if (nrow(sit29) > 0) check_results$SIT29 <- sit29
  ## SIT30----------------------------------------------------------------------------------------
  
  needed_sit30 <- c("ref_area", "ref_sector", "time_period", "D74.D", "D75.D", "D76.D", "D7N.D")
  
  
  sit30 <- data |>
    filter(sto %in% c("D74","D75", "D76", "D7N"),
           accounting_entry == "D",
           ref_sector %in% c("S1", "S13", "S2"))|>
    pivot_wider(
      names_from = c(sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_sit30) |>
    select(ref_area,ref_sector,time_period, D74.D,D75.D, D76.D,D7N.D) |>
    mutate(
      `D74.D + D75.D + D76.D` = rowSums(across(c(D74.D, D75.D, D76.D)), na.rm = TRUE),
      `D7N.D - D74.D - D75.D - D76.D` = D7N.D - `D74.D + D75.D + D76.D`
    ) |>
    filter(abs(round(`D7N.D - D74.D - D75.D - D76.D`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    sit30 <- sit30 |>
      filter(if_all(c("D74.D","D75.D", "D76.D", "D7N.D"), ~ !is.na(.x)))
  }
  
  if (nrow(sit30) > 0) check_results$SIT30 <- sit30
  ## SIT31----------------------------------------------------------------------------------------
  
  needed_sit31 <- c("ref_area", "ref_sector", "time_period", "D7N", "D75")
  
  
  sit31 <- data |>
    filter(sto %in% c("D75", "D7N"),
           !ref_sector %in% c("S1", "S2", "S13"))|>
    pivot_wider(
      names_from = c(sto),
      values_from = obs_value
    ) |>
    ensure_cols(needed_sit31) |>
    select(ref_area,ref_sector,accounting_entry,time_period, D7N, D75) |>
    mutate(
      `D7N - D75` = rowSums(across(c(D7N)), na.rm = TRUE) -
        rowSums(across(c(D75)), na.rm = TRUE)
    ) |>
    filter(abs(round(`D7N - D75`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    sit31 <- sit31 |>
      filter(if_all(c("D75", "D7N"), ~ !is.na(.x)))
  }
  
  if (nrow(sit31) > 0) check_results$SIT31 <- sit31
  ## SIT32----------------------------------------------------------------------------------------
  
  needed_sit32 <- c("ref_area", "ref_sector", "time_period", "D7N.C", "D74.C", "D75.C")
  
  
  sit32 <- data |>
    filter(sto %in% c("D74","D75", "D7N"),
           ref_sector %in% c("S1", "S2", "S13"))|>
    pivot_wider(
      names_from = c(sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_sit32) |>
    select(ref_area,ref_sector,time_period, D74.C, D75.C, D7N.C) |>
    mutate(
      `D74.C + D75.C` = rowSums(across(c(D74.C,D75.C)), na.rm = TRUE),
      `D7N.C - D74.C - D75.C` = D7N.C - `D74.C + D75.C`
    ) |>
    filter(abs(round(`D7N.C - D74.C - D75.C`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    sit32 <- sit32 |>
      filter(if_all(c("D74.C", "D75.C", "D7N.C"), ~ !is.na(.x)))
  }
  
  if (nrow(sit32) > 0) check_results$SIT32 <- sit32
  ## SIT33----------------------------------------------------------------------------------------
  
  needed_sit33 <- c("ref_area", "ref_sector", "time_period", "D91", "D9N", "D9")
  
  
  sit33 <- data |>
    filter(sto %in% c("D91","D9N", "D9"))|>
    pivot_wider(
      names_from = c(sto),
      values_from = obs_value
    ) |>
    ensure_cols(needed_sit33) |>
    select(ref_area,ref_sector,accounting_entry,time_period, D91, D9N, D9) |>
    mutate(
      `D91 + D9N` = rowSums(across(c(D91, D9N)), na.rm = TRUE),
      `D9 - D91 - D9N` = D9 - `D91 + D9N`
    ) |>
    filter(abs(round(`D9 - D91 - D9N`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    sit33 <- sit33 |>
      filter(if_all(c("D91","D9N"), ~ !is.na(.x)))
  }
  
  if (nrow(sit33) > 0) check_results$SIT33 <- sit33
  ## SIT34----------------------------------------------------------------------------------------
  
  needed_sit34 <- c("ref_area", "ref_sector", "time_period", "D9N", "D9")
  
  
  sit34 <- data |>
    filter(sto %in% c("D9", "D9N"),
           !ref_sector %in% c("S1", "S13", "S2"),
           accounting_entry == "C")|>
    pivot_wider(
      names_from = c(sto),
      values_from = obs_value
    ) |>
    ensure_cols(needed_sit34) |>
    select(ref_area,ref_sector,accounting_entry,time_period, D9N, D9) |>
    mutate(
      `D9 - D9N` = rowSums(across(c(D9)), na.rm = TRUE) -
        rowSums(across(c(D9N)), na.rm = TRUE)
    ) |>
    filter(abs(round(`D9 - D9N`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    sit34 <- sit34 |>
      filter(if_all(c("D9N"), ~ !is.na(.x)))
  }
  
  if (nrow(sit34) > 0) check_results$SIT34 <- sit34
  ## SIT35----------------------------------------------------------------------------------------
  
  needed_sit35 <- c("ref_area", "ref_sector", "time_period", "D92", "D99", "D9N")
  
  
  sit35 <- data |>
    filter(sto %in% c("D92","D99", "D9N"))|>
    pivot_wider(
      names_from = c(sto),
      values_from = obs_value
    ) |>
    ensure_cols(needed_sit35) |>
    select(ref_area,ref_sector,accounting_entry,time_period, D92, D99, D9N) |>
    mutate(
      `D92 + D99` = rowSums(across(c(D92, D99)), na.rm = TRUE),
      `D9N - D92 - D99` = D9N - `D92 + D99`
    ) |>
    filter(abs(round(`D9N - D92 - D99`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    sit35 <- sit35 |>
      filter(if_all(c("D92", "D99", "D9N"), ~ !is.na(.x)))
  }
  if (nrow(sit35) > 0) check_results$SIT35 <- sit35
  ## SIT36----------------------------------------------------------------------------------------
  
  needed_sit36 <- c("ref_area", "ref_sector", "time_period", "D9N.D", "D99.D")
  
  
  sit36 <- data |>
    filter(sto %in% c("D9N", "D99"),
           !ref_sector %in% c("S1", "S13", "S2"),
           accounting_entry == "D")|>
    pivot_wider(
      names_from = c(sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_sit36) |>
    select(ref_area,ref_sector,time_period, D9N.D, D99.D) |>
    mutate(
      `D9N.D - D99.D` = rowSums(across(c(D9N.D)), na.rm = TRUE) -
        rowSums(across(c(D99.D)), na.rm = TRUE)
    ) |>
    filter(abs(round(`D9N.D - D99.D`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    sit36 <- sit36 |>
      filter(if_all(c("D99.D", "D9N.D"), ~ !is.na(.x)))
  }
  
  if (nrow(sit36) > 0) check_results$SIT36 <- sit36
  ## SIT37----------------------------------------------------------------------------------------
  
  needed_sit37 <- c("ref_area", "ref_sector", "time_period", "D72.C", "D7N.C", "D7.C")
  
  
  sit37 <- data |>
    filter(sto %in% c("D72","D7N", "D7"),
           accounting_entry == "C",
           ref_sector %in% c("S11", "S1M", "S14", "S15"))|>
    pivot_wider(
      names_from = c(sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_sit37) |>
    select(ref_area,ref_sector,time_period, D72.C, D7N.C, D7.C) |>
    mutate(
      `D72.C + D7N.C` = rowSums(across(c(D72.C, D7N.C)), na.rm = TRUE),
      `D7.C - D72.C - D7N.C` = D7.C - `D72.C + D7N.C`
    ) |>
    filter(abs(round(`D7.C - D72.C - D7N.C`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    sit37 <- sit37 |>
      filter(if_all(c("D72.C", "D7N.C"), ~ !is.na(.x)))
  }
  if (nrow(sit37) > 0) check_results$SIT37 <- sit37
  # Balancing items-------------------------------------------------------------------------------
  
  ## BI01-----------------------------------------------------------------------------------------
  needed_bi01 <- c("ref_area", "time_period", "B1GQ.B", "P1.C", "P2.D", "D21X31.C")
  
  bi01 <- data |>
    filter(sto %in% c("B1GQ","P1", "P2", "D21X31"),
           ref_sector %in% c("S1"))|>
    pivot_wider(
      names_from = c(sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_bi01) |>
    select(ref_area,time_period, P1.C, P2.D, D21X31.C, B1GQ.B) |>
    mutate(
      `P1.C - P2.D + D21X31.C` = rowSums(across(c(P1.C, D21X31.C)), na.rm = TRUE) -
        rowSums(across(c(P2.D)), na.rm = TRUE),
      `B1GQ.B - P1.C + P2.D - D21X31.C` = B1GQ.B - `P1.C - P2.D + D21X31.C`
    ) |>
    filter(abs(round(`B1GQ.B - P1.C + P2.D - D21X31.C`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    bi01 <- bi01 |>
      filter(if_all(c("P1.C", "P2.D"), ~ !is.na(.x)))
  }
  
  if (nrow(bi01) > 0) check_results$BI01 <- bi01
  
  ## BI02-----------------------------------------------------------------------------------------
  needed_bi02 <- c("ref_area", "time_period", "ref_sector","D21X31.C", "D21.D", "D31.D")
  
  bi02 <- data |>
    filter(sto %in% c("D21X31","D21", "D31"),
           ref_sector %in% c("S1", "S1N"))|>
    pivot_wider(
      names_from = c(sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_bi02) |>
    select(ref_area,ref_sector,time_period, D21.D, D31.C, D21X31.C) |>
    mutate(
      `D21.D - D31.C` = rowSums(across(c(D21.D)), na.rm = TRUE) -
        rowSums(across(c(D31.C)), na.rm = TRUE),
      `D21X31.C - D21.D + D31.C` = D21X31.C - `D21.D - D31.C`
    ) |>
    filter(abs(round(`D21X31.C - D21.D + D31.C`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    bi02 <- bi02 |>
      filter(if_all(c("D21.D", "D31.C"), ~ !is.na(.x)))
  }
  
  if (nrow(bi02) > 0) check_results$BI02 <- bi02
  
  ## BI03-----------------------------------------------------------------------------------------
  needed_bi03 <- c("ref_area", "time_period", "ref_sector","B1G.B", "D21X31.C")
  
  bi03 <- data |>
    filter(sto %in% c("D21X31","B1G"),
           ref_sector %in% c("S1N"))|>
    pivot_wider(
      names_from = c(sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_bi03) |>
    select(ref_area,ref_sector,time_period, B1G.B, D21X31.C) |>
    mutate(
      `B1G.B - D2131.C` = rowSums(across(c(B1G.B)), na.rm = TRUE) -
        rowSums(across(c(D21X31.C)), na.rm = TRUE)
    ) |>
    filter(abs(round(`B1G.B - D2131.C`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    bi03 <- bi03 |>
      filter(if_all(c("B1G.B"), ~ !is.na(.x)))
  }
  
  if (nrow(bi03) > 0) check_results$BI03 <- bi03
  
  ## BI04-----------------------------------------------------------------------------------------
  needed_bi04 <- c("ref_area", "time_period", "ref_sector","B1G.B", "P1.C", "P2.D")
  
  bi04 <- data |>
    filter(sto %in% c("P1","P2","B1G"),
           !ref_sector %in% c("S1", "S2", "S1N"))|>
    pivot_wider(
      names_from = c(sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_bi04) |>
    select(ref_area,ref_sector,time_period, P1.C, P2.D, B1G.B) |>
    mutate(
      `P1.C - P2.D` = rowSums(across(c(P1.C)), na.rm = TRUE) -
        rowSums(across(c(P2.D)), na.rm = TRUE),
      `B1G.B - P1.C + P2.D` = B1G.B - `P1.C - P2.D`
    ) |>
    filter(abs(round(`B1G.B - P1.C + P2.D`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    bi04 <- bi04 |>
      filter(if_all(c("P1.C", "P2.D"), ~ !is.na(.x)))
  }
  
  if (nrow(bi04) > 0) check_results$BI04 <- bi04
  
  ## BI05-----------------------------------------------------------------------------------------
  needed_bi05 <- c("ref_area", "time_period", "ref_sector","B1GQ.B", "B1NQ.B", "P51C.C")
  
  bi05 <- data |>
    filter(sto %in% c("B1GQ","B1NQ","P51C"),
           ref_sector %in% c("S1"))|>
    pivot_wider(
      names_from = c(sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_bi05) |>
    select(ref_area,ref_sector,time_period, B1GQ.B, P51C.C, B1NQ.B) |>
    mutate(
      `B1GQ.B - P51C.C` = rowSums(across(c(B1GQ.B)), na.rm = TRUE) -
        rowSums(across(c(P51C.C)), na.rm = TRUE),
      `B1NQ.B - B1GQ.B + P51C.C` = B1NQ.B - `B1GQ.B - P51C.C`
    ) |>
    filter(abs(round(`B1NQ.B - B1GQ.B + P51C.C`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    bi05 <- bi05 |>
      filter(if_all(c("P51C.C", "B1NQ.B"), ~ !is.na(.x)))
  }
  
  if (nrow(bi05) > 0) check_results$BI05 <- bi05
  
  ## BI06-----------------------------------------------------------------------------------------
  needed_bi06 <- c("ref_area", "time_period", "ref_sector","B1G.B", "B1N.B", "P51C.C")
  
  bi06 <- data |>
    filter(sto %in% c("B1G","B1N","P51C"),
           !ref_sector %in% c("S1"))|>
    pivot_wider(
      names_from = c(sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_bi06) |>
    select(ref_area,ref_sector,time_period, B1G.B, P51C.C, B1N.B) |>
    mutate(
      `B1G.B - P51C.C` = rowSums(across(c(B1G.B)), na.rm = TRUE) -
        rowSums(across(c(P51C.C)), na.rm = TRUE),
      `B1N.B - B1G.B + P51C.C` = B1N.B - `B1G.B - P51C.C`
    ) |>
    filter(abs(round(`B1N.B - B1G.B + P51C.C`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    bi06 <- bi06 |>
      filter(if_all(c("P51C.C", "B1N.B"), ~ !is.na(.x)))
  }
  
  if (nrow(bi06) > 0) check_results$BI06 <- bi06
  
  ## BI07-----------------------------------------------------------------------------------------
  needed_bi07 <- c("ref_area", "time_period", "ref_sector","S1.B1GQ.B", "S1.P3.D","S1.P5.D","S2.P6.D","S2.P7.C")
  
  bi07 <- data |>
    filter(sto %in% c("B1GQ","P3","P5", "P6", "P7"),
           ref_sector %in% c("S1", "S2"))|>
    pivot_wider(
      names_from = c(ref_sector,sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_bi07) |>
    select(ref_area,time_period, S1.P3.D,S1.P5.D,S2.P6.D,S2.P7.C,S1.B1GQ.B) |>
    mutate(
      `S1.P3.D + S1.P5.D + S2.P6.D - S2.P7.C` = rowSums(across(c(S1.P3.D,S1.P5.D,S2.P6.D)), na.rm = TRUE) -
        rowSums(across(c(S2.P7.C)), na.rm = TRUE),
      `S1.B1GQ.B - S1.P3.D - S1.P5.D - S2.P6.D + S2.P7.C` = S1.B1GQ.B - `S1.P3.D + S1.P5.D + S2.P6.D - S2.P7.C`
    ) |>
    filter(abs(round(`S1.B1GQ.B - S1.P3.D - S1.P5.D - S2.P6.D + S2.P7.C`, rounding)) > threshold)
  
  if (nrow(bi07) > 0) check_results$BI07 <- bi07
  
  ## BI08-----------------------------------------------------------------------------------------
  needed_bi08 <- c("ref_area", "time_period", "ref_sector", "B2A3G.B","B1GQ.B","D3.C","D1.D","D2.D")
  
  bi08 <- data |>
    filter(sto %in% c("B2A3G","B1GQ","D1", "D2", "D3"),
           ref_sector %in% c("S1"))|>
    pivot_wider(
      names_from = c(sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_bi08) |>
    select(ref_area,ref_sector,time_period, ,B1GQ.B,D3.C,D1.D,D2.D, B2A3G.B) |>
    mutate(
      `B1GQ.B + D3.C - D1.D - D2.D` = rowSums(across(c(B1GQ.B,D3.C)), na.rm = TRUE) -
        rowSums(across(c(D1.D,D2.D)), na.rm = TRUE),
      `B2A3G.B - B1GQ.B - D3.C + D1.D + D2.D` = B2A3G.B - `B1GQ.B + D3.C - D1.D - D2.D`
    ) |>
    filter(abs(round(`B2A3G.B - B1GQ.B - D3.C + D1.D + D2.D`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    bi08 <- bi08 |>
      filter(if_all(c("D2.D", "D3.C"), ~ !is.na(.x)))
  }
  
  if (nrow(bi08) > 0) check_results$BI08 <- bi08
  
  ## BI09-----------------------------------------------------------------------------------------
  needed_bi09 <- c("ref_area", "time_period", "ref_sector", "B2A3G.B","B1G.B","D3.C","D1.D","D2.D")
  
  bi09 <- data |>
    filter(sto %in% c("B2A3G","B1G","D1", "D2", "D3"),
           !ref_sector %in% c("S1"))|>
    pivot_wider(
      names_from = c(sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_bi09) |>
    select(ref_area,ref_sector,time_period,B1G.B,D3.C,D1.D,D2.D, B2A3G.B) |>
    mutate(
      `B1G.B + D3.C - D1.D - D2.D` = rowSums(across(c(B1G.B,D3.C)), na.rm = TRUE) -
        rowSums(across(c(D1.D,D2.D)), na.rm = TRUE),
      `B2A3G.B - B1G.B - D3.C + D1.D + D2.D` = B2A3G.B - `B1G.B + D3.C - D1.D - D2.D`
    ) |>
    filter(abs(round(`B2A3G.B - B1G.B - D3.C + D1.D + D2.D`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    bi09 <- bi09 |>
      filter(if_all(c("D2.D", "D3.C"), ~ !is.na(.x)))
  }
  
  if (nrow(bi09) > 0) check_results$BI09 <- bi09
  
  ## BI10-----------------------------------------------------------------------------------------
  needed_bi10 <- c("ref_area", "time_period", "ref_sector", "B4G.B","B2A3G.B", "D4.C","D41.D","D44.D","D45.D")
  
  bi10 <- data |>
    filter(sto %in% c("B4G","B2A3G","D4", "D41", "D44", "D45"),
           ref_sector %in% c("S11", "S12"))|>
    pivot_wider(
      names_from = c(sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_bi10) |>
    select(ref_area,ref_sector,time_period,B2A3G.B,D4.C,D41.D,D44.D,D45.D, B4G.B ) |>
    mutate(
      `B2A3G.B + D4.C - D41.D - D44.D - D45.D` = rowSums(across(c(B2A3G.B,D4.C)), na.rm = TRUE) -
        rowSums(across(c(D41.D,D44.D,D45.D)), na.rm = TRUE),
      `B4G.B - B2A3G.B - D4.C + D41.D + D44.D + D45.D` = B4G.B - `B2A3G.B + D4.C - D41.D - D44.D - D45.D`
    ) |>
    filter(abs(round(`B4G.B - B2A3G.B - D4.C + D41.D + D44.D + D45.D`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    bi10 <- bi10 |>
      filter(if_all(c("D44.D", "D45.D"), ~ !is.na(.x)))
  }
  
  if (nrow(bi10) > 0) check_results$BI10 <- bi10
  
  ## BI11-----------------------------------------------------------------------------------------
  needed_bi11 <- c("ref_area", "time_period", "ref_sector", "B5G.B","B2A3G.B", "D1.C","D2.C","D3.D","D4.C","D4.D")
  
  bi11 <- data |>
    filter(sto %in% c("B5G","B2A3G", "D1","D2","D3","D4","D4"),
           ref_sector %in% c("S1"))|>
    pivot_wider(
      names_from = c(sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_bi11) |>
    select(ref_area,ref_sector,time_period,B2A3G.B,D1.C,D2.C,D3.D,D4.C, D4.D, B5G.B ) |>
    mutate(
      `B2A3G.B + D1.C + D2.C - D3.D + D4.C - D4.D` = rowSums(across(c(B2A3G.B,D1.C,D2.C,D4.C)), na.rm = TRUE) -
        rowSums(across(c(D3.D,D4.D)), na.rm = TRUE),
      `B5G.B - B2A3G.B - D1.C - D2.C + D3.D - D4.C + D4.D` = B5G.B - `B2A3G.B + D1.C + D2.C - D3.D + D4.C - D4.D`
    ) |>
    filter(abs(round(`B5G.B - B2A3G.B - D1.C - D2.C + D3.D - D4.C + D4.D`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    bi11 <- bi11 |>
      filter(if_all(c("D4.C", "D4.D"), ~ !is.na(.x)))
  }
  
  if (nrow(bi11) > 0) check_results$BI11 <- bi11
  
  ## BI12-----------------------------------------------------------------------------------------
  needed_bi12 <- c("ref_area", "time_period", "ref_sector", "B5G.B","B2A3G.B", "D4.C","D4.D")
  
  bi12 <- data |>
    filter(sto %in% c("B5G","B2A3G", "D4"),
           str_detect(ref_sector,"S11|S12"))|>
    pivot_wider(
      names_from = c(sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_bi12) |>
    select(ref_area,ref_sector,time_period,B2A3G.B,D4.C, D4.D, B5G.B ) |>
    mutate(
      `B2A3G.B + D4.C - D4.D` = rowSums(across(c(B2A3G.B,D4.C)), na.rm = TRUE) -
        rowSums(across(c(D4.D)), na.rm = TRUE),
      `B5G.B - B2A3G.B - D4.C + D4.D` = B5G.B - `B2A3G.B + D4.C - D4.D`
    ) |>
    filter(abs(round(`B5G.B - B2A3G.B - D4.C + D4.D`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    bi12 <- bi12 |>
      filter(if_all(c("D4.C", "D4.D"), ~ !is.na(.x)))
  }
  
  if (nrow(bi12) > 0) check_results$BI12 <- bi12
  
  ## BI13-----------------------------------------------------------------------------------------
  needed_bi13 <- c("ref_area", "time_period", "ref_sector", "B5G.B","B2A3G.B", "D2.C", "D3.D","D4.C","D4.D")
  
  bi13 <- data |>
    filter(sto %in% c("B5G","B2A3G", "D2", "D3","D4"),
           ref_sector == "S13")|>
    pivot_wider(
      names_from = c(sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_bi13) |>
    select(ref_area,ref_sector,time_period,B2A3G.B,D2.C, D3.D,D4.C, D4.D, B5G.B ) |>
    mutate(
      `B2A3G.B + D2.C - D3.D + D4.C - D4.D` = rowSums(across(c(B2A3G.B,D2.C, D4.C)), na.rm = TRUE) -
        rowSums(across(c(D3.D,D4.D)), na.rm = TRUE),
      `B5G.B - B2A3G.B - D2.C + D3.D - D4.C + D4.D` = B5G.B - `B2A3G.B + D2.C - D3.D + D4.C - D4.D`
    ) |>
    filter(abs(round(`B5G.B - B2A3G.B - D2.C + D3.D - D4.C + D4.D`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    bi13 <- bi13 |>
      filter(if_all(c("D4.C", "D4.D"), ~ !is.na(.x)))
  }
  
  if (nrow(bi13) > 0) check_results$BI13 <- bi13
  
  ## BI14-----------------------------------------------------------------------------------------
  needed_bi14 <- c("ref_area", "time_period", "ref_sector", "B5G.B","B2A3G.B", "D1.C","D4.C","D4.D")
  
  bi14 <- data |>
    filter(sto %in% c("B5G","B2A3G", "D1","D4"),
           ref_sector %in% c("S1M", "S14", "S15"))|>
    pivot_wider(
      names_from = c(sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_bi14) |>
    select(ref_area,ref_sector,time_period,B2A3G.B,D1.C, D4.C, D4.D, B5G.B ) |>
    mutate(
      `B2A3G.B + D1.C + D4.C - D4.D` = rowSums(across(c(B2A3G.B,D1.C, D4.C)), na.rm = TRUE) -
        rowSums(across(c(D4.D)), na.rm = TRUE),
      `B5G.B - B2A3G.B - D1.C - D4.C + D4.D` = B5G.B - `B2A3G.B + D1.C + D4.C - D4.D`
    ) |>
    filter(abs(round(`B5G.B - B2A3G.B - D1.C - D4.C + D4.D`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    bi14 <- bi14 |>
      filter(if_all(c("D4.C", "D4.D"), ~ !is.na(.x)))
  }
  
  if (nrow(bi14) > 0) check_results$BI14 <- bi14
  
  ## BI15-----------------------------------------------------------------------------------------
  needed_bi15 <- c("ref_area", "time_period", "S1.B5G.B", "S1.B1GQ.B","S2.D1.D","S2.D1.C","S2.D2.D",
                   "S2.D3.C","S2.D4.D","S2.D4.C")
  
  bi15 <- data |>
    filter(sto %in% c("B5G","B1GQ", "D1","D4", "D2", "D3"),
           ref_sector %in% c("S1", "S2"))|>
    pivot_wider(
      names_from = c(ref_sector,sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_bi15) |>
    select(ref_area,time_period,S1.B1GQ.B,S2.D1.D,S2.D1.C,S2.D2.D,S2.D3.C,S2.D4.D,S2.D4.C,S1.B5G.B ) |>
    mutate(
      `S1.B1GQ.B - S2.D1.D + S2.D1.C - S2.D2.D + S2.D3.C - S2.D4.D + S2.D4.C` = rowSums(across(c(S1.B1GQ.B,S2.D1.C,S2.D3.C,S2.D4.C)), na.rm = TRUE) -
        rowSums(across(c(S2.D1.D,S2.D2.D,S2.D4.D)), na.rm = TRUE),
      `S1.B5G.B - S1.B1GQ.B + S2.D1.D - S2.D1.C + S2.D2.D - S2.D3.C + S2.D4.D - S2.D4.C` = S1.B5G.B - `S1.B1GQ.B - S2.D1.D + S2.D1.C - S2.D2.D + S2.D3.C - S2.D4.D + S2.D4.C`
    ) |>
    filter(abs(round(`S1.B5G.B - S1.B1GQ.B + S2.D1.D - S2.D1.C + S2.D2.D - S2.D3.C + S2.D4.D - S2.D4.C`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    bi15 <- bi15 |>
      filter(if_all(c("S2.D1.C", "S2.D1.D","S2.D3.C"), ~ !is.na(.x)))
  }
  
  if (nrow(bi15) > 0) check_results$BI15 <- bi15
  
  ## BI16-----------------------------------------------------------------------------------------
  needed_bi16 <- c("ref_area", "time_period", "ref_sector", "B6G.B", "B5G.B","D5.C","D5.D","D61.C", "D61.D","D62.C",
                   "D62.D","D7.C","D7.D")
  
  bi16 <- data |>
    filter(sto %in% c("B5G","B6G", "D5","D61", "D62", "D7"),
           ref_sector %in% c("S1"))|>
    pivot_wider(
      names_from = c(sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_bi16) |>
    select(ref_area,time_period,ref_sector, B5G.B,D5.C,D5.D,D61.C,D61.D,D62.C,D62.D,D7.C,D7.D, B6G.B ) |>
    mutate(
      `B5G.B + D5.C - D5.D + D61.C - D61.D + D62.C - D62.D + D7.C - D7.D` = rowSums(across(c(B5G.B,D5.C,D61.C,D62.C,D7.C)), na.rm = TRUE) -
        rowSums(across(c(D5.D,D61.D,D62.D,D7.D)), na.rm = TRUE),
      `B6G.B - B5G.B + D5.C - D5.D - D61.C + D61.D - D62.C + D62.D - D7.C + D7.D` = B6G.B - `B5G.B + D5.C - D5.D + D61.C - D61.D + D62.C - D62.D + D7.C - D7.D`
    ) |>
    filter(abs(round(`B6G.B - B5G.B + D5.C - D5.D - D61.C + D61.D - D62.C + D62.D - D7.C + D7.D`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    bi16 <- bi16 |>
      filter(if_all(c("D61.C", "D61.D","D7.C", "D7.D"), ~ !is.na(.x)))
  }
  
  if (nrow(bi16) > 0) check_results$BI16 <- bi16
  
  ## BI17-----------------------------------------------------------------------------------------
  needed_bi17 <- c("ref_area", "time_period", "ref_sector", "B6G.B", "B5G.B","D5.D","D61.C",
                   "D62.D","D7.C","D7.D")
  
  bi17 <- data |>
    filter(sto %in% c("B5G","B6G", "D5","D61", "D62", "D7"),
           str_detect(ref_sector,"S11|S12"))|>
    pivot_wider(
      names_from = c(sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_bi17) |>
    select(ref_area,time_period,ref_sector, B5G.B,D5.D,D61.C,D62.D,D7.C,D7.D, B6G.B ) |>
    mutate(
      `B5G.B - D5.D + D61.C - D62.D + D7.C - D7.D` = rowSums(across(c(B5G.B,D61.C,D7.C)), na.rm = TRUE) -
        rowSums(across(c(D5.D,D62.D,D7.D)), na.rm = TRUE),
      `B6G.B - B5G.B + D5.D - D61.C + D62.D - D7.C + D7.D` = B6G.B - `B5G.B - D5.D + D61.C - D62.D + D7.C - D7.D`
    ) |>
    filter(abs(round(`B6G.B - B5G.B + D5.D - D61.C + D62.D - D7.C + D7.D`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    bi17 <- bi17 |>
      filter(if_all(c("D61.C", "D62.D","D7.C", "D7.D"), ~ !is.na(.x)))
  }
  
  if (nrow(bi17) > 0) check_results$BI17 <- bi17
  
  ## BI18-----------------------------------------------------------------------------------------
  needed_bi18 <- c("ref_area", "time_period", "ref_sector", "B6G.B", "B5G.B","D5.C","D5.D","D61.C",
                   "D62.D","D7.C","D7.D")
  
  bi18 <- data |>
    filter(sto %in% c("B5G","B6G", "D5","D61", "D62", "D7"),
           ref_sector == "S13")|>
    pivot_wider(
      names_from = c(sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_bi18) |>
    select(ref_area,time_period,ref_sector, B5G.B,D5.C,D5.D,D61.C,D62.D,D7.C,D7.D, B6G.B ) |>
    mutate(
      `B5G.B + D5.C - D5.D + D61.C - D62.D + D7.C - D7.D` = rowSums(across(c(B5G.B,D5.C,D61.C,D7.C)), na.rm = TRUE) -
        rowSums(across(c(D5.D,D62.D,D7.D)), na.rm = TRUE),
      `B6G.B - B5G.B - D5.C + D5.D - D61.C + D62.D - D7.C + D7.D` = B6G.B - `B5G.B + D5.C - D5.D + D61.C - D62.D + D7.C - D7.D`
    ) |>
    filter(abs(round(`B6G.B - B5G.B - D5.C + D5.D - D61.C + D62.D - D7.C + D7.D`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    bi18 <- bi18 |>
      filter(if_all(c("D7.C", "D7.D"), ~ !is.na(.x)))
  }
  
  if (nrow(bi18) > 0) check_results$BI18 <- bi18
  
  ## BI19-----------------------------------------------------------------------------------------
  needed_bi19 <- c("ref_area", "time_period", "ref_sector", "B6G.B", "B5G.B","D5.D","D61.C", "D61.D",
                   "D62.C","D62.D","D7.C","D7.D")
  
  bi19 <- data |>
    filter(sto %in% c("B5G","B6G", "D5","D61", "D62", "D7"),
           ref_sector %in% c("S1M", "S15","S15"))|>
    pivot_wider(
      names_from = c(sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_bi19) |>
    select(ref_area,time_period,ref_sector, B5G.B,D5.D,D61.C,D61.D,D62.C,D62.D,D7.C,D7.D, B6G.B ) |>
    mutate(
      `B5G.B - D5.D + D61.C - D61.D + D62.C - D62.D + D7.C - D7.D` = rowSums(across(c(B5G.B,D61.C,D62.C,D7.C)), na.rm = TRUE) -
        rowSums(across(c(D5.D,D61.D,D62.D,D7.D)), na.rm = TRUE),
      `B6G.B - B5G.B + D5.D - D61.C + D61.D - D62.C + D62.D - D7.C + D7.D` = B6G.B - `B5G.B - D5.D + D61.C - D61.D + D62.C - D62.D + D7.C - D7.D`
    ) |>
    filter(abs(round(`B6G.B - B5G.B + D5.D - D61.C + D61.D - D62.C + D62.D - D7.C + D7.D`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    bi19 <- bi19 |>
      filter(if_all(c("D61.C","D61.D", "D62.C","D62.D","D7.C", "D7.D"), ~ !is.na(.x)))
  }
  
  if (nrow(bi19) > 0) check_results$BI19 <- bi19
  
  ## BI20-----------------------------------------------------------------------------------------
  needed_bi20 <- c("ref_area", "time_period", "S1.B6G.B","S1.B5G.B","S2.D5.D","S2.D5.C","S2.D61.D",
                   "S2.D61.C", "S2.D62.D", "S2.D62.C", "S2.D7.D", "S2.D7.C")
  
  bi20 <- data |>
    filter(sto %in% c("B5G","B6G", "D5","D61", "D62", "D7"),
           ref_sector %in% c("S1", "S2"))|>
    pivot_wider(
      names_from = c(ref_sector,sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_bi20) |>
    select(ref_area,time_period,S1.B5G.B,S2.D5.D,S2.D5.C,S2.D61.D,S2.D61.C,S2.D62.D,S2.D62.C,S2.D7.D,S2.D7.C,S1.B6G.B ) |>
    mutate(
      `S1.B5G.B - S2.D5.D + S2.D5.C - S2.D61.D + S2.D61.C - S2.D62.D + S2.D62.C - S2.D7.D + S2.D7.C` =
        rowSums(across(c(S1.B5G.B,S2.D5.C,S2.D61.C,S2.D62.C,S2.D7.C)), na.rm = TRUE) -
        rowSums(across(c(S2.D5.D,S2.D61.D,S2.D62.D,S2.D7.D)), na.rm = TRUE),
      `S1.B6G.B - S1.B5G.B + S2.D5.D - S2.D5.C + S2.D61.D - S2.D61.C + S2.D62.D - S2.D62.C + S2.D7.D - S2.D7.C` = S1.B6G.B - `S1.B5G.B - S2.D5.D + S2.D5.C - S2.D61.D + S2.D61.C - S2.D62.D + S2.D62.C - S2.D7.D + S2.D7.C`
    ) |>
    filter(abs(round(`S1.B6G.B - S1.B5G.B + S2.D5.D - S2.D5.C + S2.D61.D - S2.D61.C + S2.D62.D - S2.D62.C + S2.D7.D - S2.D7.C`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    bi20 <- bi20 |>
      filter(if_all(c("S2.D5.C","S2.D61.D", "S2.D62.C","S2.D62.D","S2.D7.C", "S2.D7.D"), ~ !is.na(.x)))
  }
  if (nrow(bi20) > 0) check_results$BI20 <- bi20
  
  ## BI21-----------------------------------------------------------------------------------------
  needed_bi21 <- c("ref_area", "time_period", "ref_sector", "B7G.B", "B6G.B", "D63.C", "D63.D")
  
  bi21 <- data |>
    filter(sto %in% c("B7G","B6G", "D63"),
           ref_sector %in% c("S1M", "S14", "S15"))|>
    pivot_wider(
      names_from = c(sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_bi21) |>
    select(ref_area,time_period,ref_sector, B6G.B,D63.C,D63.D,B7G.B ) |>
    mutate(
      `B6G.B + D63.C - D63.D` =  rowSums(across(c(B6G.B,D63.C)), na.rm = TRUE) -
        rowSums(across(c(D63.D)), na.rm = TRUE),
      `B7G.B - B6G.B - D63.C + D63.D` = B7G.B - `B6G.B + D63.C - D63.D`
    ) |>
    filter(abs(round(`B7G.B - B6G.B - D63.C + D63.D`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    bi21 <- bi21 |>
      filter(if_all(c("D63.C", "D63.D"), ~ !is.na(.x)))
  }
  
  if (nrow(bi21) > 0) check_results$BI21 <- bi21
  
  ## BI22-----------------------------------------------------------------------------------------
  needed_bi22 <- c("ref_area", "time_period", "ref_sector", "B7G.B", "B6G.B", "D63.D")
  
  bi22 <- data |>
    filter(sto %in% c("B7G","B6G", "D63"),
           ref_sector %in% c("S13"))|>
    pivot_wider(
      names_from = c(sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_bi22) |>
    select(ref_area,time_period,ref_sector, B6G.B,D63.D,B7G.B ) |>
    mutate(
      `B6G.B - D63.D` =  rowSums(across(c(B6G.B)), na.rm = TRUE) -
        rowSums(across(c(D63.D)), na.rm = TRUE),
      `B7G.B - B6G.B + D63.D` = B7G.B - `B6G.B - D63.D`
    ) |>
    filter(abs(round(`B7G.B - B6G.B + D63.D`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    bi22 <- bi22 |>
      filter(if_all(c("D63.D"), ~ !is.na(.x)))
  }
  
  if (nrow(bi22) > 0) check_results$BI22 <- bi22
  
  ## BI23-----------------------------------------------------------------------------------------
  needed_bi23 <- c("ref_area", "time_period", "ref_sector", "B6G.B","D8.C","D8.D","P3.D", "B8G.B")
  
  bi23 <- data |>
    filter(sto %in% c("B8G","B6G", "D8", "P3"),
           ref_sector %in% c("S1", "S1M", "S14", "S15"))|>
    pivot_wider(
      names_from = c(sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_bi23) |>
    select(ref_area,time_period,ref_sector, B6G.B,D8.C,D8.D,P3.D, B8G.B ) |>
    mutate(
      `B6G.B + D8.C - D8.D - P3.D` =  rowSums(across(c(B6G.B, D8.C)), na.rm = TRUE) -
        rowSums(across(c(D8.D,P3.D)), na.rm = TRUE),
      `B8G.B - B6G.B - D8.C + D8.D + P3.D` = B8G.B - `B6G.B + D8.C - D8.D - P3.D`
    ) |>
    filter(abs(round(`B8G.B - B6G.B - D8.C + D8.D + P3.D`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    bi23 <- bi23 |>
      filter(if_all(c("D8.C", "D8.D"), ~ !is.na(.x)))
  }
  
  if (nrow(bi23) > 0) check_results$BI23 <- bi23
  
  ## BI24-----------------------------------------------------------------------------------------
  needed_bi24 <- c("ref_area", "time_period", "ref_sector", "B6G.B","D8.D", "B8G.B")
  
  bi24 <- data |>
    filter(sto %in% c("B8G","B6G", "D8"),
           str_detect(ref_sector,"S11|S12"))|>
    pivot_wider(
      names_from = c(sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_bi24) |>
    select(ref_area,time_period,ref_sector, B6G.B,D8.D, B8G.B ) |>
    mutate(
      `B6G.B  - D8.D` =  rowSums(across(c(B6G.B)), na.rm = TRUE) -
        rowSums(across(c(D8.D)), na.rm = TRUE),
      `B8G.B - B6G.B + D8.D` = B8G.B - `B6G.B  - D8.D`
    ) |>
    filter(abs(round(`B8G.B - B6G.B + D8.D`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    bi24 <- bi24 |>
      filter(if_all(c("D8.D"), ~ !is.na(.x)))
  }
  
  if (nrow(bi24) > 0) check_results$BI24 <- bi24
  
  ## BI25-----------------------------------------------------------------------------------------
  needed_bi25 <- c("ref_area", "time_period", "ref_sector", "B6G.B","D8.D", "P3.D", "B8G.B")
  
  bi25 <- data |>
    filter(sto %in% c("B8G","B6G", "D8", "P3"),
           ref_sector == "S13")|>
    pivot_wider(
      names_from = c(sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_bi25) |>
    select(ref_area,time_period,ref_sector, B6G.B,D8.D, P3.D,B8G.B ) |>
    mutate(
      `B6G.B  - D8.D - P3.D` =  rowSums(across(c(B6G.B)), na.rm = TRUE) -
        rowSums(across(c(D8.D,P3.D)), na.rm = TRUE),
      `B8G.B - B6G.B + D8.D + P3.D` = B8G.B - `B6G.B  - D8.D - P3.D`
    ) |>
    filter(abs(round(`B8G.B - B6G.B + D8.D + P3.D`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    bi25 <- bi25 |>
      filter(if_all(c("D8.D"), ~ !is.na(.x)))
  }
  
  if (nrow(bi25) > 0) check_results$BI25 <- bi25
  
  ## BI26-----------------------------------------------------------------------------------------
  needed_bi26 <- c("ref_area", "time_period", "S1.B8G.B","S1.B6G.B","S2.D8.D","S2.D8.C","S1.P3.D")
  
  bi26 <- data |>
    filter(sto %in% c("B8G","B6G", "D8","P3"),
           ref_sector %in% c("S1", "S2"))|>
    pivot_wider(
      names_from = c(ref_sector,sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_bi26) |>
    select(ref_area,time_period,S1.B6G.B,S2.D8.D,S2.D8.C,S1.P3.D,S1.B8G.B ) |>
    mutate(
      `S1.B6G.B - S2.D8.D + S2.D8.C - S1.P3.D` =
        rowSums(across(c(S1.B6G.B,S2.D8.C)), na.rm = TRUE) -
        rowSums(across(c(S2.D8.D,S1.P3.D)), na.rm = TRUE),
      `S1.B8G.B - S1.B6G.B + S2.D8.D - S2.D8.C - S1.P3.D` = S1.B8G.B - `S1.B6G.B - S2.D8.D + S2.D8.C - S1.P3.D`
    ) |>
    filter(abs(round(`S1.B8G.B - S1.B6G.B + S2.D8.D - S2.D8.C - S1.P3.D`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    bi26 <- bi26 |>
      filter(if_all(c("S2.D8.C","S2.D8.D"), ~ !is.na(.x)))
  }
  
  if (nrow(bi26) > 0) check_results$BI26 <- bi26
  
  
  ## BI27-----------------------------------------------------------------------------------------
  needed_bi27 <- c("ref_area", "time_period", "B101.B","B8G.B","D9.C","D9.D","P51C.C")
  
  bi27 <- data |>
    filter(sto %in% c("B101","B8G","D9","P51C"),
           ref_sector %in% c("S1"))|>
    pivot_wider(
      names_from = c(sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_bi27) |>
    select(ref_area,time_period,ref_sector,B8G.B,D9.C,D9.D,P51C.C,B101.B) |>
    mutate(
      `B8G.B + D9.C - D9.D - P51C.C` =
        rowSums(across(c(B8G.B,D9.C)), na.rm = TRUE) -
        rowSums(across(c(D9.D,P51C.C)), na.rm = TRUE),
      `B101.B - B8G.B - D9.C + D9.D + P51C.C` = B101.B - `B8G.B + D9.C - D9.D - P51C.C`
    ) |>
    filter(abs(round(`B101.B - B8G.B - D9.C + D9.D + P51C.C`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    bi27 <- bi27 |>
      filter(if_all(c("D9.C","D9.D"), ~ !is.na(.x)))
  }
  
  if (nrow(bi27) > 0) check_results$BI27 <- bi27
  
  
  ## BI28-----------------------------------------------------------------------------------------
  needed_bi28 <- c("ref_area", "time_period", "B101.B","B12.B","D9.D","D9.C")
  
  bi28 <- data |>
    filter(sto %in% c("B101","B12","D9"),
           ref_sector %in% c("S2"))|>
    pivot_wider(
      names_from = c(sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_bi28) |>
    select(ref_area,time_period,ref_sector,B12.B,D9.D,D9.C,B101.B) |>
    mutate(
      `B12.B + D9.D - D9.C` =
        rowSums(across(c(B12.B,D9.D)), na.rm = TRUE) -
        rowSums(across(c(D9.C)), na.rm = TRUE),
      `B101.B - B12.B - D9.D + D9.C` = B101.B - `B12.B + D9.D - D9.C`
    ) |>
    filter(abs(round(`B101.B - B12.B - D9.D + D9.C`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    bi28 <- bi28 |>
      filter(if_all(c("D9.C","D9.D"), ~ !is.na(.x)))
  }
  
  if (nrow(bi28) > 0) check_results$BI28 <- bi28
  
  ## BI29-----------------------------------------------------------------------------------------
  needed_bi29 <- c("ref_area", "time_period", "S1.B101.B","S1.B8G.B","S2.D9.D","S2.D9.C","S1.P51C.C")
  
  bi29 <- data |>
    filter(sto %in% c("B101","B8G","D9", "P51C"),
           ref_sector %in% c("S2", "S1"))|>
    pivot_wider(
      names_from = c(ref_sector,sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_bi29) |>
    select(ref_area,time_period,S1.B8G.B,S2.D9.D,S2.D9.C,S1.P51C.C,S1.B101.B) |>
    mutate(
      `S1.B8G.B - S2.D9.D + S2.D9.C - S1.P51C.C` =
        rowSums(across(c(S1.B8G.B,S2.D9.C)), na.rm = TRUE) -
        rowSums(across(c(S2.D9.D, S1.P51C.C)), na.rm = TRUE),
      `S1.B101.B - S1.B8G.B + S2.D9.D - S2.D9.C + S1.P51C.C` = S1.B101.B - `S1.B8G.B - S2.D9.D + S2.D9.C - S1.P51C.C`
    ) |>
    filter(abs(round(`S1.B101.B - S1.B8G.B + S2.D9.D - S2.D9.C + S1.P51C.C`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    bi29 <- bi29 |>
      filter(if_all(c("S2.D9.C","S2.D9.D"), ~ !is.na(.x)))
  }
  
  if (nrow(bi29) > 0) check_results$BI29 <- bi29
  
  ## BI30-----------------------------------------------------------------------------------------
  needed_bi30 <- c("ref_area", "time_period", "B8G.B","D9.C","D9.D","P5.D","NP.D", "B9.B")
  
  bi30 <- data |>
    filter(sto %in% c("B8G","B9","NP", "P5", "D9"),
           !ref_sector %in% c("S2"))|>
    pivot_wider(
      names_from = c(sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_bi30) |>
    select(ref_area,time_period,ref_sector,B8G.B,D9.C,D9.D,P5.D,NP.D, B9.B) |>
    mutate(
      `B8G.B + D9.C - D9.D - P5.D - NP.D` =
        rowSums(across(c(B8G.B,D9.C)), na.rm = TRUE) -
        rowSums(across(c(D9.D, P5.D,NP.D)), na.rm = TRUE),
      `B9.B - B8G.B - D9.C + D9.D + P5.D + NP.D` = B9.B - `B8G.B + D9.C - D9.D - P5.D - NP.D`
    ) |>
    filter(abs(round(`B9.B - B8G.B - D9.C + D9.D + P5.D + NP.D`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    bi30 <- bi30 |>
      filter(if_all(c("D9.C","D9.D"), ~ !is.na(.x)))
  }
  
  if (nrow(bi30) > 0) check_results$BI30 <- bi30
  
  ## BI31-----------------------------------------------------------------------------------------
  needed_bi31 <- c("ref_area", "time_period", "B12.B","D9.C","D9.D","NP.C", "B9.B")
  
  bi31 <- data |>
    filter(sto %in% c("B9","B12","NP", "D9"),
           ref_sector %in% c("S2"))|>
    pivot_wider(
      names_from = c(sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_bi31) |>
    select(ref_area,time_period,ref_sector,B12.B,D9.C,D9.D,NP.C, B9.B) |>
    mutate(
      `B12.B + D9.D - D9.C - NP.C` =
        rowSums(across(c(B12.B,D9.D)), na.rm = TRUE) -
        rowSums(across(c(D9.C, NP.C)), na.rm = TRUE),
      `B9.B - B12.B - D9.D + D9.C + NP.C` = B9.B - `B12.B + D9.D - D9.C - NP.C`
    ) |>
    filter(abs(round(`B9.B - B12.B - D9.D + D9.C + NP.C`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    bi31 <- bi31 |>
      filter(if_all(c("D9.C","D9.D"), ~ !is.na(.x)))
  }
  
  if (nrow(bi31) > 0) check_results$BI31 <- bi31
  
  ## BI32-----------------------------------------------------------------------------------------
  needed_bi32 <- c("ref_area", "time_period","ref_sector", "B9.B","OTR.C","OTE.D")
  
  bi32 <- data |>
    filter(sto %in% c("B9","OTR","OTE"),
           ref_sector %in% c("S13"))|>
    pivot_wider(
      names_from = c(sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_bi32) |>
    select(ref_area,time_period,ref_sector,OTR.C, OTE.D,B9.B,) |>
    mutate(
      `OTR.C - OTE.D` =
        rowSums(across(c(OTR.C)), na.rm = TRUE) -
        rowSums(across(c(OTE.D)), na.rm = TRUE),
      `B9.B - OTR.C + OTE.D` = B9.B - `OTR.C - OTE.D`
    ) |>
    filter(abs(round(`B9.B - OTR.C + OTE.D`, rounding)) > threshold)
  
  if (nrow(bi32) > 0) check_results$BI32 <- bi32
  
  
  ## BI33-----------------------------------------------------------------------------------------
  needed_bi33 <- c("ref_area", "ref_sector","time_period", "B9.B","B101.B","P51C.C", "P5.D", "NP.D")
  
  bi33 <- data |>
    filter(sto %in% c("B9","B101","NP", "P5", "P51C"),
           !ref_sector %in% c("S2"))|>
    pivot_wider(
      names_from = c(sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_bi33) |>
    select(ref_area,time_period,ref_sector,B101.B,P51C.C,P5.D,NP.D,B9.B) |>
    mutate(
      `B101.B + P51C.C - P5.D - NP.D` =
        rowSums(across(c(B101.B,P51C.C)), na.rm = TRUE) -
        rowSums(across(c(P5.D, NP.D)), na.rm = TRUE),
      `B9.B - B101.B - P51C.C + P5.D + NP.D` = B9.B - `B101.B + P51C.C - P5.D - NP.D`
    ) |>
    filter(abs(round(`B9.B - B101.B - P51C.C + P5.D + NP.D`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    bi33 <- bi33 |>
      filter(if_all(c("NP.D"), ~ !is.na(.x)))
  }
  
  if (nrow(bi33) > 0) check_results$BI33 <- bi33
  
  ## BI34-----------------------------------------------------------------------------------------
  needed_bi34 <- c("ref_area", "ref_sector","time_period", "B9.B","B101.B","NP.C")
  
  bi34 <- data |>
    filter(sto %in% c("B9","B101","NP"),
           ref_sector %in% c("S2"))|>
    pivot_wider(
      names_from = c(sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_bi34) |>
    select(ref_area,time_period,ref_sector,B101.B,NP.C,B9.B) |>
    mutate(
      `B101.B - NP.C` =
        rowSums(across(c(B101.B)), na.rm = TRUE) -
        rowSums(across(c(NP.C)), na.rm = TRUE),
      `B9.B - B101.B - NP.C` = B9.B - `B101.B - NP.C`
    ) |>
    filter(abs(round(`B9.B - B101.B - NP.C`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    bi34 <- bi34 |>
      filter(if_all(c("NP.C"), ~ !is.na(.x)))
  }
  
  if (nrow(bi34) > 0) check_results$BI34 <- bi34
  
  ## BI35-----------------------------------------------------------------------------------------
  needed_bi35 <- c("ref_area", "time_period", "S1.B9.B","S1.B8G.B","S2.D9.C", "S2.D9.D", "S1.P5.D","S1.NP.D")
  
  bi35 <- data |>
    filter(sto %in% c("B9","B8G","D9", "P5", "NP"),
           ref_sector %in% c("S1","S2"))|>
    pivot_wider(
      names_from = c(ref_sector,sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_bi35) |>
    select(ref_area,time_period,S1.B8G.B,S2.D9.D,S2.D9.C,S1.P5.D,S1.NP.D,S1.B9.B) |>
    mutate(
      `S1.B8G.B - S2.D9.D + S2.D9.C - S1.P5.D - S1.NP.D` =
        rowSums(across(c(S1.B8G.B,S2.D9.C)), na.rm = TRUE) -
        rowSums(across(c(S2.D9.D,S1.P5.D,S1.NP.D)), na.rm = TRUE),
      `S1.B9.B - S1.B8G.B + S2.D9.D - S2.D9.C + S1.P5.D + S1.NP.D` = S1.B9.B - `S1.B8G.B - S2.D9.D + S2.D9.C - S1.P5.D - S1.NP.D`
    ) |>
    filter(abs(round(`S1.B9.B - S1.B8G.B + S2.D9.D - S2.D9.C + S1.P5.D + S1.NP.D`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    bi35 <- bi35 |>
      filter(if_all(c("S1.NP.D", "S2.D9.C","S2.D9.D"), ~ !is.na(.x)))
  }
  
  if (nrow(bi35) > 0) check_results$BI35 <- bi35
  
  ## BI36-----------------------------------------------------------------------------------------
  needed_bi36 <- c("ref_area", "time_period", "S1.B101.B","S2.B101.B","S1.P5.D","S1.P51C.C")
  
  bi36 <- data |>
    filter(sto %in% c("B101","P5","P51C"),
           ref_sector %in% c("S1","S2"))|>
    pivot_wider(
      names_from = c(ref_sector,sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_bi36) |>
    select(ref_area,time_period,S2.B101.B,S1.P5.D,S1.P51C.C,S1.B101.B) |>
    mutate(
      `- S2.B101.B + S1.P5.D - S1.P51C.C` =
        rowSums(across(c(S1.P5.D)), na.rm = TRUE)-
        rowSums(across(c(S2.B101.B,S1.P51C.C)), na.rm = TRUE),
      `S1.B101.B + S2.B101.B - S1.P5.D + S1.P51C.C` = S1.B101.B - `- S2.B101.B + S1.P5.D - S1.P51C.C`
    ) |>
    filter(abs(round(`S1.B101.B + S2.B101.B - S1.P5.D + S1.P51C.C`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    bi36 <- bi36 |>
      filter(if_all(c("S2.B101.B", "S1.B101.B"), ~ !is.na(.x)))
  }
  
  if (nrow(bi36) > 0) check_results$BI36 <- bi36
  
  ## BI37-----------------------------------------------------------------------------------------
  needed_bi37 <- c("ref_area", "time_period", "ref_sector", "B11.B","P7.C","P6.C")
  
  bi37 <- data |>
    filter(sto %in% c("B11","P6","P7"),
           ref_sector %in% c("S2"))|>
    pivot_wider(
      names_from = c(sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_bi37) |>
    select(ref_area,time_period,P7.C, P6.D, B11.B) |>
    mutate(
      `P7.C - P6.D` =
        rowSums(across(c(P7.C)), na.rm = TRUE)-
        rowSums(across(c(P6.D)), na.rm = TRUE),
      `B11.B - P7.C + P6-D` = B11.B - `P7.C - P6.D`
    ) |>
    filter(abs(round(`B11.B - P7.C + P6-D`, rounding)) > threshold)
  
  if (nrow(bi37) > 0) check_results$BI37 <- bi37
  
  ## BI38-----------------------------------------------------------------------------------------
  needed_bi38 <- c("ref_area", "time_period", "ref_sector","B11.B","D1.D","D1.C","D2.D","D3.C","D4.D",
                   "D4.C", "D5.D","D5.C","D61.D","D61.C","D62.D","D62.C","D7.D","D7.C","D8.D","D8.C","B12.B")
  
  bi38 <- data |>
    filter(sto %in% c("B11","D1", "D2","D3", "D4", "D5", "D61", "D62", "D7", "D8", "B12"),
           ref_sector %in% c("S2"))|>
    pivot_wider(
      names_from = c(sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_bi38) |>
    select(ref_area,time_period,ref_sector,B11.B,D1.D,D1.C,D2.D,D3.C,D4.D,
           D4.C, D5.D,D5.C,D61.D,D61.C,D62.D,D62.C,D7.D,D7.C,D8.D,D8.C,B12.B) |>
    mutate(
      `B11.B + D1.D - D1.C + D2.D - D3.C + D4.D - D4.C + D5.D - D5.C + D61.D - D61.C + D62.D - D62.C + D7.D - D7.C + D8.D - D8.C` =
        rowSums(across(c(B11.B, D1.D,D2.D,D4.D,D5.D,D61.D,D62.D,D7.D,D8.D)), na.rm = TRUE)-
        rowSums(across(c(D1.C, D3.C,D4.C,D5.C,D61.C,D62.C,D7.C,D8.C)), na.rm = TRUE),
      `B12.B - B11.B - D1.D + D1.C - D2.D + D3.C - D4.D + D4.C - D5.D + D5.C - D61.D + D61.C - D62.D + D62.C - D7.D + D7.C - D8.D + D8.C` =
        B12.B - `B11.B + D1.D - D1.C + D2.D - D3.C + D4.D - D4.C + D5.D - D5.C + D61.D - D61.C + D62.D - D62.C + D7.D - D7.C + D8.D - D8.C`
    ) |>
    filter(abs(round(`B12.B - B11.B - D1.D + D1.C - D2.D + D3.C - D4.D + D4.C - D5.D + D5.C - D61.D + D61.C - D62.D + D62.C - D7.D + D7.C - D8.D + D8.C`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    bi38 <- bi38 |>
      filter(if_all(c("D8.C", "D8.D"), ~ !is.na(.x)))
  }
  
  if (nrow(bi38) > 0) check_results$BI38 <- bi38
  
  ## BI39-----------------------------------------------------------------------------------------
  needed_bi39 <- c("ref_area", "time_period", "ref_sector","OTR.C","P1O.C","D2.C","D39.C","D4.C",
                   "D5.C","D61.C","D7.C","D9.C")
  
  bi39 <- data |>
    filter(sto %in% c("OTR","P1O","D2","D39","D4","D5","D61","D7","D9"),
           ref_sector %in% c("S13"),
           accounting_entry == "C")|>
    pivot_wider(
      names_from = c(sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_bi39) |>
    select(ref_area,time_period,ref_sector,P1O.C,D2.C,D39.C,D4.C,D5.C,D61.C,D7.C,D9.C, OTR.C) |>
    mutate(
      `P1O.C + D2.C + D39.C + D4.C + D5.C + D61.C + D7.C + D9.C` =
        rowSums(across(c(P1O.C,D2.C,D39.C,D4.C,D5.C,D61.C,D7.C,D9.C)), na.rm = TRUE),
      `OTR.C - P1O.C - D2.C - D39.C - D4.C - D5.C - D61.C - D7.C - D9.C` =
        OTR.C - `P1O.C + D2.C + D39.C + D4.C + D5.C + D61.C + D7.C + D9.C`
    ) |>
    filter(abs(round(`OTR.C - P1O.C - D2.C - D39.C - D4.C - D5.C - D61.C - D7.C - D9.C`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    bi39 <- bi39 |>
      filter(if_all(c("D7.C", "D9.C"), ~ !is.na(.x)))
  }
  
  if (nrow(bi39) > 0) check_results$BI39 <- bi39
  
  ## BI40-----------------------------------------------------------------------------------------
  needed_bi40 <- c("ref_area", "time_period", "ref_sector", "OTE.D","P2.D","P5.D","D1.D","D29.D","D3.D","D4.D",
                   "D5.D","D62.D","D632.D","D7.D","D8.D","D9.D","NP.D")
  
  bi40 <- data |>
    filter(sto %in% c("OTE","P2", "P5", "D1","D29","D3", "D4", "D5", "D62", "D632", "D7", "D8", "D9", "NP"),
           ref_sector %in% c("S13"),
           accounting_entry == "D")|>
    pivot_wider(
      names_from = c(sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_bi40) |>
    select(ref_area,time_period,ref_sector,P2.D,P5.D,D1.D,D29.D,D3.D,D4.D,D5.D,D62.D,D632.D,D7.D,D8.D,D9.D,NP.D, OTE.D) |>
    mutate(
      `P2.D + P5.D + D1.D + D29.D + D3.D + D4.D + D5.D + D62.D + D632.D + D7.D + D8.D + D9.D + NP.D` =
        rowSums(across(c(P2.D,P5.D,D1.D,D29.D,D3.D,D4.D,D5.D,D62.D,D632.D,D7.D,D8.D,D9.D,NP.D)), na.rm = TRUE),
      `OTE.D - P2.D - P5.D - D1.D - D29.D - D3.D - D4.D - D5.D - D62.D - D632.D - D7.D - D8.D - D9.D - NP.D` =
        OTE.D - `P2.D + P5.D + D1.D + D29.D + D3.D + D4.D + D5.D + D62.D + D632.D + D7.D + D8.D + D9.D + NP.D`
    ) |>
    filter(abs(round(`OTE.D - P2.D - P5.D - D1.D - D29.D - D3.D - D4.D - D5.D - D62.D - D632.D - D7.D - D8.D - D9.D - NP.D`, rounding)) > threshold)
  
  if (nrow(bi40) > 0) check_results$BI40 <- bi40
  
  ## BI41-----------------------------------------------------------------------------------------
  needed_bi41 <- c("ref_area", "time_period", "ref_sector","P31.D","D63.D")
  
  bi41 <- data |>
    filter(sto %in% c("P31", "D63"),
           ref_sector %in% c("S13"),
           accounting_entry == "D")|>
    pivot_wider(
      names_from = c(sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_bi41) |>
    select(ref_area,time_period,ref_sector,P31.D,D63.D) |>
    mutate(
      `P31.D - D63.D` =
        rowSums(across(c(P31.D)), na.rm = TRUE) - rowSums(across(c(D63.D)), na.rm = TRUE)) |>
    filter(abs(round(`P31.D - D63.D`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    bi41 <- bi41 |>
      filter(if_all(c("D63.D", "P31.D"), ~ !is.na(.x)))
  }
  
  if (nrow(bi41) > 0) check_results$BI41 <- bi41
  ## BI42-----------------------------------------------------------------------------------------
  needed_bi42 <- c("ref_area", "time_period", "ref_sector","P3.D","P1.C","P1O.C","D632.D")
  
  bi42 <- data |>
    filter(sto %in% c("P3","P1", "P1O","D632"),
           ref_sector %in% c("S13"))|>
    pivot_wider(
      names_from = c(sto,accounting_entry),
      values_from = obs_value,
      names_sep = "."
    ) |>
    ensure_cols(needed_bi42) |>
    select(ref_area,time_period,ref_sector,P1.C, P1O.C,D632.D,P3.D) |>
    mutate(
      `P1.C - P1O.C + D632.D` =
        rowSums(across(c(P1.C,D632.D)), na.rm = TRUE) -
        rowSums(across(c(P1O.C)), na.rm = TRUE),
      `P3.D - P1.C - +P1O.C - D632.D` =
        P3.D - `P1.C - P1O.C + D632.D`
    ) |>
    filter(abs(round(`P3.D - P1.C - +P1O.C - D632.D`, rounding)) > threshold)
  
  if (clean_NA == TRUE) {
    bi42<- bi42 |>
      filter(if_all(c("P1O.C","D632.D"), ~ !is.na(.x)))
  }
  
  if (nrow(bi42) > 0) check_results$BI42 <- bi42
  
  map(check_results, ~ mutate(.x, across(everything(), ~ replace(.x, is.na(.x), "NaN"))))
  
  if (length(check_results) == 0) {
    cli::cli_inform("All consistent!")
  } else {
    cli::cli_inform("Writing file")
    openxlsx::write.xlsx(check_results,
                         file = paste0(paste0(output_sel, "/", as.character(format(Sys.time(), "%Y%m%d_%H%M%S")), ".xlsx")),
                         asTable = TRUE,
                         overwrite = TRUE
    )
    
    cli::cli_alert_success(paste0("File created at: ", output_sel, "/", as.character(format(Sys.time(), "%Y%m%d_%H%M%S")), ".xlsx"))
  }
  return(check_results)
}

