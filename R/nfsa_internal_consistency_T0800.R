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
                                            threshold = 0,
                                            rounding = 2) {


  lookup <- nfsa::nfsa_sto_lookup

  data <- dataset |>
    dplyr::mutate(obs_value = janitor::round_half_up(obs_value, rounding)) |>
    nfsa::nfsa_separate_id()

  # Helper function to check if required combinations exist before pivoting
  has_required_combos <- function(df, required_combos) {
    if (nrow(df) == 0) return(FALSE)
    all(required_combos %in% names(df))
  }




  # Uses vs Resources-----------------------------------------------------------
  ## UR01-----------------------------------------------------------------------

  ur01_filtered <- data |>


    dplyr::filter(sto %in% c(
      "D1", "D11", "D12", "D4", "D41", "D41G","D42", "D421","D422", "D43", "D44",
      "D441", "D442", "D443", "D45",  "D5", "D51", "D59", "D6", "D61",
      "D611", "D612", "D613", "D614","D61SC", "D62", "D7", "D71", "D72",
      "D74", "D75", "D8", "D9", "D91", "D92", "D99"
    ),
    ref_sector %in% c("S1", "S2")) |>


    tidyr::pivot_wider(names_from = c(ref_sector, accounting_entry),
                       values_from = obs_value,
                       names_sep = ".")



  if(has_required_combos(ur01_filtered, c("S1.D", "S2.C", "S1.C", "S2.D"))){


    ur01 <- ur01_filtered |>


      dplyr::select(ref_area, sto, time_period, S1.D, S2.C, S1.C, S2.D) |>


      dplyr::rowwise() |>
      dplyr::mutate(
        `S1.D + S2.C` = sum(c(S1.D, S2.C), na.rm = TRUE),
        `S1.C + S2.D` = sum(c(S1.C, S2.D), na.rm = TRUE),
        check = round(`S1.D + S2.C` - `S1.C + S2.D`, rounding)
      ) |>
      dplyr::filter(abs(check) > threshold)


  }

  ## UR02---------------------------------------------------------------------

  ur02_filtered <- data |>


    dplyr::filter(sto %in% c(
      "D2", "D21", "D29"
    ),
    ref_sector %in% c("S1", "S2")) |>


    tidyr::pivot_wider(names_from = c(ref_sector, accounting_entry),
                       values_from = obs_value,
                       names_sep = ".")



  if(has_required_combos(ur02_filtered, c("S1.D", "S1.C", "S2.D"))){


    ur02 <- ur02_filtered |>


      dplyr::select(ref_area, sto, time_period, S1.D, S1.C, S2.D) |>


      dplyr::rowwise() |>
      dplyr::mutate(
        `S1.C + S2.D` = sum(c(S1.C, S2.D), na.rm = TRUE),
        check = round(S1.D - `S1.C + S2.D`, rounding)
      ) |>
      dplyr::filter(abs(check) > threshold)


  }

  ## UR03---------------------------------------------------------------------

  ur03_filtered <- data |>


    dplyr::filter(sto %in% c(
      "D3", "D31", "D39"
    ),
    ref_sector %in% c("S1", "S2")) |>


    tidyr::pivot_wider(names_from = c(ref_sector, accounting_entry),
                       values_from = obs_value,
                       names_sep = ".")



  if(has_required_combos(ur03_filtered, c("S1.D", "S2.C", "S1.C"))){


    ur03 <- ur03_filtered |>


      dplyr::select(ref_area, sto, time_period, S1.D, S2.C, S1.C) |>


      dplyr::rowwise() |>
      dplyr::mutate(
        `S1.D + S2.C` = sum(c(S1.D, S2.C), na.rm = TRUE),
        check = round(S1.C - `S1.D + S2.C`, rounding)
      ) |>
      dplyr::filter(abs(check) > threshold)


  }

  ## UR04---------------------------------------------------------------------

  ur04_filtered <- data |>


    dplyr::filter(sto %in% c(
      "D63", "D631", "D632", "P51C"
    ),
    ref_sector %in% c("S1")) |>


    tidyr::pivot_wider(names_from = c(ref_sector, accounting_entry),
                       values_from = obs_value,
                       names_sep = ".")



  if(has_required_combos(ur04_filtered, c("S1.D", "S1.C"))){


    ur04 <- ur04_filtered |>


      dplyr::select(ref_area, sto, time_period, S1.D, S1.C) |>


      dplyr::rowwise() |>
      dplyr::mutate(
        `S1.D - S1.C` = sum(c(S1.D, -S1.C), na.rm = TRUE),
      ) |>
      dplyr::filter(abs(`S1.D - S1.C`) > threshold)


  }

  ## UR05---------------------------------------------------------------------

  ur05_filtered <- data |>


    dplyr::filter(sto %in% c(
      "D43", "D74", "D76"
    ),
    ref_sector %in% c("S1", "S2")) |>


    tidyr::pivot_wider(names_from = c(ref_sector, accounting_entry),
                       values_from = obs_value,
                       names_sep = ".")



  if(has_required_combos(ur05_filtered, c("S1.D", "S2.D"))){


    ur05 <- ur05_filtered |>


      dplyr::select(ref_area, sto, time_period, S1.D, S2.D) |>


      dplyr::rowwise() |>
      dplyr::mutate(
        `S1.D - S2.D` = sum(c(S1.D, -S2.D), na.rm = TRUE),
      ) |>
      dplyr::filter(abs(`S1.D - S2.D`) > threshold)


  }

  ## UR06---------------------------------------------------------------------

  ur06_filtered <- data |>


    dplyr::filter(sto %in% c(
      "NP"
    ),
    ref_sector %in% c("S1", "S2")) |>


    tidyr::pivot_wider(names_from = c(ref_sector, accounting_entry),
                       values_from = obs_value,
                       names_sep = ".")



  if(has_required_combos(ur06_filtered, c("S1.D", "S2.C"))){


    ur06 <- ur06_filtered |>


      dplyr::select(ref_area, sto, time_period, S1.D, S2.C) |>


      dplyr::rowwise() |>
      dplyr::mutate(
        `S1.D + S2.C` = sum(c(S1.D, S2.C), na.rm = TRUE),
      ) |>
      dplyr::filter(abs(`S1.D + S2.C`) > threshold)


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
    ) |>
    dplyr::select(ref_area, sto, time_period, S1, S2) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `S1 + S2` = sum(c(S1, S2), na.rm = TRUE),
    ) |>
    dplyr::filter(abs(`S1 + S2`) > threshold)

  ## UR08---------------------------------------------------------------------

  ur08_filtered <- data |>


    dplyr::filter(sto %in% c(
      "D43", "D74"
    ),
    ref_sector %in% c("S1", "S2")) |>


    tidyr::pivot_wider(names_from = c(ref_sector,accounting_entry),
                       values_from = obs_value,
                       names_sep = ".")



  if(has_required_combos(ur08_filtered, c("S1.C", "S2.C"))){


    ur08 <- ur08_filtered |>


      dplyr::select(ref_area, sto, time_period, S1.C, S2.C) |>


      dplyr::rowwise() |>
      dplyr::mutate(
        `S1.C - S2.C` = sum(c(S1.C, -S2.C), na.rm = TRUE),
      ) |>
      dplyr::filter(abs(`S1.C - S2.C`) > threshold)


  }

  ## UR09---------------------------------------------------------------------

  ur09_filtered <- data |>


    dplyr::filter(sto %in% c(
      "D12","D611","D612"
    ),
    ref_sector %in% c("S1", "S2")) |>


    tidyr::pivot_wider(names_from = c(ref_sector,sto,accounting_entry),
                       values_from = obs_value,
                       names_sep = ".")



  if(has_required_combos(ur09_filtered, c("S1.D12.D", "S2.D12.C", "S1.D611.C", "S1.D612.C", "S2.D611.D", "S2.D612.D"))){


    ur09 <- ur09_filtered |>


      dplyr::select(ref_area, time_period, S1.D12.D, S2.D12.C,S1.D611.C,S1.D612.C,S2.D611.D,S2.D612.D) |>


      dplyr::rowwise() |>
      dplyr::mutate(
        `S1.D12.D + S2.D12.C` = sum(c(S1.D12.D,S2.D12.C), na.rm = TRUE),
        `S1.D611.C + S1.D612.C + S2.D611.D + S2.D612.D` = sum(c(S1.D611.C,S1.D612.C,S2.D611.D,S2.D612.D), na.rm = TRUE),
        check = round(`S1.D12.D + S2.D12.C` - `S1.D611.C + S1.D612.C + S2.D611.D + S2.D612.D`, rounding)
      ) |>
      dplyr::filter(abs(check) > threshold)


  }
  ## UR10---------------------------------------------------------------------

  ur10_filtered <- data |>


    dplyr::filter(sto %in% c(
      "D442", "D614"
    ),
    ref_sector %in% c("S1", "S2")
    ) |>
    nfsa_unite_id() |>
    dplyr::filter(id %in% c("S1.D442.D", "S2.D442.C", "S1.D614.C", "S2.D614.D")) |>


    tidyr::pivot_wider(names_from = id,
                       values_from = obs_value)



  if(has_required_combos(ur10_filtered, c("S1.D442.D", "S2.D442.C", "S1.D614.C", "S2.D614.D"))){


    ur10 <- ur10_filtered |>


      dplyr::select(ref_area, time_period, S1.D442.D, S2.D442.C, S1.D614.C, S2.D614.D) |>


      dplyr::rowwise() |>
      dplyr::mutate(
        `S1.D442.D, S2.D442.C` = sum(c(S1.D442.D, S2.D442.C), na.rm = TRUE),
        `S1.D614.C + S2.D614.D` = sum(c(S1.D614.C, S2.D614.D), na.rm = TRUE),
        check = round(`S1.D442.D, S2.D442.C` > `S1.D614.C + S2.D614.D`, rounding)
      ) |>
      dplyr::filter(abs(check) > threshold)


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
    ) |>
    dplyr::select(ref_area, sto, time_period, S1N, S11, S12, S13, S1M, S1) |>
    dplyr::rowwise() |>
    dplyr::mutate(`S1N + S11 + S12 + S13 + S1M` = sum(c(S1N, S11, S12, S13, S1M), na.rm = TRUE)) |>
    dplyr::mutate(check = round(S1 - `S1N + S11 + S12 + S13 + S1M`, rounding)) |>
    dplyr::filter(abs(check) > threshold)

  ## S1SS02-------------------------------------------------------------------
  s1ss02 <- data |>
    dplyr::filter(ref_sector %in% c("S1", "S11", "S12", "S13", "S1M")) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    dplyr::filter(sto %in% c(
      "P2.D", "P5.D", "P51G.D", "P51G_N111G.D", "P51G_N112G.D", "P51G_N1121G.D", "P51G_N1122G.D",
      "P52.D", "P53.D", "D1.D", "D11.D", "D12.D", "D29.D", "D4.D", "D41.D", "D44.D", "D441.D",
      "D442.D", "D443.D", "D45.D", "D41G.D", "D5.D", "D51.D", "D59.D", "D6.D", "D62.D", "D7.D",
      "D71.D", "D75.D", "D8.D", "D9.D", "D99.D", "P51.C", "NP.D", "P1.C", "P11.C", "P12.C",
      "D39.C", "D4.C", "D41.C", "D42.C", "D421.C", "D422.C", "D43.C", "D44.C", "D441.C", "D442.C",
      "D443.C", "D45.C", "D41G.C", "D6.C", "D61.C", "D611.C", "D612.C", "D613.C", "D614.C",
      "D61SC.C", "D7.C", "D72.C", "D75.C", "D9.C", "D92.C", "D99.C", "P51C.C", "B2A3G.B",
      "B4G.B", "B5G.B", "B6G.B",  "B8G.B", "B101.B", "B9.B", "B9X9F", "EMP.PS", "EMP.HW"
    )) |>
    tidyr::pivot_wider(
      names_from = ref_sector,
      values_from = obs_value
    ) |>
    dplyr::select(ref_area, sto, time_period, S11, S12, S13, S1M, S1) |>
    dplyr::rowwise() |>
    dplyr::mutate(`S11 + S12 + S13 + S1M` = sum(c(S11, S12, S13, S1M), na.rm = TRUE)) |>
    dplyr::mutate(check = round(S1 - `S11 + S12 + S13 + S1M`, rounding)) |>
    dplyr::filter(abs(check) > threshold)

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
    ) |>
    dplyr::select(ref_area, sto, time_period, S11, S12, S1M, S1) |>
    dplyr::rowwise() |>
    dplyr::mutate(`S11 + S12 + S1M` = sum(c(S11, S12, S1M), na.rm = TRUE)) |>
    dplyr::mutate(check = round(S1 - `S11 + S12 + S1M`, rounding)) |>
    dplyr::filter(abs(check) > threshold)

  ## S1SS04-------------------------------------------------------------------
  s1ss04 <- data |>
    dplyr::filter(ref_sector %in% c("S1", "S11", "S12","S13")) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    dplyr::filter(sto %in% c(
      "D42.D", "D421.D"
    )) |>
    tidyr::pivot_wider(
      names_from = ref_sector,
      values_from = obs_value
    ) |>
    dplyr::select(ref_area, sto, time_period, S11, S12, S13, S1) |>
    dplyr::rowwise() |>
    dplyr::mutate(`S11 + S12 + S13` = sum(c(S11, S12, S13), na.rm = TRUE)) |>
    dplyr::mutate(check = round(S1 - `S11 + S12 + S13`, rounding)) |>
    dplyr::filter(abs(check) > threshold)

  ## S1SS05-------------------------------------------------------------------
  s1ss05 <- data |>
    dplyr::filter(ref_sector %in% c("S1", "S11", "S12")) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    dplyr::filter(sto %in% c(
      "D422.D"
    )) |>
    tidyr::pivot_wider(
      names_from = ref_sector,
      values_from = obs_value
    )
  if(nrow(s1ss05) == 6){
    dplyr::select(ref_area, sto, time_period, S11, S12,  S1) |>
      dplyr::rowwise() |>
      dplyr::mutate(`S11 + S12` = sum(c(S11, S12), na.rm = TRUE)) |>
      dplyr::mutate(check = round(S1 - `S11 + S12`, rounding)) |>
      dplyr::filter(abs(check) > threshold)
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
    ) |>
    dplyr::select(ref_area, sto, time_period, S12, S13,  S1) |>
    dplyr::rowwise() |>
    dplyr::mutate(`S12 + S13` = sum(c(S12, S13), na.rm = TRUE)) |>
    dplyr::mutate(check = round(S1 - `S12 + S13`, rounding)) |>
    dplyr::filter(abs(check) > threshold)

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
    ) |>
    dplyr::select(ref_area, sto, time_period, S13,  S1) |>
    dplyr::rowwise() |>
    dplyr::mutate(check = round(S1 - S13, rounding)) |>
    dplyr::filter(abs(check) > threshold)

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
    ) |>
    dplyr::select(ref_area, sto, time_period, S13, S1M,  S1) |>
    dplyr::rowwise() |>
    dplyr::mutate(`S13 + S1M` = sum(c(S13, S1M), na.rm = TRUE)) |>
    dplyr::mutate(check = round(S1 - `S13 + S1M`, rounding)) |>
    dplyr::filter(abs(check) > threshold)

  ## S1SS09-------------------------------------------------------------------
  s1ss09 <- data |>
    dplyr::filter(ref_sector %in% c("S1",  "S1M")) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    dplyr::filter(sto %in% c(
      "D61.D", "D611.D", "D612.D", "D613.D", "D614.D", "D61SC.D", "D1.C", "D11.C", "D12.C",
      "D62.C", "D63.C", "D631.C", "D632.C", "D8.C", "B3G.B"
    )) |>
    tidyr::pivot_wider(
      names_from = ref_sector,
      values_from = obs_value
    ) |>
    dplyr::select(ref_area, sto, time_period, S1M,  S1) |>
    dplyr::rowwise() |>
    dplyr::mutate(check = round(S1 - S1M, rounding)) |>
    dplyr::filter(abs(check) > threshold)

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
    ) |>
    dplyr::select(ref_area, sto, time_period, S1N,  S1) |>
    dplyr::rowwise() |>
    dplyr::mutate(check = round(S1 - S1N, rounding)) |>
    dplyr::filter(abs(check) > threshold)

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
    ) |>
    dplyr::select(ref_area, sto, time_period, S1M,  S14) |>
    dplyr::rowwise() |>
    dplyr::mutate(check = round(S1M - S14, rounding)) |>
    dplyr::filter(abs(check) > threshold)

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
    ) |>
    dplyr::select(ref_area, sto, time_period, S1M,  S15) |>
    dplyr::rowwise() |>
    dplyr::mutate(check = round(S1M - S15, rounding)) |>
    dplyr::filter(abs(check) > threshold)

  ## S1SS13-------------------------------------------------------------------
  s1ss13 <- data |>
    dplyr::filter(ref_sector %in% c("S1M",  "S14", "S15")) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    dplyr::filter(sto %in% c(
      "P2.D", "P3.D", "P31.D", "P5.D", "P51G.D", "P51G_N111G.D","P51G_N112G.D",
      "P51G_N1121G.D", "P51G_N1122G.D", "P52.D", "P53.D", "D1.D", "D11.D", "D12.D",
      "D2.D", "D29.D", "D4.D", "D41.D", "D43.D", "D44.D", "D441.D","D442.D", "D443.D",
      "D45.D", "D41G.D", "D5.D", "D51.D", "D59.D", "D6.D", "D62.D", "D7.D", "D71.D",
      "D75.D", "D8.D", "D9.D", "D91.D", "D99.D", "P51C.D", "NP.D", "P1.C", "P11.C",
      "P12.C", "D3.C", "D39.C", "D4.C", "D41.C", "D42.C", "D421.C", "D422.C", "D43.C",
      "D44.C", "D441.C", "D442.C", "D443.C", "D45.C", "D41G.C", "D6.C", "D61.C", "D611.C",
      "D612.C", "D613.C", "D614.C", "D61SC.C", "D7.C", "D72.C", "D75.C", "D9.C", "D92.C",
      "D99.C", "P51C.C", "B2A3G.B",  "B4G.B", "B5G.B", "B6G.B", "B7G.B", "B8G.B",
      "B101.B", "B9.B", "B9X9F._Z", "B1G.B", "B1N.B", "EMP.PS", "EMP.HW"
    )) |>
    tidyr::pivot_wider(
      names_from = ref_sector,
      values_from = obs_value
    ) |>
    dplyr::select(ref_area, sto, time_period, S14, S15,  S1M) |>
    dplyr::rowwise() |>
    dplyr::mutate(`S14 + S15` = sum(c(S14, S15), na.rm = TRUE)) |>
    dplyr::mutate(check = round(S1M - `S14 + S15`, rounding)) |>
    dplyr::filter(abs(check) > threshold)

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
    ) |>
    dplyr::select(ref_area,time_period,S1N.B1G.B,S11.B1G.B,S12.B1G.B,S13.B1G.B,S1M.B1G.B, S1.B1GQ.B ) |>
    dplyr::rowwise() |>
    dplyr::mutate(`sum_B1G` = sum(c(S1N.B1G.B,S11.B1G.B,S12.B1G.B,S13.B1G.B,S1M.B1G.B), na.rm = TRUE)) |>
    dplyr::mutate(check = round(S1.B1GQ.B - `sum_B1G`, rounding)) |>
    dplyr::filter(abs(check) > threshold)

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
    ) |>
    dplyr::select(ref_area, ref_sector, time_period, P31.D, P32.D, P3.D) |>
    dplyr::rowwise() |>
    dplyr::mutate(`P31.D + P32.D` = sum(c(P31.D,P32.D), na.rm = TRUE)) |>
    dplyr::mutate(check = round(P3.D - `P31.D + P32.D`, rounding)) |>
    dplyr::filter(abs(check) > threshold)

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
    ) |>
    dplyr::select(ref_area, ref_sector, time_period, P3.D, P31.D) |>
    dplyr::rowwise() |>
    dplyr::mutate(check = round(P3.D - P31.D, rounding)) |>
    dplyr::filter(abs(check) > threshold)


  ## SIT03----------------------------------------------------------------------------------------

  sit03 <- data |>
    dplyr::filter(
      sto %in% c("P5", "P51G", "P52", "P53"),
      accounting_entry == "D"
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(
      names_from = sto,
      values_from = obs_value
    ) |>
    dplyr::select(ref_area, ref_sector, time_period, P51G.D, P52.D,P53.D, P5.D) |>
    dplyr::rowwise() |>
    dplyr::mutate(`P51G.D + P52.D + P53.D` = sum(P51G.D,P52.D,P53.D, na.rm = TRUE),
                  check = round(P5.D - `P51G.D + P52.D + P53.D`, rounding)) |>
    dplyr::filter(abs(check) > threshold)

  ## SIT05----------------------------------------------------------------------------------------

  sit05 <- data |>
    dplyr::filter(
      sto %in% c("D1", "D11", "D12")
    ) |>
    tidyr::pivot_wider(
      names_from = sto,
      values_from = obs_value
    ) |>
    dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D11, D12, D1) |>
    dplyr::rowwise() |>
    dplyr::mutate(`D11 + D12` = sum(D11,D12, na.rm = TRUE),
                  check = round(D1 - `D11 + D12`, rounding)) |>
    dplyr::filter(abs(check) > threshold)

  ## SIT06----------------------------------------------------------------------------------------

  sit06 <- data |>
    dplyr::filter(
      sto %in% c("D2", "D21", "D29")
    ) |>
    tidyr::pivot_wider(
      names_from = sto,
      values_from = obs_value
    ) |>
    dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D21, D29, D2) |>
    dplyr::rowwise() |>
    dplyr::mutate(`D21 + D29` = sum(D21,D29, na.rm = TRUE),
                  check = round(D2 - `D21 + D29`, rounding)) |>
    dplyr::filter(abs(check) > threshold)

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
    ) |>
    dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D2, D21) |>
    dplyr::rowwise() |>
    dplyr::mutate(check = round(D2 - D21, rounding)) |>
    dplyr::filter(abs(check) > threshold)

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
    ) |>
    dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D2, D29) |>
    dplyr::rowwise() |>
    dplyr::mutate(check = round(D2 - D29, rounding)) |>
    dplyr::filter(abs(check) > threshold)

  ## SIT09----------------------------------------------------------------------------------------

  sit09 <- data |>
    dplyr::filter(
      sto %in% c("D3", "D31", "D39"),
      ref_sector %in% c("S1","S2", "S13"),
    ) |>
    tidyr::pivot_wider(
      names_from = sto,
      values_from = obs_value
    ) |>
    dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D31, D39, D3) |>
    dplyr::rowwise() |>
    dplyr::mutate(`D31 + D39` = sum(c(D31,D39), na.rm = TRUE),
                  check = round(D3 - `D31 + D39`, rounding)) |>
    dplyr::filter(abs(check) > threshold)

  ## SIT10----------------------------------------------------------------------------------------

  sit10 <- data |>
    dplyr::filter(
      sto %in% c("D4", "D41", "D42", "D43", "D44", "D45"),
    ) |>
    tidyr::pivot_wider(
      names_from = sto,
      values_from = obs_value
    ) |>
    dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D41, D42, D43, D44, D45, D4) |>
    dplyr::rowwise() |>
    dplyr::mutate(`D41 + D42 + D43 + D44 + D45` = sum(c(D41, D42, D43, D44, D45), na.rm = TRUE),
                  check = round(D4 - `D41 + D42 + D43 + D44 + D45`, rounding)) |>
    dplyr::filter(abs(check) > threshold)

  ## SIT11----------------------------------------------------------------------------------------

  sit11 <- data |>
    dplyr::filter(
      sto %in% c("D4", "D41", "D42",  "D44", "D45"),
      ref_sector == "S13",
      accounting_entry == "D"
    ) |>
    tidyr::pivot_wider(
      names_from = sto,
      values_from = obs_value
    ) |>
    dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D41, D42, D44, D45, D4) |>
    dplyr::rowwise() |>
    dplyr::mutate(`D41 + D42 + D44 + D45` = sum(c(D41, D42,  D44, D45), na.rm = TRUE),
                  check = round(D4 - `D41 + D42 + D44 + D45`, rounding)) |>
    dplyr::filter(abs(check) > threshold)

  ## SIT12----------------------------------------------------------------------------------------

  sit12_filtered <- data |>


    dplyr::filter(sto %in% c("D4", "D41", "D43",  "D44", "D45"),
                  ref_sector %in% c("S1M", "S14", "S15"),
                  accounting_entry == "D")



  if(has_required_combos(sit12_filtered, c(""))){


    sit12 <- sit12_filtered |>


      tidyr::pivot_wider(names_from = sto,
                         values_from = obs_value) |>


      dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D41, D43, D44, D45, D4) |>


      dplyr::rowwise() |>
      dplyr::mutate(`D41 + D43 + D44 + D45` = sum(c(D41, D43,  D44, D45), na.rm = TRUE),
                    check = round(D4 - `D41 + D43 + D44 + D45`, rounding)) |>
      dplyr::filter(abs(check) > threshold)


  }
  ## SIT15----------------------------------------------------------------------------------------

  sit15_filtered <- data |>


    dplyr::filter(sto %in% c("D42", "D421", "D422"))



  if(has_required_combos(sit15_filtered, c(""))){


    sit15 <- sit15_filtered |>


      tidyr::pivot_wider(names_from = sto,
                         values_from = obs_value) |>


      dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D42, D421, D422) |>


      dplyr::rowwise() |>
      dplyr::mutate(`D421 + D422` = sum(c(D421, D422), na.rm = TRUE),
                    check = round(D42 - `D421 + D422`, rounding)) |>
      dplyr::filter(abs(check) > threshold)


  }

  ## SIT16----------------------------------------------------------------------------------------

  sit16 <- data |>
    dplyr::filter(
      sto %in% c("D44", "D441", "D442", "D443")
    ) |>
    tidyr::pivot_wider(
      names_from = sto,
      values_from = obs_value
    ) |>
    dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D44, D441, D442, D443) |>
    dplyr::rowwise() |>
    dplyr::mutate(`D441 + D442 + D443` = sum(c(D441, D442, D443), na.rm = TRUE),
                  check = round(D44 - `D441 + D442 + D443`, rounding)) |>
    dplyr::filter(abs(check) > threshold)

  ## SIT17----------------------------------------------------------------------------------------

  sit17 <- data |>
    dplyr::filter(
      sto %in% c("D5", "D51", "D59")
    ) |>
    tidyr::pivot_wider(
      names_from = sto,
      values_from = obs_value
    ) |>
    dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D51, D59, D5) |>
    dplyr::rowwise() |>
    dplyr::mutate(`D51 + D59` = sum(c(D51, D59), na.rm = TRUE),
                  check = round(D5 - `D51 + D59`, rounding)) |>
    dplyr::filter(abs(check) > threshold)

  ## SIT18----------------------------------------------------------------------------------------

  sit18 <- data |>
    dplyr::filter(
      sto %in% c("D6", "D61", "D62", "D63"),
      ref_sector != "S2"
    ) |>
    tidyr::pivot_wider(
      names_from = sto,
      values_from = obs_value
    ) |>
    dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D61, D62, D63, D6) |>
    dplyr::rowwise() |>
    dplyr::mutate(`D61 + D62 + D63` = sum(c(D61, D62, D63), na.rm = TRUE),
                  check = round(D6 - `D61 + D62 + D63`, rounding)) |>
    dplyr::filter(abs(check) > threshold)

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
    ) |>
    dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D6, D62) |>
    dplyr::rowwise() |>
    dplyr::mutate(check = round(D6 - D62, rounding)) |>
    dplyr::filter(abs(check) > threshold)

  ## SIT20----------------------------------------------------------------------------------------

  sit20 <- data |>
    dplyr::filter(
      sto %in% c("D6", "D62", "D63"),
      ref_sector %in% c("S13", "S15"),
      accounting_entry == "D"
    ) |>
    tidyr::pivot_wider(
      names_from = sto,
      values_from = obs_value
    ) |>
    dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D62, D63, D6) |>
    dplyr::rowwise() |>
    dplyr::mutate(`D62 + D63` = sum(c(D62, D63), na.rm = TRUE),
                  check = round(D6 - `D62 + D63`, rounding)) |>
    dplyr::filter(abs(check) > threshold)

  ## SIT20----------------------------------------------------------------------------------------

  sit20b <- data |>
    dplyr::filter(
      sto %in% c("D6", "D61", "D62"),
      ref_sector %in% c("S14"),
      accounting_entry == "D"
    ) |>
    tidyr::pivot_wider(
      names_from = sto,
      values_from = obs_value
    ) |>
    dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D61, D62, D6) |>
    dplyr::rowwise() |>
    dplyr::mutate(`D61 + D62` = sum(c(D61, D62), na.rm = TRUE),
                  check = round(D6 - `D61 + D62`, rounding)) |>
    dplyr::filter(abs(check) > threshold)

  ## SIT21----------------------------------------------------------------------------------------

  sit21 <- data |>
    dplyr::filter(
      sto %in% c("D6", "D61", "D62"),
      ref_sector %in% c("S2")
    ) |>
    tidyr::pivot_wider(
      names_from = sto,
      values_from = obs_value
    ) |>
    dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D61, D62, D6) |>
    dplyr::rowwise() |>
    dplyr::mutate(`D61 + D62` = sum(c(D61, D62), na.rm = TRUE),
                  check = round(D6 - `D61 + D62`, rounding)) |>
    dplyr::filter(abs(check) > threshold)

  ## SIT22----------------------------------------------------------------------------------------

  sit22 <- data |>
    dplyr::filter(
      sto %in% c("D61", "D611", "D612", "D613", "D614", "D61SC"),
    ) |>
    tidyr::pivot_wider(
      names_from = sto,
      values_from = obs_value
    ) |>
    dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D611, D612, D613, D614, D61SC, D61) |>
    dplyr::rowwise() |>
    dplyr::mutate(`sum_D61` = sum(c(D611, D612, D613, D614, -D61SC), na.rm = TRUE),
                  check = round(D61 - `sum_D61`, rounding)) |>
    dplyr::filter(abs(check) > threshold)

  ## SIT23----------------------------------------------------------------------------------------

  sit23 <- data |>
    dplyr::filter(
      sto %in% c("D63", "D631", "D632"),
    ) |>
    tidyr::pivot_wider(
      names_from = sto,
      values_from = obs_value
    ) |>
    dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D631, D632, D63) |>
    dplyr::rowwise() |>
    dplyr::mutate(`D631 + D632` = sum(c(D631, D632), na.rm = TRUE),
                  check = round(D63 - `D631 + D632`, rounding)) |>
    dplyr::filter(abs(check) > threshold) |>
    dplyr::filter(if_all(c(D631,D632), ~ !is.na(.x )))

  ## SIT24----------------------------------------------------------------------------------------

  sit24 <- data |>
    dplyr::filter(
      sto %in% c("D7", "D71", "D72", "D73", "D74", "D75", "D76"),
    ) |>
    tidyr::pivot_wider(
      names_from = sto,
      values_from = obs_value
    ) |>
    dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D71, D72,  D74, D75, D76, D7) |>
    dplyr::rowwise() |>
    dplyr::mutate(`sum_D7` = sum(c(D71, D72,  D74, D75, D76), na.rm = TRUE),
                  check = round(D7 - `sum_D7`, rounding)) |>
    dplyr::filter(abs(check) > threshold)

  ## SIT25----------------------------------------------------------------------------------------

  sit25 <- data |>
    dplyr::filter(
      sto %in% c("D7", "D71", "D75"),
      ref_sector %in% c("S11", "S1M", "S14", "S15"),
      accounting_entry == "D"
    ) |>
    tidyr::pivot_wider(
      names_from = sto,
      values_from = obs_value
    ) |>
    dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D71, D75, D7) |>
    dplyr::rowwise() |>
    dplyr::mutate(`D71 + D75` = sum(c(D71, D75), na.rm = TRUE),
                  check = round(D7 - `D71 + D75`, rounding)) |>
    dplyr::filter(abs(check) > threshold)

  ## SIT26----------------------------------------------------------------------------------------

  sit26 <- data |>
    dplyr::filter(
      sto %in% c("D7", "D71", "D72", "D75"),
      str_detect(ref_sector, "S12")
    ) |>
    tidyr::pivot_wider(
      names_from = sto,
      values_from = obs_value
    ) |>
    dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D71, D72, D75, D7) |>
    dplyr::rowwise() |>
    dplyr::mutate(`D71 + D72 + D75` = sum(c(D71,D72, D75), na.rm = TRUE),
                  check = round(D7 - `D71 + D72 + D75`, rounding)) |>
    dplyr::filter(abs(check) > threshold)

  ## SIT27----------------------------------------------------------------------------------------

  sit27 <- data |>
    dplyr::filter(
      sto %in% c("D7", "D71", "D72", "D74","D75"),
      ref_sector %in% c("S2", "S1", "S13"),
      accounting_entry == "C"
    ) |>
    tidyr::pivot_wider(
      names_from = sto,
      values_from = obs_value
    ) |>
    dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D71, D72, D74, D75, D7) |>
    dplyr::rowwise() |>
    dplyr::mutate(`D71 + D72 + D74 + D75` = sum(c(D71,D72,D74, D75), na.rm = TRUE),
                  check = round(D7 - `D71 + D72 + D74 + D75`, rounding)) |>
    dplyr::filter(abs(check) > threshold)

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
    ) |>
    dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D74, D74_4Y) |>
    dplyr::filter(D74 < D74_4Y)

  ## SIT29----------------------------------------------------------------------------------------

  sit29 <- data |>
    dplyr::filter(
      sto %in% c("D9", "D91", "D92", "D99"),
      ref_sector %in% c("S2", "S1", "S13")
    ) |>
    tidyr::pivot_wider(
      names_from = sto,
      values_from = obs_value
    ) |>
    dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D91, D92, D99, D9) |>
    dplyr::rowwise() |>
    dplyr::mutate(`D91 + D92 + D99` = sum(c(D91,D92,D99), na.rm = TRUE),
                  check = round(D9 - `D91 + D92 + D99`, rounding)) |>
    dplyr::filter(abs(check) > threshold)

  ## SIT30----------------------------------------------------------------------------------------

  sit30 <- data |>
    dplyr::filter(
      sto %in% c("D9", "D91",  "D99"),
      ref_sector %in% c("S11", "S12",  "S1M", "S14", "S15"),
      accounting_entry == "D"
    ) |>
    tidyr::pivot_wider(
      names_from = sto,
      values_from = obs_value
    ) |>
    dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D91,  D99, D9) |>
    dplyr::rowwise() |>
    dplyr::mutate(`D91 + D99` = sum(c(D91,D99), na.rm = TRUE),
                  check = round(D9 - `D91 + D99`, rounding)) |>
    dplyr::filter(abs(check) > threshold)

  ## SIT30b----------------------------------------------------------------------------------------

  sit30b <- data |>
    dplyr::filter(
      sto %in% c("D9", "D92",  "D99"),
      ref_sector %in% c("S13"),
      accounting_entry == "D"
    ) |>
    tidyr::pivot_wider(
      names_from = sto,
      values_from = obs_value
    ) |>
    dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D92,  D99, D9) |>
    dplyr::rowwise() |>
    dplyr::mutate(`D92 + D99` = sum(c(D92,D99), na.rm = TRUE),
                  check = round(D9 - `D92 + D99`, rounding)) |>
    dplyr::filter(abs(check) > threshold)

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
    ) |>
    dplyr::select(ref_area, ref_sector, accounting_entry,time_period, P61,  P62, P6) |>
    dplyr::rowwise() |>
    dplyr::mutate(`P61 + P62` = sum(c(P61,P62), na.rm = TRUE),
                  check = round(P6 - `P61 + P62`, rounding)) |>
    dplyr::filter(abs(check) > threshold)

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
    ) |>
    dplyr::select(ref_area, ref_sector, accounting_entry,time_period, P62, P62F) |>
    dplyr::filter(P62F > P62)

  ## SIT33----------------------------------------------------------------------------------------

  sit33 <- data |>
    dplyr::filter(
      sto %in% c("D21", "D211", "D212", "D214"),
      ref_sector %in% c("S1", "S13", "S2")
    ) |>
    tidyr::pivot_wider(
      names_from = sto,
      values_from = obs_value
    ) |>
    dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D211, D212, D214, D21) |>
    dplyr::rowwise() |>
    dplyr::mutate(`D211 + D212 + D214` = sum(c(D211, D212, D214), na.rm = TRUE),
                  check = round(D21 - `D211 + D212 + D214`, rounding)) |>
    dplyr::filter(abs(check) > threshold) |>
    dplyr::filter(if_all(c(D211, D212, D214), ~ !is.na(.x )))

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
    ) |>
    dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D3, D31) |>
    dplyr::rowwise() |>
    dplyr::mutate(`check` = sum(c(D3, -D31), na.rm = TRUE)) |>
    dplyr::filter(abs(check) > threshold)

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
    ) |>
    dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D3, D39) |>
    dplyr::rowwise() |>
    dplyr::mutate(`check` = sum(c(D3, -D39), na.rm = TRUE)) |>
    dplyr::filter(abs(check) > threshold)

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
    ) |>
    dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D6, D61) |>
    dplyr::rowwise() |>
    dplyr::mutate(`check` = sum(c(D6, -D61), na.rm = TRUE)) |>
    dplyr::filter(abs(check) > threshold)

  ## SIT37----------------------------------------------------------------------------------------

  sit37 <- data |>
    dplyr::filter(
      sto %in% c("D7", "D72", "D75"),
      ref_sector %in% c("S11", "S1M", "S14", "S15"),
      accounting_entry == "C"
    ) |>
    tidyr::pivot_wider(
      names_from = sto,
      values_from = obs_value
    ) |>
    dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D72, D75, D7) |>
    dplyr::rowwise() |>
    dplyr::mutate(`D72 + D75` = sum(c(D72, D75), na.rm = TRUE),
                  check = round(D7 - `D72 + D75`, rounding)) |>
    dplyr::filter(abs(check) > threshold)

  ## SIT38----------------------------------------------------------------------------------------

  sit38 <- data |>
    dplyr::filter(
      sto %in% c("D9", "D92", "D99"),
      !ref_sector %in% c("S1", "S2", "S13"),
      accounting_entry == "C"
    ) |>
    tidyr::pivot_wider(
      names_from = sto,
      values_from = obs_value
    ) |>
    dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D92, D99, D9) |>
    dplyr::rowwise() |>
    dplyr::mutate(`D92 + D99` = sum(c(D92, D99), na.rm = TRUE),
                  check = round(D9 - `D92 + D99`, rounding)) |>
    dplyr::filter(abs(check) > threshold)

  ## SIT39----------------------------------------------------------------------------------------

  sit39 <- data |>
    dplyr::filter(
      sto %in% c("P1", "P11", "P12", "P13"),
      accounting_entry == "C"
    ) |>
    tidyr::pivot_wider(
      names_from = sto,
      values_from = obs_value
    ) |>
    dplyr::select(ref_area, ref_sector, accounting_entry,time_period, P11, P12,P13, P1) |>
    dplyr::rowwise() |>
    dplyr::mutate(`P11 + P12 + P13` = sum(c(P11, P12, P13), na.rm = TRUE),
                  check = round(P1 - `P11 + P12 + P13`, rounding)) |>
    dplyr::filter(abs(check) > threshold)

  ## SIT40----------------------------------------------------------------------------------------

  sit40 <- data |>
    dplyr::filter(
      sto %in% c("P1", "P11", "P12"),
      !ref_sector %in% c("S1","S13", "S15", "S1M"),
      accounting_entry == "C"
    ) |>
    tidyr::pivot_wider(
      names_from = sto,
      values_from = obs_value
    ) |>
    dplyr::select(ref_area, ref_sector, accounting_entry,time_period, P11, P12, P1) |>
    dplyr::rowwise() |>
    dplyr::mutate(`P11 + P12` = sum(c(P11, P12), na.rm = TRUE),
                  check = round(P1 - `P11 + P12`, rounding)) |>
    dplyr::filter(abs(check) > threshold)

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
    ) |>
    dplyr::select(ref_area, ref_sector, accounting_entry,time_period, P7, P71, P72) |>
    dplyr::rowwise() |>
    dplyr::mutate(`P71 + P72` = sum(c(P71, P72), na.rm = TRUE),
                  check = round(P7 - `P71 + P72`, rounding)) |>
    dplyr::filter(abs(check) > threshold)

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
    ) |>
    dplyr::select(ref_area, ref_sector, accounting_entry,time_period, P72, P72F) |>
    dplyr::filter(P72F > P72)

  ## SIT43----------------------------------------------------------------------------------------
  check <- data |>
    dplyr::filter(sto == "D43_I9")

  if(nrow(check) > 0){
    sit43 <- data |>
      dplyr::filter(
        sto %in% c("D43", "D43_I9", "D43_J9")
      ) |>
      tidyr::pivot_wider(
        names_from = sto,
        values_from = obs_value
      ) |>
      dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D43, D43_I9, D43_J9) |>
      dplyr::rowwise() |>
      dplyr::mutate(`D43_I9 + D43_J9` = sum(c(D43_I9, D43_J9), na.rm = TRUE),
                    check = round(D43 - `D43_I9 + D43_J9`, rounding)) |>
      dplyr::filter(abs(check) > threshold) |>
      dplyr::filter(if_all(c(D43_I9,D43_J9), ~ !is.na(.x )))}
  rm(check)

  ## SIT44----------------------------------------------------------------------------------------

  sit44 <- data |>
    dplyr::filter(
      sto %in% c("D43", "D43_B6", "D43_D6")
    ) |>
    tidyr::pivot_wider(
      names_from = sto,
      values_from = obs_value
    ) |>
    dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D43, D43_B6, D43_D6) |>
    dplyr::rowwise() |>
    dplyr::mutate(`D43_B6 + D43_D6` = sum(c(D43_B6, D43_D6), na.rm = TRUE),
                  check = round(D43 - `D43_B6 + D43_D6`, rounding)) |>
    dplyr::filter(abs(check) > threshold) |>
    dplyr::filter(if_all(c(D43_B6,D43_D6), ~ !is.na(.x )))

  # Balancing items-------------------------------------------------------------------------------

  ## BI01-----------------------------------------------------------------------------------------
  BI01 <- data |>
    dplyr::filter(
      ref_sector == "S1",
      sto == "B1GQ" & accounting_entry == "B" |
        sto == "P1" & accounting_entry == "C" |
        sto == "P2" & accounting_entry == "D" |
        sto == "D21X31" & accounting_entry == "C"
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                       values_from = obs_value) |>
    dplyr::select(ref_area, ref_sector, time_period, P1.C, P2.D, D21X31.C, B1GQ.B ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `P1.C - P2.D + D21X31.C` = P1.C - P2.D + D21X31.C,
      check = round(B1GQ.B - `P1.C - P2.D + D21X31.C`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)

  ## BI02-----------------------------------------------------------------------------------------

  BI02 <- data |>
    dplyr::filter(
      ref_sector %in% c("S1", "S1N"),
      sto == "D21X31" & accounting_entry == "C" |
        sto == "D21" & accounting_entry == "D" |
        sto == "D31" & accounting_entry == "C"
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                       values_from = obs_value) |>
    dplyr::select(ref_area, ref_sector, time_period, D21X31.C, D21.D, D31.C) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `D21.D - D31.C` = D21.D - D31.C,
      check = round(D21X31.C - `D21.D - D31.C`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)

  ## BI03-----------------------------------------------------------------------------------------

  BI03 <- data |>
    dplyr::filter(
      ref_sector == "S1N",
      sto %in% c("B1G", "D21X31"),
      accounting_entry != "D"
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                       values_from = obs_value) |>
    dplyr::select(ref_area, ref_sector, time_period, B1G.B, D21X31.C) |>
    dplyr::rowwise() |>
    dplyr::mutate(check = round(B1G.B - D21X31.C, rounding)) |>
    dplyr::filter(abs(check) > threshold)

  ## BI04-----------------------------------------------------------------------------------------
  BI04 <- data |>
    dplyr::filter(
      sto %in% c("B1G", "P1", "P2")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                       values_from = obs_value) |>
    dplyr::select(ref_area, ref_sector, time_period, B1G.B, P1.C, P2.D) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `P1.C - P2.D` = P1.C - P2.D,
      check = round(B1G.B - `P1.C - P2.D`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)

  ## BI05-----------------------------------------------------------------------------------------

  BI05 <- data |>
    dplyr::filter(
      ref_sector == "S1",
      sto %in% c("B1GQ", "B1NQ", "P51C"),
      accounting_entry != "C"
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                       values_from = obs_value) |>
    dplyr::select(ref_area, ref_sector, time_period, B1NQ.B, B1GQ.B, P51C.D) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `B1GQ.B - P51C.D` = B1GQ.B - P51C.D,
      check = round(B1NQ.B - `B1GQ.B - P51C.D`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)

  ## BI06-----------------------------------------------------------------------------------------
  BI06 <- data |>
    dplyr::filter(
      ref_sector == "S1N",
      sto %in% c("B1G", "B1N"),
      accounting_entry == "B"
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                       values_from = obs_value) |>
    dplyr::select(ref_area, ref_sector, time_period, B1N.B, B1G.B) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `check` = B1G.B - B1N.B
    ) |>
    dplyr::filter(abs(check) > threshold)

  ## BI07-----------------------------------------------------------------------------------------
  BI07 <- data |>
    dplyr::filter(
      ref_sector != "S1N",
      sto %in% c("B1G", "B1N", "P51C"),
      accounting_entry != "D"
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                       values_from = obs_value) |>
    dplyr::select(ref_area, ref_sector, time_period, B1N.B, P51C.C,B1G.B) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `B1N.B + P51C.C` = B1N.B + P51C.C,
      check = round(B1G.B - `B1N.B + P51C.C`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)

  ## BI08-----------------------------------------------------------------------------------------
  BI08 <- data |>
    dplyr::filter(
      ref_sector %in% c("S1","S2"),
      sto %in% c("B1GQ", "P3", "P5", "P6", "P7")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    dplyr::select(-ref_sector) |>
    tidyr::pivot_wider(names_from = sto,
                       values_from = obs_value) |>
    dplyr::select(ref_area, time_period, P3.D, P5.D,P6.D,P7.C,B1GQ.B) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `B1GQ_cal` = P3.D + P5.D + P6.D - P7.C,
      check = round(B1GQ.B - `B1GQ_cal`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)

  ## BI09-----------------------------------------------------------------------------------------
  BI09 <- data |>
    dplyr::filter(
      ref_sector %in% c("S1"),
      sto %in% c("B1GQ", "B2A3G", "D1", "D2", "D3")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                       values_from = obs_value) |>
    dplyr::select(ref_area, ref_sector, time_period, B2A3G.B, D1.D,D3.C,D2.D,,B1GQ.B) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `B1GQ_cal` = B2A3G.B + D1.D + D2.D - D3.C,
      check = round(B1GQ.B - `B1GQ_cal`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)

  ## BI10-----------------------------------------------------------------------------------------
  BI10 <- data |>
    dplyr::filter(
      ref_sector != "S1",
      sto %in% c("B1G", "B2A3G", "D1", "D2", "D3")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                       values_from = obs_value) |>
    dplyr::select(ref_area, ref_sector, time_period, B2A3G.B, D1.D,D3.C,D2.D,,B1G.B) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `B1G_cal` = B2A3G.B + D1.D + D2.D - D3.C,
      check = round(B1G.B - `B1G_cal`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)

  ## BI11-----------------------------------------------------------------------------------------
  BI11 <- data |>
    dplyr::filter(
      ref_sector %in% c("S1","S1M", "S14"),
      sto %in% c("B2G", "B2A3G", "B3G"),
      accounting_entry == "B"
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                       values_from = obs_value) |>
    dplyr::select(ref_area, ref_sector, time_period, B2G.B, B3G.B,B2A3G.B) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `B2G.B + B3G.B` = sum(c(B2G.B, B3G.B),na.rm = TRUE),
      check = round(B2A3G.B - `B2G.B + B3G.B`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)

  ## BI12-----------------------------------------------------------------------------------------
  # B2G not reported
  # BI12 <- data |>
  #   dplyr::filter(
  #     !ref_sector %in% c("S1","S1M", "S14"),
  #     sto %in% c("B2G", "B2A3G"),
  #     accounting_entry == "B"
  #   ) |>
  #   unite("sto", c(sto, accounting_entry), sep = ".") |>
  #   tidyr::pivot_wider(names_from = sto,
  #               values_from = obs_value) |>
  #   dplyr::select(ref_area, ref_sector, time_period, B2G.B, B2A3G.B) |>
  #   dplyr::mutate(check = round(B2A3G.B - B2G.B, rounding)
  #   ) |>
  #   dplyr::filter(abs(check) > threshold)

  ## BI13-----------------------------------------------------------------------------------------
  BI13 <- data |>
    dplyr::filter(
      ref_sector %in% c("S11","S12"),
      sto %in% c("B4G", "B2A3G", "D4","D41", "D44", "D45"),
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                       values_from = obs_value) |>
    dplyr::select(ref_area, ref_sector, time_period, B2A3G.B, D4.C,D41.D,D44.D,D45.D, B4G.B) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `B4G_cal` = sum(c(B2A3G.B,D4.C,-D41.D,-D44.D,-D45.D),na.rm = TRUE),
      check = round(B4G.B - `B4G_cal`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)

  ## BI14-----------------------------------------------------------------------------------------
  BI14 <- data |>
    dplyr::filter(
      ref_sector %in% c("S1"),
      sto %in% c("B5G", "B2A3G", "D1","D2", "D3", "D4"),
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                       values_from = obs_value) |>
    dplyr::select(ref_area, ref_sector, time_period, B2A3G.B, D1.C,D2.C,D3.D,D4.C, D4.D,B5G.B) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `B5G_cal` = sum(c(B2A3G.B,D1.C,D2.C,-D3.D,D4.C,-D4.D),na.rm = TRUE),
      check = round(B5G.B - `B5G_cal`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)

  ## BI15-----------------------------------------------------------------------------------------
  BI15 <- data |>
    dplyr::filter(
      !ref_sector %in% c("S1", "S13", "S14", "S1M"),
      sto %in% c("B5G", "B2A3G",  "D4"),
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                       values_from = obs_value) |>
    dplyr::select(ref_area, ref_sector, time_period, B2A3G.B, D4.C,D4.D, B5G.B) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `B5G_cal` = sum(c(B2A3G.B,D4.C,-D4.D),na.rm = TRUE),
      check = round(B5G.B - `B5G_cal`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)

  ## BI16-----------------------------------------------------------------------------------------
  BI16 <- data |>
    dplyr::filter(
      ref_sector %in% c("S13"),
      sto %in% c("B5G", "B2A3G", "D2", "D3" , "D4")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                       values_from = obs_value) |>
    dplyr::select(ref_area, ref_sector, time_period, B2A3G.B, D2.C,D3.D,D4.C,D4.D, B5G.B) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `B5G_cal` = sum(c(B2A3G.B,D2.C,-D3.D,D4.C,-D4.D),na.rm = TRUE),
      check = round(B5G.B - `B5G_cal`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)

  ## BI17-----------------------------------------------------------------------------------------
  BI17 <- data |>
    dplyr::filter(
      ref_sector %in% c("S1M", "S14"),
      sto %in% c("B5G", "B2A3G", "D1",  "D4")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                       values_from = obs_value) |>
    dplyr::select(ref_area, ref_sector, time_period, B2A3G.B, D1.C,D4.C,D4.D, B5G.B) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `B5G_cal` = sum(c(B2A3G.B,D1.C,D4.C,-D4.D),na.rm = TRUE),
      check = round(B5G.B - `B5G_cal`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)

  ## BI19-----------------------------------------------------------------------------------------
  BI19 <- data |>
    dplyr::filter(
      ref_sector %in% c("S1"),
      sto %in% c("B5G", "D5", "D61","D62", "D7", "B6G")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                       values_from = obs_value) |>
    dplyr::select(ref_area, ref_sector, time_period, B5G.B,D5.C,D5.D,D61.C,D61.D,D62.C,D62.D,D7.C,D7.D,B6G.B) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `B6G_cal` = sum(c(B5G.B,D5.C,-D5.D,D61.C,-D61.D,D62.C,-D62.D,D7.C,-D7.D),na.rm = TRUE),
      check = round(B6G.B - `B6G_cal`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)

  ## BI20-----------------------------------------------------------------------------------------
  BI20 <- data |>
    dplyr::filter(
      ref_sector %in% c("S11", "S12", "S15"),
      sto %in% c("B5G", "D5", "D61","D62", "D7", "B6G")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                       values_from = obs_value) |>
    dplyr::select(ref_area, ref_sector, time_period, B5G.B,D5.D,D61.C,D62.D,D7.C,D7.D,B6G.B) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `B6G_cal` = sum(c(B5G.B,-D5.D,D61.C,-D62.D,D7.C,-D7.D),na.rm = TRUE),
      check = round(B6G.B - `B6G_cal`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)

  ## BI21-----------------------------------------------------------------------------------------
  BI21 <- data |>
    dplyr::filter(
      ref_sector %in% c("S13"),
      sto %in% c("B5G", "D5", "D61","D62", "D7", "B6G")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                       values_from = obs_value) |>
    dplyr::select(ref_area, ref_sector, time_period, B5G.B,D5.C,D5.D,D61.C,D62.D,D7.C,D7.D,B6G.B) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `B6G_cal` = sum(c(B5G.B,D5.C,-D5.D,D61.C,-D62.D,D7.C,-D7.D),na.rm = TRUE),
      check = round(B6G.B - `B6G_cal`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)

  ## BI22-----------------------------------------------------------------------------------------
  BI22 <- data |>
    dplyr::filter(
      ref_sector %in% c("S14", "S1M"),
      sto %in% c("B5G", "D5", "D61","D62", "D7", "B6G")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                       values_from = obs_value) |>
    dplyr::select(ref_area, ref_sector, time_period, B5G.B,D5.D,D5.D,D61.C,D61.D,D62.C,D62.D,D7.C,D7.D,B6G.B) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `B6G_cal` = sum(c(B5G.B,-D5.D,D61.C,-D61.D,D62.C,-D62.D,D7.C,-D7.D),na.rm = TRUE),
      check = round(B6G.B - `B6G_cal`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)

  ## BI24-----------------------------------------------------------------------------------------
  BI24 <- data |>
    dplyr::filter(
      ref_sector %in% c("S1", "S1M"),
      sto %in% c("B7G", "B6G", "D63")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                       values_from = obs_value) |>
    dplyr::select(ref_area, ref_sector, time_period, B6G.B,D63.C,D63.D,B7G.B) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `B7G_cal` = sum(c(B6G.B,D63.C,-D63.D),na.rm = TRUE),
      check = round(B7G.B - `B7G_cal`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)

  ## BI25-----------------------------------------------------------------------------------------
  # B7G does not exist
  # BI25 <- data |>
  #   dplyr::filter(
  #     ref_sector %in% c("S11", "S12"),
  #     sto %in% c("B7G", "B6G")
  #   ) |>
  #   unite("sto", c(sto, accounting_entry), sep = ".") |>
  #   tidyr::pivot_wider(names_from = sto,
  #               values_from = obs_value) |>
  #   dplyr::select(ref_area, ref_sector, time_period, B6G.B,B7G.B) |>
  #   dplyr::rowwise() |>
  #   dplyr::mutate(
  #     check = round(B7G.B - B6G.B, rounding)
  #   ) |>
  #   dplyr::filter(abs(check) > threshold)
  #

  ## BI26-----------------------------------------------------------------------------------------
  BI26 <- data |>
    dplyr::filter(
      ref_sector %in% c("S13", "S15"),
      sto %in% c("B7G", "B6G", "D63"),
      accounting_entry != "C"
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                       values_from = obs_value) |>
    dplyr::select(ref_area, ref_sector, time_period, B6G.B,D63.D,B7G.B) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `B7G_cal` = sum(c(B6G.B,-D63.D),na.rm = TRUE),
      check = round(B7G.B - `B7G_cal`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)

  ## BI27-----------------------------------------------------------------------------------------
  BI27 <- data |>
    dplyr::filter(
      ref_sector %in% c("S14"),
      sto %in% c("B7G", "B6G", "D63"),
      accounting_entry != "D"
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                       values_from = obs_value) |>
    dplyr::select(ref_area, ref_sector, time_period, B6G.B,D63.C,B7G.B) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `B6G + D63.C` = sum(c(B6G.B,D63.C),na.rm = TRUE),
      check = round(B7G.B - `B6G + D63.C`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)

  ## BI28-----------------------------------------------------------------------------------------
  BI28 <- data |>
    dplyr::filter(
      ref_sector %in% c("S1", "S1M", "S14"),
      sto %in% c("B8G", "B6G", "D8", "P3")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                       values_from = obs_value) |>
    dplyr::select(ref_area, ref_sector, time_period, B6G.B,D8.C,D8.D,P3.D,B8G.B) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `B8G_cal` = sum(c(B6G.B,D8.C,-D8.D,-P3.D),na.rm = TRUE),
      check = round(B8G.B - `B8G_cal`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)

  ## BI29-----------------------------------------------------------------------------------------
  BI29 <- data |>
    dplyr::filter(
      ref_sector %in% c("S11", "S12"),
      sto %in% c("B8G", "B6G", "D8")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                       values_from = obs_value) |>
    dplyr::select(ref_area, ref_sector, time_period, B6G.B,D8.D,B8G.B) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `B6G.B - D8.D` = sum(c(B6G.B,-D8.D),na.rm = TRUE),
      check = round(B8G.B - `B6G.B - D8.D`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)

  ## BI30-----------------------------------------------------------------------------------------
  BI30_filtered <- data |>

    dplyr::filter(ref_sector %in% c("S13", "S15"),
                  sto %in% c("B8G", "B6G", "D8", "P3"),
                  accounting_entry != "C"
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".")


  if(has_required_combos(BI30_filtered, c(""))){

    BI30 <- BI30_filtered |>

      tidyr::pivot_wider(names_from = sto,
                         values_from = obs_value) |>

      dplyr::select(ref_area, ref_sector, time_period, B6G.B,D8.D,P3.D,B8G.B) |>

      dplyr::rowwise() |>
      dplyr::mutate(
        `B6G.B - D8.D - P3.D` = sum(c(B6G.B,-D8.D, -P3.D),na.rm = TRUE),
        check = round(B8G.B - `B6G.B - D8.D - P3.D`, rounding)
      ) |>
      dplyr::filter(abs(check) > threshold)

  }
  ## BI32-----------------------------------------------------------------------------------------
  BI32 <- data |>
    dplyr::filter(
      !ref_sector %in% c("S1", "S2"),
      sto %in% c("B8G", "B101", "D9", "P51C")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                       values_from = obs_value) |>
    dplyr::select(ref_area, ref_sector, time_period, B8G.B,D9.C,D9.D,P51C.C,B101.B) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `B101_calc` = sum(c(B8G.B,D9.C,-D9.D,-P51C.C),na.rm = TRUE),
      check = round(B101.B - `B101_calc`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)

  ## BI33-----------------------------------------------------------------------------------------
  BI33 <- data |>
    dplyr::filter(
      ref_sector %in% c("S2"),
      sto %in% c("B101", "D9", "B12")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                       values_from = obs_value) |>
    dplyr::select(ref_area, ref_sector, time_period, B12.B,D9.C,D9.D,B101.B) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `B101_calc` = sum(c(B12.B,D9.D,-D9.C),na.rm = TRUE),
      check = round(B101.B - `B101_calc`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)

  ## BI35-----------------------------------------------------------------------------------------
  BI35 <- data |>
    dplyr::filter(
      !ref_sector %in% c("S2"),
      sto %in% c("B9", "B8G", "D9", "P5", "NP")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                       values_from = obs_value) |>
    dplyr::select(ref_area, ref_sector, time_period, B8G.B,D9.C,D9.D,P5.D,NP.D,B9.B) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `B9_calc` = sum(c(B8G.B,D9.C,-D9.D,-P5.D,-NP.D),na.rm = TRUE),
      check = round(B9.B - `B9_calc`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)

  ## BI36-----------------------------------------------------------------------------------------
  BI36 <- data |>
    dplyr::filter(
      ref_sector %in% c("S2"),
      sto %in% c("B9", "B12", "D9", "NP")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                       values_from = obs_value) |>
    dplyr::select(ref_area, ref_sector, time_period, B12.B,D9.C,D9.D,NP.C,B9.B) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `B9_calc` = sum(c(B12.B,D9.D,-D9.C,-NP.C),na.rm = TRUE),
      check = round(B9.B - `B9_calc`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)

  ## BI37-----------------------------------------------------------------------------------------
  BI37 <- data |>
    dplyr::filter(
      ref_sector %in% c("S13"),
      sto %in% c("B9", "OTR", "OTE")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                       values_from = obs_value) |>
    dplyr::select(ref_area, ref_sector, time_period, OTR.C, OTE.D,B9.B) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `OTR.C - OTE.D` = sum(c(OTR.C,-OTE.D),na.rm = TRUE),
      check = round(B9.B - `OTR.C - OTE.D`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)

  ## BI38-----------------------------------------------------------------------------------------
  BI38 <- data |>
    dplyr::filter(
      !ref_sector %in% c("S2"),
      sto %in% c("B9", "B101", "P51C", "P5", "NP")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                       values_from = obs_value) |>
    dplyr::select(ref_area, ref_sector, time_period, B101.B, P51C.C, P5.D,NP.D,B9.B) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `B9_calc` = sum(c(B101.B,P51C.C,-P5.D,-NP.D),na.rm = TRUE),
      check = round(B9.B - `B9_calc`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)

  ## BI39-----------------------------------------------------------------------------------------
  BI39 <- data |>
    dplyr::filter(
      ref_sector %in% c("S2"),
      sto %in% c("B9", "B101", "NP")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                       values_from = obs_value) |>
    dplyr::select(ref_area, ref_sector, time_period, B101.B, NP.C,B9.B) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `B101.B - NP.C` = sum(c(B101.B,-NP.C),na.rm = TRUE),
      check = round(B9.B - `B101.B - NP.C`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)

  ## BI41-----------------------------------------------------------------------------------------
  BI41_filtered <- data |>

    dplyr::filter(ref_sector %in% c("S1", "S2"),
                  sto %in% c("B101", "P5", "P51C")
    ) |>
    unite("sto", c(ref_sector,sto, accounting_entry), sep = ".")


  if(has_required_combos(BI41_filtered, c("S2.B101.B", "S1.P5.D", "S1.P51C.C", "S1.B101.B"))){

    BI41 <- BI41_filtered |>

      tidyr::pivot_wider(names_from = sto,
                         values_from = obs_value) |>

      dplyr::select(ref_area, time_period, S2.B101.B, S1.P5.D, S1.P51C.C, S1.B101.B) |>

      dplyr::rowwise() |>
      dplyr::mutate(
        `S1.B101.B_cal` = sum(c(-S2.B101.B,S1.P5.D,-S1.P51C.C),na.rm = TRUE),
        check = round(S1.B101.B - `S1.B101.B_cal`, rounding)
      ) |>
      dplyr::filter(abs(check) > threshold)

  }

  ## BI42-----------------------------------------------------------------------------------------
  BI42 <- data |>
    dplyr::filter(
      ref_sector %in% c("S2"),
      sto %in% c("B11", "P6", "P7")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                       values_from = obs_value) |>
    dplyr::select(ref_area, time_period, P7.C,P6.D,B11.B) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `P7.C - P6.D` = sum(c(P7.C,-P6.D),na.rm = TRUE),
      check = round(B11.B - `P7.C - P6.D`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)

  ## BI43-----------------------------------------------------------------------------------------
  BI43 <- data |>
    dplyr::filter(
      ref_sector %in% c("S2"),
      sto %in% c("B12","B11", "D1", "D2", "D3", "D4", "D5", "D61", "D62", "D7", "D8" )
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                       values_from = obs_value) |>
    dplyr::select(ref_area, time_period, B11.B,,D1.D,D1.C,D2.D,D3.C,D4.D,D4.C,D5.D,D5.C,D61.D,D61.C,D62.D,D62.C,D7.D,D7.C,D8.D,D8.C, B12.B) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `B12.B_calc` = sum(c(B11.B,D1.D,-D1.C,D2.D,-D3.C,D4.D,-D4.C,D5.D,-D5.C,D61.D,-D61.C,D62.D,-D62.C,D7.D,-D7.C,D8.D,-D8.C),na.rm = TRUE),
      check = round(B12.B - `B12.B_calc`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)

  ## BI44-----------------------------------------------------------------------------------------
  BI44 <- data |>
    dplyr::filter(
      ref_sector %in% c("S13"),
      sto %in% c("OTR", "P1O", "D2", "D39", "D4", "D5", "D61", "D7", "D9" )
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                       values_from = obs_value) |>
    dplyr::select(ref_area, time_period, P1O.C,D2.C,D39.C,D4.C,D5.C,D61.C,D7.C,D9.C,OTR.C) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `OTR.C_calc` = sum(c(P1O.C,D2.C,D39.C,D4.C,D5.C,D61.C,D7.C,D9.C),na.rm = TRUE),
      check = round(OTR.C - `OTR.C_calc`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)

  ## BI45-----------------------------------------------------------------------------------------
  BI45 <- data |>
    dplyr::filter(
      ref_sector %in% c("S13"),
      sto %in% c("OTE","P2","P5","D1","D29","D3","D4","D5","D62","D632","D7","D8","D9","NP")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                       values_from = obs_value) |>
    dplyr::select(ref_area, time_period, P2.D,P5.D,D1.D,D29.D,D3.D,D4.D,D5.D,D62.D,D632.D,D7.D,D8.D,D9.D,NP.D,OTE.D) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `OTE.D_calc` = sum(c(P2.D,P5.D,D1.D,D29.D,D3.D,D4.D,D5.D,D62.D,D632.D,D7.D,D8.D,D9.D,NP.D),na.rm = TRUE),
      check = round(OTE.D - `OTE.D_calc`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)

  ## BI46-----------------------------------------------------------------------------------------
  BI46 <- data |>
    dplyr::filter(
      ref_sector %in% c("S13"),
      sto %in% c("P31", "D63")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                       values_from = obs_value) |>
    dplyr::select(ref_area, time_period, P31.D,D63.D) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      check = round(P31.D - `D63.D`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)

  ## BI47-----------------------------------------------------------------------------------------
  BI47 <- data |>
    dplyr::filter(
      ref_sector %in% c("S13"),
      sto %in% c("P3", "P1", "P1O", "D632")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    tidyr::pivot_wider(names_from = sto,
                       values_from = obs_value) |>
    dplyr::select(ref_area, time_period, P1.C,P1O.C,D632.D,P3.D) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      `P3.D_calc` = sum(c(P1.C,-P1O.C,D632.D),na.rm = TRUE),
      check = round(P3.D - `P3.D_calc`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)

  rm(list = ls(pattern = "_filtered"))

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
                         file = paste0(paste0(output_sel, "/T0800_", as.character(format(Sys.time(), "%Y%m%d_%H%M%S")), ".xlsx")),
                         asTable = TRUE,
                         overwrite = TRUE
    )
  }
  cli::cli_alert_success(paste0("File created at: ", output_sel, "/", as.character(format(Sys.time(), "%Y%m%d_%H%M%S")), ".xlsx"))
}

