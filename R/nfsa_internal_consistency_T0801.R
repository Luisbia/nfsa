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

  # Helper function to check if required combinations exist before pivoting
  has_required_combos <- function(df, required_combos) {
    if (nrow(df) == 0) return(FALSE)
    actual_combos <- df |>
      dplyr::distinct(ref_sector, accounting_entry)
    all(required_combos %in% paste0(actual_combos$ref_sector, ".", actual_combos$accounting_entry))
  }




  # Uses vs Resources-----------------------------------------------------------
  ## UR01-----------------------------------------------------------------------

  ur01_filtered <- data |>


    dplyr::filter(sto %in% c(
        "D1", "D4", "D41", "D4N","D41G","D42", "D421","D422", "D43", "D44",
        "D45",  "D5", "D6", "D61","D62", "D7", "D71", "D72", "D7N",
        "D74", "D75", "D8", "D9", "D9N","D91", "D92", "D99"
      ),
      ref_sector %in% c("S1", "S2"))



  if(has_required_combos(ur01_filtered, c("S1.D", "S2.C", "S1.C", "S2.D"))){


    ur01 <- ur01_filtered |>


      tidyr::pivot_wider(names_from = c(ref_sector, accounting_entry),
      values_from = obs_value,
      names_sep = ".") |>


      dplyr::select(ref_area, sto, time_period, S1.D, S2.C, S1.C, S2.D) |>


      dplyr::rowwise() |>
    dplyr::mutate(
      `S1.D + S2.C` = sum(c(S1.D, S2.C), na.rm = TRUE),
      `S1.C + S2.D` = sum(c(S1.C, S2.D), na.rm = TRUE),
      check = round(`S1.D + S2.C` - `S1.C + S2.D`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold) |>
    dplyr::filter(if_all(c(S1.D,S1.C), ~ !is.na(.x )))


  }

  ## UR02---------------------------------------------------------------------

  ur02_filtered <- data |>


    dplyr::filter(sto %in% c(
        "D2", "D21", "D29"
      ),
      ref_sector %in% c("S1", "S2"))



  if(has_required_combos(ur02_filtered, c("S1.D", "S1.C", "S2.D"))){


    ur02 <- ur02_filtered |>


      tidyr::pivot_wider(names_from = c(ref_sector, accounting_entry),
      values_from = obs_value,
      names_sep = ".") |>


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
      ref_sector %in% c("S1", "S2"))



  if(has_required_combos(ur03_filtered, c("S1.D", "S2.C", "S1.C"))){


    ur03 <- ur03_filtered |>


      tidyr::pivot_wider(names_from = c(ref_sector, accounting_entry),
      values_from = obs_value,
      names_sep = ".") |>


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
        "D63", "P51C"
      ),
      ref_sector %in% c("S1"))



  if(has_required_combos(ur04_filtered, c("S1.D", "S1.C"))){


    ur04 <- ur04_filtered |>


      tidyr::pivot_wider(names_from = c(ref_sector, accounting_entry),
      values_from = obs_value,
      names_sep = ".") |>


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
      ref_sector %in% c("S1", "S2"))



  if(has_required_combos(ur05_filtered, c("S1.D", "S2.D"))){


    ur05 <- ur05_filtered |>


      tidyr::pivot_wider(names_from = c(ref_sector, accounting_entry),
      values_from = obs_value,
      names_sep = ".") |>


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
      ref_sector %in% c("S1", "S2"))



  if(has_required_combos(ur06_filtered, c("S1.D", "S2.C"))){


    ur06 <- ur06_filtered |>


      tidyr::pivot_wider(names_from = c(ref_sector, accounting_entry),
      values_from = obs_value,
      names_sep = ".") |>


      dplyr::select(ref_area, sto, time_period, S1.D, S2.C) |>


      dplyr::rowwise() |>
    dplyr::mutate(
      `S1.D + S2.C` = sum(c(S1.D, S2.C), na.rm = TRUE),
    ) |>
    dplyr::filter(abs(`S1.D + S2.C`) > threshold)


  }

  ## UR07---------------------------------------------------------------------

  ur07_filtered <- data |>


    dplyr::filter(sto %in% c(
        "B9", "B9X9F"
      ),
      ref_sector %in% c("S1", "S2"))



  if(has_required_combos(ur07_filtered, c(""))){


    ur07 <- ur07_filtered |>


      tidyr::pivot_wider(names_from = c(ref_sector),
      values_from = obs_value,
      names_sep = ".") |>


      dplyr::select(ref_area, sto, time_period, S1, S2) |>


      dplyr::rowwise() |>
    dplyr::mutate(
      `S1 + S2` = sum(c(S1, S2), na.rm = TRUE),
    ) |>
    dplyr::filter(abs(`S1 + S2`) > threshold)


  }
  ## UR08---------------------------------------------------------------------

  ur08_filtered <- data |>


    dplyr::filter(sto %in% c(
        "D43", "D74"
      ),
      ref_sector %in% c("S1", "S2"))



  if(has_required_combos(ur08_filtered, c("S1.C", "S2.C"))){


    ur08 <- ur08_filtered |>


      tidyr::pivot_wider(names_from = c(ref_sector,accounting_entry),
      values_from = obs_value,
      names_sep = ".") |>


      dplyr::select(ref_area, sto, time_period, S1.C, S2.C) |>


      dplyr::rowwise() |>
    dplyr::mutate(
      `S1.C - S2.C` = sum(c(S1.C, -S2.C), na.rm = TRUE),
    ) |>
    dplyr::filter(abs(`S1.C - S2.C`) > threshold)


  }

  # S1 vs Sum of Sub-sectors--------------------------------------------------
  ## S1SS01-------------------------------------------------------------------
  s1ss01_filtered <- data |>

    dplyr::filter(ref_sector %in% c("S1", "S11", "S12", "S13", "S1M", "S1N")) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    dplyr::filter(sto %in% c(
      "D2.D", "D3.C"
    ))


  if(has_required_combos(s1ss01_filtered, c(""))){

    s1ss01 <- s1ss01_filtered |>

      tidyr::pivot_wider(names_from = ref_sector,
      values_from = obs_value) |>

      dplyr::select(ref_area, sto, time_period, S1N, S11, S12, S13, S1M, S1) |>

      dplyr::rowwise() |>
    dplyr::mutate(`S1N + S11 + S12 + S13 + S1M` = sum(c(S1N, S11, S12, S13, S1M), na.rm = TRUE)) |>
    dplyr::mutate(check = round(S1 - `S1N + S11 + S12 + S13 + S1M`, rounding)) |>
    dplyr::filter(abs(check) > threshold) |>
    dplyr::filter(if_all(c(S11,S12,S1M), ~ !is.na(.x )))

  }
  ## S1SS02-------------------------------------------------------------------
  s1ss02_filtered <- data |>

    dplyr::filter(ref_sector %in% c("S1", "S11", "S12", "S13", "S1M")) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    dplyr::filter(sto %in% c(
      "P2.D", "P5.D", "P51G.D", "P5M.D", "D1.D", "D29.D", "D4.D", "D41.D", "D4N.D","D45.D", "D41G.D",
      "D5.D", "D6.D", "D62.D", "D7.D", "D71.D", "D7N.D","D75.D", "D8.D", "D9.D", "D9.N","D99.D",
      "P51.C", "NP.D", "P1.C", "D39.C", "D4.C", "D41.C", "D4N.C","D42.C", "D43.C", "D44.C", "D45.C",
      "D41G.C", "D6.C", "D61.C", "D7.C", "D7N.C","D72.C", "D75.C", "D9.C", "D9N.C","D92.C", "D99.C",
      "P51C.C", "B2A3G.B", "B4G.B", "B5G.B", "B6G.B", "B8G.B", "B101.B", "B9.B", "B9X9F", "EMP.PS", "EMP.HW"
    ))


  if(has_required_combos(s1ss02_filtered, c(""))){

    s1ss02 <- s1ss02_filtered |>

      tidyr::pivot_wider(names_from = ref_sector,
      values_from = obs_value) |>

      dplyr::select(ref_area, sto, time_period, S11, S12, S13, S1M, S1) |>

      dplyr::rowwise() |>
    dplyr::mutate(`S11 + S12 + S13 + S1M` = sum(c(S11, S12, S13, S1M), na.rm = TRUE)) |>
    dplyr::mutate(check = round(S1 - `S11 + S12 + S13 + S1M`, rounding)) |>
    dplyr::filter(abs(check) > threshold) |>
    dplyr::filter(if_all(c(S11,S12, S1M), ~ !is.na(.x )))

  }
  ## S1SS03-------------------------------------------------------------------
    s1ss03_filtered <- data |>

      dplyr::filter(ref_sector %in% c("S1", "S11", "S12","S1M")) |>
      unite("sto", c(sto, accounting_entry), sep = ".") |>
      dplyr::filter(sto %in% c(
        "D43.D", "D91.D"
      ))


    if(has_required_combos(s1ss03_filtered, c(""))){

      s1ss03 <- s1ss03_filtered |>

        tidyr::pivot_wider(names_from = ref_sector,
        values_from = obs_value) |>

        dplyr::select(ref_area, sto, time_period, S11, S12, S1M, S1) |>

        dplyr::rowwise() |>
      dplyr::mutate(`S11 + S12 + S1M` = sum(c(S11, S12, S1M), na.rm = TRUE)) |>
      dplyr::mutate(check = round(S1 - `S11 + S12 + S1M`, rounding)) |>
      dplyr::filter(abs(check) > threshold) |>
      dplyr::filter(if_all(c(S11,S12, S1M), ~ !is.na(.x )))

    }
  ## S1SS04-------------------------------------------------------------------


    s1ss04_filtered <- data |>



      dplyr::filter(ref_sector %in% c("S1", "S11", "S12","S13")) |>
      unite("sto", c(sto, accounting_entry), sep = ".") |>
      dplyr::filter(sto %in% c(
        "D42.D", "D44.D"
      ))




    if(has_required_combos(s1ss04_filtered, c(""))){



      s1ss04 <- s1ss04_filtered |>



        tidyr::pivot_wider(names_from = ref_sector,
        values_from = obs_value) |>



        dplyr::select(ref_area, sto, time_period, S11, S12, S13, S1) |>



        dplyr::rowwise() |>
      dplyr::mutate(`S11 + S12 + S13` = sum(c(S11, S12, S13), na.rm = TRUE)) |>
      dplyr::mutate(check = round(S1 - `S11 + S12 + S13`, rounding)) |>
      dplyr::filter(abs(check) > threshold)



    }
  ## S1SS05-------------------------------------------------------------------
    s1ss05_filtered <- data |>

      dplyr::filter(ref_sector %in% c("S1", "S11", "S12")) |>
      unite("sto", c(sto, accounting_entry), sep = ".") |>
      dplyr::filter(sto %in% c(
        "D43.D"
      ))


    if(has_required_combos(s1ss05_filtered, c(""))){

      s1ss05 <- s1ss05_filtered |>

        tidyr::pivot_wider(names_from = ref_sector,
        values_from = obs_value) |>

        dplyr::select(ref_area, sto, time_period, S11, S12,  S1) |>

        dplyr::rowwise() |>
      dplyr::mutate(`S11 + S12` = sum(c(S11, S12), na.rm = TRUE)) |>
      dplyr::mutate(check = round(S1 - `S11 + S12`, rounding)) |>
      dplyr::filter(abs(check) > threshold) |>
      dplyr::filter(if_all(c(S11,S12), ~ !is.na(.x )))

    }

  ## S1SS06-------------------------------------------------------------------
    s1ss06_filtered <- data |>

      dplyr::filter(ref_sector %in% c("S1", "S12", "S13")) |>
      unite("sto", c(sto, accounting_entry), sep = ".") |>
      dplyr::filter(sto %in% c(
        "D72.D", "D71.C"
      ))


    if(has_required_combos(s1ss06_filtered, c(""))){

      s1ss06 <- s1ss06_filtered |>

        tidyr::pivot_wider(names_from = ref_sector,
        values_from = obs_value) |>

        dplyr::select(ref_area, sto, time_period, S12, S13,  S1) |>

        dplyr::rowwise() |>
      dplyr::mutate(`S12 + S13` = sum(c(S12, S13), na.rm = TRUE)) |>
      dplyr::mutate(check = round(S1 - `S12 + S13`, rounding)) |>
      dplyr::filter(abs(check) > threshold)|>
      dplyr::filter(if_all(c(S12), ~ !is.na(.x )))

    }

  ## S1SS07-------------------------------------------------------------------
  s1ss07_filtered <- data |>

    dplyr::filter(ref_sector %in% c("S1",  "S13")) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    dplyr::filter(sto %in% c(
      "P32.D", "D3.D", "D31.D", "D39.D", "D74.D", "D74_4Y.D", "D76.D", "D92.D", "D2.C", "D21.C",
      "D211.C", "D212.C", "D214.C", "D29.C", "D5.C", "D51.C", "D59.C", "D74.C", "D91.C"
    ))


  if(has_required_combos(s1ss07_filtered, c(""))){

    s1ss07 <- s1ss07_filtered |>

      tidyr::pivot_wider(names_from = ref_sector,
      values_from = obs_value) |>

      dplyr::select(ref_area, sto, time_period, S13,  S1) |>

      dplyr::rowwise() |>
    dplyr::mutate(check = round(S1 - S13, rounding)) |>
    dplyr::filter(abs(check) > threshold)

  }

  ## S1SS08-------------------------------------------------------------------

    s1ss08_filtered <- data |>


      dplyr::filter(ref_sector %in% c("S1",  "S13", "S1M")) |>
      unite("sto", c(sto, accounting_entry), sep = ".") |>
      dplyr::filter(sto %in% c(
        "P3.D", "P31.D", "D63.D", "D631.D", "D632.D", "P13.C"
      ))



    if(has_required_combos(s1ss08_filtered, c(""))){


      s1ss08 <- s1ss08_filtered |>


        tidyr::pivot_wider(names_from = ref_sector,
        values_from = obs_value) |>


        dplyr::select(ref_area, sto, time_period, S13, S1M,  S1) |>


        dplyr::rowwise() |>
      dplyr::mutate(`S13 + S1M` = sum(c(S13, S1M), na.rm = TRUE)) |>
      dplyr::mutate(check = round(S1 - `S13 + S1M`, rounding)) |>
      dplyr::filter(abs(check) > threshold) |>
      dplyr::filter(if_all(c(S1M), ~ !is.na(.x )))


    }
  ## S1SS09-------------------------------------------------------------------
    s1ss09_filtered <- data |>

      dplyr::filter(ref_sector %in% c("S1",  "S1M")) |>
      unite("sto", c(sto, accounting_entry), sep = ".") |>
      dplyr::filter(sto %in% c(
        "D61.D", "D1.C", "D62.C", "D63.C",  "D8.C", "B3G.B"
      ))


    if(has_required_combos(s1ss09_filtered, c(""))){

      s1ss09 <- s1ss09_filtered |>

        tidyr::pivot_wider(names_from = ref_sector,
        values_from = obs_value) |>

        dplyr::select(ref_area, sto, time_period, S1M,  S1) |>

        dplyr::rowwise() |>
      dplyr::mutate(check = round(S1 - S1M, rounding)) |>
      dplyr::filter(abs(check) > threshold)|>
      dplyr::filter(if_all(c(S1M), ~ !is.na(.x )))

    }

  ## S1SS10-------------------------------------------------------------------
  s1ss10_filtered <- data |>

    dplyr::filter(ref_sector %in% c("S1",  "S1N")) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    dplyr::filter(sto %in% c(
      "D21.D", "D31.C", "D21X31.C"
    ))


  if(has_required_combos(s1ss10_filtered, c(""))){

    s1ss10 <- s1ss10_filtered |>

      tidyr::pivot_wider(names_from = ref_sector,
      values_from = obs_value) |>

      dplyr::select(ref_area, sto, time_period, S1N,  S1) |>

      dplyr::rowwise() |>
    dplyr::mutate(check = round(S1 - S1N, rounding)) |>
    dplyr::filter(abs(check) > threshold)

  }
  ## S1SS11-------------------------------------------------------------------

    s1ss11_filtered <- data |>


      dplyr::filter(ref_sector %in% c("S1M",  "S14")) |>
      unite("sto", c(sto, accounting_entry), sep = ".") |>
      dplyr::filter(sto %in% c(
        "B3G.B", "D1.C", "D11.C", "D12.C", "D61.D", "D611.D", "D612.D", "D613.D", "D614.D", "D61SC.D",
        "D62.C", "D63.C", "D631.C", "D632.C", "D8.C"
      ))



    if(has_required_combos(s1ss11_filtered, c(""))){


      s1ss11 <- s1ss11_filtered |>


        tidyr::pivot_wider(names_from = ref_sector,
        values_from = obs_value) |>


        dplyr::select(ref_area, sto, time_period, S1M,  S14) |>


        dplyr::rowwise() |>
      dplyr::mutate(check = round(S1M - S14, rounding)) |>
      dplyr::filter(abs(check) > threshold)


    }
  ## S1SS12-------------------------------------------------------------------

    s1ss12_filtered <- data |>


      dplyr::filter(ref_sector %in% c("S1M",  "S15")) |>
      unite("sto", c(sto, accounting_entry), sep = ".") |>
      dplyr::filter(sto %in% c(
        "D63.D", "D631.D", "D632.D", "P13.C"
      ))



    if(has_required_combos(s1ss12_filtered, c(""))){


      s1ss12 <- s1ss12_filtered |>


        tidyr::pivot_wider(names_from = ref_sector,
        values_from = obs_value) |>


        dplyr::select(ref_area, sto, time_period, S1M,  S15) |>


        dplyr::rowwise() |>
      dplyr::mutate(check = round(S1M - S15, rounding)) |>
      dplyr::filter(abs(check) > threshold)


    }

  ## S1SS13-------------------------------------------------------------------

    s1ss13_filtered <- data |>


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
      ))



    if(has_required_combos(s1ss13_filtered, c(""))){


      s1ss13 <- s1ss13_filtered |>


        tidyr::pivot_wider(names_from = ref_sector,
        values_from = obs_value) |>


        dplyr::select(ref_area, sto, time_period, S14, S15,  S1M) |>


        dplyr::rowwise() |>
      dplyr::mutate(`S14 + S15` = sum(c(S14, S15), na.rm = TRUE)) |>
      dplyr::mutate(check = round(S1M - `S14 + S15`, rounding)) |>
      dplyr::filter(abs(check) > threshold)|>
      dplyr::filter(if_all(c(S14,S15), ~ !is.na(.x )))


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

  sit01_filtered <- data |>


    dplyr::filter(ref_sector %in% c("S1", "S13"),
      sto %in% c("P3", "P31", "P32"),
      accounting_entry == "D"
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".")



  if(has_required_combos(sit01_filtered, c(""))){


    sit01 <- sit01_filtered |>


      tidyr::pivot_wider(names_from = sto,
      values_from = obs_value) |>


      dplyr::select(ref_area, ref_sector, time_period, P31.D, P32.D, P3.D) |>


      dplyr::rowwise() |>
    dplyr::mutate(`P31.D + P32.D` = sum(c(P31.D,P32.D), na.rm = TRUE)) |>
    dplyr::mutate(check = round(P3.D - `P31.D + P32.D`, rounding)) |>
    dplyr::filter(abs(check) > threshold)


  }

  ## SIT02----------------------------------------------------------------------------------------
    sit02_filtered <- data |>

      dplyr::filter(ref_sector %in% c("S1M", "S14", "S15"),
        sto %in% c("P3", "P31"),
        accounting_entry == "D"
      ) |>
      unite("sto", c(sto, accounting_entry), sep = ".")


    if(has_required_combos(sit02_filtered, c(""))){

      sit02 <- sit02_filtered |>

        tidyr::pivot_wider(names_from = sto,
        values_from = obs_value) |>

        dplyr::select(ref_area, ref_sector, time_period, P3.D, P31.D) |>

        dplyr::rowwise() |>
      dplyr::mutate(check = round(P3.D - P31.D, rounding)) |>
      dplyr::filter(abs(check) > threshold)

    }


  ## SIT03----------------------------------------------------------------------------------------

  sit03_filtered <- data |>


    dplyr::filter(sto %in% c("P5", "P51G", "P5M"),
      accounting_entry == "D"
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".")



  if(has_required_combos(sit03_filtered, c(""))){


    sit03 <- sit03_filtered |>


      tidyr::pivot_wider(names_from = sto,
      values_from = obs_value) |>


      dplyr::select(ref_area, ref_sector, time_period, P51G.D, P5M.D, P5.D) |>


      dplyr::rowwise() |>
    dplyr::mutate(`P51G.D + P5M.D` = sum(P51G.D,P5M.D, na.rm = TRUE),
           check = round(P5.D - `P51G.D + P5M.D`, rounding)) |>
    dplyr::filter(abs(check) > threshold)|>
      dplyr::filter(if_all(c(P51G.D,P5M.D), ~ !is.na(.x )))


  }
  ## SIT06----------------------------------------------------------------------------------------
  sit06_filtered <- data |>

    dplyr::filter(sto %in% c("D2", "D21", "D29"))


  if(has_required_combos(sit06_filtered, c(""))){

    sit06 <- sit06_filtered |>

      tidyr::pivot_wider(names_from = sto,
      values_from = obs_value) |>

      dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D21, D29, D2) |>

      dplyr::rowwise() |>
    dplyr::mutate(`D21 + D29` = sum(D21,D29, na.rm = TRUE),
           check = round(D2 - `D21 + D29`, rounding)) |>
    dplyr::filter(abs(check) > threshold)

  }
  ## SIT07----------------------------------------------------------------------------------------

  sit07_filtered <- data |>


    dplyr::filter(sto %in% c("D2", "D21"),
      ref_sector =="S1N",
      accounting_entry == "D")



  if(has_required_combos(sit07_filtered, c(""))){


    sit07 <- sit07_filtered |>


      tidyr::pivot_wider(names_from = sto,
      values_from = obs_value) |>


      dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D2, D21) |>


      dplyr::rowwise() |>
    dplyr::mutate(check = round(D2 - D21, rounding)) |>
    dplyr::filter(abs(check) > threshold)


  }
  ## SIT08----------------------------------------------------------------------------------------

  sit08_filtered <- data |>


    dplyr::filter(sto %in% c("D2", "D29"),
      !ref_sector %in% c("S1","S1N", "S2"),
      accounting_entry == "D")



  if(has_required_combos(sit08_filtered, c(""))){


    sit08 <- sit08_filtered |>


      tidyr::pivot_wider(names_from = sto,
      values_from = obs_value) |>


      dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D2, D29) |>


      dplyr::rowwise() |>
    dplyr::mutate(check = round(D2 - D29, rounding)) |>
    dplyr::filter(abs(check) > threshold)


  }
  ## SIT09----------------------------------------------------------------------------------------

  sit09_filtered <- data |>


    dplyr::filter(sto %in% c("D3", "D31", "D39"),
      ref_sector %in% c("S1","S2", "S13"),)



  if(has_required_combos(sit09_filtered, c(""))){


    sit09 <- sit09_filtered |>


      tidyr::pivot_wider(names_from = sto,
      values_from = obs_value) |>


      dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D31, D39, D3) |>


      dplyr::rowwise() |>
    dplyr::mutate(`D31 + D39` = sum(c(D31,D39), na.rm = TRUE),
           check = round(D3 - `D31 + D39`, rounding)) |>
    dplyr::filter(abs(check) > threshold)


  }
  ## SIT14----------------------------------------------------------------------------------------

  sit14_filtered <- data |>


    dplyr::filter(sto %in% c("D4", "D41", "D4N"))



  if(has_required_combos(sit14_filtered, c(""))){


    sit14 <- sit14_filtered |>


      tidyr::pivot_wider(names_from = sto,
      values_from = obs_value) |>


      dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D41, D4N, D4) |>


      dplyr::rowwise() |>
    dplyr::mutate(`D41 + D4N` = sum(c(D41, D4N), na.rm = TRUE),
           check = round(D4 - `D41 + D4N`, rounding)) |>
    dplyr::filter(abs(check) > threshold)


  }

  ## SIT18----------------------------------------------------------------------------------------

  sit18_filtered <- data |>


    dplyr::filter(sto %in% c("D6", "D61", "D62", "D63"),
      ref_sector !="S2")



  if(has_required_combos(sit18_filtered, c(""))){


    sit18 <- sit18_filtered |>


      tidyr::pivot_wider(names_from = sto,
      values_from = obs_value) |>


      dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D61, D62, D63, D6) |>


      dplyr::rowwise() |>
    dplyr::mutate(`D61 + D62 + D63` = sum(c(D61, D62, D63), na.rm = TRUE),
           check = round(D6 - `D61 + D62 + D63`, rounding)) |>
    dplyr::filter(abs(check) > threshold)


  }

  ## SIT19----------------------------------------------------------------------------------------

  sit19_filtered <- data |>


    dplyr::filter(sto %in% c("D6", "D62"),
      ref_sector %in% c("S11", "S12"),
      accounting_entry == "D")



  if(has_required_combos(sit19_filtered, c(""))){


    sit19 <- sit19_filtered |>


      tidyr::pivot_wider(names_from = sto,
      values_from = obs_value) |>


      dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D6, D62) |>


      dplyr::rowwise() |>
    dplyr::mutate(check = round(D6 - D62, rounding)) |>
    dplyr::filter(abs(check) > threshold)


  }
  ## SIT20----------------------------------------------------------------------------------------

  sit20_filtered <- data |>


    dplyr::filter(sto %in% c("D6", "D62", "D63"),
      ref_sector %in% c("S13"),
      accounting_entry == "D")



  if(has_required_combos(sit20_filtered, c(""))){


    sit20 <- sit20_filtered |>


      tidyr::pivot_wider(names_from = sto,
      values_from = obs_value) |>


      dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D62, D63, D6) |>


      dplyr::rowwise() |>
    dplyr::mutate(`D62 + D63` = sum(c(D62, D63), na.rm = TRUE),
           check = round(D6 - `D62 + D63`, rounding)) |>
    dplyr::filter(abs(check) > threshold)


  }
  ## SIT21----------------------------------------------------------------------------------------

  sit21_filtered <- data |>


    dplyr::filter(sto %in% c("D6", "D61", "D62"),
      ref_sector %in% c("S2"))



  if(has_required_combos(sit21_filtered, c(""))){


    sit21 <- sit21_filtered |>


      tidyr::pivot_wider(names_from = sto,
      values_from = obs_value) |>


      dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D61, D62, D6) |>


      dplyr::rowwise() |>
    dplyr::mutate(`D61 + D62` = sum(c(D61, D62), na.rm = TRUE),
           check = round(D6 - `D61 + D62`, rounding)) |>
    dplyr::filter(abs(check) > threshold)


  }

  ## SIT23----------------------------------------------------------------------------------------

  sit23_filtered <- data |>


    dplyr::filter(sto %in% c("D63", "D631", "D632"),)



  if(has_required_combos(sit23_filtered, c(""))){


    sit23 <- sit23_filtered |>


      tidyr::pivot_wider(names_from = sto,
      values_from = obs_value) |>


      dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D631, D632, D63) |>


      dplyr::rowwise() |>
    dplyr::mutate(`D631 + D632` = sum(c(D631, D632), na.rm = TRUE),
           check = round(D63 - `D631 + D632`, rounding)) |>
    dplyr::filter(abs(check) > threshold) |>
    dplyr::filter(if_all(c(D631,D632), ~ !is.na(.x )))


  }

  ## SIT28----------------------------------------------------------------------------------------

  ## SIT28----------------------------------------------------------------------------------------

    sit28_filtered <- data |>


      dplyr::filter(sto %in% c("D74", "D74_4Y"),
        ref_sector %in% c("S1", "S13"),
        accounting_entry == "D")



    if(has_required_combos(sit28_filtered, c(""))){


      sit28 <- sit28_filtered |>


        tidyr::pivot_wider(names_from = sto,
        values_from = obs_value) |>


        dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D74, D74_4Y) |>


        dplyr::filter(D74 < D74_4Y)


    }


  ## SIT31----------------------------------------------------------------------------------------

  sit31_filtered <- data |>


    dplyr::filter(sto %in% c("P6", "P61",  "P62"),
      ref_sector %in% c("S2"),
      accounting_entry == "D")



  if(has_required_combos(sit31_filtered, c(""))){


    sit31 <- sit31_filtered |>


      tidyr::pivot_wider(names_from = sto,
      values_from = obs_value) |>


      dplyr::select(ref_area, ref_sector, accounting_entry,time_period, P61,  P62, P6) |>


      dplyr::rowwise() |>
    dplyr::mutate(`P61 + P62` = sum(c(P61,P62), na.rm = TRUE),
           check = round(P6 - `P61 + P62`, rounding)) |>
    dplyr::filter(abs(check) > threshold)


  }
  ## SIT32----------------------------------------------------------------------------------------

  sit32_filtered <- data |>


    dplyr::filter(sto %in% c("P62", "P62F"),
      ref_sector %in% c("S2"),
      accounting_entry == "D")



  if(has_required_combos(sit32_filtered, c(""))){


    sit32 <- sit32_filtered |>


      tidyr::pivot_wider(names_from = sto,
      values_from = obs_value) |>


      dplyr::select(ref_area, ref_sector, accounting_entry,time_period, P62, P62F) |>


      dplyr::filter(P62F > P62)


  }

  ## SIT34----------------------------------------------------------------------------------------

  sit34_filtered <- data |>


    dplyr::filter(sto %in% c("D3", "D31"),
      ref_sector %in% c("S1N"),
      accounting_entry == "C")



  if(has_required_combos(sit34_filtered, c(""))){


    sit34 <- sit34_filtered |>


      tidyr::pivot_wider(names_from = sto,
      values_from = obs_value) |>


      dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D3, D31) |>


      dplyr::rowwise() |>
    dplyr::mutate(`check` = sum(c(D3, -D31), na.rm = TRUE)) |>
    dplyr::filter(abs(check) > threshold)


  }

  ## SIT35----------------------------------------------------------------------------------------

  sit35_filtered <- data |>


    dplyr::filter(sto %in% c("D3", "D39"),
      !ref_sector %in% c("S1", "S2", "S1N"),
      accounting_entry == "C")



  if(has_required_combos(sit35_filtered, c(""))){


    sit35 <- sit35_filtered |>


      tidyr::pivot_wider(names_from = sto,
      values_from = obs_value) |>


      dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D3, D39) |>


      dplyr::rowwise() |>
    dplyr::mutate(`check` = sum(c(D3, -D39), na.rm = TRUE)) |>
    dplyr::filter(abs(check) > threshold)


  }
  ## SIT36----------------------------------------------------------------------------------------

  sit36_filtered <- data |>


    dplyr::filter(sto %in% c("D6", "D61"),
      !ref_sector %in% c("S1", "S2", "S14", "S1M"),
      accounting_entry == "C")



  if(has_required_combos(sit36_filtered, c(""))){


    sit36 <- sit36_filtered |>


      tidyr::pivot_wider(names_from = sto,
      values_from = obs_value) |>


      dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D6, D61) |>


      dplyr::rowwise() |>
    dplyr::mutate(`check` = sum(c(D6, -D61), na.rm = TRUE)) |>
    dplyr::filter(abs(check) > threshold)


  }

  ## SIT41----------------------------------------------------------------------------------------

  sit41_filtered <- data |>


    dplyr::filter(sto %in% c("P7", "P71", "P72"),
      ref_sector %in% c("S2"),
      accounting_entry == "C")



  if(has_required_combos(sit41_filtered, c(""))){


    sit41 <- sit41_filtered |>


      tidyr::pivot_wider(names_from = sto,
      values_from = obs_value) |>


      dplyr::select(ref_area, ref_sector, accounting_entry,time_period, P7, P71, P72) |>


      dplyr::rowwise() |>
    dplyr::mutate(`P71 + P72` = sum(c(P71, P72), na.rm = TRUE),
           check = round(P7 - `P71 + P72`, rounding)) |>
    dplyr::filter(abs(check) > threshold)


  }
  ## SIT42----------------------------------------------------------------------------------------

  sit42_filtered <- data |>


    dplyr::filter(sto %in% c("P72", "P72F"),
      ref_sector %in% c("S2"),
      accounting_entry == "C")



  if(has_required_combos(sit42_filtered, c(""))){


    sit42 <- sit42_filtered |>


      tidyr::pivot_wider(names_from = sto,
      values_from = obs_value) |>


      dplyr::select(ref_area, ref_sector, accounting_entry,time_period, P72, P72F) |>


      dplyr::filter(P72F > P72)


  }
  ## SIT43----------------------------------------------------------------------------------------

    sit43_filtered <- data |>


      dplyr::filter(sto %in% c("D43", "D43_I9", "D43_J9"))



    if(has_required_combos(sit43_filtered, c(""))){


      sit43 <- sit43_filtered |>


        tidyr::pivot_wider(names_from = sto,
        values_from = obs_value) |>


        dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D43, D43_I9, D43_J9) |>


        dplyr::rowwise() |>
      dplyr::mutate(`D43_I9 + D43_J9` = sum(c(D43_I9, D43_J9), na.rm = TRUE),
             check = round(D43 - `D43_I9 + D43_J9`, rounding)) |>
      dplyr::filter(abs(check) > threshold) |>
      dplyr::filter(if_all(c(D43_I9,D43_J9), ~ !is.na(.x )))


    }
  ## SIT44----------------------------------------------------------------------------------------

    sit44_filtered <- data |>


      dplyr::filter(sto %in% c("D43", "D43_B6", "D43_D6"))



    if(has_required_combos(sit44_filtered, c(""))){


      sit44 <- sit44_filtered |>


        tidyr::pivot_wider(names_from = sto,
        values_from = obs_value) |>


        dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D43, D43_B6, D43_D6) |>


        dplyr::rowwise() |>
      dplyr::mutate(`D43_B6 + D43_D6` = sum(c(D43_B6, D43_D6), na.rm = TRUE),
             check = round(D43 - `D43_B6 + D43_D6`, rounding)) |>
      dplyr::filter(abs(check) > threshold) |>
      dplyr::filter(if_all(c(D43_B6,D43_D6), ~ !is.na(.x )))


    }
  ## SIT45----------------------------------------------------------------------------------------
  sit45_filtered <- data |>

    dplyr::filter(sto %in% c("D4N", "D42", "D43", "D44", "D45"))


  if(has_required_combos(sit45_filtered, c(""))){

    sit45 <- sit45_filtered |>

      tidyr::pivot_wider(names_from = sto,
      values_from = obs_value) |>

      dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D42, D43, D44, D45, D4N) |>

      dplyr::rowwise() |>
    dplyr::mutate(`D4N_calc` = sum(c(D42, D43, D44, D45), na.rm = TRUE),
           check = round(D4N - `D4N_calc`, rounding)) |>
    dplyr::filter(abs(check) > threshold)

  }
  ## SIT46----------------------------------------------------------------------------------------
  sit46_filtered <- data |>

    dplyr::filter(sto %in% c("D4N", "D42",  "D44", "D45"),
      ref_sector == "S13",
      accounting_entry == "D")


  if(has_required_combos(sit46_filtered, c(""))){

    sit46 <- sit46_filtered |>

      tidyr::pivot_wider(names_from = sto,
      values_from = obs_value) |>

      dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D42,  D44, D45, D4N) |>

      dplyr::rowwise() |>
    dplyr::mutate(`D4N_calc` = sum(c(D42, D44, D45), na.rm = TRUE),
           check = round(D4N - `D4N_calc`, rounding)) |>
    dplyr::filter(abs(check) > threshold)

  }

  ## SIT47----------------------------------------------------------------------------------------
  sit47_filtered <- data |>

    dplyr::filter(sto %in% c("D4N", "D45"),
      ref_sector %in% c("S1M", "S14", "S15"),
      accounting_entry == "D")


  if(has_required_combos(sit47_filtered, c(""))){

    sit47 <- sit47_filtered |>

      tidyr::pivot_wider(names_from = sto,
      values_from = obs_value) |>

      dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D45, D4N) |>

      dplyr::mutate(check = round(D4N - D45, rounding)) |>
    dplyr::filter(abs(check) > threshold)

  }

  ## SIT49----------------------------------------------------------------------------------------
  sit49_filtered <- data |>

    dplyr::filter(sto %in% c("D7N", "D71", "D72", "D7"))


  if(has_required_combos(sit49_filtered, c(""))){

    sit49 <- sit49_filtered |>

      tidyr::pivot_wider(names_from = sto,
      values_from = obs_value) |>

      dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D71, D72, D7N,D7) |>

      dplyr::rowwise() |>
    dplyr::mutate(`D7_calc` = sum(c(D71, D72, D7N), na.rm = TRUE),
           check = round(D7 - `D7_calc`, rounding)) |>
    dplyr::filter(abs(check) > threshold)

  }
  ## SIT50----------------------------------------------------------------------------------------

    sit50_filtered <- data |>


      dplyr::filter(sto %in% c("D7N", "D71", "D7"),
        accounting_entry == "D",
        ref_sector %in% c("S11", "S1M", "S14", "S15"))



    if(has_required_combos(sit50_filtered, c(""))){


      sit50 <- sit50_filtered |>


        tidyr::pivot_wider(names_from = sto,
        values_from = obs_value) |>


        dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D71, D7N,D7) |>


        dplyr::rowwise() |>
      dplyr::mutate(`D71 +D7N` = sum(c(D71,  D7N), na.rm = TRUE),
             check = round(D7 - `D71 +D7N`, rounding)) |>
      dplyr::filter(abs(check) > threshold)


    }

  ## SIT51----------------------------------------------------------------------------------------

    sit51_filtered <- data |>


      dplyr::filter(sto %in% c("D7N", "D74", "D75", "D76"),
        accounting_entry == "D",
        ref_sector %in% c("S1", "S13", "S2"))



    if(has_required_combos(sit51_filtered, c(""))){


      sit51 <- sit51_filtered |>


        tidyr::pivot_wider(names_from = sto,
        values_from = obs_value) |>


        dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D74,  D75, D76, D7N) |>


        dplyr::rowwise() |>
      dplyr::mutate(`D7N_cal` = sum(c(D74,  D75, D76), na.rm = TRUE),
             check = round(D7N - `D7N_cal`, rounding)) |>
      dplyr::filter(abs(check) > threshold) |>
      dplyr::filter(if_all(c(D74,D75,D76), ~ !is.na(.x )))


    }

  ## SIT52----------------------------------------------------------------------------------------
    sit52_filtered <- data |>

      dplyr::filter(sto %in% c("D7N", "D75"),
        ref_sector %in% c("S11", "S12", "S1M", "S14", "S15"))


    if(has_required_combos(sit52_filtered, c(""))){

      sit52 <- sit52_filtered |>

        tidyr::pivot_wider(names_from = sto,
        values_from = obs_value) |>

        dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D75,  D7N) |>

        dplyr::rowwise() |>
      dplyr::mutate(check = round(D7N - D75, rounding)) |>
      dplyr::filter(abs(check) > threshold)

    }
  ## SIT53----------------------------------------------------------------------------------------

  sit53_filtered <- data |>


    dplyr::filter(sto %in% c("D7N", "D74", "D75"),
      accounting_entry == "C",
      ref_sector %in% c("S1", "S13", "S2"))



  if(has_required_combos(sit53_filtered, c(""))){


    sit53 <- sit53_filtered |>


      tidyr::pivot_wider(names_from = sto,
      values_from = obs_value) |>


      dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D74,  D75, D7N) |>


      dplyr::rowwise() |>
    dplyr::mutate(`D7N_cal` = sum(c(D74,  D75), na.rm = TRUE),
           check = round(D7N - `D7N_cal`, rounding)) |>
    dplyr::filter(abs(check) > threshold) |>
      dplyr::filter(if_all(c(D74,D75), ~ !is.na(.x )))


  }
  ## SIT54----------------------------------------------------------------------------------------

    sit54_filtered <- data |>


      dplyr::filter(sto %in% c("D9", "D91", "D9N"))



    if(has_required_combos(sit54_filtered, c(""))){


      sit54 <- sit54_filtered |>


        tidyr::pivot_wider(names_from = sto,
        values_from = obs_value) |>


        dplyr::select(ref_area, ref_sector, accounting_entry,time_period, D91,  D9N, D9) |>


        dplyr::rowwise() |>
      dplyr::mutate(`D91 +D9N` = sum(c(D91,  D9N), na.rm = TRUE),
             check = round(D9 - `D91 +D9N`, rounding)) |>
      dplyr::filter(abs(check) > threshold)


    }
  ## SIT55----------------------------------------------------------------------------------------

    sit55_filtered <- data |>


      dplyr::filter(sto %in% c("D9", "D9N"),
        !ref_sector %in% c("S1", "S2", "S13"),
        accounting_entry == "C")



    if(has_required_combos(sit55_filtered, c(""))){


      sit55 <- sit55_filtered |>


        tidyr::pivot_wider(names_from = sto,
        values_from = obs_value) |>


        dplyr::select(ref_area, ref_sector, accounting_entry,time_period,  D9N, D9) |>


        dplyr::mutate(check = round(D9 - D9N, rounding)) |>
      dplyr::filter(abs(check) > threshold)


    }

  ## SIT56----------------------------------------------------------------------------------------

  sit56_filtered <- data |>


    dplyr::filter(sto %in% c("D9N", "D92", "D99"))



  if(has_required_combos(sit56_filtered, c(""))){


    sit56 <- sit56_filtered |>


      tidyr::pivot_wider(names_from = sto,
      values_from = obs_value) |>


      dplyr::select(ref_area, ref_sector, accounting_entry,time_period,  D92, D99, D9N) |>


      dplyr::rowwise() |>
    dplyr::mutate(`D92 +D99` = sum(c(D92,  D99), na.rm = TRUE),
           check = round(D9N - `D92 +D99`, rounding)) |>
    dplyr::filter(abs(check) > threshold)|>
    dplyr::filter(if_all(c(D92,D99), ~ !is.na(.x )))


  }

  ## SIT57----------------------------------------------------------------------------------------

    sit57_filtered <- data |>


      dplyr::filter(sto %in% c("D9N", "D99"),
        accounting_entry == "D",
        !ref_sector %in% c("S13", "S2", "S1"))



    if(has_required_combos(sit57_filtered, c(""))){


      sit57 <- sit57_filtered |>


        tidyr::pivot_wider(names_from = sto,
        values_from = obs_value) |>


        dplyr::select(ref_area, ref_sector, accounting_entry,time_period,  D9N, D99) |>


        dplyr::mutate(check = round(D9N - D99, rounding)) |>
      dplyr::filter(abs(check) > threshold)


    }
  ## SIT58----------------------------------------------------------------------------------------

    sit58_filtered <- data |>


      dplyr::filter(sto %in% c("D7", "D72", "D7N"),
        accounting_entry == "C",
        ref_sector %in% c("S11", "S1M", "S14", "S15"))



    if(has_required_combos(sit58_filtered, c(""))){


      sit58 <- sit58_filtered |>


        tidyr::pivot_wider(names_from = sto,
        values_from = obs_value) |>


        dplyr::select(ref_area, ref_sector, accounting_entry,time_period,  D72, D7N, D7) |>


        dplyr::rowwise() |>
      dplyr::mutate(`D7N +D72` = sum(c(D7N,  D72), na.rm = TRUE),
             check = round(D7 - `D7N +D72`, rounding)) |>
      dplyr::filter(abs(check) > threshold)


    }

  # Balancing items-------------------------------------------------------------------------------

  ## BI01-----------------------------------------------------------------------------------------

    BI01_filtered <- data |>


      dplyr::filter(ref_sector == "S1",
        sto %in% c("P1", "P2", "B1GQ", "D21X31")
      ) |>
      unite("sto", c(sto, accounting_entry), sep = ".")



    if(has_required_combos(BI01_filtered, c(""))){


      BI01 <- BI01_filtered |>


        tidyr::pivot_wider(names_from = sto,
                  values_from = obs_value) |>


        dplyr::select(ref_area, ref_sector, time_period, P1.C, P2.D, D21X31.C, B1GQ.B) |>


        dplyr::rowwise() |>
      dplyr::mutate(
        `P1.C - P2.D + D21X31.C` = P1.C - P2.D + D21X31.C,
        check = round(B1GQ.B - `P1.C - P2.D + D21X31.C`, rounding)
      ) |>
      dplyr::filter(abs(check) > threshold)


    }
  ## BI02-----------------------------------------------------------------------------------------

  BI02_filtered <- data |>


    dplyr::filter(ref_sector %in% c("S1", "S1N"),
      sto %in% c("D21", "D31", "D21X31")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".")



  if(has_required_combos(BI02_filtered, c(""))){


    BI02 <- BI02_filtered |>


      tidyr::pivot_wider(names_from = sto,
                values_from = obs_value) |>


      dplyr::select(ref_area, ref_sector, time_period, D21X31.C, D21.D, D31.C) |>


      dplyr::rowwise() |>
    dplyr::mutate(
      `D21.D - D31.C` = D21.D - D31.C,
      check = round(D21X31.C - `D21.D - D31.C`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)


  }

  ## BI03-----------------------------------------------------------------------------------------

  BI03_filtered <- data |>


    dplyr::filter(ref_sector == "S1N",
      sto %in% c("B1G", "D21X31"),
      accounting_entry != "D"
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".")



  if(has_required_combos(BI03_filtered, c(""))){


    BI03 <- BI03_filtered |>


      tidyr::pivot_wider(names_from = sto,
                values_from = obs_value) |>


      dplyr::select(ref_area, ref_sector, time_period, B1G.B, D21X31.C) |>


      dplyr::rowwise() |>
    dplyr::mutate(check = round(B1G.B - D21X31.C, rounding)) |>
    dplyr::filter(abs(check) > threshold)


  }

  ## BI04-----------------------------------------------------------------------------------------
  BI04_filtered <- data |>

    dplyr::filter(sto %in% c("B1G", "P1", "P2")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".")


  if(has_required_combos(BI04_filtered, c(""))){

    BI04 <- BI04_filtered |>

      tidyr::pivot_wider(names_from = sto,
                values_from = obs_value) |>

      dplyr::select(ref_area, ref_sector, time_period, B1G.B, P1.C, P2.D) |>

      dplyr::rowwise() |>
    dplyr::mutate(
      `P1.C - P2.D` = P1.C - P2.D,
      check = round(B1G.B - `P1.C - P2.D`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)

  }

  ## BI05-----------------------------------------------------------------------------------------

  BI05_filtered <- data |>


    dplyr::filter(ref_sector == "S1",
      sto %in% c("B1GQ", "B1NQ", "P51C"),
      accounting_entry != "C"
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".")



  if(has_required_combos(BI05_filtered, c(""))){


    BI05 <- BI05_filtered |>


      tidyr::pivot_wider(names_from = sto,
                values_from = obs_value) |>


      dplyr::select(ref_area, ref_sector, time_period, B1NQ.B, B1GQ.B, P51C.D) |>


      dplyr::rowwise() |>
    dplyr::mutate(
      `B1GQ.B - P51C.D` = B1GQ.B - P51C.D,
      check = round(B1NQ.B - `B1GQ.B - P51C.D`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)


  }


  ## BI07-----------------------------------------------------------------------------------------
  BI07_filtered <- data |>

    dplyr::filter(ref_sector != "S1N",
      sto %in% c("B1G", "B1N", "P51C"),
      accounting_entry != "D"
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".")


  if(has_required_combos(BI07_filtered, c(""))){

    BI07 <- BI07_filtered |>

      tidyr::pivot_wider(names_from = sto,
                values_from = obs_value) |>

      dplyr::select(ref_area, ref_sector, time_period, B1N.B, P51C.C,B1G.B) |>

      dplyr::rowwise() |>
    dplyr::mutate(
      `B1N.B + P51C.C` = B1N.B + P51C.C,
      check = round(B1G.B - `B1N.B + P51C.C`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)

  }

  ## BI08-----------------------------------------------------------------------------------------
  BI08_filtered <- data |>

    dplyr::filter(ref_sector %in% c("S1","S2"),
      sto %in% c("B1GQ", "P3", "P5", "P6", "P7")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".") |>
    dplyr::select(-ref_sector)


  if(has_required_combos(BI08_filtered, c(""))){

    BI08 <- BI08_filtered |>

      tidyr::pivot_wider(names_from = sto,
                values_from = obs_value) |>

      dplyr::select(ref_area,  time_period, P3.D, P5.D,P6.D,P7.C,B1GQ.B) |>

      dplyr::rowwise() |>
    dplyr::mutate(
      `B1GQ_cal` = P3.D + P5.D + P6.D - P7.C,
      check = round(B1GQ.B - `B1GQ_cal`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)

  }

  ## BI09-----------------------------------------------------------------------------------------
  BI09_filtered <- data |>

    dplyr::filter(ref_sector %in% c("S1"),
      sto %in% c("B1GQ", "B2A3G", "D1", "D2", "D3")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".")


  if(has_required_combos(BI09_filtered, c(""))){

    BI09 <- BI09_filtered |>

      tidyr::pivot_wider(names_from = sto,
                values_from = obs_value) |>

      dplyr::select(ref_area, ref_sector, time_period, B2A3G.B, D1.D,D3.C,D2.D,,B1GQ.B) |>

      dplyr::rowwise() |>
    dplyr::mutate(
      `B1GQ_cal` = B2A3G.B + D1.D + D2.D - D3.C,
      check = round(B1GQ.B - `B1GQ_cal`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)

  }

  ## BI10-----------------------------------------------------------------------------------------
  BI10_filtered <- data |>

    dplyr::filter(ref_sector != "S1",
      sto %in% c("B1G", "B2A3G", "D1", "D2", "D3")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".")


  if(has_required_combos(BI10_filtered, c(""))){

    BI10 <- BI10_filtered |>

      tidyr::pivot_wider(names_from = sto,
                values_from = obs_value) |>

      dplyr::select(ref_area, ref_sector, time_period, B2A3G.B, D1.D,D3.C,D2.D,,B1G.B) |>

      dplyr::rowwise() |>
    dplyr::mutate(
      `B1G_cal` = B2A3G.B + D1.D + D2.D - D3.C,
      check = round(B1G.B - `B1G_cal`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)

  }
  ## BI13-----------------------------------------------------------------------------------------

    BI13_filtered <- data |>


      dplyr::filter(ref_sector %in% c("S11","S12"),
        sto %in% c("B4G", "B2A3G", "D4","D41", "D44", "D45"),
      ) |>
      unite("sto", c(sto, accounting_entry), sep = ".")



    if(has_required_combos(BI13_filtered, c(""))){


      BI13 <- BI13_filtered |>


        tidyr::pivot_wider(names_from = sto,
                  values_from = obs_value) |>


        dplyr::select(ref_area, ref_sector, time_period, B2A3G.B, D4.C,D41.D,D44.D,D45.D, B4G.B) |>


        dplyr::rowwise() |>
      dplyr::mutate(
        `B4G_cal` = sum(c(B2A3G.B,D4.C,-D41.D,-D44.D,-D45.D),na.rm = TRUE),
        check = round(B4G.B - `B4G_cal`, rounding)
      ) |>
      dplyr::filter(abs(check) > threshold)


    }
  ## BI14-----------------------------------------------------------------------------------------

    BI14_filtered <- data |>


      dplyr::filter(ref_sector %in% c("S1"),
        sto %in% c("B5G", "B2A3G", "D1","D2", "D3", "D4"),
      ) |>
      unite("sto", c(sto, accounting_entry), sep = ".")



    if(has_required_combos(BI14_filtered, c(""))){


      BI14 <- BI14_filtered |>


        tidyr::pivot_wider(names_from = sto,
                  values_from = obs_value) |>


        dplyr::select(ref_area, ref_sector, time_period, B2A3G.B, D1.C,D2.C,D3.D,D4.C, D4.D,B5G.B) |>


        dplyr::rowwise() |>
      dplyr::mutate(
        `B5G_cal` = sum(c(B2A3G.B,D1.C,D2.C,-D3.D,D4.C,-D4.D),na.rm = TRUE),
        check = round(B5G.B - `B5G_cal`, rounding)
      ) |>
      dplyr::filter(abs(check) > threshold) |>
      dplyr::filter(if_all(c(D4.C,D4.D), ~ !is.na(.x )))


    }

  ## BI15-----------------------------------------------------------------------------------------
    BI15_filtered <- data |>

      dplyr::filter(!ref_sector %in% c("S1", "S13", "S14", "S1M"),
        sto %in% c("B5G", "B2A3G",  "D4"),
      ) |>
      unite("sto", c(sto, accounting_entry), sep = ".")


    if(has_required_combos(BI15_filtered, c(""))){

      BI15 <- BI15_filtered |>

        tidyr::pivot_wider(names_from = sto,
                  values_from = obs_value) |>

        dplyr::select(ref_area, ref_sector, time_period, B2A3G.B, D4.C,D4.D, B5G.B) |>

        dplyr::rowwise() |>
      dplyr::mutate(
        `B5G_cal` = sum(c(B2A3G.B,D4.C,-D4.D),na.rm = TRUE),
        check = round(B5G.B - `B5G_cal`, rounding)
      ) |>
      dplyr::filter(abs(check) > threshold)

    }
  ## BI16-----------------------------------------------------------------------------------------
  BI16_filtered <- data |>

    dplyr::filter(ref_sector %in% c("S13"),
      sto %in% c("B5G", "B2A3G", "D2", "D3" , "D4")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".")


  if(has_required_combos(BI16_filtered, c(""))){

    BI16 <- BI16_filtered |>

      tidyr::pivot_wider(names_from = sto,
                values_from = obs_value) |>

      dplyr::select(ref_area, ref_sector, time_period, B2A3G.B, D2.C,D3.D,D4.C,D4.D, B5G.B) |>

      dplyr::rowwise() |>
    dplyr::mutate(
      `B5G_cal` = sum(c(B2A3G.B,D2.C,-D3.D,D4.C,-D4.D),na.rm = TRUE),
      check = round(B5G.B - `B5G_cal`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)

  }
  ## BI17-----------------------------------------------------------------------------------------

    BI17_filtered <- data |>


      dplyr::filter(ref_sector %in% c("S1M", "S14", "S15"),
        sto %in% c("B5G", "B2A3G", "D1",  "D4")
      ) |>
      unite("sto", c(sto, accounting_entry), sep = ".")



    if(has_required_combos(BI17_filtered, c(""))){


      BI17 <- BI17_filtered |>


        tidyr::pivot_wider(names_from = sto,
                  values_from = obs_value) |>


        dplyr::select(ref_area, ref_sector, time_period, B2A3G.B, D1.C,D4.C,D4.D, B5G.B) |>


        dplyr::rowwise() |>
      dplyr::mutate(
        `B5G_cal` = sum(c(B2A3G.B,D1.C,D4.C,-D4.D),na.rm = TRUE),
        check = round(B5G.B - `B5G_cal`, rounding)
      ) |>
      dplyr::filter(abs(check) > threshold)


    }

  ## BI18-----------------------------------------------------------------------------------------

    BI18_filtered <- data |>


      dplyr::filter(ref_sector %in% c("S1", "S2"),
        sto %in% c("B5G", "B1GQ", "D1",  "D2", "D3", "D4")
      ) |>
      unite("sto", c(ref_sector,sto, accounting_entry), sep = ".")



    if(has_required_combos(BI18_filtered, c("S1.B1GQ.B", "S2.D1.D", "S2.D1.C", "S2.D2.D", "S2.D3.C", "S2.D4.D", "S2.D4.C", "S1.B5G.B"))){


      BI18 <- BI18_filtered |>


        tidyr::pivot_wider(names_from = sto,
                  values_from = obs_value) |>


        dplyr::select(ref_area, time_period, S1.B1GQ.B,S2.D1.D,S2.D1.C,S2.D2.D,S2.D3.C,S2.D4.D,S2.D4.C,S1.B5G.B) |>


        dplyr::rowwise() |>
      dplyr::mutate(
        `B5G_cal` = sum(c(S1.B1GQ.B,-S2.D1.D,S2.D1.C,-S2.D2.D,S2.D3.C,-S2.D4.D,S2.D4.C),na.rm = TRUE),
        check = round(S1.B5G.B - `B5G_cal`, rounding)
      ) |>
      dplyr::filter(abs(check) > threshold)


    }

  ## BI19-----------------------------------------------------------------------------------------

    BI19_filtered <- data |>


      dplyr::filter(ref_sector %in% c("S1"),
        sto %in% c("B5G", "D5", "D61","D62", "D7", "B6G")
      ) |>
      unite("sto", c(sto, accounting_entry), sep = ".")



    if(has_required_combos(BI19_filtered, c(""))){


      BI19 <- BI19_filtered |>


        tidyr::pivot_wider(names_from = sto,
                  values_from = obs_value) |>


        dplyr::select(ref_area, ref_sector, time_period, B5G.B,D5.C,D5.D,D61.C,D61.D,D62.C,D62.D,D7.C,D7.D,B6G.B) |>


        dplyr::rowwise() |>
      dplyr::mutate(
        `B6G_cal` = sum(c(B5G.B,D5.C,-D5.D,D61.C,-D61.D,D62.C,-D62.D,D7.C,-D7.D),na.rm = TRUE),
        check = round(B6G.B - `B6G_cal`, rounding)
      ) |>
      dplyr::filter(abs(check) > threshold) |>
      dplyr::filter(if_all(c(D61.D,D62.C), ~ !is.na(.x )))


    }
  ## BI20-----------------------------------------------------------------------------------------

    BI20_filtered <- data |>


      dplyr::filter(ref_sector %in% c("S11", "S12", "S15"),
        sto %in% c("B5G", "D5", "D61","D62", "D7", "B6G")
      ) |>
      unite("sto", c(sto, accounting_entry), sep = ".")



    if(has_required_combos(BI20_filtered, c(""))){


      BI20 <- BI20_filtered |>


        tidyr::pivot_wider(names_from = sto,
                  values_from = obs_value) |>


        dplyr::select(ref_area, ref_sector, time_period, B5G.B,D5.D,D61.C,D62.D,D7.C,D7.D,B6G.B) |>


        dplyr::rowwise() |>
      dplyr::mutate(
        `B6G_cal` = sum(c(B5G.B,-D5.D,D61.C,-D62.D,D7.C,-D7.D),na.rm = TRUE),
        check = round(B6G.B - `B6G_cal`, rounding)
      ) |>
      dplyr::filter(abs(check) > threshold)


    }

  ## BI21-----------------------------------------------------------------------------------------
  BI21_filtered <- data |>

    dplyr::filter(ref_sector %in% c("S13"),
      sto %in% c("B5G", "D5", "D61","D62", "D7", "B6G")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".")


  if(has_required_combos(BI21_filtered, c(""))){

    BI21 <- BI21_filtered |>

      tidyr::pivot_wider(names_from = sto,
                values_from = obs_value) |>

      dplyr::select(ref_area, ref_sector, time_period, B5G.B,D5.C,D5.D,D61.C,D62.D,D7.C,D7.D,B6G.B) |>

      dplyr::rowwise() |>
    dplyr::mutate(
      `B6G_cal` = sum(c(B5G.B,D5.C,-D5.D,D61.C,-D62.D,D7.C,-D7.D),na.rm = TRUE),
      check = round(B6G.B - `B6G_cal`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)

  }

  ## BI22-----------------------------------------------------------------------------------------

    BI22_filtered <- data |>


      dplyr::filter(ref_sector %in% c("S14", "S1M"),
        sto %in% c("B5G", "D5", "D61","D62", "D7", "B6G")
      ) |>
      unite("sto", c(sto, accounting_entry), sep = ".")



    if(has_required_combos(BI22_filtered, c(""))){


      BI22 <- BI22_filtered |>


        tidyr::pivot_wider(names_from = sto,
                  values_from = obs_value) |>


        dplyr::select(ref_area, ref_sector, time_period, B5G.B,D5.D,D5.D,D61.C,D61.D,D62.C,D62.D,D7.C,D7.D,B6G.B) |>


        dplyr::rowwise() |>
      dplyr::mutate(
        `B6G_cal` = sum(c(B5G.B,-D5.D,D61.C,-D61.D,D62.C,-D62.D,D7.C,-D7.D),na.rm = TRUE),
        check = round(B6G.B - `B6G_cal`, rounding)
      ) |>
      dplyr::filter(abs(check) > threshold)


    }


  ## BI23-----------------------------------------------------------------------------------------

  BI23_filtered <- data |>


    dplyr::filter(ref_sector %in% c("S1", "S2"),
      sto %in% c("B6G", "B5G","D5","D61","D62","D7")
    ) |>
    unite("sto", c(ref_sector,sto, accounting_entry), sep = ".")



  if(has_required_combos(BI23_filtered, c("S1.B5G.B", "S2.D5.D", "S2.D5.C", "S2.D61.D", "S2.D61.C", "S2.D62.D", "S2.D62.C", "S2.D7.D", "S2.D7.C", "S1.B6G.B"))){


    BI23 <- BI23_filtered |>


      tidyr::pivot_wider(names_from = sto,
                values_from = obs_value) |>


      dplyr::select(ref_area,  time_period, S1.B5G.B,S2.D5.D,S2.D5.C,S2.D61.D,S2.D61.C,S2.D62.D,S2.D62.C,S2.D7.D,S2.D7.C, S1.B6G.B) |>


      dplyr::rowwise() |>
    dplyr::mutate(
      `B6G_cal` = sum(c(S1.B5G.B,-S2.D5.D,S2.D5.C,-S2.D61.D,S2.D61.C,-S2.D62.D,S2.D62.C,-S2.D7.D,S2.D7.C),na.rm = TRUE),
      check = round(S1.B6G.B - `B6G_cal`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)


  }

  ## BI24-----------------------------------------------------------------------------------------

    BI24_filtered <- data |>


      dplyr::filter(ref_sector %in% c("S1", "S1M", "S14", "S15"),
        sto %in% c("B7G", "B6G", "D63")
      ) |>
      unite("sto", c(sto, accounting_entry), sep = ".")



    if(has_required_combos(BI24_filtered, c(""))){


      BI24 <- BI24_filtered |>


        tidyr::pivot_wider(names_from = sto,
                  values_from = obs_value) |>


        dplyr::select(ref_area, ref_sector, time_period, B6G.B,D63.C,D63.D,B7G.B) |>


        dplyr::rowwise() |>
      dplyr::mutate(
        `B7G_cal` = sum(c(B6G.B,D63.C,-D63.D),na.rm = TRUE),
        check = round(B7G.B - `B7G_cal`, rounding)
      ) |>
      dplyr::filter(abs(check) > threshold)


    }

  ## BI26-----------------------------------------------------------------------------------------
  BI26_filtered <- data |>

    dplyr::filter(ref_sector %in% c("S13", "S15"),
      sto %in% c("B7G", "B6G", "D63"),
      accounting_entry != "C"
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".")


  if(has_required_combos(BI26_filtered, c(""))){

    BI26 <- BI26_filtered |>

      tidyr::pivot_wider(names_from = sto,
                values_from = obs_value) |>

      dplyr::select(ref_area, ref_sector, time_period, B6G.B,D63.D,B7G.B) |>

      dplyr::rowwise() |>
    dplyr::mutate(
      `B7G_cal` = sum(c(B6G.B,-D63.D),na.rm = TRUE),
      check = round(B7G.B - `B7G_cal`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)

  }

  ## BI28-----------------------------------------------------------------------------------------
  BI28_filtered <- data |>

    dplyr::filter(ref_sector %in% c("S1", "S1M", "S14"),
      sto %in% c("B8G", "B6G", "D8", "P3")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".")


  if(has_required_combos(BI28_filtered, c(""))){

    BI28 <- BI28_filtered |>

      tidyr::pivot_wider(names_from = sto,
                values_from = obs_value) |>

      dplyr::select(ref_area, ref_sector, time_period, B6G.B,D8.C,D8.D,P3.D,B8G.B) |>

      dplyr::rowwise() |>
    dplyr::mutate(
      `B8G_cal` = sum(c(B6G.B,D8.C,-D8.D,-P3.D),na.rm = TRUE),
      check = round(B8G.B - `B8G_cal`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)

  }

  ## BI29-----------------------------------------------------------------------------------------

    BI29_filtered <- data |>


      dplyr::filter(ref_sector %in% c("S11", "S12"),
        sto %in% c("B8G", "B6G", "D8")
      ) |>
      unite("sto", c(sto, accounting_entry), sep = ".")



    if(has_required_combos(BI29_filtered, c(""))){


      BI29 <- BI29_filtered |>


        tidyr::pivot_wider(names_from = sto,
                  values_from = obs_value) |>


        dplyr::select(ref_area, ref_sector, time_period, B6G.B,D8.D,B8G.B) |>


        dplyr::rowwise() |>
      dplyr::mutate(
        `B6G.B - D8.D` = sum(c(B6G.B,-D8.D),na.rm = TRUE),
        check = round(B8G.B - `B6G.B - D8.D`, rounding)
      ) |>
      dplyr::filter(abs(check) > threshold)


    }
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
  ## BI31-----------------------------------------------------------------------------------------

    BI31_filtered <- data |>


      dplyr::filter(ref_sector %in% c("S1", "S2"),
        sto %in% c("B8G", "B6G", "D8", "P3")
      ) |>
      unite("sto", c(ref_sector,sto, accounting_entry), sep = ".")



    if(has_required_combos(BI31_filtered, c("S1.B6G.B", "S2.D8.D", "S2.D8.C", "S1.P3.D", "S1.B8G.B"))){


      BI31 <- BI31_filtered |>


        tidyr::pivot_wider(names_from = sto,
                  values_from = obs_value) |>


        dplyr::select(ref_area, time_period, S1.B6G.B,S2.D8.D,S2.D8.C,S1.P3.D,S1.B8G.B) |>


        dplyr::rowwise() |>
      dplyr::mutate(
        `B8G_calc` = sum(c(S1.B6G.B,-S2.D8.D,S2.D8.C,-S1.P3.D),na.rm = TRUE),
        check = round(S1.B8G.B - `B8G_calc`, rounding)
      ) |>
      dplyr::filter(abs(check) > threshold)


    }

  ## BI32-----------------------------------------------------------------------------------------
  BI32_filtered <- data |>

    dplyr::filter(!ref_sector %in% c("S2"),
      sto %in% c("B8G", "B101", "D9", "P51C")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".")


  if(has_required_combos(BI32_filtered, c(""))){

    BI32 <- BI32_filtered |>

      tidyr::pivot_wider(names_from = sto,
                values_from = obs_value) |>

      dplyr::select(ref_area, ref_sector, time_period, B8G.B,D9.C,D9.D,P51C.C,B101.B) |>

      dplyr::rowwise() |>
    dplyr::mutate(
      `B101_calc` = sum(c(B8G.B,D9.C,-D9.D,-P51C.C),na.rm = TRUE),
      check = round(B101.B - `B101_calc`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold) |>
    dplyr::filter(if_all(c(D9.C,D9.D), ~ !is.na(.x )))

  }


  ## BI33-----------------------------------------------------------------------------------------
  BI33_filtered <- data |>

    dplyr::filter(ref_sector %in% c("S2"),
      sto %in% c("B101", "D9", "B12")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".")


  if(has_required_combos(BI33_filtered, c(""))){

    BI33 <- BI33_filtered |>

      tidyr::pivot_wider(names_from = sto,
                values_from = obs_value) |>

      dplyr::select(ref_area, ref_sector, time_period, B12.B,D9.C,D9.D,B101.B) |>

      dplyr::rowwise() |>
    dplyr::mutate(
      `B101_calc` = sum(c(B12.B,D9.D,-D9.C),na.rm = TRUE),
      check = round(B101.B - `B101_calc`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)

  }

  ## BI34-----------------------------------------------------------------------------------------
  BI34_filtered <- data |>

    dplyr::filter(ref_sector %in% c("S1","S2"),
      sto %in% c("B101", "B8G","D9", "P51C")
    ) |>
    unite("sto", c(ref_sector,sto, accounting_entry), sep = ".")


  if(has_required_combos(BI34_filtered, c("S1.B8G.B", "S2.D9.D", "S2.D9.C", "S1.P51C.C", "S1.B101.B"))){

    BI34 <- BI34_filtered |>

      tidyr::pivot_wider(names_from = sto,
                values_from = obs_value) |>

      dplyr::select(ref_area,  time_period, S1.B8G.B,S2.D9.D,S2.D9.C,S1.P51C.C,S1.B101.B) |>

      dplyr::rowwise() |>
    dplyr::mutate(
      `B101_calc` = sum(c(S1.B8G.B,-S2.D9.D,S2.D9.C,-S1.P51C.C),na.rm = TRUE),
      check = round(S1.B101.B - `B101_calc`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)

  }

  ## BI35-----------------------------------------------------------------------------------------
  BI35_filtered <- data |>

    dplyr::filter(!ref_sector %in% c("S2"),
      sto %in% c("B9", "B8G", "D9", "P5", "NP")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".")


  if(has_required_combos(BI35_filtered, c(""))){

    BI35 <- BI35_filtered |>

      tidyr::pivot_wider(names_from = sto,
                values_from = obs_value) |>

      dplyr::select(ref_area, ref_sector, time_period, B8G.B,D9.C,D9.D,P5.D,NP.D,B9.B) |>

      dplyr::rowwise() |>
    dplyr::mutate(
      `B9_calc` = sum(c(B8G.B,D9.C,-D9.D,-P5.D,-NP.D),na.rm = TRUE),
      check = round(B9.B - `B9_calc`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)|>
      dplyr::filter(if_all(c(D9.C,D9.D), ~ !is.na(.x )))

  }

  ## BI36-----------------------------------------------------------------------------------------
  BI36_filtered <- data |>

    dplyr::filter(ref_sector %in% c("S2"),
      sto %in% c("B9", "B12", "D9", "NP")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".")


  if(has_required_combos(BI36_filtered, c(""))){

    BI36 <- BI36_filtered |>

      tidyr::pivot_wider(names_from = sto,
                values_from = obs_value) |>

      dplyr::select(ref_area, ref_sector, time_period, B12.B,D9.C,D9.D,NP.C,B9.B) |>

      dplyr::rowwise() |>
    dplyr::mutate(
      `B9_calc` = sum(c(B12.B,D9.D,-D9.C,-NP.C),na.rm = TRUE),
      check = round(B9.B - `B9_calc`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)

  }

  ## BI37-----------------------------------------------------------------------------------------
  BI37_filtered <- data |>

    dplyr::filter(ref_sector %in% c("S13"),
      sto %in% c("B9", "OTR", "OTE")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".")


  if(has_required_combos(BI37_filtered, c(""))){

    BI37 <- BI37_filtered |>

      tidyr::pivot_wider(names_from = sto,
                values_from = obs_value) |>

      dplyr::select(ref_area, ref_sector, time_period, OTR.C, OTE.D,B9.B) |>

      dplyr::rowwise() |>
    dplyr::mutate(
      `OTR.C - OTE.D` = sum(c(OTR.C,-OTE.D),na.rm = TRUE),
      check = round(B9.B - `OTR.C - OTE.D`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)

  }
  ## BI38-----------------------------------------------------------------------------------------
  BI38_filtered <- data |>

    dplyr::filter(!ref_sector %in% c("S2"),
      sto %in% c("B9", "B101", "P51C", "P5", "NP")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".")


  if(has_required_combos(BI38_filtered, c(""))){

    BI38 <- BI38_filtered |>

      tidyr::pivot_wider(names_from = sto,
                values_from = obs_value) |>

      dplyr::select(ref_area, ref_sector, time_period, B101.B, P51C.C, P5.D,NP.D,B9.B) |>

      dplyr::rowwise() |>
    dplyr::mutate(
      `B9_calc` = sum(c(B101.B,P51C.C,-P5.D,-NP.D),na.rm = TRUE),
      check = round(B9.B - `B9_calc`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)

  }
  ## BI39-----------------------------------------------------------------------------------------
  BI39_filtered <- data |>

    dplyr::filter(ref_sector %in% c("S2"),
      sto %in% c("B9", "B101", "NP")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".")


  if(has_required_combos(BI39_filtered, c(""))){

    BI39 <- BI39_filtered |>

      tidyr::pivot_wider(names_from = sto,
                values_from = obs_value) |>

      dplyr::select(ref_area, ref_sector, time_period, B101.B, NP.C,B9.B) |>

      dplyr::rowwise() |>
    dplyr::mutate(
      `B101.B - NP.C` = sum(c(B101.B,-NP.C),na.rm = TRUE),
      check = round(B9.B - `B101.B - NP.C`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)

  }

  ## BI40-----------------------------------------------------------------------------------------
  BI40_filtered <- data |>

    dplyr::filter(ref_sector %in% c("S1","S2"),
      sto %in% c("B9", "B8G", "D9", "P5", "NP")
    ) |>
    unite("sto", c(ref_sector,sto, accounting_entry), sep = ".")


  if(has_required_combos(BI40_filtered, c("S1.B8G.B", "S2.D9.D", "S2.D9.C", "S1.P5.D", "S1.NP.D", "S1.B9.B"))){

    BI40 <- BI40_filtered |>

      tidyr::pivot_wider(names_from = sto,
                values_from = obs_value) |>

      dplyr::select(ref_area,  time_period, S1.B8G.B,S2.D9.D,S2.D9.C,S1.P5.D,S1.NP.D, S1.B9.B) |>

      dplyr::rowwise() |>
    dplyr::mutate(
      `B9_calc` = sum(c(S1.B8G.B,-S2.D9.D,S2.D9.C,-S1.P5.D,-S1.NP.D),na.rm = TRUE),
      check = round(S1.B9.B - `B9_calc`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)

  }
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
  BI42_filtered <- data |>

    dplyr::filter(ref_sector %in% c("S2"),
      sto %in% c("B11", "P6", "P7")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".")


  if(has_required_combos(BI42_filtered, c(""))){

    BI42 <- BI42_filtered |>

      tidyr::pivot_wider(names_from = sto,
                values_from = obs_value) |>

      dplyr::select(ref_area, time_period, P7.C,P6.D,B11.B) |>

      dplyr::rowwise() |>
    dplyr::mutate(
      `P7.C - P6.D` = sum(c(P7.C,-P6.D),na.rm = TRUE),
      check = round(B11.B - `P7.C - P6.D`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)

  }

  ## BI43-----------------------------------------------------------------------------------------
    BI43_filtered <- data |>

      dplyr::filter(ref_sector %in% c("S2"),
        sto %in% c("B12","B11", "D1", "D2", "D3", "D4", "D5", "D61", "D62", "D7", "D8" )
      ) |>
      unite("sto", c(sto, accounting_entry), sep = ".")


    if(has_required_combos(BI43_filtered, c(""))){

      BI43 <- BI43_filtered |>

        tidyr::pivot_wider(names_from = sto,
                  values_from = obs_value) |>

        dplyr::select(ref_area, time_period, B11.B,,D1.D,D1.C,D2.D,D3.C,D4.D,D4.C,D5.D,D5.C,D61.D,D61.C,D62.D,D62.C,D7.D,D7.C,D8.D,D8.C, B12.B) |>

        dplyr::rowwise() |>
      dplyr::mutate(
        `B12.B_calc` = sum(c(B11.B,D1.D,-D1.C,D2.D,-D3.C,D4.D,-D4.C,D5.D,-D5.C,D61.D,-D61.C,D62.D,-D62.C,D7.D,-D7.C,D8.D,-D8.C),na.rm = TRUE),
        check = round(B12.B - `B12.B_calc`, rounding)
      ) |>
      dplyr::filter(abs(check) > threshold)

    }

  ## BI44-----------------------------------------------------------------------------------------
  BI44_filtered <- data |>

    dplyr::filter(ref_sector %in% c("S13"),
      sto %in% c("OTR", "P1O", "D2", "D39", "D4", "D5", "D61", "D7", "D9" )
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".")


  if(has_required_combos(BI44_filtered, c(""))){

    BI44 <- BI44_filtered |>

      tidyr::pivot_wider(names_from = sto,
                values_from = obs_value) |>

      dplyr::select(ref_area, time_period, P1O.C,D2.C,D39.C,D4.C,D5.C,D61.C,D7.C,D9.C,OTR.C) |>

      dplyr::rowwise() |>
    dplyr::mutate(
      `OTR.C_calc` = sum(c(P1O.C,D2.C,D39.C,D4.C,D5.C,D61.C,D7.C,D9.C),na.rm = TRUE),
      check = round(OTR.C - `OTR.C_calc`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)

  }

  ## BI45-----------------------------------------------------------------------------------------
  BI45_filtered <- data |>

    dplyr::filter(ref_sector %in% c("S13"),
      sto %in% c("OTE","P2","P5","D1","D29","D3","D4","D5","D62","D632","D7","D8","D9","NP")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".")


  if(has_required_combos(BI45_filtered, c(""))){

    BI45 <- BI45_filtered |>

      tidyr::pivot_wider(names_from = sto,
                values_from = obs_value) |>

      dplyr::select(ref_area, time_period, P2.D,P5.D,D1.D,D29.D,D3.D,D4.D,D5.D,D62.D,D632.D,D7.D,D8.D,D9.D,NP.D,OTE.D) |>

      dplyr::rowwise() |>
    dplyr::mutate(
      `OTE.D_calc` = sum(c(P2.D,P5.D,D1.D,D29.D,D3.D,D4.D,D5.D,D62.D,D632.D,D7.D,D8.D,D9.D,NP.D),na.rm = TRUE),
      check = round(OTE.D - `OTE.D_calc`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)

  }
  ## BI46-----------------------------------------------------------------------------------------
  BI46_filtered <- data |>

    dplyr::filter(ref_sector %in% c("S13"),
      sto %in% c("P31", "D63")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".")


  if(has_required_combos(BI46_filtered, c(""))){

    BI46 <- BI46_filtered |>

      tidyr::pivot_wider(names_from = sto,
                values_from = obs_value) |>

      dplyr::select(ref_area, time_period, P31.D,D63.D) |>

      dplyr::rowwise() |>
    dplyr::mutate(
      check = round(P31.D - `D63.D`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)

  }

  ## BI47-----------------------------------------------------------------------------------------
  BI47_filtered <- data |>

    dplyr::filter(ref_sector %in% c("S13"),
      sto %in% c("P3", "P1", "P1O", "D632")
    ) |>
    unite("sto", c(sto, accounting_entry), sep = ".")


  if(has_required_combos(BI47_filtered, c(""))){

    BI47 <- BI47_filtered |>

      tidyr::pivot_wider(names_from = sto,
                values_from = obs_value) |>

      dplyr::select(ref_area, time_period, P1.C,P1O.C,D632.D,P3.D) |>

      dplyr::rowwise() |>
    dplyr::mutate(
      `P3.D_calc` = sum(c(P1.C,-P1O.C,D632.D),na.rm = TRUE),
      check = round(P3.D - `P3.D_calc`, rounding)
    ) |>
    dplyr::filter(abs(check) > threshold)

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

