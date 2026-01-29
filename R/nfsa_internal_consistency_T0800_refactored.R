#' Performs internal consistency checks on NFSA data (T0800).
#'
#' This function takes an NFSA dataset and performs a series of internal
#' consistency checks based on predefined rules. It identifies discrepancies
#' in the data based on a specified threshold and outputs the results to an
#' Excel file.
#'
#' @param dataset A data frame containing the NFSA data.
#' @param output_sel Output directory path. Defaults to "output/internal".
#' @param threshold Threshold for flagging discrepancies. Defaults to 0.
#' @param rounding Number of decimal places to round to. Defaults to 2.
#'
#' @return None. Writes results to an Excel file.
#' @export

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Check if required columns exist in a dataframe
#' @noRd
has_required_cols <- function(df, required_cols) {
  if (nrow(df) == 0) return(FALSE)
  if (length(required_cols) == 0 || all(required_cols == "")) return(TRUE)
  all(required_cols %in% names(df))
}

#' Vectorized formula calculation with coefficients
#' @noRd
calc_formula <- function(df, terms, na_rm = TRUE) {
  result <- rep(0, nrow(df))
  for (col in names(terms)) {
    if (col %in% names(df)) {
      vals <- df[[col]]
      if (na_rm) vals[is.na(vals)] <- 0
      result <- result + terms[[col]] * vals
    }
  }
  result
}

#' Execute a standard consistency check
#' @noRd
run_check <- function(data, config, threshold, rounding) {
  tryCatch({
    filtered <- data

    # Step 1: Apply initial filters
    if (!is.null(config$sto_filter)) {
      if (isTRUE(config$sto_filter_exclude)) {
        filtered <- filtered |> dplyr::filter(!sto %in% config$sto_filter)
      } else {
        filtered <- filtered |> dplyr::filter(sto %in% config$sto_filter)
      }
    }

    if (!is.null(config$sector_filter)) {
      if (isTRUE(config$sector_filter_exclude)) {
        filtered <- filtered |> dplyr::filter(!ref_sector %in% config$sector_filter)
      } else {
        filtered <- filtered |> dplyr::filter(ref_sector %in% config$sector_filter)
      }
    }

    if (!is.null(config$entry_filter)) {
      if (isTRUE(config$entry_filter_exclude)) {
        filtered <- filtered |> dplyr::filter(!accounting_entry %in% config$entry_filter)
      } else {
        filtered <- filtered |> dplyr::filter(accounting_entry %in% config$entry_filter)
      }
    }

    # Step 2: Unite columns if specified
    if (!is.null(config$unite_cols)) {
      filtered <- filtered |>
        tidyr::unite("sto", dplyr::all_of(config$unite_cols), sep = ".")
    }

    # Step 3: Apply post-unite filter if specified
    if (!is.null(config$sto_post_filter)) {
      filtered <- filtered |> dplyr::filter(sto %in% config$sto_post_filter)
    }

    # Step 4: Drop ref_sector before pivot if specified
    if (isTRUE(config$drop_ref_sector)) {
      filtered <- filtered |> dplyr::select(-ref_sector)
    }

    # Step 5: Pivot wider
    pivot_cols <- config$pivot_names %||% "sto"

    pivoted <- filtered |>
      tidyr::pivot_wider(
        names_from = dplyr::all_of(pivot_cols),
        values_from = obs_value,
        names_sep = "."
      )

    # Step 6: Check required columns
    if (!has_required_cols(pivoted, config$required_cols)) {
      return(NULL)
    }

    # Step 7: Select columns
    select_cols <- config$select_cols
    available_cols <- intersect(select_cols, names(pivoted))
    if (length(available_cols) < length(select_cols)) {
      return(NULL)
    }

    result <- pivoted |> dplyr::select(dplyr::all_of(select_cols))

    # Step 8: Calculate check value using vectorized operations
    if (!is.null(config$formula_terms)) {
      calc_col_name <- config$calc_col_name %||% "calculated"
      result[[calc_col_name]] <- calc_formula(result, config$formula_terms)

      if (!is.null(config$check_col)) {
        result$check <- round(result[[config$check_col]] - result[[calc_col_name]], rounding)
      } else {
        result$check <- round(result[[calc_col_name]], rounding)
      }
    } else if (!is.null(config$sum_cols)) {
      calc_col_name <- config$calc_col_name %||% paste(config$sum_cols, collapse = " + ")
      result[[calc_col_name]] <- rowSums(result[, config$sum_cols, drop = FALSE], na.rm = TRUE)
      result$check <- round(result[[config$check_col]] - result[[calc_col_name]], rounding)
    } else if (!is.null(config$diff_cols)) {
      result$check <- round(result[[config$diff_cols[1]]] - result[[config$diff_cols[2]]], rounding)
    }

    # Step 9: Apply threshold filter
    if (!is.null(config$inequality_filter)) {
      col1 <- config$inequality_filter$col1
      col2 <- config$inequality_filter$col2
      op <- config$inequality_filter$op
      if (op == "<") {
        result <- result |> dplyr::filter(.data[[col1]] < .data[[col2]])
      } else if (op == ">") {
        result <- result |> dplyr::filter(.data[[col1]] > .data[[col2]])
      }
    } else {
      result <- result |> dplyr::filter(abs(check) > threshold)
    }

    # Step 10: Apply if_all filter if specified
    if (!is.null(config$require_non_na)) {
      result <- result |>
        dplyr::filter(dplyr::if_all(dplyr::all_of(config$require_non_na), ~ !is.na(.x)))
    }

    if (nrow(result) == 0) return(NULL)
    result

  }, error = function(e) {
    NULL
  })
}

# =============================================================================
# CHECK CONFIGURATIONS
# =============================================================================

get_ur_checks <- function() {
  list(
    ur01 = list(
      sto_filter = c("D1", "D11", "D12", "D4", "D41", "D41G", "D42", "D421", "D422", "D43", "D44",
                     "D441", "D442", "D443", "D45", "D5", "D51", "D59", "D6", "D61",
                     "D611", "D612", "D613", "D614", "D61SC", "D62", "D7", "D71", "D72",
                     "D74", "D75", "D8", "D9", "D91", "D92", "D99"),
      sector_filter = c("S1", "S2"),
      pivot_names = c("ref_sector", "accounting_entry"),
      required_cols = c("S1.D", "S2.C", "S1.C", "S2.D"),
      select_cols = c("ref_area", "sto", "time_period", "S1.D", "S2.C", "S1.C", "S2.D"),
      formula_terms = list("S1.D" = 1, "S2.C" = 1, "S1.C" = -1, "S2.D" = -1),
      calc_col_name = "S1.D + S2.C - S1.C - S2.D"
    ),
    ur02 = list(
      sto_filter = c("D2", "D21", "D29"),
      sector_filter = c("S1", "S2"),
      pivot_names = c("ref_sector", "accounting_entry"),
      required_cols = c("S1.D", "S1.C", "S2.D"),
      select_cols = c("ref_area", "sto", "time_period", "S1.D", "S1.C", "S2.D"),
      formula_terms = list("S1.D" = 1, "S1.C" = -1, "S2.D" = -1),
      calc_col_name = "S1.D - S1.C - S2.D"
    ),
    ur03 = list(
      sto_filter = c("D3", "D31", "D39"),
      sector_filter = c("S1", "S2"),
      pivot_names = c("ref_sector", "accounting_entry"),
      required_cols = c("S1.D", "S2.C", "S1.C"),
      select_cols = c("ref_area", "sto", "time_period", "S1.D", "S2.C", "S1.C"),
      formula_terms = list("S1.C" = 1, "S1.D" = -1, "S2.C" = -1),
      calc_col_name = "S1.C - S1.D - S2.C"
    ),
    ur04 = list(
      sto_filter = c("D63", "D631", "D632", "P51C"),
      sector_filter = c("S1"),
      pivot_names = c("ref_sector", "accounting_entry"),
      required_cols = c("S1.D", "S1.C"),
      select_cols = c("ref_area", "sto", "time_period", "S1.D", "S1.C"),
      diff_cols = c("S1.D", "S1.C")
    ),
    ur05 = list(
      sto_filter = c("D43", "D74", "D76"),
      sector_filter = c("S1", "S2"),
      pivot_names = c("ref_sector", "accounting_entry"),
      required_cols = c("S1.D", "S2.D"),
      select_cols = c("ref_area", "sto", "time_period", "S1.D", "S2.D"),
      diff_cols = c("S1.D", "S2.D")
    ),
    ur06 = list(
      sto_filter = c("NP"),
      sector_filter = c("S1", "S2"),
      pivot_names = c("ref_sector", "accounting_entry"),
      required_cols = c("S1.D", "S2.C"),
      select_cols = c("ref_area", "sto", "time_period", "S1.D", "S2.C"),
      formula_terms = list("S1.D" = 1, "S2.C" = 1),
      calc_col_name = "S1.D + S2.C"
    ),
    ur07 = list(
      sto_filter = c("B9", "B9X9F"),
      sector_filter = c("S1", "S2"),
      pivot_names = c("ref_sector"),
      select_cols = c("ref_area", "sto", "time_period", "S1", "S2"),
      formula_terms = list("S1" = 1, "S2" = 1),
      calc_col_name = "S1 + S2"
    ),
    ur08 = list(
      sto_filter = c("D43", "D74"),
      sector_filter = c("S1", "S2"),
      pivot_names = c("ref_sector", "accounting_entry"),
      required_cols = c("S1.C", "S2.C"),
      select_cols = c("ref_area", "sto", "time_period", "S1.C", "S2.C"),
      diff_cols = c("S1.C", "S2.C")
    ),
    ur09 = list(
      sto_filter = c("D12", "D611", "D612"),
      sector_filter = c("S1", "S2"),
      pivot_names = c("ref_sector", "sto", "accounting_entry"),
      required_cols = c("S1.D12.D", "S2.D12.C", "S1.D611.C", "S1.D612.C", "S2.D611.D", "S2.D612.D"),
      select_cols = c("ref_area", "time_period", "S1.D12.D", "S2.D12.C", "S1.D611.C", "S1.D612.C", "S2.D611.D", "S2.D612.D"),
      formula_terms = list("S1.D12.D" = 1, "S2.D12.C" = 1, "S1.D611.C" = -1, "S1.D612.C" = -1, "S2.D611.D" = -1, "S2.D612.D" = -1),
      calc_col_name = "check_calc"
    )
  )
}

get_s1ss_checks <- function() {
  list(
    s1ss01 = list(
      sector_filter = c("S1", "S11", "S12", "S13", "S1M", "S1N"),
      unite_cols = c("sto", "accounting_entry"),
      sto_post_filter = c("D2.D", "D3.C"),
      pivot_names = c("ref_sector"),
      select_cols = c("ref_area", "sto", "time_period", "S1N", "S11", "S12", "S13", "S1M", "S1"),
      sum_cols = c("S1N", "S11", "S12", "S13", "S1M"),
      check_col = "S1",
      calc_col_name = "S1N + S11 + S12 + S13 + S1M"
    ),
    s1ss02 = list(
      sector_filter = c("S1", "S11", "S12", "S13", "S1M"),
      unite_cols = c("sto", "accounting_entry"),
      sto_post_filter = c(
        "P2.D", "P5.D", "P51G.D", "P51G_N111G.D", "P51G_N112G.D", "P51G_N1121G.D", "P51G_N1122G.D",
        "P52.D", "P53.D", "D1.D", "D11.D", "D12.D", "D29.D", "D4.D", "D41.D", "D44.D", "D441.D",
        "D442.D", "D443.D", "D45.D", "D41G.D", "D5.D", "D51.D", "D59.D", "D6.D", "D62.D", "D7.D",
        "D71.D", "D75.D", "D8.D", "D9.D", "D99.D", "P51.C", "NP.D", "P1.C", "P11.C", "P12.C",
        "D39.C", "D4.C", "D41.C", "D42.C", "D421.C", "D422.C", "D43.C", "D44.C", "D441.C", "D442.C",
        "D443.C", "D45.C", "D41G.C", "D6.C", "D61.C", "D611.C", "D612.C", "D613.C", "D614.C",
        "D61SC.C", "D7.C", "D72.C", "D75.C", "D9.C", "D92.C", "D99.C", "P51C.C", "B2A3G.B",
        "B4G.B", "B5G.B", "B6G.B", "B8G.B", "B101.B", "B9.B", "B9X9F", "EMP.PS", "EMP.HW"
      ),
      pivot_names = c("ref_sector"),
      select_cols = c("ref_area", "sto", "time_period", "S11", "S12", "S13", "S1M", "S1"),
      sum_cols = c("S11", "S12", "S13", "S1M"),
      check_col = "S1",
      calc_col_name = "S11 + S12 + S13 + S1M"
    ),
    s1ss03 = list(
      sector_filter = c("S1", "S11", "S12", "S1M"),
      unite_cols = c("sto", "accounting_entry"),
      sto_post_filter = c("D43.D", "D91.D"),
      pivot_names = c("ref_sector"),
      select_cols = c("ref_area", "sto", "time_period", "S11", "S12", "S1M", "S1"),
      sum_cols = c("S11", "S12", "S1M"),
      check_col = "S1",
      calc_col_name = "S11 + S12 + S1M"
    ),
    s1ss04 = list(
      sector_filter = c("S1", "S11", "S12", "S13"),
      unite_cols = c("sto", "accounting_entry"),
      sto_post_filter = c("D42.D", "D421.D"),
      pivot_names = c("ref_sector"),
      select_cols = c("ref_area", "sto", "time_period", "S11", "S12", "S13", "S1"),
      sum_cols = c("S11", "S12", "S13"),
      check_col = "S1",
      calc_col_name = "S11 + S12 + S13"
    ),
    s1ss06 = list(
      sector_filter = c("S1", "S12", "S13"),
      unite_cols = c("sto", "accounting_entry"),
      sto_post_filter = c("D72.D", "D71.C"),
      pivot_names = c("ref_sector"),
      select_cols = c("ref_area", "sto", "time_period", "S12", "S13", "S1"),
      sum_cols = c("S12", "S13"),
      check_col = "S1",
      calc_col_name = "S12 + S13"
    ),
    s1ss07 = list(
      sector_filter = c("S1", "S13"),
      unite_cols = c("sto", "accounting_entry"),
      sto_post_filter = c(
        "P32.D", "D3.D", "D31.D", "D39.D", "D74.D", "D74_4Y.D", "D76.D", "D92.D", "D2.C", "D21.C",
        "D211.C", "D212.C", "D214.C", "D29.C", "D5.C", "D51.C", "D59.C", "D74.C", "D91.C"
      ),
      pivot_names = c("ref_sector"),
      select_cols = c("ref_area", "sto", "time_period", "S13", "S1"),
      diff_cols = c("S1", "S13")
    ),
    s1ss08 = list(
      sector_filter = c("S1", "S13", "S1M"),
      unite_cols = c("sto", "accounting_entry"),
      sto_post_filter = c("P3.D", "P31.D", "D63.D", "D631.D", "D632.D", "P13.C"),
      pivot_names = c("ref_sector"),
      select_cols = c("ref_area", "sto", "time_period", "S13", "S1M", "S1"),
      sum_cols = c("S13", "S1M"),
      check_col = "S1",
      calc_col_name = "S13 + S1M"
    ),
    s1ss09 = list(
      sector_filter = c("S1", "S1M"),
      unite_cols = c("sto", "accounting_entry"),
      sto_post_filter = c(
        "D61.D", "D611.D", "D612.D", "D613.D", "D614.D", "D61SC.D", "D1.C", "D11.C", "D12.C",
        "D62.C", "D63.C", "D631.C", "D632.C", "D8.C", "B3G.B"
      ),
      pivot_names = c("ref_sector"),
      select_cols = c("ref_area", "sto", "time_period", "S1M", "S1"),
      diff_cols = c("S1", "S1M")
    ),
    s1ss10 = list(
      sector_filter = c("S1", "S1N"),
      unite_cols = c("sto", "accounting_entry"),
      sto_post_filter = c("D21.D", "D31.C", "D21X31.C"),
      pivot_names = c("ref_sector"),
      select_cols = c("ref_area", "sto", "time_period", "S1N", "S1"),
      diff_cols = c("S1", "S1N")
    ),
    s1ss11 = list(
      sector_filter = c("S1M", "S14"),
      unite_cols = c("sto", "accounting_entry"),
      sto_post_filter = c(
        "B3G.B", "D1.C", "D11.C", "D12.C", "D61.D", "D611.D", "D612.D", "D613.D", "D614.D", "D61SC.D",
        "D62.C", "D63.C", "D631.C", "D632.C", "D8.C"
      ),
      pivot_names = c("ref_sector"),
      select_cols = c("ref_area", "sto", "time_period", "S1M", "S14"),
      diff_cols = c("S1M", "S14")
    ),
    s1ss12 = list(
      sector_filter = c("S1M", "S15"),
      unite_cols = c("sto", "accounting_entry"),
      sto_post_filter = c("D63.D", "D631.D", "D632.D", "P13.C"),
      pivot_names = c("ref_sector"),
      select_cols = c("ref_area", "sto", "time_period", "S1M", "S15"),
      diff_cols = c("S1M", "S15")
    ),
    s1ss13 = list(
      sector_filter = c("S1M", "S14", "S15"),
      unite_cols = c("sto", "accounting_entry"),
      sto_post_filter = c(
        "P2.D", "P3.D", "P31.D", "P5.D", "P51G.D", "P51G_N111G.D", "P51G_N112G.D",
        "P51G_N1121G.D", "P51G_N1122G.D", "P52.D", "P53.D", "D1.D", "D11.D", "D12.D",
        "D2.D", "D29.D", "D4.D", "D41.D", "D43.D", "D44.D", "D441.D", "D442.D", "D443.D",
        "D45.D", "D41G.D", "D5.D", "D51.D", "D59.D", "D6.D", "D62.D", "D7.D", "D71.D",
        "D75.D", "D8.D", "D9.D", "D91.D", "D99.D", "P51C.D", "NP.D", "P1.C", "P11.C",
        "P12.C", "D3.C", "D39.C", "D4.C", "D41.C", "D42.C", "D421.C", "D422.C", "D43.C",
        "D44.C", "D441.C", "D442.C", "D443.C", "D45.C", "D41G.C", "D6.C", "D61.C", "D611.C",
        "D612.C", "D613.C", "D614.C", "D61SC.C", "D7.C", "D72.C", "D75.C", "D9.C", "D92.C",
        "D99.C", "P51C.C", "B2A3G.B", "B4G.B", "B5G.B", "B6G.B", "B7G.B", "B8G.B",
        "B101.B", "B9.B", "B9X9F._Z", "B1G.B", "B1N.B", "EMP.PS", "EMP.HW"
      ),
      pivot_names = c("ref_sector"),
      select_cols = c("ref_area", "sto", "time_period", "S14", "S15", "S1M"),
      sum_cols = c("S14", "S15"),
      check_col = "S1M",
      calc_col_name = "S14 + S15"
    ),
    s1ss20 = list(
      unite_cols = c("ref_sector", "sto", "accounting_entry"),
      sto_post_filter = c("S1.B1GQ.B", "S1N.B1G.B", "S11.B1G.B", "S12.B1G.B", "S13.B1G.B", "S1M.B1G.B"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "time_period", "S1N.B1G.B", "S11.B1G.B", "S12.B1G.B", "S13.B1G.B", "S1M.B1G.B", "S1.B1GQ.B"),
      sum_cols = c("S1N.B1G.B", "S11.B1G.B", "S12.B1G.B", "S13.B1G.B", "S1M.B1G.B"),
      check_col = "S1.B1GQ.B",
      calc_col_name = "sum_B1G"
    )
  )
}

get_sit_checks <- function() {
  list(
    sit01 = list(
      sector_filter = c("S1", "S13"),
      sto_filter = c("P3", "P31", "P32"),
      entry_filter = c("D"),
      unite_cols = c("sto", "accounting_entry"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "time_period", "P31.D", "P32.D", "P3.D"),
      sum_cols = c("P31.D", "P32.D"),
      check_col = "P3.D",
      calc_col_name = "P31.D + P32.D"
    ),
    sit02 = list(
      sector_filter = c("S1M", "S14", "S15"),
      sto_filter = c("P3", "P31"),
      entry_filter = c("D"),
      unite_cols = c("sto", "accounting_entry"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "time_period", "P3.D", "P31.D"),
      diff_cols = c("P3.D", "P31.D")
    ),
    sit03 = list(
      sto_filter = c("P5", "P51G", "P52", "P53"),
      entry_filter = c("D"),
      unite_cols = c("sto", "accounting_entry"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "time_period", "P51G.D", "P52.D", "P53.D", "P5.D"),
      sum_cols = c("P51G.D", "P52.D", "P53.D"),
      check_col = "P5.D",
      calc_col_name = "P51G.D + P52.D + P53.D"
    ),
    sit05 = list(
      sto_filter = c("D1", "D11", "D12"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "accounting_entry", "time_period", "D11", "D12", "D1"),
      sum_cols = c("D11", "D12"),
      check_col = "D1",
      calc_col_name = "D11 + D12"
    ),
    sit06 = list(
      sto_filter = c("D2", "D21", "D29"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "accounting_entry", "time_period", "D21", "D29", "D2"),
      sum_cols = c("D21", "D29"),
      check_col = "D2",
      calc_col_name = "D21 + D29"
    ),
    sit07 = list(
      sto_filter = c("D2", "D21"),
      sector_filter = c("S1N"),
      entry_filter = c("D"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "accounting_entry", "time_period", "D2", "D21"),
      diff_cols = c("D2", "D21")
    ),
    sit08 = list(
      sto_filter = c("D2", "D29"),
      sector_filter = c("S1", "S1N", "S2"),
      sector_filter_exclude = TRUE,
      entry_filter = c("D"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "accounting_entry", "time_period", "D2", "D29"),
      diff_cols = c("D2", "D29")
    ),
    sit09 = list(
      sto_filter = c("D3", "D31", "D39"),
      sector_filter = c("S1", "S2", "S13"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "accounting_entry", "time_period", "D31", "D39", "D3"),
      sum_cols = c("D31", "D39"),
      check_col = "D3",
      calc_col_name = "D31 + D39"
    ),
    sit10 = list(
      sto_filter = c("D4", "D41", "D42", "D43", "D44", "D45"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "accounting_entry", "time_period", "D41", "D42", "D43", "D44", "D45", "D4"),
      sum_cols = c("D41", "D42", "D43", "D44", "D45"),
      check_col = "D4",
      calc_col_name = "D41 + D42 + D43 + D44 + D45"
    ),
    sit11 = list(
      sto_filter = c("D4", "D41", "D42", "D44", "D45"),
      sector_filter = c("S13"),
      entry_filter = c("D"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "accounting_entry", "time_period", "D41", "D42", "D44", "D45", "D4"),
      sum_cols = c("D41", "D42", "D44", "D45"),
      check_col = "D4",
      calc_col_name = "D41 + D42 + D44 + D45"
    ),
    sit12 = list(
      sto_filter = c("D4", "D41", "D43", "D44", "D45"),
      sector_filter = c("S1M", "S14", "S15"),
      entry_filter = c("D"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "accounting_entry", "time_period", "D41", "D43", "D44", "D45", "D4"),
      sum_cols = c("D41", "D43", "D44", "D45"),
      check_col = "D4",
      calc_col_name = "D41 + D43 + D44 + D45"
    ),
    sit15 = list(
      sto_filter = c("D42", "D421", "D422"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "accounting_entry", "time_period", "D42", "D421", "D422"),
      sum_cols = c("D421", "D422"),
      check_col = "D42",
      calc_col_name = "D421 + D422"
    ),
    sit16 = list(
      sto_filter = c("D44", "D441", "D442", "D443"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "accounting_entry", "time_period", "D44", "D441", "D442", "D443"),
      sum_cols = c("D441", "D442", "D443"),
      check_col = "D44",
      calc_col_name = "D441 + D442 + D443"
    ),
    sit17 = list(
      sto_filter = c("D5", "D51", "D59"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "accounting_entry", "time_period", "D51", "D59", "D5"),
      sum_cols = c("D51", "D59"),
      check_col = "D5",
      calc_col_name = "D51 + D59"
    ),
    sit18 = list(
      sto_filter = c("D6", "D61", "D62", "D63"),
      sector_filter = c("S2"),
      sector_filter_exclude = TRUE,
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "accounting_entry", "time_period", "D61", "D62", "D63", "D6"),
      sum_cols = c("D61", "D62", "D63"),
      check_col = "D6",
      calc_col_name = "D61 + D62 + D63"
    ),
    sit19 = list(
      sto_filter = c("D6", "D62"),
      sector_filter = c("S11", "S12"),
      entry_filter = c("D"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "accounting_entry", "time_period", "D6", "D62"),
      diff_cols = c("D6", "D62")
    ),
    sit20 = list(
      sto_filter = c("D6", "D62", "D63"),
      sector_filter = c("S13", "S15"),
      entry_filter = c("D"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "accounting_entry", "time_period", "D62", "D63", "D6"),
      sum_cols = c("D62", "D63"),
      check_col = "D6",
      calc_col_name = "D62 + D63"
    ),
    sit20b = list(
      sto_filter = c("D6", "D61", "D62"),
      sector_filter = c("S14"),
      entry_filter = c("D"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "accounting_entry", "time_period", "D61", "D62", "D6"),
      sum_cols = c("D61", "D62"),
      check_col = "D6",
      calc_col_name = "D61 + D62"
    ),
    sit21 = list(
      sto_filter = c("D6", "D61", "D62"),
      sector_filter = c("S2"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "accounting_entry", "time_period", "D61", "D62", "D6"),
      sum_cols = c("D61", "D62"),
      check_col = "D6",
      calc_col_name = "D61 + D62"
    ),
    sit22 = list(
      sto_filter = c("D61", "D611", "D612", "D613", "D614", "D61SC"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "accounting_entry", "time_period", "D611", "D612", "D613", "D614", "D61SC", "D61"),
      formula_terms = list("D611" = 1, "D612" = 1, "D613" = 1, "D614" = 1, "D61SC" = -1),
      check_col = "D61",
      calc_col_name = "sum_D61"
    ),
    sit23 = list(
      sto_filter = c("D63", "D631", "D632"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "accounting_entry", "time_period", "D631", "D632", "D63"),
      sum_cols = c("D631", "D632"),
      check_col = "D63",
      calc_col_name = "D631 + D632",
      require_non_na = c("D631", "D632")
    ),
    sit24 = list(
      sto_filter = c("D7", "D71", "D72", "D73", "D74", "D75", "D76"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "accounting_entry", "time_period", "D71", "D72", "D74", "D75", "D76", "D7"),
      sum_cols = c("D71", "D72", "D74", "D75", "D76"),
      check_col = "D7",
      calc_col_name = "sum_D7"
    ),
    sit25 = list(
      sto_filter = c("D7", "D71", "D75"),
      sector_filter = c("S11", "S1M", "S14", "S15"),
      entry_filter = c("D"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "accounting_entry", "time_period", "D71", "D75", "D7"),
      sum_cols = c("D71", "D75"),
      check_col = "D7",
      calc_col_name = "D71 + D75"
    ),
    sit27 = list(
      sto_filter = c("D7", "D71", "D72", "D74", "D75"),
      sector_filter = c("S2", "S1", "S13"),
      entry_filter = c("C"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "accounting_entry", "time_period", "D71", "D72", "D74", "D75", "D7"),
      sum_cols = c("D71", "D72", "D74", "D75"),
      check_col = "D7",
      calc_col_name = "D71 + D72 + D74 + D75"
    ),
    sit28 = list(
      sto_filter = c("D74", "D74_4Y"),
      sector_filter = c("S1", "S13"),
      entry_filter = c("D"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "accounting_entry", "time_period", "D74", "D74_4Y"),
      inequality_filter = list(col1 = "D74", col2 = "D74_4Y", op = "<")
    ),
    sit29 = list(
      sto_filter = c("D9", "D91", "D92", "D99"),
      sector_filter = c("S2", "S1", "S13"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "accounting_entry", "time_period", "D91", "D92", "D99", "D9"),
      sum_cols = c("D91", "D92", "D99"),
      check_col = "D9",
      calc_col_name = "D91 + D92 + D99"
    ),
    sit30 = list(
      sto_filter = c("D9", "D91", "D99"),
      sector_filter = c("S11", "S12", "S1M", "S14", "S15"),
      entry_filter = c("D"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "accounting_entry", "time_period", "D91", "D99", "D9"),
      sum_cols = c("D91", "D99"),
      check_col = "D9",
      calc_col_name = "D91 + D99"
    ),
    sit30b = list(
      sto_filter = c("D9", "D92", "D99"),
      sector_filter = c("S13"),
      entry_filter = c("D"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "accounting_entry", "time_period", "D92", "D99", "D9"),
      sum_cols = c("D92", "D99"),
      check_col = "D9",
      calc_col_name = "D92 + D99"
    ),
    sit31 = list(
      sto_filter = c("P6", "P61", "P62"),
      sector_filter = c("S2"),
      entry_filter = c("D"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "accounting_entry", "time_period", "P61", "P62", "P6"),
      sum_cols = c("P61", "P62"),
      check_col = "P6",
      calc_col_name = "P61 + P62"
    ),
    sit32 = list(
      sto_filter = c("P62", "P62F"),
      sector_filter = c("S2"),
      entry_filter = c("D"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "accounting_entry", "time_period", "P62", "P62F"),
      inequality_filter = list(col1 = "P62F", col2 = "P62", op = ">")
    ),
    sit33 = list(
      sto_filter = c("D21", "D211", "D212", "D214"),
      sector_filter = c("S1", "S13", "S2"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "accounting_entry", "time_period", "D211", "D212", "D214", "D21"),
      sum_cols = c("D211", "D212", "D214"),
      check_col = "D21",
      calc_col_name = "D211 + D212 + D214",
      require_non_na = c("D211", "D212", "D214")
    ),
    sit34 = list(
      sto_filter = c("D3", "D31"),
      sector_filter = c("S1N"),
      entry_filter = c("C"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "accounting_entry", "time_period", "D3", "D31"),
      diff_cols = c("D3", "D31")
    ),
    sit35 = list(
      sto_filter = c("D3", "D39"),
      sector_filter = c("S1", "S2", "S1N"),
      sector_filter_exclude = TRUE,
      entry_filter = c("C"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "accounting_entry", "time_period", "D3", "D39"),
      diff_cols = c("D3", "D39")
    ),
    sit36 = list(
      sto_filter = c("D6", "D61"),
      sector_filter = c("S1", "S2", "S14", "S1M"),
      sector_filter_exclude = TRUE,
      entry_filter = c("C"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "accounting_entry", "time_period", "D6", "D61"),
      diff_cols = c("D6", "D61")
    ),
    sit37 = list(
      sto_filter = c("D7", "D72", "D75"),
      sector_filter = c("S11", "S1M", "S14", "S15"),
      entry_filter = c("C"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "accounting_entry", "time_period", "D72", "D75", "D7"),
      sum_cols = c("D72", "D75"),
      check_col = "D7",
      calc_col_name = "D72 + D75"
    ),
    sit38 = list(
      sto_filter = c("D9", "D92", "D99"),
      sector_filter = c("S1", "S2", "S13"),
      sector_filter_exclude = TRUE,
      entry_filter = c("C"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "accounting_entry", "time_period", "D92", "D99", "D9"),
      sum_cols = c("D92", "D99"),
      check_col = "D9",
      calc_col_name = "D92 + D99"
    ),
    sit39 = list(
      sto_filter = c("P1", "P11", "P12", "P13"),
      entry_filter = c("C"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "accounting_entry", "time_period", "P11", "P12", "P13", "P1"),
      sum_cols = c("P11", "P12", "P13"),
      check_col = "P1",
      calc_col_name = "P11 + P12 + P13"
    ),
    sit40 = list(
      sto_filter = c("P1", "P11", "P12"),
      sector_filter = c("S1", "S13", "S15", "S1M"),
      sector_filter_exclude = TRUE,
      entry_filter = c("C"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "accounting_entry", "time_period", "P11", "P12", "P1"),
      sum_cols = c("P11", "P12"),
      check_col = "P1",
      calc_col_name = "P11 + P12"
    ),
    sit41 = list(
      sto_filter = c("P7", "P71", "P72"),
      sector_filter = c("S2"),
      entry_filter = c("C"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "accounting_entry", "time_period", "P7", "P71", "P72"),
      sum_cols = c("P71", "P72"),
      check_col = "P7",
      calc_col_name = "P71 + P72"
    ),
    sit42 = list(
      sto_filter = c("P72", "P72F"),
      sector_filter = c("S2"),
      entry_filter = c("C"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "accounting_entry", "time_period", "P72", "P72F"),
      inequality_filter = list(col1 = "P72F", col2 = "P72", op = ">")
    ),
    sit44 = list(
      sto_filter = c("D43", "D43_B6", "D43_D6"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "accounting_entry", "time_period", "D43", "D43_B6", "D43_D6"),
      sum_cols = c("D43_B6", "D43_D6"),
      check_col = "D43",
      calc_col_name = "D43_B6 + D43_D6",
      require_non_na = c("D43_B6", "D43_D6")
    )
  )
}

get_bi_checks <- function() {
  list(
    BI01 = list(
      sector_filter = c("S1"),
      sto_filter = c("P1", "P2", "B1GQ", "D21X31"),
      unite_cols = c("sto", "accounting_entry"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "time_period", "P1.C", "P2.D", "D21X31.C", "B1GQ.B"),
      formula_terms = list("P1.C" = 1, "P2.D" = -1, "D21X31.C" = 1),
      check_col = "B1GQ.B",
      calc_col_name = "P1.C - P2.D + D21X31.C"
    ),
    BI02 = list(
      sector_filter = c("S1", "S1N"),
      sto_filter = c("D21", "D31", "D21X31"),
      unite_cols = c("sto", "accounting_entry"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "time_period", "D21X31.C", "D21.D", "D31.C"),
      formula_terms = list("D21.D" = 1, "D31.C" = -1),
      check_col = "D21X31.C",
      calc_col_name = "D21.D - D31.C"
    ),
    BI03 = list(
      sector_filter = c("S1N"),
      sto_filter = c("B1G", "D21X31"),
      entry_filter = c("D"),
      entry_filter_exclude = TRUE,
      unite_cols = c("sto", "accounting_entry"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "time_period", "B1G.B", "D21X31.C"),
      diff_cols = c("B1G.B", "D21X31.C")
    ),
    BI04 = list(
      sto_filter = c("B1G", "P1", "P2"),
      unite_cols = c("sto", "accounting_entry"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "time_period", "B1G.B", "P1.C", "P2.D"),
      formula_terms = list("P1.C" = 1, "P2.D" = -1),
      check_col = "B1G.B",
      calc_col_name = "P1.C - P2.D"
    ),
    BI05 = list(
      sector_filter = c("S1"),
      sto_filter = c("B1GQ", "B1NQ", "P51C"),
      entry_filter = c("C"),
      entry_filter_exclude = TRUE,
      unite_cols = c("sto", "accounting_entry"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "time_period", "B1NQ.B", "B1GQ.B", "P51C.D"),
      formula_terms = list("B1GQ.B" = 1, "P51C.D" = -1),
      check_col = "B1NQ.B",
      calc_col_name = "B1GQ.B - P51C.D"
    ),
    BI06 = list(
      sector_filter = c("S1N"),
      sto_filter = c("B1G", "B1N"),
      entry_filter = c("B"),
      unite_cols = c("sto", "accounting_entry"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "time_period", "B1N.B", "B1G.B"),
      diff_cols = c("B1G.B", "B1N.B")
    ),
    BI07 = list(
      sector_filter = c("S1N"),
      sector_filter_exclude = TRUE,
      sto_filter = c("B1G", "B1N", "P51C"),
      entry_filter = c("D"),
      entry_filter_exclude = TRUE,
      unite_cols = c("sto", "accounting_entry"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "time_period", "B1N.B", "P51C.C", "B1G.B"),
      formula_terms = list("B1N.B" = 1, "P51C.C" = 1),
      check_col = "B1G.B",
      calc_col_name = "B1N.B + P51C.C"
    ),
    BI08 = list(
      sector_filter = c("S1", "S2"),
      sto_filter = c("B1GQ", "P3", "P5", "P6", "P7"),
      unite_cols = c("sto", "accounting_entry"),
      drop_ref_sector = TRUE,
      pivot_names = c("sto"),
      select_cols = c("ref_area", "time_period", "P3.D", "P5.D", "P6.D", "P7.C", "B1GQ.B"),
      formula_terms = list("P3.D" = 1, "P5.D" = 1, "P6.D" = 1, "P7.C" = -1),
      check_col = "B1GQ.B",
      calc_col_name = "B1GQ_cal"
    ),
    BI09 = list(
      sector_filter = c("S1"),
      sto_filter = c("B1GQ", "B2A3G", "D1", "D2", "D3"),
      unite_cols = c("sto", "accounting_entry"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "time_period", "B2A3G.B", "D1.D", "D3.C", "D2.D", "B1GQ.B"),
      formula_terms = list("B2A3G.B" = 1, "D1.D" = 1, "D2.D" = 1, "D3.C" = -1),
      check_col = "B1GQ.B",
      calc_col_name = "B1GQ_cal"
    ),
    BI10 = list(
      sector_filter = c("S1"),
      sector_filter_exclude = TRUE,
      sto_filter = c("B1G", "B2A3G", "D1", "D2", "D3"),
      unite_cols = c("sto", "accounting_entry"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "time_period", "B2A3G.B", "D1.D", "D3.C", "D2.D", "B1G.B"),
      formula_terms = list("B2A3G.B" = 1, "D1.D" = 1, "D2.D" = 1, "D3.C" = -1),
      check_col = "B1G.B",
      calc_col_name = "B1G_cal"
    ),
    BI11 = list(
      sector_filter = c("S1", "S1M", "S14"),
      sto_filter = c("B2G", "B2A3G", "B3G"),
      entry_filter = c("B"),
      unite_cols = c("sto", "accounting_entry"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "time_period", "B2G.B", "B3G.B", "B2A3G.B"),
      sum_cols = c("B2G.B", "B3G.B"),
      check_col = "B2A3G.B",
      calc_col_name = "B2G.B + B3G.B"
    ),
    BI13 = list(
      sector_filter = c("S11", "S12"),
      sto_filter = c("B4G", "B2A3G", "D4", "D41", "D44", "D45"),
      unite_cols = c("sto", "accounting_entry"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "time_period", "B2A3G.B", "D4.C", "D41.D", "D44.D", "D45.D", "B4G.B"),
      formula_terms = list("B2A3G.B" = 1, "D4.C" = 1, "D41.D" = -1, "D44.D" = -1, "D45.D" = -1),
      check_col = "B4G.B",
      calc_col_name = "B4G_cal"
    ),
    BI14 = list(
      sector_filter = c("S1"),
      sto_filter = c("B5G", "B2A3G", "D1", "D2", "D3", "D4"),
      unite_cols = c("sto", "accounting_entry"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "time_period", "B2A3G.B", "D1.C", "D2.C", "D3.D", "D4.C", "D4.D", "B5G.B"),
      formula_terms = list("B2A3G.B" = 1, "D1.C" = 1, "D2.C" = 1, "D3.D" = -1, "D4.C" = 1, "D4.D" = -1),
      check_col = "B5G.B",
      calc_col_name = "B5G_cal"
    ),
    BI15 = list(
      sector_filter = c("S1", "S13", "S14", "S1M"),
      sector_filter_exclude = TRUE,
      sto_filter = c("B5G", "B2A3G", "D4"),
      unite_cols = c("sto", "accounting_entry"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "time_period", "B2A3G.B", "D4.C", "D4.D", "B5G.B"),
      formula_terms = list("B2A3G.B" = 1, "D4.C" = 1, "D4.D" = -1),
      check_col = "B5G.B",
      calc_col_name = "B5G_cal"
    ),
    BI16 = list(
      sector_filter = c("S13"),
      sto_filter = c("B5G", "B2A3G", "D2", "D3", "D4"),
      unite_cols = c("sto", "accounting_entry"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "time_period", "B2A3G.B", "D2.C", "D3.D", "D4.C", "D4.D", "B5G.B"),
      formula_terms = list("B2A3G.B" = 1, "D2.C" = 1, "D3.D" = -1, "D4.C" = 1, "D4.D" = -1),
      check_col = "B5G.B",
      calc_col_name = "B5G_cal"
    ),
    BI17 = list(
      sector_filter = c("S1M", "S14"),
      sto_filter = c("B5G", "B2A3G", "D1", "D4"),
      unite_cols = c("sto", "accounting_entry"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "time_period", "B2A3G.B", "D1.C", "D4.C", "D4.D", "B5G.B"),
      formula_terms = list("B2A3G.B" = 1, "D1.C" = 1, "D4.C" = 1, "D4.D" = -1),
      check_col = "B5G.B",
      calc_col_name = "B5G_cal"
    ),
    BI19 = list(
      sector_filter = c("S1"),
      sto_filter = c("B5G", "D5", "D61", "D62", "D7", "B6G"),
      unite_cols = c("sto", "accounting_entry"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "time_period", "B5G.B", "D5.C", "D5.D", "D61.C", "D61.D", "D62.C", "D62.D", "D7.C", "D7.D", "B6G.B"),
      formula_terms = list("B5G.B" = 1, "D5.C" = 1, "D5.D" = -1, "D61.C" = 1, "D61.D" = -1, "D62.C" = 1, "D62.D" = -1, "D7.C" = 1, "D7.D" = -1),
      check_col = "B6G.B",
      calc_col_name = "B6G_cal"
    ),
    BI20 = list(
      sector_filter = c("S11", "S12", "S15"),
      sto_filter = c("B5G", "D5", "D61", "D62", "D7", "B6G"),
      unite_cols = c("sto", "accounting_entry"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "time_period", "B5G.B", "D5.D", "D61.C", "D62.D", "D7.C", "D7.D", "B6G.B"),
      formula_terms = list("B5G.B" = 1, "D5.D" = -1, "D61.C" = 1, "D62.D" = -1, "D7.C" = 1, "D7.D" = -1),
      check_col = "B6G.B",
      calc_col_name = "B6G_cal"
    ),
    BI21 = list(
      sector_filter = c("S13"),
      sto_filter = c("B5G", "D5", "D61", "D62", "D7", "B6G"),
      unite_cols = c("sto", "accounting_entry"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "time_period", "B5G.B", "D5.C", "D5.D", "D61.C", "D62.D", "D7.C", "D7.D", "B6G.B"),
      formula_terms = list("B5G.B" = 1, "D5.C" = 1, "D5.D" = -1, "D61.C" = 1, "D62.D" = -1, "D7.C" = 1, "D7.D" = -1),
      check_col = "B6G.B",
      calc_col_name = "B6G_cal"
    ),
    BI22 = list(
      sector_filter = c("S14", "S1M"),
      sto_filter = c("B5G", "D5", "D61", "D62", "D7", "B6G"),
      unite_cols = c("sto", "accounting_entry"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "time_period", "B5G.B", "D5.D", "D61.C", "D61.D", "D62.C", "D62.D", "D7.C", "D7.D", "B6G.B"),
      formula_terms = list("B5G.B" = 1, "D5.D" = -1, "D61.C" = 1, "D61.D" = -1, "D62.C" = 1, "D62.D" = -1, "D7.C" = 1, "D7.D" = -1),
      check_col = "B6G.B",
      calc_col_name = "B6G_cal"
    ),
    BI24 = list(
      sector_filter = c("S1", "S1M"),
      sto_filter = c("B7G", "B6G", "D63"),
      unite_cols = c("sto", "accounting_entry"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "time_period", "B6G.B", "D63.C", "D63.D", "B7G.B"),
      formula_terms = list("B6G.B" = 1, "D63.C" = 1, "D63.D" = -1),
      check_col = "B7G.B",
      calc_col_name = "B7G_cal"
    ),
    BI26 = list(
      sector_filter = c("S13", "S15"),
      sto_filter = c("B7G", "B6G", "D63"),
      entry_filter = c("C"),
      entry_filter_exclude = TRUE,
      unite_cols = c("sto", "accounting_entry"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "time_period", "B6G.B", "D63.D", "B7G.B"),
      formula_terms = list("B6G.B" = 1, "D63.D" = -1),
      check_col = "B7G.B",
      calc_col_name = "B7G_cal"
    ),
    BI27 = list(
      sector_filter = c("S14"),
      sto_filter = c("B7G", "B6G", "D63"),
      entry_filter = c("D"),
      entry_filter_exclude = TRUE,
      unite_cols = c("sto", "accounting_entry"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "time_period", "B6G.B", "D63.C", "B7G.B"),
      formula_terms = list("B6G.B" = 1, "D63.C" = 1),
      check_col = "B7G.B",
      calc_col_name = "B6G + D63.C"
    ),
    BI28 = list(
      sector_filter = c("S1", "S1M", "S14"),
      sto_filter = c("B8G", "B6G", "D8", "P3"),
      unite_cols = c("sto", "accounting_entry"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "time_period", "B6G.B", "D8.C", "D8.D", "P3.D", "B8G.B"),
      formula_terms = list("B6G.B" = 1, "D8.C" = 1, "D8.D" = -1, "P3.D" = -1),
      check_col = "B8G.B",
      calc_col_name = "B8G_cal"
    ),
    BI29 = list(
      sector_filter = c("S11", "S12"),
      sto_filter = c("B8G", "B6G", "D8"),
      unite_cols = c("sto", "accounting_entry"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "time_period", "B6G.B", "D8.D", "B8G.B"),
      formula_terms = list("B6G.B" = 1, "D8.D" = -1),
      check_col = "B8G.B",
      calc_col_name = "B6G.B - D8.D"
    ),
    BI30 = list(
      sector_filter = c("S13", "S15"),
      sto_filter = c("B8G", "B6G", "D8", "P3"),
      entry_filter = c("C"),
      entry_filter_exclude = TRUE,
      unite_cols = c("sto", "accounting_entry"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "time_period", "B6G.B", "D8.D", "P3.D", "B8G.B"),
      formula_terms = list("B6G.B" = 1, "D8.D" = -1, "P3.D" = -1),
      check_col = "B8G.B",
      calc_col_name = "B6G.B - D8.D - P3.D"
    ),
    BI32 = list(
      sector_filter = c("S1", "S2"),
      sector_filter_exclude = TRUE,
      sto_filter = c("B8G", "B101", "D9", "P51C"),
      unite_cols = c("sto", "accounting_entry"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "time_period", "B8G.B", "D9.C", "D9.D", "P51C.C", "B101.B"),
      formula_terms = list("B8G.B" = 1, "D9.C" = 1, "D9.D" = -1, "P51C.C" = -1),
      check_col = "B101.B",
      calc_col_name = "B101_calc"
    ),
    BI33 = list(
      sector_filter = c("S2"),
      sto_filter = c("B101", "D9", "B12"),
      unite_cols = c("sto", "accounting_entry"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "time_period", "B12.B", "D9.C", "D9.D", "B101.B"),
      formula_terms = list("B12.B" = 1, "D9.D" = 1, "D9.C" = -1),
      check_col = "B101.B",
      calc_col_name = "B101_calc"
    ),
    BI35 = list(
      sector_filter = c("S2"),
      sector_filter_exclude = TRUE,
      sto_filter = c("B9", "B8G", "D9", "P5", "NP"),
      unite_cols = c("sto", "accounting_entry"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "time_period", "B8G.B", "D9.C", "D9.D", "P5.D", "NP.D", "B9.B"),
      formula_terms = list("B8G.B" = 1, "D9.C" = 1, "D9.D" = -1, "P5.D" = -1, "NP.D" = -1),
      check_col = "B9.B",
      calc_col_name = "B9_calc"
    ),
    BI36 = list(
      sector_filter = c("S2"),
      sto_filter = c("B9", "B12", "D9", "NP"),
      unite_cols = c("sto", "accounting_entry"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "time_period", "B12.B", "D9.C", "D9.D", "NP.C", "B9.B"),
      formula_terms = list("B12.B" = 1, "D9.D" = 1, "D9.C" = -1, "NP.C" = -1),
      check_col = "B9.B",
      calc_col_name = "B9_calc"
    ),
    BI37 = list(
      sector_filter = c("S13"),
      sto_filter = c("B9", "OTR", "OTE"),
      unite_cols = c("sto", "accounting_entry"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "time_period", "OTR.C", "OTE.D", "B9.B"),
      formula_terms = list("OTR.C" = 1, "OTE.D" = -1),
      check_col = "B9.B",
      calc_col_name = "OTR.C - OTE.D"
    ),
    BI38 = list(
      sector_filter = c("S2"),
      sector_filter_exclude = TRUE,
      sto_filter = c("B9", "B101", "P51C", "P5", "NP"),
      unite_cols = c("sto", "accounting_entry"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "time_period", "B101.B", "P51C.C", "P5.D", "NP.D", "B9.B"),
      formula_terms = list("B101.B" = 1, "P51C.C" = 1, "P5.D" = -1, "NP.D" = -1),
      check_col = "B9.B",
      calc_col_name = "B9_calc"
    ),
    BI39 = list(
      sector_filter = c("S2"),
      sto_filter = c("B9", "B101", "NP"),
      unite_cols = c("sto", "accounting_entry"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "ref_sector", "time_period", "B101.B", "NP.C", "B9.B"),
      formula_terms = list("B101.B" = 1, "NP.C" = -1),
      check_col = "B9.B",
      calc_col_name = "B101.B - NP.C"
    ),
    BI41 = list(
      sector_filter = c("S1", "S2"),
      sto_filter = c("B101", "P5", "P51C"),
      unite_cols = c("ref_sector", "sto", "accounting_entry"),
      pivot_names = c("sto"),
      required_cols = c("S2.B101.B", "S1.P5.D", "S1.P51C.C", "S1.B101.B"),
      select_cols = c("ref_area", "time_period", "S2.B101.B", "S1.P5.D", "S1.P51C.C", "S1.B101.B"),
      formula_terms = list("S2.B101.B" = -1, "S1.P5.D" = 1, "S1.P51C.C" = -1),
      check_col = "S1.B101.B",
      calc_col_name = "S1.B101.B_cal"
    ),
    BI42 = list(
      sector_filter = c("S2"),
      sto_filter = c("B11", "P6", "P7"),
      unite_cols = c("sto", "accounting_entry"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "time_period", "P7.C", "P6.D", "B11.B"),
      formula_terms = list("P7.C" = 1, "P6.D" = -1),
      check_col = "B11.B",
      calc_col_name = "P7.C - P6.D"
    ),
    BI43 = list(
      sector_filter = c("S2"),
      sto_filter = c("B12", "B11", "D1", "D2", "D3", "D4", "D5", "D61", "D62", "D7", "D8"),
      unite_cols = c("sto", "accounting_entry"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "time_period", "B11.B", "D1.D", "D1.C", "D2.D", "D3.C", "D4.D", "D4.C", "D5.D", "D5.C", "D61.D", "D61.C", "D62.D", "D62.C", "D7.D", "D7.C", "D8.D", "D8.C", "B12.B"),
      formula_terms = list("B11.B" = 1, "D1.D" = 1, "D1.C" = -1, "D2.D" = 1, "D3.C" = -1, "D4.D" = 1, "D4.C" = -1, "D5.D" = 1, "D5.C" = -1, "D61.D" = 1, "D61.C" = -1, "D62.D" = 1, "D62.C" = -1, "D7.D" = 1, "D7.C" = -1, "D8.D" = 1, "D8.C" = -1),
      check_col = "B12.B",
      calc_col_name = "B12.B_calc"
    ),
    BI44 = list(
      sector_filter = c("S13"),
      sto_filter = c("OTR", "P1O", "D2", "D39", "D4", "D5", "D61", "D7", "D9"),
      unite_cols = c("sto", "accounting_entry"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "time_period", "P1O.C", "D2.C", "D39.C", "D4.C", "D5.C", "D61.C", "D7.C", "D9.C", "OTR.C"),
      formula_terms = list("P1O.C" = 1, "D2.C" = 1, "D39.C" = 1, "D4.C" = 1, "D5.C" = 1, "D61.C" = 1, "D7.C" = 1, "D9.C" = 1),
      check_col = "OTR.C",
      calc_col_name = "OTR.C_calc"
    ),
    BI45 = list(
      sector_filter = c("S13"),
      sto_filter = c("OTE", "P2", "P5", "D1", "D29", "D3", "D4", "D5", "D62", "D632", "D7", "D8", "D9", "NP"),
      unite_cols = c("sto", "accounting_entry"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "time_period", "P2.D", "P5.D", "D1.D", "D29.D", "D3.D", "D4.D", "D5.D", "D62.D", "D632.D", "D7.D", "D8.D", "D9.D", "NP.D", "OTE.D"),
      formula_terms = list("P2.D" = 1, "P5.D" = 1, "D1.D" = 1, "D29.D" = 1, "D3.D" = 1, "D4.D" = 1, "D5.D" = 1, "D62.D" = 1, "D632.D" = 1, "D7.D" = 1, "D8.D" = 1, "D9.D" = 1, "NP.D" = 1),
      check_col = "OTE.D",
      calc_col_name = "OTE.D_calc"
    ),
    BI46 = list(
      sector_filter = c("S13"),
      sto_filter = c("P31", "D63"),
      unite_cols = c("sto", "accounting_entry"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "time_period", "P31.D", "D63.D"),
      diff_cols = c("P31.D", "D63.D")
    ),
    BI47 = list(
      sector_filter = c("S13"),
      sto_filter = c("P3", "P1", "P1O", "D632"),
      unite_cols = c("sto", "accounting_entry"),
      pivot_names = c("sto"),
      select_cols = c("ref_area", "time_period", "P1.C", "P1O.C", "D632.D", "P3.D"),
      formula_terms = list("P1.C" = 1, "P1O.C" = -1, "D632.D" = 1),
      check_col = "P3.D",
      calc_col_name = "P3.D_calc"
    )
  )
}

# Special check for sit43 (conditional on D43_I9 existing)
run_sit43_check <- function(data, threshold, rounding) {
  tryCatch({
    check_data <- data |> dplyr::filter(sto == "D43_I9")
    if (nrow(check_data) == 0) return(NULL)

    result <- data |>
      dplyr::filter(sto %in% c("D43", "D43_I9", "D43_J9")) |>
      tidyr::pivot_wider(names_from = sto, values_from = obs_value) |>
      dplyr::select(ref_area, ref_sector, accounting_entry, time_period, D43, D43_I9, D43_J9) |>
      dplyr::mutate(
        `D43_I9 + D43_J9` = rowSums(dplyr::pick(D43_I9, D43_J9), na.rm = TRUE),
        check = round(D43 - `D43_I9 + D43_J9`, rounding)
      ) |>
      dplyr::filter(abs(check) > threshold) |>
      dplyr::filter(dplyr::if_all(c(D43_I9, D43_J9), ~ !is.na(.x)))

    if (nrow(result) == 0) return(NULL)
    result
  }, error = function(e) NULL)
}

# =============================================================================
# MAIN FUNCTION
# =============================================================================

nfsa_internal_consistency_T0800 <- function(dataset,
                                            output_sel = here::here("output", "internal"),
                                            threshold = 0,
                                            rounding = 2) {

  # Prepare data
  data <- dataset |>
    dplyr::mutate(obs_value = janitor::round_half_up(obs_value, rounding)) |>
    nfsa::nfsa_separate_id()

  # Get all check configurations
  ur_checks <- get_ur_checks()
  s1ss_checks <- get_s1ss_checks()
  sit_checks <- get_sit_checks()
  bi_checks <- get_bi_checks()

  # Run all checks
  run_checks_batch <- function(checks) {
    results <- purrr::map(checks, ~run_check(data, .x, threshold, rounding))
    names(results) <- names(checks)
    purrr::keep(results, ~!is.null(.x) && nrow(.x) > 0)
  }

  list_ur <- run_checks_batch(ur_checks)
  list_s1ss <- run_checks_batch(s1ss_checks)
  list_sit <- run_checks_batch(sit_checks)
  list_BI <- run_checks_batch(bi_checks)

  # Run special sit43 check
  sit43_result <- run_sit43_check(data, threshold, rounding)
  if (!is.null(sit43_result) && nrow(sit43_result) > 0) {
    list_sit$sit43 <- sit43_result
  }

  # Combine all results
  list_ir <- c(list_ur, list_s1ss, list_sit, list_BI)

  # Replace NA with "NaN" for Excel compatibility
  list_ir <- purrr::map(list_ir, ~dplyr::mutate(.x, dplyr::across(dplyr::everything(), ~replace(.x, is.na(.x), "NaN"))))

  # Output results
  if (length(list_ir) == 0) {
    cli::cli_inform("All consistent!")
  } else {
    output_file <- paste0(output_sel, "/T0800_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    openxlsx::write.xlsx(
      list_ir,
      file = output_file,
      asTable = TRUE,
      overwrite = TRUE
    )
  }
  cli::cli_alert_success(paste0("File created at: ", output_sel, "/", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"))
}
