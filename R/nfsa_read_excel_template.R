
#' Read NFSA T0800 or T0801 Excel Template
#'
#' Reads selected variables from the sector worksheets of an NFSA
#' Excel template and combines them into a single data frame.
#' Supports annual (T0800) and quarterly (T0801) templates.
#'
#' The function applies table-specific Excel ranges, variable
#' selections and time-period formatting.
#'
#' @param file Character string. Path to the Excel template.
#' @param table Character string. Table identifier, either
#'   "T0801" (default) or "T0800".
#'
#' @return A tibble containing:
#' \describe{
#'   \item{id}{NFSA identifier created by `nfsa_unite_id()`.}
#'   \item{time_period}{Reference period. Character in YYYY-QQ
#'     format for T0801, integer year for T0800.}
#'   \item{obs_value}{Numeric observation value.}
#' }
#'
#' @importFrom readxl read_xlsx
#' @importFrom dplyr select mutate across all_of
#' @importFrom tidyr pivot_longer separate_wider_delim
#' @importFrom purrr imap_dfr
#' @importFrom stringr str_sub
#'
#' @examples
#' \dontrun{
#' nfsa_read_excel_template(
#'   file = "path/to/EE1_2025_F.xlsx",
#'   table = "T0800"
#' )
#'
#' nfsa_read_excel_template(
#'   file = "path/to/EE1_2026_Q1.xlsx",
#'   table = "T0801"
#' )
#' }
#'
#' @export
nfsa_read_excel_template <- function(
    file,
    table = "T0801"
) {

  table <- table

  if (!file.exists(file)) {
    stop("File does not exist: ", file)
  }

  # Convert space-separated variable names to vectors
  vars <- function(x) {
    strsplit(trimws(x), "\\s+")[[1]]
  }

  # ============================================================
  # CONFIGURATION: T0801 (QUARTERLY)
  # ============================================================

  config_T0801 <- list(

    S1 = list(
      range = "A30:JL500",
      columns = vars("
        P2.D P3.D P31.D P32.D P5.D P51G.D P5M.D
        D1.D D2.D D21.D D29.D D3.D D31.D D39.D
        D4.D D41.D D4N.D D42.D D43.D D44.D D45.D D41G.D
        D5.D D6.D D61.D D62.D D63.D D631.D D632.D
        D7.D D71.D D72.D D7N.D D74.D D74_4Y.D
        D75.D D76.D D8.D D9.D D91.D D9N.D D92.D D99.D
        P51C.D NP.D
        P1.C D1.C D2.C D21.C D29.C D3.C D31.C D39.C
        D21X31.C D4.C D41.C D4N.C D42.C D43.C D44.C
        D45.C D41G.C D5.C D6.C D61.C D62.C D63.C
        D7.C D71.C D72.C D7N.C D74.C D75.C
        D8.C D9.C D91.C D9N.C D92.C D99.C P51C.C
        B1GQ.B B1NQ.B B2A3G.B B3G.B B4G.B B5G.B
        B6G.B B8G.B B101.B B9.B B9X9F._Z
      ")
    ),

    S1N = list(
      range = "A30:Q500",
      columns = vars("
        D2.D D21.D D3.C D31.C D21X31.C B1G.B
      ")
    ),

    S11 = list(
      range = "A30:GX500",
      columns = vars("
        P2.D P5.D P51G.D P5M.D
        D1.D D2.D D29.D D4.D D41.D D4N.D D42.D
        D43.D D43_I10.D D43_J10.D D43_B6.D D43_D6.D
        D44.D D45.D D41G.D D5.D D6.D D62.D
        D7.D D71.D D7N.D D75.D D8.D D9.D D91.D
        D9N.D D99.D P51C.D NP.D
        P1.C D3.C D39.C D4.C D41.C D4N.C D42.C
        D43.C D43_I10.C D43_J10.C D43_B6.C D43_D6.C
        D44.C D45.C D41G.C D6.C D61.C
        D7.C D72.C D7N.C D75.C D9.C D92.C D99.C P51C.C
        B1G.B B1N.B B2A3G.B B4G.B B5G.B B6G.B
        B8G.B B101.B B9.B B9X9F._Z
      ")
    ),

    S12 = list(
      range = "A30:HD500",
      columns = vars("
        P2.D P5.D P51G.D P5M.D
        D1.D D2.D D29.D D4.D D41.D D4N.D D42.D
        D43.D D43_I10.D D43_J10.D D43_B6.D D43_D6.D
        D44.D D45.D D41G.D D5.D D6.D D62.D
        D7.D D71.D D72.D D7N.D D75.D D8.D D9.D D91.D
        D9N.D D99.D P51C.D NP.D
        P1.C D3.C D39.C D4.C D41.C D4N.C D42.C
        D43.C D43_I10.C D43_J10.C D43_B6.C D43_D6.C
        D44.C D45.C D41G.C D6.C D61.C
        D7.C D72.C D7N.C D75.C D9.C D92.C D99.C P51C.C
        B1G.B B1N.B B2A3G.B B4G.B B5G.B B6G.B
        B8G.B B101.B B9.B B9X9F._Z
      ")
    ),

    S13 = list(
      range = "A30:IT500",
      columns = vars("
        P2.D P3.D P31.D P32.D P5.D P51G.D P5M.D
        D1.D D2.D D29.D D3.D D31.D D39.D D4.D
        D41.D D4N.D D42.D D44.D D45.D D41G.D
        D5.D D6.D D62.D D63.D D631.D D632.D
        D7.D D71.D D72.D D7N.D D74.D D74_4Y.D
        D75.D D76.D D8.D D9.D D9N.D D92.D D99.D
        P51C.D NP.D OTE.D
        P1.C P1O.C D2.C D21.C D211.C D29.C D3.C
        D39.C D4.C D41.C D4N.C D42.C D43.C
        D44.C D45.C D41G.C D5.C D6.C D61.C
        D7.C D71.C D72.C D7N.C D74.C D75.C
        D9.C D91.C D9N.C D92.C D99.C P51C.C OTR.C
        B1G.B B1N.B B2A3G.B B4G.B B5G.B B6G.B
        B7G.B B8G.B B101.B B9.B B9X9F._Z
      ")
    ),

    S1M = list(
      range = "A30:HM500",
      columns = vars("
        P2.D P3.D P31.D P5.D P51G.D P5M.D
        D1.D D2.D D29.D D4.D D41.D D4N.D D45.D
        D41G.D D5.D D6.D D61.D D62.D D63.D
        D631.D D632.D D7.D D71.D D7N.D
        D75.D D8.D D9.D D91.D D9N.D D99.D P51C.D NP.D
        P1.C D1.C D3.C D39.C D4.C D41.C D4N.C
        D42.C D43.C D44.C D45.C D41G.C
        D6.C D61.C D62.C D63.C D631.C D632.C
        D7.C D72.C D7N.C D75.C D8.C D9.C D9N.C
        D92.C D99.C P51C.C
        B1G.B B1N.B B2A3G.B B3G.B B4G.B B5G.B
        B6G.B B7G.B B8G.B B101.B B9.B B9X9F._Z
        LE_N111G.D LE_N211G.D
      ")
    ),

    S2 = list(
      range = "A30:HD500",
      columns = vars("
        P6.D P61.D P62.D P62F.D
        D1.C D3.C D31.C D39.C D4.C D41.C D4N.C
        D42.C D43.C D44.C D45.C D41G.C D5.C D6.C
        D61.C D62.C D7.C D71.C D72.C D7N.C D74.C
        D75.C D8.C D9.C D91.C D9N.C D92.C D99.C NP.C
        P7.C P71.C P72.C P72F.C
        D1.D D2.D D21.D D29.D D4.D D41.D D4N.D
        D42.D D43.D D44.D D45.D D41G.D D5.D D6.D
        D61.D D62.D D7.D D71.D D72.D D7N.D
        D74.D D75.D D76.D D8.D D9.D D91.D D9N.D
        D92.D D99.D
        B101.B B9.B B11.B B12.B B9X9F._Z
      ")
    )
  )

  # ============================================================
  # CONFIGURATION: T0800 (ANNUAL)
  # ============================================================

  config_T0800 <- list(

    S1 = list(
      range = "A30:NU100",
      columns = vars("
        P2.D P3.D P31.D P32.D P5.D P51G.D
        P51G_N111G.D P51G_N112G.D P51G_N1121G.D
        P51G_N1122G.D P52.D P53.D
        D1.D D11.D D12.D D2.D D21.D D29.D
        D3.D D31.D D39.D D4.D D41.D
        D42.D D421.D D422.D D43.D
        D44.D D441.D D442.D D443.D D45.D D41G.D
        D5.D D51.D D59.D D6.D D61.D
        D611.D D612.D D613.D D614.D D61SC.D
        D62.D D63.D D631.D D632.D
        D7.D D71.D D72.D D74.D D74_4Y.D
        D75.D D76.D D8.D D9.D D91.D D92.D D99.D
        P51C.D NP.D
        P1.C P11.C P12.C P13.C
        D1.C D11.C D12.C D2.C D21.C D211.C D212.C D214.C
        D29.C D3.C D31.C D39.C D21X31.C
        D4.C D41.C D42.C D421.C D422.C
        D43.C D44.C D441.C D442.C D443.C D45.C D41G.C
        D5.C D51.C D59.C D6.C D61.C
        D611.C D612.C D613.C D614.C D61SC.C
        D62.C D63.C D631.C D632.C
        D7.C D71.C D72.C D74.C D75.C
        D8.C D9.C D91.C D92.C D99.C P51C.C
        B1GQ.B B1NQ.B B2A3G.B B2G.B B3G.B B4G.B
        B5G.B B6G.B B7G.B B8G.B B101.B
        B9.B B9X9F._Z
      ")
    ),

    S1N = list(
      range = "A30:V100",
      columns = vars("
        D2.D D21.D D3.C D31.C D21X31.C B1G.B B1N.B
      ")
    ),

    S11 = list(
      range = "A30:JH100",
      columns = vars("
        P2.D P5.D P51G.D
        P51G_N111G.D P51G_N112G.D
        P51G_N1121G.D P51G_N1122G.D P52.D P53.D
        D1.D D11.D D12.D D2.D D29.D D4.D D41.D
        D42.D D421.D D422.D D43.D
        D44.D D441.D D442.D D443.D D45.D D41G.D
        D5.D D51.D D59.D D6.D D62.D
        D7.D D71.D D75.D D8.D D9.D D91.D D99.D
        P51C.D NP.D
        P1.C P11.C P12.C D3.C D39.C D4.C D41.C
        D42.C D421.C D422.C D43.C
        D44.C D441.C D442.C D443.C D45.C D41G.C
        D6.C D61.C D611.C D612.C D613.C D614.C
        D61SC.C D7.C D72.C D75.C
        D9.C D92.C D99.C P51C.C
        B1G.B B1N.B B2A3G.B B4G.B B5G.B B6G.B
        B8G.B B101.B B9.B B9X9F._Z
      ")
    ),

    S12 = list(
      range = "A30:JN100",
      columns = vars("
        P2.D P5.D P51G.D
        P51G_N111G.D P51G_N112G.D
        P51G_N1121G.D P51G_N1122G.D P52.D P53.D
        D1.D D11.D D12.D D2.D D29.D D4.D D41.D
        D42.D D421.D D422.D D43.D
        D44.D D441.D D442.D D443.D D45.D D41G.D
        D5.D D51.D D59.D D6.D D62.D
        D7.D D71.D D72.D D75.D D8.D D9.D D91.D D99.D
        P51C.D NP.D
        P1.C P11.C P12.C D3.C D39.C D4.C D41.C
        D42.C D421.C D422.C D43.C
        D44.C D441.C D442.C D443.C D45.C D41G.C
        D6.C D61.C D611.C D612.C D613.C D614.C
        D61SC.C D7.C D71.C D72.C D75.C
        D9.C D92.C D99.C P51C.C
        B1G.B B1N.B B2A3G.B B4G.B B5G.B B6G.B
        B8G.B B101.B B9.B B9X9F._Z
      ")
    ),

    S13 = list(
      range = "A30:LP100",
      columns = vars("
        P2.D P3.D P31.D P32.D P5.D P51G.D
        P51G_N111G.D P51G_N112G.D
        P51G_N1121G.D P51G_N1122G.D P52.D P53.D
        D1.D D11.D D12.D D2.D D29.D
        D3.D D31.D D39.D D4.D D41.D
        D42.D D421.D D44.D
        D441.D D442.D D443.D D45.D D41G.D
        D5.D D51.D D59.D D6.D D62.D
        D63.D D631.D D632.D
        D7.D D71.D D72.D D74.D D74_4Y.D
        D75.D D76.D D8.D D9.D D92.D D99.D
        P51C.D NP.D OTE.D
        P1.C P1O.C P11.C P12.C P13.C
        D2.C D21.C D211.C D212.C D214.C
        D29.C D3.C D39.C D4.C D41.C
        D42.C D421.C D422.C D43.C
        D44.C D441.C D442.C D443.C D45.C D41G.C
        D5.C D51.C D59.C D6.C D61.C
        D611.C D612.C D613.C D614.C D61SC.C
        D7.C D71.C D72.C D74.C D75.C
        D9.C D91.C D92.C D99.C P51C.C OTR.C
        B1G.B B1N.B B2A3G.B B4G.B B5G.B B6G.B
        B7G.B B8G.B B101.B B9.B B9X9F._Z
      ")
    ),

    S1M = list(
      range = "A30:KR100",
      columns = vars("
        P2.D P3.D P31.D P5.D P51G.D
        P51G_N111G.D P51G_N112G.D
        P51G_N1121G.D P51G_N1122G.D P52.D P53.D
        D1.D D11.D D12.D D2.D D29.D D4.D D41.D
        D43.D D44.D D441.D D442.D D443.D D45.D D41G.D
        D5.D D51.D D59.D D6.D D61.D
        D611.D D612.D D613.D D614.D D61SC.D
        D62.D D63.D D631.D D632.D
        D7.D D71.D D75.D D8.D D9.D D91.D D99.D
        P51C.D NP.D
        P1.C P11.C P12.C P13.C D1.C D11.C D12.C
        D3.C D39.C D4.C D41.C
        D42.C D421.C D422.C D43.C
        D44.C D441.C D442.C D443.C D45.C D41G.C
        D6.C D61.C D611.C D612.C D613.C D614.C
        D61SC.C D62.C D63.C D631.C D632.C
        D7.C D72.C D75.C D8.C D9.C D92.C D99.C P51C.C
        B1G.B B1N.B B2A3G.B B2G.B B3G.B B4G.B
        B5G.B B6G.B B7G.B B8G.B B101.B
        B9.B B9X9F._Z
      ")
    ),

    S2 = list(
      range = "A30:KC100",
      columns = vars("
        P6.D P61.D P62.D P62F.D
        D1.C D11.C D12.C D3.C D31.C D39.C D4.C D41.C
        D42.C D421.C D422.C D43.C
        D44.C D441.C D442.C D443.C D45.C D41G.C
        D5.C D51.C D59.C D6.C D61.C
        D611.C D612.C D613.C D614.C D61SC.C
        D62.C D7.C D71.C D72.C D74.C
        D75.C D8.C D9.C D91.C D92.C D99.C NP.C
        P7.C P71.C P72.C P72F.C
        D1.D D11.D D12.D D2.D D21.D
        D211.D D212.D D214.D D29.D D4.D
        D41.D D42.D D421.D D422.D D43.D
        D44.D D441.D D442.D D443.D D45.D D41G.D
        D5.D D51.D D59.D D6.D D61.D
        D611.D D612.D D613.D D614.D D61SC.D D62.D
        D7.D D71.D D72.D D74.D D75.D D76.D
        D8.D D9.D D91.D D92.D D99.D
        B11.B B12.B B101.B B9.B B9X9F._Z
      ")
    )
  )

  # ============================================================
  # SELECT CONFIGURATION
  # ============================================================

  config <- switch(
    table,
    T0800 = config_T0800,
    T0801 = config_T0801
  )

  # ============================================================
  # COMMON PROCESSING
  # ============================================================

  read_sector <- function(sector, settings) {

    # Read and select required variables
    data <- readxl::read_xlsx(
      path = file,
      sheet = sector,
      range = settings$range
    ) |>
      dplyr::select(
        time_period = `TIME ▼`,
        dplyr::all_of(settings$columns)
      ) |>
      dplyr::mutate(
        dplyr::across(
          -time_period,
          as.numeric
        )
      )

    # Format time period according to table
    if (table == "T0800") {

      data <- data |>
        dplyr::mutate(
          time_period = as.integer(time_period)
        )

    } else {

      data <- data |>
        dplyr::mutate(
          time_period = paste0(
            stringr::str_sub(time_period, 1, 4),
            "-",
            stringr::str_sub(time_period, 5, 6)
          )
        )
    }

    # Transform into long format
    data |>
      tidyr::pivot_longer(
        cols = -time_period,
        names_to = "sto",
        values_to = "obs_value",
        values_drop_na = TRUE
      ) |>
      dplyr::mutate(
        ref_sector = sector
      ) |>
      tidyr::separate_wider_delim(
        sto,
        delim = ".",
        names = c("sto", "accounting_entry")
      ) |>
      dplyr::select(
        time_period,
        ref_sector,
        sto,
        accounting_entry,
        obs_value
      ) |>
      stats::na.omit()
  }

  # ============================================================
  # READ ALL SECTORS AND RETURN DATA
  # ============================================================

  data <- purrr::imap_dfr(
    config,
    ~ read_sector(
      sector = .y,
      settings = .x
    )
  ) |>
    nfsa_unite_id() |>
    dplyr::select(
      id,
      time_period,
      obs_value
    )

  return(data)
}
