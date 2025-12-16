#' Read NFSA T0801 Excel template
#'
#' Imports and reshapes all sector sheets from an NFSA T0801 Excel template
#' into a single tidy data frame with one row per time-period/sector/item.
#'
#' The function expects the workbook structure of the NFSA T0801 template:
#' sheets `S1`, `S1N`, `S11`, `S12`, `S13`, `S1M`, and `S2` with data starting
#' at row 30 and specific column ranges for each sheet.
#'
#' @param file Path to the NFSA T0801 Excel workbook (character string),
#'   typically ending in `.xlsx`.
#'
#' @return A tibble with the following columns:
#' \describe{
#'   \item{time_period}{Period in `YYYY-MM` format derived from the
#'   `TIME ▼` column.}
#'   \item{ref_sector}{Reference sector code (e.g. `"S1"`, `"S1N"`, `"S11"`,
#'   `"S12"`, `"S13"`, `"S1M"`, `"S2"`).}
#'   \item{sto}{Short transaction/series code parsed from the column names
#'   before the `.` separator.}
#'   \item{accounting_entry}{Accounting entry suffix parsed from the column
#'   names after the `.` separator (e.g. `"D"`, `"C"`, `"_Z"`).}
#'   \item{obs_value}{Numeric observation value for the corresponding
#'   time-period, sector, and item.}
#' }
#'
#' @details
#' For each sector sheet, the function:
#' \itemize{
#'   \item Reads the specified cell range with \code{readxl::read_xlsx}.
#'   \item Renames the `TIME ▼` column to \code{time_period} and coerces all
#'   other selected columns to numeric.
#'   \item Pivots the data to long format with \code{tidyr::pivot_longer},
#'   parsing column names into \code{sto} and \code{accounting_entry} using
#'   \code{tidyr::separate_wider_delim(".")}.
#'   \item Converts the period codes from `YYYYMM` to `YYYY-MM` using
#'   \code{stringr::str_sub}.
#'   \item Adds a sector identifier \code{ref_sector} corresponding to the
#'   sheet name and drops rows with missing values.
#' }
#' All processed sector tables are combined with \code{dplyr::bind_rows}.
#'
#' @seealso
#'   \code{\link[readxl]{read_xlsx}},
#'   \code{\link[tidyr]{pivot_longer}},
#'   \code{\link[tidyr]{separate_wider_delim}},
#'   \code{\link[dplyr]{bind_rows}}.
#'
#' @examples
#' \dontrun{
#' # Path to an NFSA T0801 Excel template
#' t0801_path <- "data-raw/T0801_template.xlsx"
#'
#' # Read and tidy all sector sheets
#' t0801_data <- nfsa_read_excel_template_T0801(t0801_path)
#'
#' # Inspect the first rows
#' dplyr::glimpse(t0801_data)
#' }
#'
nfsa_read_excel_template_T0801 <- function (file){
library(tidyverse)
library(readxl)

S1 <- read_xlsx(file,
                sheet = "S1",
                range = "A30:JL500") |>
  select(time_period = `TIME ▼`, P2.D,P3.D,P31.D,P32.D,P5.D,P51G.D,
         P5M.D,D1.D,D2.D,D21.D,D29.D,D3.D,D31.D,D39.D,D4.D,D41.D,
         D4N.D,D42.D,D43.D,D44.D,D45.D,D41G.D,D5.D,D6.D,D61.D,D62.D,
         D63.D,D631.D,D632.D,D7.D,D71.D,D72.D,D7N.D,D74.D,D74_4Y.D,
         D75.D,D76.D,D8.D,D9.D,D91.D,D9N.D,D92.D,D99.D,P51C.D,NP.D,
         P1.C,D1.C,D2.C,D21.C,D29.C,D3.C,D31.C,D39.C,D21X31.C,D4.C,
         D41.C,D4N.C, D42.C,D43.C,D44.C,D45.C,D41G.C,D5.C,D6.C,D61.C,
         D62.C,D63.D,D7.C,D71.C,D72.C,D7N.C,D74.C,
         D75.C,D8.C,D9.C,D91.C,D9N.C,D92.C,D99.C,P51C.C,B1GQ.B,
         B1NQ.B, B2A3G.B, B3G.B, B4G.B, B5G.B, B6G.B, B8G.B, B101.B,
         B9.B, B9X9F._Z) |>
  mutate(across(-time_period,as.numeric)) |>
  pivot_longer(-time_period,
               names_to = "sto",
               values_to = "obs_value") |>
  mutate(time_period = paste0(str_sub(time_period,1,4),
                              "-",
                              str_sub(time_period,5,6))) |>
  mutate(ref_sector = "S1") |>
  separate_wider_delim(sto,
                       delim = ".",
                       names= c("sto","accounting_entry")) |>
  select(time_period,ref_sector,sto,accounting_entry,obs_value) |>
  na.omit()

S1N <- read_xlsx(file,
                 sheet = "S1N",
                 range = "A30:Q500") |>
  select(time_period = `TIME ▼`, D2.D, D21.D,D3.C,D31.C,D21X31.C,B1G.B) |>
  mutate(across(-time_period,as.numeric)) |>
  pivot_longer(-time_period,
               names_to = "sto",
               values_to = "obs_value") |>
  mutate(time_period = paste0(str_sub(time_period,1,4),
                              "-",
                              str_sub(time_period,5,6))) |>
  mutate(ref_sector = "S1N") |>
  separate_wider_delim(sto,
                       delim = ".",
                       names= c("sto","accounting_entry")) |>
  select(time_period,ref_sector,sto,accounting_entry,obs_value)|>
  na.omit()

S11 <- read_xlsx(file,
                 sheet = "S11",
                 range = "A30:GX500") |>
  select(time_period = `TIME ▼`, P2.D,P5.D,P51G.D,
         P5M.D,D1.D,D2.D,D29.D,D4.D,D41.D,
         D4N.D,D42.D,D43.D,D43_I9.D,D43_J9.D, D43_B6.D,D43_D6.D,
         D44.D,D45.D,D41G.D,D5.D,D6.D,D62.D,
         D7.D,D71.D,D7N.D,
         D75.D,D8.D,D9.D,D91.D,D9N.D,D99.D,P51C.D,NP.D,
         P1.C,D3.C,D39.C,D4.C,
         D41.C,D4N.C, D42.C,D43.C,D43_I9.C,D43_J9.C, D43_B6.C,D43_D6.C,
         D44.C,D45.C,D41G.C,D6.C,D61.C,
         D7.C,D72.C,D7N.C,
         D75.C,D9.C,D92.C,D99.C,P51C.C,B1G.B,
         B1N.B, B2A3G.B, B4G.B, B5G.B, B6G.B, B8G.B, B101.B,
         B9.B, B9X9F._Z) |>
  mutate(across(-time_period,as.numeric)) |>
  pivot_longer(-time_period,
               names_to = "sto",
               values_to = "obs_value") |>
  mutate(time_period = paste0(str_sub(time_period,1,4),
                              "-",
                              str_sub(time_period,5,6))) |>
  mutate(ref_sector = "S11") |>
  separate_wider_delim(sto,
                       delim = ".",
                       names= c("sto","accounting_entry")) |>
  select(time_period,ref_sector,sto,accounting_entry,obs_value)|>
  na.omit()

S12 <- read_xlsx(file,
                 sheet = "S12",
                 range = "A30:HD500") |>
  select(time_period = `TIME ▼`, P2.D,P5.D,P51G.D,
         P5M.D,D1.D,D2.D,D29.D,D4.D,D41.D,
         D4N.D,D42.D,D43.D,D43_I9.D,D43_J9.D, D43_B6.D,D43_D6.D,
         D44.D,D45.D,D41G.D,D5.D,D6.D,D62.D,
         D7.D,D71.D,D72.D,D7N.D,
         D75.D,D8.D,D9.D,D91.D,D9N.D,D99.D,P51C.D,NP.D,
         P1.C,D3.C,D39.C,D4.C,
         D41.C,D4N.C, D42.C,D43.C,D43_I9.C,D43_J9.C, D43_B6.C,D43_D6.C,
         D44.C,D45.C,D41G.C,D6.C,D61.C,
         D7.C,D72.C,D7N.C,
         D75.C,D9.C,D92.C,D99.C,P51C.C,B1G.B,
         B1N.B, B2A3G.B, B4G.B, B5G.B, B6G.B, B8G.B, B101.B,
         B9.B, B9X9F._Z) |>
  mutate(across(-time_period,as.numeric)) |>
  pivot_longer(-time_period,
               names_to = "sto",
               values_to = "obs_value") |>
  mutate(time_period = paste0(str_sub(time_period,1,4),
                              "-",
                              str_sub(time_period,5,6))) |>
  mutate(ref_sector = "S12") |>
  separate_wider_delim(sto,
                       delim = ".",
                       names= c("sto","accounting_entry")) |>
  select(time_period,ref_sector,sto,accounting_entry,obs_value)|>
  na.omit()


S13 <- read_xlsx(file,
                 sheet = "S13",
                 range = "A30:IT500") |>
  select(time_period = `TIME ▼`, P2.D,P3.D, P31.D,P32.D,P5.D,P51G.D, P5M.D,
         D1.D,D2.D,D29.D,D3.D,D31.D,D39.D, D4.D,D41.D, D4N.D,D42.D,D44.D,
         D45.D,D41G.D,D5.D,D6.D,D62.D,D63.D,D631.D,D632.D,D7.D,D71.D,D72.D,D7N.D,
         D74.D,D74_4Y.D,D75.D,D76.D,D8.D,D9.D,D9N.D,D92.D,D99.D,P51C.D,NP.D,OTE.D,
         P1.C,P1O.C,D2.C,D21.C,D211.C,D29.C,D3.C,D39.C,D4.C,
         D41.C,D4N.C, D42.C,D43.C,
         D44.C,D45.C,D41G.C,D5.C,D6.C,D61.C,
         D7.C,D71.C,D72.C,D7N.C,
         D75.C,D9.C,D91.C,D9N.C,D92.C,D99.C,P51C.C,OTR.C,B1G.B,
         B1N.B, B2A3G.B, B4G.B, B5G.B, B6G.B, B7G.B,B8G.B, B101.B,
         B9.B, B9X9F._Z) |>
  mutate(across(-time_period,as.numeric)) |>
  pivot_longer(-time_period,
               names_to = "sto",
               values_to = "obs_value") |>
  mutate(time_period = paste0(str_sub(time_period,1,4),
                              "-",
                              str_sub(time_period,5,6))) |>
  mutate(ref_sector = "S13") |>
  separate_wider_delim(sto,
                       delim = ".",
                       names= c("sto","accounting_entry")) |>
  select(time_period,ref_sector,sto,accounting_entry,obs_value)|>
  na.omit()

S1M <- read_xlsx(file,
                 sheet = "S1M",
                 range = "A30:HM500") |>
  select(time_period = `TIME ▼`, P2.D,P3.D, P31.D,P5.D,P51G.D, P5M.D,
         D1.D,D2.D,D29.D,D4.D,D41.D, D4N.D,D45.D,D41G.D,D5.D,D6.D,
         D61.D,D62.D,D63.D,D631.D,D632.D,D7.D,D71.D,D7N.D,
         D75.D,D8.D,D9.D,D91.D,D9N.D,D99.D,P51C.D,NP.D,
         P1.C,D1.C,D3.C,D39.C,D4.C,
         D41.C,D4N.C, D42.C,D43.C,
         D44.C,D45.C,D41G.C,D6.C,D61.C,D62.C,D63.C,D631.C,D632.C,
         D7.C,D72.C,D7N.C, D75.C,D8.C,D9.C,D9N.C,D92.C,D99.C,P51C.C,B1G.B,
         B1N.B, B2A3G.B, B3G.B, B4G.B, B5G.B, B6G.B, B7G.B,B8G.B, B101.B,
         B9.B, B9X9F._Z, LE_N111G.D, LE_N211G.D) |>
  mutate(across(-time_period,as.numeric)) |>
  pivot_longer(-time_period,
               names_to = "sto",
               values_to = "obs_value") |>
  mutate(time_period = paste0(str_sub(time_period,1,4),
                              "-",
                              str_sub(time_period,5,6))) |>
  mutate(ref_sector = "S1M") |>
  separate_wider_delim(sto,
                       delim = ".",
                       names= c("sto","accounting_entry")) |>
  select(time_period,ref_sector,sto,accounting_entry,obs_value)|>
  na.omit()

S2 <- read_xlsx(file,
                 sheet = "S2",
                 range = "A30:HD500") |>
  select(time_period = `TIME ▼`, P6.D,P61.D, P62.D, P62F.D,
         D1.C,D3.C,D31.C,D39.C,D4.C,D41.C, D4N.C,D42.C, D43.C,D44.C,D45.C,D41G.C,D5.C,D6.C,
         D61.C,D62.C,D7.C,D71.C,D72.C,D7N.C,D74.C,
         D75.C,D8.C,D9.C,D91.C,D9N.C,D92.C,D99.C,NP.C,
         P7.C,P71.C,P72.C,P72F.C,D1.D,D2.D,D21.D,D29.D,D4.D,
         D41.D,D4N.D, D42.D,D43.D,
         D44.D,D45.D,D41G.D,D5.D,D6.D,D61.D,D62.D,
         D7.D,D71.D,D72.D,D7N.D, D74.D,D75.D,D76.D,D8.D,D9.D,D91.D,D9N.D,D92.D,D99.D,
         B101.B,B9.B,B11.B, B12.B,B9X9F._Z) |>
  mutate(across(-time_period,as.numeric)) |>
  pivot_longer(-time_period,
               names_to = "sto",
               values_to = "obs_value") |>
  mutate(time_period = paste0(str_sub(time_period,1,4),
                              "-",
                              str_sub(time_period,5,6))) |>
  mutate(ref_sector = "S2") |>
  separate_wider_delim(sto,
                       delim = ".",
                       names= c("sto","accounting_entry")) |>
  select(time_period,ref_sector,sto,accounting_entry,obs_value)|>
  na.omit()


data <- bind_rows(S1,S1N,S11,S12,S13,S1M,S2)
}
nfsa_read_excel_template_T0801 <- function (file){
library(tidyverse)
library(readxl)

S1 <- read_xlsx(file,
                sheet = "S1",
                range = "A30:JL500") |>
  select(time_period = `TIME ▼`, P2.D,P3.D,P31.D,P32.D,P5.D,P51G.D,
         P5M.D,D1.D,D2.D,D21.D,D29.D,D3.D,D31.D,D39.D,D4.D,D41.D,
         D4N.D,D42.D,D43.D,D44.D,D45.D,D41G.D,D5.D,D6.D,D61.D,D62.D,
         D63.D,D631.D,D632.D,D7.D,D71.D,D72.D,D7N.D,D74.D,D74_4Y.D,
         D75.D,D76.D,D8.D,D9.D,D91.D,D9N.D,D92.D,D99.D,P51C.D,NP.D,
         P1.C,D1.C,D2.C,D21.C,D29.C,D3.C,D31.C,D39.C,D21X31.C,D4.C,
         D41.C,D4N.C, D42.C,D43.C,D44.C,D45.C,D41G.C,D5.C,D6.C,D61.C,
         D62.C,D63.D,D7.C,D71.C,D72.C,D7N.C,D74.C,
         D75.C,D8.C,D9.C,D91.C,D9N.C,D92.C,D99.C,P51C.C,B1GQ.B,
         B1NQ.B, B2A3G.B, B3G.B, B4G.B, B5G.B, B6G.B, B8G.B, B101.B,
         B9.B, B9X9F._Z) |>
  mutate(across(-time_period,as.numeric)) |>
  pivot_longer(-time_period,
               names_to = "sto",
               values_to = "obs_value") |>
  mutate(time_period = paste0(str_sub(time_period,1,4),
                              "-",
                              str_sub(time_period,5,6))) |>
  mutate(ref_sector = "S1") |>
  separate_wider_delim(sto,
                       delim = ".",
                       names= c("sto","accounting_entry")) |>
  select(time_period,ref_sector,sto,accounting_entry,obs_value) |>
  na.omit()

S1N <- read_xlsx(file,
                 sheet = "S1N",
                 range = "A30:Q500") |>
  select(time_period = `TIME ▼`, D2.D, D21.D,D3.C,D31.C,D21X31.C,B1G.B) |>
  mutate(across(-time_period,as.numeric)) |>
  pivot_longer(-time_period,
               names_to = "sto",
               values_to = "obs_value") |>
  mutate(time_period = paste0(str_sub(time_period,1,4),
                              "-",
                              str_sub(time_period,5,6))) |>
  mutate(ref_sector = "S1N") |>
  separate_wider_delim(sto,
                       delim = ".",
                       names= c("sto","accounting_entry")) |>
  select(time_period,ref_sector,sto,accounting_entry,obs_value)|>
  na.omit()

S11 <- read_xlsx(file,
                 sheet = "S11",
                 range = "A30:GX500") |>
  select(time_period = `TIME ▼`, P2.D,P5.D,P51G.D,
         P5M.D,D1.D,D2.D,D29.D,D4.D,D41.D,
         D4N.D,D42.D,D43.D,D43_I9.D,D43_J9.D, D43_B6.D,D43_D6.D,
         D44.D,D45.D,D41G.D,D5.D,D6.D,D62.D,
         D7.D,D71.D,D7N.D,
         D75.D,D8.D,D9.D,D91.D,D9N.D,D99.D,P51C.D,NP.D,
         P1.C,D3.C,D39.C,D4.C,
         D41.C,D4N.C, D42.C,D43.C,D43_I9.C,D43_J9.C, D43_B6.C,D43_D6.C,
         D44.C,D45.C,D41G.C,D6.C,D61.C,
         D7.C,D72.C,D7N.C,
         D75.C,D9.C,D92.C,D99.C,P51C.C,B1G.B,
         B1N.B, B2A3G.B, B4G.B, B5G.B, B6G.B, B8G.B, B101.B,
         B9.B, B9X9F._Z) |>
  mutate(across(-time_period,as.numeric)) |>
  pivot_longer(-time_period,
               names_to = "sto",
               values_to = "obs_value") |>
  mutate(time_period = paste0(str_sub(time_period,1,4),
                              "-",
                              str_sub(time_period,5,6))) |>
  mutate(ref_sector = "S11") |>
  separate_wider_delim(sto,
                       delim = ".",
                       names= c("sto","accounting_entry")) |>
  select(time_period,ref_sector,sto,accounting_entry,obs_value)|>
  na.omit()

S12 <- read_xlsx(file,
                 sheet = "S12",
                 range = "A30:HD500") |>
  select(time_period = `TIME ▼`, P2.D,P5.D,P51G.D,
         P5M.D,D1.D,D2.D,D29.D,D4.D,D41.D,
         D4N.D,D42.D,D43.D,D43_I9.D,D43_J9.D, D43_B6.D,D43_D6.D,
         D44.D,D45.D,D41G.D,D5.D,D6.D,D62.D,
         D7.D,D71.D,D72.D,D7N.D,
         D75.D,D8.D,D9.D,D91.D,D9N.D,D99.D,P51C.D,NP.D,
         P1.C,D3.C,D39.C,D4.C,
         D41.C,D4N.C, D42.C,D43.C,D43_I9.C,D43_J9.C, D43_B6.C,D43_D6.C,
         D44.C,D45.C,D41G.C,D6.C,D61.C,
         D7.C,D72.C,D7N.C,
         D75.C,D9.C,D92.C,D99.C,P51C.C,B1G.B,
         B1N.B, B2A3G.B, B4G.B, B5G.B, B6G.B, B8G.B, B101.B,
         B9.B, B9X9F._Z) |>
  mutate(across(-time_period,as.numeric)) |>
  pivot_longer(-time_period,
               names_to = "sto",
               values_to = "obs_value") |>
  mutate(time_period = paste0(str_sub(time_period,1,4),
                              "-",
                              str_sub(time_period,5,6))) |>
  mutate(ref_sector = "S12") |>
  separate_wider_delim(sto,
                       delim = ".",
                       names= c("sto","accounting_entry")) |>
  select(time_period,ref_sector,sto,accounting_entry,obs_value)|>
  na.omit()


S13 <- read_xlsx(file,
                 sheet = "S13",
                 range = "A30:IT500") |>
  select(time_period = `TIME ▼`, P2.D,P3.D, P31.D,P32.D,P5.D,P51G.D, P5M.D,
         D1.D,D2.D,D29.D,D3.D,D31.D,D39.D, D4.D,D41.D, D4N.D,D42.D,D44.D,
         D45.D,D41G.D,D5.D,D6.D,D62.D,D63.D,D631.D,D632.D,D7.D,D71.D,D72.D,D7N.D,
         D74.D,D74_4Y.D,D75.D,D76.D,D8.D,D9.D,D9N.D,D92.D,D99.D,P51C.D,NP.D,OTE.D,
         P1.C,P1O.C,D2.C,D21.C,D211.C,D29.C,D3.C,D39.C,D4.C,
         D41.C,D4N.C, D42.C,D43.C,
         D44.C,D45.C,D41G.C,D5.C,D6.C,D61.C,
         D7.C,D71.C,D72.C,D7N.C,
         D75.C,D9.C,D91.C,D9N.C,D92.C,D99.C,P51C.C,OTR.C,B1G.B,
         B1N.B, B2A3G.B, B4G.B, B5G.B, B6G.B, B7G.B,B8G.B, B101.B,
         B9.B, B9X9F._Z) |>
  mutate(across(-time_period,as.numeric)) |>
  pivot_longer(-time_period,
               names_to = "sto",
               values_to = "obs_value") |>
  mutate(time_period = paste0(str_sub(time_period,1,4),
                              "-",
                              str_sub(time_period,5,6))) |>
  mutate(ref_sector = "S13") |>
  separate_wider_delim(sto,
                       delim = ".",
                       names= c("sto","accounting_entry")) |>
  select(time_period,ref_sector,sto,accounting_entry,obs_value)|>
  na.omit()

S1M <- read_xlsx(file,
                 sheet = "S1M",
                 range = "A30:HM500") |>
  select(time_period = `TIME ▼`, P2.D,P3.D, P31.D,P5.D,P51G.D, P5M.D,
         D1.D,D2.D,D29.D,D4.D,D41.D, D4N.D,D45.D,D41G.D,D5.D,D6.D,
         D61.D,D62.D,D63.D,D631.D,D632.D,D7.D,D71.D,D7N.D,
         D75.D,D8.D,D9.D,D91.D,D9N.D,D99.D,P51C.D,NP.D,
         P1.C,D1.C,D3.C,D39.C,D4.C,
         D41.C,D4N.C, D42.C,D43.C,
         D44.C,D45.C,D41G.C,D6.C,D61.C,D62.C,D63.C,D631.C,D632.C,
         D7.C,D72.C,D7N.C, D75.C,D8.C,D9.C,D9N.C,D92.C,D99.C,P51C.C,B1G.B,
         B1N.B, B2A3G.B, B3G.B, B4G.B, B5G.B, B6G.B, B7G.B,B8G.B, B101.B,
         B9.B, B9X9F._Z, LE_N111G.D, LE_N211G.D) |>
  mutate(across(-time_period,as.numeric)) |>
  pivot_longer(-time_period,
               names_to = "sto",
               values_to = "obs_value") |>
  mutate(time_period = paste0(str_sub(time_period,1,4),
                              "-",
                              str_sub(time_period,5,6))) |>
  mutate(ref_sector = "S1M") |>
  separate_wider_delim(sto,
                       delim = ".",
                       names= c("sto","accounting_entry")) |>
  select(time_period,ref_sector,sto,accounting_entry,obs_value)|>
  na.omit()

S2 <- read_xlsx(file,
                 sheet = "S2",
                 range = "A30:HD500") |>
  select(time_period = `TIME ▼`, P6.D,P61.D, P62.D, P62F.D,
         D1.C,D3.C,D31.C,D39.C,D4.C,D41.C, D4N.C,D42.C, D43.C,D44.C,D45.C,D41G.C,D5.C,D6.C,
         D61.C,D62.C,D7.C,D71.C,D72.C,D7N.C,D74.C,
         D75.C,D8.C,D9.C,D91.C,D9N.C,D92.C,D99.C,NP.C,
         P7.C,P71.C,P72.C,P72F.C,D1.D,D2.D,D21.D,D29.D,D4.D,
         D41.D,D4N.D, D42.D,D43.D,
         D44.D,D45.D,D41G.D,D5.D,D6.D,D61.D,D62.D,
         D7.D,D71.D,D72.D,D7N.D, D74.D,D75.D,D76.D,D8.D,D9.D,D91.D,D9N.D,D92.D,D99.D,
         B101.B,B9.B,B11.B, B12.B,B9X9F._Z) |>
  mutate(across(-time_period,as.numeric)) |>
  pivot_longer(-time_period,
               names_to = "sto",
               values_to = "obs_value") |>
  mutate(time_period = paste0(str_sub(time_period,1,4),
                              "-",
                              str_sub(time_period,5,6))) |>
  mutate(ref_sector = "S2") |>
  separate_wider_delim(sto,
                       delim = ".",
                       names= c("sto","accounting_entry")) |>
  select(time_period,ref_sector,sto,accounting_entry,obs_value)|>
  na.omit()


data <- bind_rows(S1,S1N,S11,S12,S13,S1M,S2)
}

