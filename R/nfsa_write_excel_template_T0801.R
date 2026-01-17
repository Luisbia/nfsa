#' Create NFSA Excel template T0801
#'
#' Generate an Excel file for table T0801 by reshaping NFSA data and
#' writing it into a predefined Excel template.
#'
#' The function splits the `id` column into NFSA components, filters
#' valid reference sectors and time periods, reshapes identifiers, and
#' writes the resulting data to the `data` sheet of the template before
#' saving a timestamped copy in the selected output directory.
#'
#' @param data A data frame containing at least the columns
#'   `id`, `ref_area`, `time_period`, and `obs_value`. The `id` column must
#'   encode `ref_sector`, `sto`, and `accounting_entry` separated by dots
#'   (for example, `"S1.A.B"`).
#' @param template Path to the Excel template file to use for
#'   table T0801. Defaults to `here("assets", "template_T0801.xlsx")`.
#' @param output_sel Path to the output directory where the generated
#'   Excel file will be saved. Defaults to
#'   `here("output", "excel_template")`.
#'
#' @details
#' The function:
#' - Splits `id` into `ref_sector`, `sto`, and `accounting_entry`.
#' - Keeps only selected NFSA reference sectors and observations from
#'   1999-Q1 onwards.
#' - Rebuilds a compact `id` by combining period, reference area,
#'   and NFSA identifier and writes this to the `data` sheet in the template,
#'   starting at cell A1.
#'
#' The output file name follows the pattern
#' `"T0801_<country>_<YYYYMMDD_HHMMSS>.xlsx"`, where `<country>` must be
#' available in the calling environment.
#'
#' @returns
#' Invisibly returns the `Workbook` object (from `openxlsx`) after the
#' data have been written and the file has been created.
#'
#' @seealso
#' \code{\link[openxlsx]{loadWorkbook}},
#' \code{\link[openxlsx]{writeData}},
#' \code{\link[openxlsx]{saveWorkbook}} for manipulating Excel workbooks.
#'
#' @examples
#' \dontrun{
#' library(tidyverse)
#' library(here)
#' library(openxlsx)
#'
#' # Example input data
#' example_data <- tibble::tibble(
#'   id          = c("S1.A.B", "S11.C.D"),
#'   ref_area    = c("EA", "EA"),
#'   time_period = c("1999-Q1", "2000-Q2"),
#'   obs_value   = c(100, 200)
#' )
#'
#' country <- "EA"
#' nfsa_write_excel_template_T0801(
#'   data     = example_data,
#'   template = here::here("assets", "template_T0801.xlsx"),
#'   output_sel = here::here("output", "excel_template")
#' )
#' }
#'
#' @export
nfsa_write_excel_template_T0801 <- function(data,
                                            template = here::here("assets","template_T0801.xlsx"),
                                            output_sel = here::here("output","excel_template")) {

  country <- unique(data$ref_area)
  nfsa_data <- data |>
    tidyr::separate_wider_delim(cols = id,
                         delim = ".",
                         names = c("ref_sector", "sto", "accounting_entry")) |>
    dplyr::filter(ref_sector %in% c("S1", "S1N", "S11", "S12", "S13","S1M", "S2"),
           time_period >= "1999-Q1") |>
    stats::na.omit() |>
    tidyr::unite("id",c(ref_sector,sto,accounting_entry), sep = ".") |>
    stats::na.omit() |>
    dplyr::select(ref_area,id,time_period,obs_value) |>
    dplyr::mutate(time_period = stringr::str_remove(time_period,"-")) |>
    tidyr::unite("id",c(time_period,ref_area,id),sep = ".")

  wb <- openxlsx::loadWorkbook(template)
  openxlsx::writeData(wb, "data", nfsa_data, startRow = 1, startCol = 1)

  openxlsx::saveWorkbook(wb, ,file =paste0(output_sel,"/T0801_",country,"_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"))

  cli::cli_alert_success(paste0("File created in ",output_sel,"/T0801_",country,"_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"))
}
