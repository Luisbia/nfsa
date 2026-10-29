#'Export to excel a data frame
#'
#'@description
#'
#'For closer inspection of small data frames this functions exports it to excel and opens the file.
#' @param .data the data.frame, data.table to be exported
#' @return an excel file in a temporary folder
#' @importFrom fs file_show file_temp
#' @importFrom readr write_excel_csv
#' @export
#'
#' @examples
#'
#'  nfsa_to_excel(mtcars)

nfsa_to_excel <- function(.data){

  if (interactive()) {
    tmp <- fs::file_temp("excel", ext = "csv")
    readr::write_excel_csv(.data, tmp)
    fs::file_show(tmp)
  }
  invisible(.data)
}
