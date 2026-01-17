#' Convert and move T0801SA files to parquet format from QSA folder.
#'
#' This function converts T0801SA XML files for a specified country to parquet format,
#'  cleans the data, and saves it to a specified output directory.  It handles variations
#'  in the file structure, including the presence or absence of an embargo date.
#'
#' @param country A character string specifying the country code for the T0801 files.
#' @param input_sel A character string specifying the path to the directory containing the
#'   original XML files. Defaults to "M:/nas/QSA10/Production/2025Q2/(2) SA&V/(2_1) Original transmission/(2_1_1) Original xml files/".
#' @param output_sel A character string specifying the path to the directory where the
#'   converted parquet files will be saved. Defaults to "M:/nas/Rprod/data/q/new/sca".
#'
#' @return None. This function saves parquet files to the specified output directory.
#'
#' @examples
#' \dontrun{
#' nfsa_move_convert_T0801SA_QSA10(country = "BE")
#' }
#'
#' @export
nfsa_move_convert_T0801SA_QSA10 <- function(country,
                                          input_sel = "M:/nas/QSA10/Production/2025Q2/(2) SA&V/(2_1) Original transmission/(2_1_1) Original xml files/",
                                          output_sel = "M:/nas/Rprod/data/q/new/sca/"){

  # Deprecation warning
  lifecycle::deprecate_soft(
    when = "0.2.0",
    what = "nfsa_move_convert_T0801SA_QSA10()",
    with = "nfsa_move_convert(table = 'T0801SA', process_all = TRUE)"
  )

  nfsa_move_convert(
    country = country,
    table = "T0801SA",
    period = NULL,
    input_sel = input_sel,
    output_sel = output_sel,
    process_all = TRUE
  )
}
