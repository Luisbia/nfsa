#' Convert and move T0800 files to parquet format from ASA folder.
#'
#' This function converts T0800 XML files for a specified country to parquet format,
#'  cleans the data, and saves it to a specified output directory.  It handles variations
#'  in the file structure, including the presence or absence of an embargo date.
#'
#' @param country A character string specifying the country code for the T0800 files.
#' @param input_sel A character string specifying the path to the directory containing the
#'   original XML files. Defaults to "M:/nas/ASA10/Production/2024/(1) ASA/(1_1) Original transmission/(1_1_1) Original xml files/".
#' @param output_sel A character string specifying the path to the directory where the
#'   converted parquet files will be saved. Defaults to M:/nas/Rprod/data/a/new.
#'
#' @return None. This function saves parquet files to the specified output directory.
#'
#' @examples
#' \dontrun{
#' nfsa_move_convert_T0800_ASA10(country = "BE")
#' }
#'
#' @export
nfsa_move_convert_T0800_ASA10 <- function(country,
                                    input_sel = "M:/nas/ASA10/Production/2024/(1) ASA/(1_1) Original transmission/(1_1_1) Original xml files/",
                                    output_sel = "M:/nas/Rprod/data/a/new/"){

  # Deprecation warning
  lifecycle::deprecate_soft(
    when = "1.0.0",
    what = "nfsa_move_convert_T0800_ASA10()",
    with = "nfsa_move_convert(table = 'T0800', process_all = TRUE)"
  )

  nfsa_move_convert(
    country = country,
    table = "T0800",
    period = NULL,
    input_sel = input_sel,
    output_sel = output_sel,
    process_all = TRUE
  )
  }


