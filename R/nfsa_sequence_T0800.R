#' @title Show data or revisions as sequence of accounts for T0800
#'
#' @description This function takes T0800 data if there is not a version `column` or calculates the revision of the two more
#' recent versions, formats the output, and exports it to an Excel file based on a
#'   specified template.
#'
#' @param data A data frame with T0800 data for one or several countries and years.
#' @param template A character string specifying the file path to the Excel template file. Default: `M:/nas/Rprod/assets/seq_accounts_T0800.xlsx`.
#' @param output_sel A character string specifying the file path to the directory where the output Excel file will be saved. Default: `here::here()`.
#'
#' @return None. The function saves an Excel file to the specified output directory.
#'   The filename includes the country code(s) and a timestamp. A success message is printed to the console
#'   indicating the location of the created file, or if there are no revisions.
#'
#'
#' @examples
#' \dontrun{
#'   # Example usage with specific country and default settings
#'   nfsa_sequence_T0800(data = dataset)
#'
#'   # Example usage with specifit output holder
#'   nfsa_sequence_T0800(data = dataset, output_sel = "C:/tmp/")
#'
#' }
#' @import dplyr
#' @import tidyr
#' @import openxlsx
#' @import cli
#' @export
nfsa_sequence_T0800 <- function(data,
                                template = "M:/nas/Rprod/assets/seq_accounts_T0800.xlsx",
                                output_sel = here::here()) {

country <-sort(unique(data$ref_area))

if ("version" %in% names(data) == FALSE){

  nfsa_data <- data |>
    dplyr::mutate(obs_value = round(obs_value,1)) |>
    dplyr::mutate(obs_value = ifelse(is.na(obs_value), ':', obs_value)) |>
    tidyr::unite("id",time_period,ref_area,id,sep = ".")

  } else {

  nfsa_data <- data |>
    dplyr::arrange(version) |>
    dplyr::group_by(ref_area,id,time_period) |>
    dplyr::slice_tail(n=2) |>
    dplyr::mutate(obs_value = obs_value - dplyr::lag(obs_value,1)) |>
    dplyr::mutate(obs_value = ifelse(is.na(obs_value), ':', obs_value)) |>
    dplyr::ungroup() |>
    tidyr::unite("id",time_period,ref_area,id,sep = ".") |>
    dplyr::arrange(desc(version)) |>
    dplyr::group_by(id) |>
    dplyr::slice_head(n=1) |>
    dplyr::select(-version) |>
    dplyr::ungroup()

}
  wb <- loadWorkbook(template)
  writeData(wb, "data", nfsa_data, startRow = 1, startCol = 1)


  if(length(country) == 1) {
    saveWorkbook(wb, ,file =paste0(output_sel,"/T0800_",country,"_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),"_seq_accounts.xlsx"))

    cli::cli_alert_success(paste0("File created in ",output_sel,"/T0800_",country,"_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),"_seq_accounts.xlsx"))
  }

  if(length(country) > 1) {
    saveWorkbook(wb, ,file = paste0(output_sel,"/T0800_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),"_seq_accounts.xlsx"))

    cli::cli_alert_success(paste0("File created in ",output_sel,"/T0800_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),"_seq_accounts.xlsx"))
  }

}

