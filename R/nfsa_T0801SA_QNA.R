#' @title Compare NFSA (T0801SA) and Quarterly National Accounts (QNA) Data
#'
#' @description This function compares data from the NFSA (National Financial Statistics Accounts, table T0801SA)
#'   with corresponding data from the Quarterly National Accounts (QNA) from Eurostat. It identifies discrepancies
#'   between the two datasets and outputs the results to an Excel file.
#'
#' @param country A character string specifying the country code (e.g., "DE" for Germany).
#' @param threshold A numeric value specifying the minimum absolute difference between NFSA and QNA values for a discrepancy to be flagged. Default is 1.
#' @param input_sel A character string specifying the directory path where input data is stored. The default path is "data/q/new/sca" relative to the project directory.  Note that this parameter is not used in the function.
#' @param output_sel A character string specifying the directory path where the output Excel file will be saved. The default path is "output/inter_domain" relative to the project directory.
#'
#' @return A data frame containing the comparison results, including the NFSA value, QNA value, the difference between them (`diff`), the difference as a percentage of GDP (`as_GDP`), and a logical flag (`threshold`) indicating if the absolute percentage difference exceeds 0.3.  Returns `nama_nfsa` data frame that contains comparison data.
#'
#' @examples
#' \dontrun{
#' # Compare NFSA and QNA data for Germany with default settings
#' result <- nfsa_T0801SA_QNA(country = "DE")
#'
#' # Compare NFSA and QNA data for France with a threshold of 5
#' result <- nfsa_T0801SA_QNA(country = "FR", threshold = 5)
#' }
#'
#' @export
nfsa_T0801SA_QNA <- function(country,
                             threshold = 1,
                             input_sel = here::here("data", "q", "new", "sca"),
                             output_sel = here::here("output", "inter_domain")){
  pacman::p_load(tidyverse,arrow,writexl,here,restatapi)
  options(warn=-1)

  cli::cli_progress_message("Collecting NFSA...")
  nfsa <- nfsa_get_data(country = country,table = "T0801SA", type = "new") |>
    select(ref_area,id,time_period,nfsa = obs_value)

  ## NAMA -----
  cli::cli_progress_message("Collecting QNA...")
  nama <- get_eurostat_data("namq_10_gdp",
                            filters = list(
                              geo = country,
                              unit = "CP_MNAC",
                              s_adj = "SCA"
                            ),
                            stringsAsFactors = FALSE
  ) |>
    mutate(ref_sector = case_when(
      na_item %in%
        c("P6", "P7", "P61", "P62", "P71", "P72") ~ "S2",
      .default = as.character("S1")
    )) |>
    mutate(accounting_entry = case_when(
      na_item %in% c("B1G", "B1GQ", "B2A3G", "B11", "B111", "B112") ~ "B",
      na_item %in% c("D1","P3", "P5", "P51G", "P6") ~ "D",
      na_item %in% c("P7", "P71", "P72") ~ "C"
    )) |>
    unite("id", c(ref_sector, na_item, accounting_entry), sep=".") |>
    rename(time_period = time, ref_area = geo) |>
    select(ref_area,id, time_period, nama = values)



  nama_nfsa <- full_join(nama, nfsa,by = join_by(ref_area, id,
                                                 time_period)) |>
    na.omit() |>
    mutate(diff = round(nfsa-nama,2)) |>
    group_by(ref_area,time_period) |>
    mutate(as_GDP = round(diff*100/nama[id =="S1.B1GQ.B"],3)) |>
    ungroup() |>
    filter(abs(diff)>threshold) |>
    mutate(threshold = ifelse(abs(as_GDP) > 0.3, TRUE,FALSE))



  if (nrow(nama_nfsa) == 0) {
    cli::cli_alert_success("T0801SA and QNA fully consistent!")

  }

  if (length(unique(nama_nfsa$ref_area)) == 1) {
    openxlsx::write.xlsx(nama_nfsa,
                         file = paste0(output_sel,"/T0801SA_QNA_",unique(nama_nfsa$ref_area), "_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"),
                         overwrite = TRUE,
                         asTable = TRUE)

    cli::cli_alert_success(paste0("File created in ", output_sel,"/T0801SA_QNA_",unique(nama_nfsa$ref_area), "_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"))

  }

  if (length(unique(nama_nfsa$ref_area)) > 1) {
    openxlsx::write.xlsx(nama_nfsa,
                         file = paste0(output_sel,"/T0801SA_QNA_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"),
                         overwrite = TRUE,
                         asTable = TRUE)

    cli::cli_alert_success(paste0("File created in ", output_sel,"/T0801SA_QNA_",as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),".xlsx"))
  }
  options(warn=0)
  return(nama_nfsa)
}


