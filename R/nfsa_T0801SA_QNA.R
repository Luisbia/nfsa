#' @title Compare NFSA (T0801SA) and Quarterly National Accounts (QNA) Data
#'
#' @description This function compares data from the NFSA  (table T0801SA)
#'   with corresponding data from the Quarterly National Accounts (QNA) from Eurostat. It identifies discrepancies
#'   between the two datasets and outputs the results to an Excel file.
#'
#' @param data A dataframe containing T0801SA data.
#' @param threshold A numeric value specifying the minimum absolute difference between NFSA and QNA values for a discrepancy to be flagged. Default is 5.
#'
#' @return A data frame containing the comparison results, including the NFSA value, QNA value, the difference between them (`diff`), the difference as a percentage of GDP (`as_GDP`), and a logical flag (`threshold`) indicating if the absolute percentage difference exceeds 0.3.  Returns `nama_nfsa` data frame that contains comparison data.
#'
#' @examples
#' \dontrun{
#' # Compare NFSA and QNA data for Germany with default settings
#' result <- nfsa_T0801SA_QNA(data = dat)
#'
#' # Compare NFSA and QNA data for France with a threshold of 5
#' result <- nfsa_T0801SA_QNA(country = "FR", threshold = 10)
#' }
#' @import cli
#' @importFrom restatapi get_eurostat_data
#' @import dplyr
#' @import tidyr
#' @export
nfsa_T0801SA_QNA <- function(data,
                             threshold = 5){

country <- sort(unique(data$ref_area))

  nfsa <- data |>
    select(ref_area,id,time_period,qsa = obs_value)

  ## NAMA -----
  cli::cli_progress_message("Collecting QNA...")
  nama <- restatapi::get_eurostat_data("namq_10_gdp",
                            filters = list(
                              geo = country,
                              unit = "CP_MNAC",
                              s_adj = "SCA"
                            ),
                            stringsAsFactors = FALSE
  ) |>
    dplyr::mutate(ref_sector = case_when(
      na_item %in%
        c("P6", "P7", "P61", "P62", "P71", "P72") ~ "S2",
      .default = as.character("S1")
    )) |>
    dplyr::mutate(accounting_entry = case_when(
      na_item %in% c("B1G", "B1GQ", "B2A3G", "B11", "B111", "B112") ~ "B",
      na_item %in% c("D1","P3", "P5", "P51G", "P6") ~ "D",
      na_item %in% c("P7", "P71", "P72") ~ "C",
      na_item %in% c("P6", "P61", "P62") ~ "D"
    )) |>
    tidyr::unite("id", c(ref_sector, na_item, accounting_entry), sep=".") |>
    dplyr::rename(time_period = time, ref_area = geo) |>
    dplyr::select(ref_area,id, time_period, qna = values)



  nama_nfsa <- dplyr::left_join(nama, nfsa,by = join_by(ref_area, id,
                                                 time_period)) |>
    tidyr::drop_na() |>
    dplyr::mutate(diff = round(qna-qsa,2)) |>
    dplyr::filter(abs(diff)>threshold) |>
    dplyr::mutate(`In %` = round(diff*100/qsa,1)) |>
    dplyr::mutate(qna_eurobase_date = today()) |>
    dplyr::select(ref_area,id,time_period,qna_eurobase_date,qna,qsa,`Difference` = diff,`In %`)



  if (nrow(nama_nfsa) == 0) {
    cli::cli_alert_success("T0801SA and QNA fully consistent!")
  }
  return(nama_nfsa)
}


