
#' Create a virtual database of data sent to eurobase for further processing
#'
#'We pool a set of parquet files into an arrow dataset that can be queried. Do not forget to add
#'collect() at the end of your query
#' @param path_sel Path to the parquet files containing the data
#' @param table_sel eurobase table to query. By default "nasq_10_nf_tr". Other options are:
#'                  "nasa_10_nf_tr", "nasq_10_ki", "nasa_10_ki"
#'
#' @returns an arrow dataset or a data.frame if we write collect().
#' @export nfsa_revision_eurobase
#' @importFrom arrow open_dataset
#'
#' @examples
#'
#' nfsa_revision_eurobase(path_sel = "D:/data/eurobase/parquet") |>
#' dplyr::filter(ref_area == "ES" & sto == "B1GQ") |>
#' dplyr::collect()
#'
#' nfsa_revision_eurobase(path_sel = "D:/data/eurobase/parquet",
#'                       table_sel = "nasa_10_nf_tr") |>
#' dplyr::filter(ref_area == "ES" & sto == "B1GQ") |>
#' dplyr::collect()
nfsa_revision_eurobase <- function(path_sel, table_sel = "nasq_10_nf_tr"){

dat<- arrow::open_dataset(list.files(path = path_sel,
                                     pattern = table_sel,
                                     full.names = TRUE)) }
