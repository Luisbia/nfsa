#' Identify the need to ask for a new T0801SA
#'
#' @param country Character string specifying one country code.
#' @param ref_period Transmission reference period, such as `"2026Q2"` or
#'   `"2026_0002"`, passed to `nfsa_get_data()`.
#'
#' @returns a data.frame showing T0801 values before and after the transmission
#' of T081SA or a printed statement declaring no need to ask for a new transmission.
#' @importFrom cli cli_alert_danger cli_alert_info cli_alert_success
#' @importFrom dplyr arrange bind_rows distinct filter inner_join join_by mutate pick pull select
#' @importFrom lubridate ymd
#' @importFrom stringr str_sub
#' @importFrom tidyr drop_na pivot_wider
#' @export
#'
#' @examples
#' se <-nfsa_T0801SA_retransmit("SE", ref_period = "2026Q2")
nfsa_T0801SA_retransmit <- function(country,
                                    ref_period){
## Just extracting GDP to get dates
sca_date <- nfsa_get_data(country = country,
                     table = "T0801SA",
                     filters = c("sto == 'B1GQ'"),
                     complete = TRUE,
                     ref_period = ref_period,
                     last = FALSE) |>
  dplyr::select(ref_area,version,received) |>
  dplyr::filter(version == max(version)) |>
  dplyr::distinct() |>
  dplyr::mutate(received = lubridate::ymd(stringr::str_sub(received,1,10)),
         ref_quarter = ref_period,
         table_identifier = "T0801SA")

nsa_dates <- nfsa_get_data(country = country,
                     table = "T0801",
                     filters = c("sto == 'B1GQ'"),
                     complete = TRUE,
                     last = FALSE) |>
  dplyr::select(ref_area,table_identifier,version,received) |>
  dplyr::distinct() |>
  dplyr::mutate(received = lubridate::ymd(stringr::str_sub(received,1,10)),
         ref_quarter = stringr::str_sub(version,1,9)) |>
  dplyr::filter(ref_quarter %in% sca_date$ref_quarter)

if (max(nsa_dates$received)>sca_date$received){
  tmp <- dplyr::bind_rows(sca_date,nsa_dates) |>
    dplyr::mutate(time_span = received - received[table_identifier == "T0801SA"])

  cli::cli_alert_info(paste0("T0801SA was received at ",sca_date$received) )
  cli::cli_alert_danger(paste0("T0801 was received at ",max(nsa_dates$received)) )
  pre_sca_version <- tmp |>
    dplyr::filter(table_identifier == "T0801") |>
    dplyr::filter(time_span == min(time_span)) |>
    dplyr::select(version) |>
    dplyr::pull()


versions<-  nfsa_get_data(country = country,
                complete = TRUE,
                ref_period = ref_period,
                last = FALSE)

  pre_sca_version_df <- versions |>
    dplyr::filter(version %in% pre_sca_version) |>
    dplyr::select(ref_area,version,id,time_period,obs_value) |>
    tidyr::drop_na()

  post_sca_version_df <- versions|>
    dplyr::filter(version ==max(version)) |>
    dplyr::select(ref_area,version,id,time_period,obs_value) |>
    tidyr::drop_na()

  version_changes <- dplyr::bind_rows(pre_sca_version_df, post_sca_version_df) |>
    dplyr::arrange(version) |>
    tidyr::pivot_wider(names_from = version,
                values_from = obs_value) |>
    tidyr::drop_na() |>
    dplyr::mutate(change = round(unlist(dplyr::pick(5) - dplyr::pick(4)))) |>
    dplyr::filter(abs (change) > 2)

  sca <- nfsa_get_data(country = country,
                       table = "T0801SA",
                       ref_period = ref_period)

  combined <- dplyr::inner_join(version_changes, sca,by = dplyr::join_by(ref_area, id, time_period))
  return(combined)
} else {
    cli::cli_alert_success ("T0801SA arrived later or at the same time than T0801 ")
  }
}

