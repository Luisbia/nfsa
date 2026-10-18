#' #' Identify the need to ask for a new T0801SA
#' 
#' @param country_sel a character string for an individual country.
#'
#' @returns a data.frame showing T0801 values before and after the transmission 
#' of T081SA or a printed statement declaring no need to ask for a new transmission.
#' @export
#'
#' @examples
#' se <-nfsa_T0801SA_retransmit("SE")
nfsa_T0801SA_retransmit <- function(country_sel = "SK"){
## Just extracting GDP to get dates
sca_date <- nfsa_get_data(country = country_sel,
                     table = "T0801SA",
                     filters = c("sto == 'B1GQ'"),
                     complete = TRUE) |> 
  select(ref_area,version,received) |> 
  distinct() |> 
  mutate(received = lubridate::ymd(str_sub(received,1,10)),
         ref_quarter = str_sub(version,1,9),
         table_identifier = "T0801SA") 

nsa_dates <- nfsa_get_data_all(country = country_sel,
                     table = "T0801",
                     filters = c("sto == 'B1GQ'"),
                     complete = TRUE) |> 
  select(ref_area,table_identifier,version,received) |> 
  distinct() |> 
  mutate(received = lubridate::ymd(str_sub(received,1,10)),
         ref_quarter = str_sub(version,1,9)) |> 
  filter(ref_quarter %in% sca_date$ref_quarter)

if (max(nsa_dates$received)>sca_date$received){
  tmp <- bind_rows(sca_date,nsa_dates) |> 
    mutate(time_span = received - received[table_identifier == "T0801SA"])

  pre_sca_version <- tmp |> 
    filter(time_span == min(time_span)) |> 
    select(version) |> 
    pull()
  

  
  pre_sca_version_df <- nfsa_get_data_all(country = country_sel,
                                          complete = TRUE) |> 
    filter(version %in% pre_sca_version) |> 
    select(ref_area,version,id,time_period,obs_value) |> 
    na.omit()
  
  post_sca_version_df <- nfsa_get_data(country = country_sel,
                                          complete = TRUE) |> 
    select(ref_area,version,id,time_period,obs_value) |> 
    na.omit()

  version_changes <- bind_rows(pre_sca_version_df, post_sca_version_df) |>
    arrange(version) |> 
    pivot_wider(names_from = version,
                values_from = obs_value) |> 
    na.omit() |> 
    mutate(change = round(unlist(pick(5) - pick(4)))) |> 
    filter(abs (change) > 2)

  sca <- nfsa_get_data(country = country_sel,
                       table = "T0801SA")
  
  combined <- inner_join(version_changes, sca,by = join_by(ref_area, id, time_period)) 
  return(combined)
} else {
    print("T0801SA arrived later or at the same time than T0801 ")
  }
}

