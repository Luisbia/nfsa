#' @title Compare NFSA T0800 data with GFS data
#'
#' @description This function compares NFSA T0800 data with GFS data for a given country and year, identifying discrepancies above a specified threshold.
#'
#' @param ref_year A character string specifying the quarter in the format "YYYY" (e.g., 2025).
#' @param threshold A numeric value specifying the threshold for the absolute difference between NFSA and GFS values.  Defaults to 5.
#'
#' @return A data frame
#'
#' @details The function performs the following steps:
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' nfsa_T0800_GFS(data = dat, ref_year = 2025, threshold = 0.5)
#' }
#'
#' @import cli
#' @import dplyr
#' @import purrr
#' @import dplyr
#' @import stringr
#' @import readsdmx
#' @import janitor
#' @import tibble
#' @export
nfsa_T0800_GFS <- function(data ,
                           ref_year,
                           threshold = 5){


  limit_validation <- as.numeric(max(data$time_period)) -4


  nfsa_data <- data |>
    dplyr::select(ref_area,id,time_period,nfsa = obs_value)

  ## GFS -----
  country <- sort(unique(data$ref_area))
  country <- stringr::str_replace(country,"EL", "GR")# EL is GR in nama file names
  cli::cli_progress_message("Collecting GFS...")
  gfs_files <- list.files(path = paste0("M:/nas/ASA10/Production/",ref_year,"/(1) ASA/(1_2) Validation in progress/(1_2_5) Consistency checks - ASA vs GFS/Input"),
                          pattern = "xml$",
                          full.names = TRUE,
                          recursive = TRUE) |>
    as_tibble() |>
    dplyr::mutate(ref_area = str_sub(value,-21,-20),
           update = as.numeric(str_sub(value,-16,-5))) |>
    dplyr::filter(ref_area %in% country) |>
    dplyr::group_by(ref_area) |>
    dplyr::arrange(update,.by_group = TRUE) |>
    dplyr::slice_tail(n=1) |>
    dplyr::ungroup() |>
    dplyr::select(value) |>
    dplyr::pull(value) |>
    purrr::set_names()

  read_gfs_files <- function(file){
    file_timestamp <- stringr::str_extract(file,"\\d{12}(?=\\.xml$)")
    gfs_data <- readsdmx::read_sdmx(file) |>
      janitor::clean_names() |>
      dplyr::filter(adjustment == "N") |>
      dplyr::select(ref_area,ref_sector,sto,accounting_entry,time_period,gfs = obs_value) |>
      nfsa::nfsa_unite_id() |>
      dplyr::mutate(gfs = as.numeric(gfs),
                    source_date = as.Date(file_timestamp, format = "%Y%m%d%H%M")) |>
      dplyr::distinct()
  }

  gfs_data <- purrr::map(gfs_files,read_gfs_files, .progress = TRUE) |>
    purrr::list_rbind(names_to = "file")  |>
    dplyr::mutate(ref_area = dplyr::if_else(ref_area == "GR", "EL", ref_area)) |>
    dplyr::mutate(status = dplyr::if_else(stringr::str_detect(file,"non-Validated"),"Non-Validated", "Validated")) |>
    dplyr::select(-file)


  country <- stringr::str_replace(country,"GR", "EL")# EL is GR in nama file names

  gfs_nfsa <- dplyr::full_join(gfs_data, nfsa_data,by = join_by(ref_area, id,time_period)) |>
    tidyr::drop_na() |>
    dplyr::mutate(diff = round(nfsa-gfs,2)) |>
    dplyr::filter(abs(diff)> threshold)

  tmp <- nfsa_data |>
    dplyr::filter(id == "S1.B1GQ.B") |>
    dplyr::select(ref_area,time_period,gdp=nfsa)

  gfs_nfsa <- dplyr::left_join(gfs_nfsa,tmp, by = join_by(ref_area, time_period)) |>
    dplyr::mutate(as_GDP = round(diff*100/gdp,3)) |>
    #select(-gdp) |>
    dplyr::mutate(threshold = dplyr::if_else(abs(as_GDP) > 0.3, "More than 0.3% of GDP","Less than 0.3% of GDP")) |>
    mutate(validate = ifelse(threshold == TRUE &
                               id == "S13.B9.B" &
                               time_period>= limit_validation,
                             "NOT VALIDATE","VALIDATE")) |>
    dplyr::select(ref_area,id,time_period,gfs_date = source_date,gfs_status = status,
                  gfs,nfsa,Difference = diff,`As % GDP` = as_GDP,`Over 0.3 % GDP?` = threshold, `Validate?` =validate) |>
    dplyr::arrange(time_period,id)

  if (nrow(gfs_nfsa) == 0) {
    cli::cli_alert_success("T0800 and GFS fully consistent!")

  }

  return(gfs_nfsa)
}






