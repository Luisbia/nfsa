#' Compare NFSA and GFS Seasonally Adjusted data .
#'
#' This function compares data from the NFSA  (table T0801SA) with corresponding data from the GFS and
#' identifies discrepancies between the two datasets .
#'
#' @param country A character string representing the country code (e.g., "DE").
#' @param ref_quarter A character string specifying the quarter in the format "YYYYQQ" (e.g., "2023Q1").
#' @param threshold A numeric value specifying the minimum absolute difference between NFSA and GFS data to be considered a discrepancy. Default is 1.
#'
#' @return A data frame containing the joined NFSA and GFS data, the difference between them,
#'         and the difference as a percentage of GDP.
#'         If no discrepancies are found, the function prints a success message and returns an empty data frame.
#'
#' @examples
#' \dontrun{
#'   # Compare NFSA and GFS data for Germany with a threshold of 10
#'   result <- nfsa_T0801SA_GFS(data = dat, quarter = "2025Q3")
#'
#' }
#'
#' @export
nfsa_T0801SA_GFS <- function(data,
                             ref_quarter = "2026Q2",
                             threshold = 5){

  nfsa_data <- data |>
  dplyr::select(ref_area,id,time_period,qsa = obs_value)

  ## GFS -----
  cli::cli_progress_message("Collecting GFS...")
  country <- sort(unique(data$ref_area))
  country <- stringr::str_replace(country,"EL", "GR")# EL is GR in nama file names

  gfs_files <- list.files(path = paste0("M:/nas/QSA10/Production/",ref_quarter,"/(1) QSA/(1_2) Validation in progress/(1_2_5) Consistency checks - QSA vs GFS/Input"),
                                        pattern = "xml$",
                                        full.names = TRUE,
                                        recursive = TRUE) |>
    tibble::as_tibble() |>
    dplyr::mutate(ref_area = stringr::str_sub(value,-21,-20),
           update = as.numeric(stringr::str_sub(value,-16,-5))) |>
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
      dplyr::filter(adjustment == "Y") |>
      dplyr::select(ref_area,ref_sector,sto,accounting_entry,time_period,gfs = obs_value) |>
      nfsa::nfsa_unite_id() |>
      dplyr::mutate(gfs = as.numeric(gfs),
                    source_date = as.Date(file_timestamp, format = "%Y%m%d%H%M")) |>
      dplyr::distinct()
  }

  gfs_data <- purrr::map(gfs_files,read_gfs_files, .progress = TRUE) |>
    purrr::list_rbind(names_to = "file")|>
    dplyr::mutate(ref_area = dplyr::if_else(ref_area == "GR", "EL", ref_area)) |>
    dplyr::select(-file)


  gfs_nfsa <- full_join(gfs_data, nfsa_data,by = join_by(ref_area, id,time_period)) |>
    tidyr::drop_na() |>
    dplyr::mutate(diff = round(gfs - qsa,2)) |>
    dplyr::filter(abs(diff)> threshold)

  tmp <- nfsa_data |>
    filter(id == "S1.B1GQ.B") |>
    select(ref_area,time_period,gdp=qsa)

  gfs_nfsa <- left_join(gfs_nfsa,tmp, by = join_by(ref_area, time_period)) |>
    mutate(as_GDP = round(diff*100/gdp,1)) |>
    select(-gdp) |>
    dplyr::select(ref_area,id,time_period,gfs_date = source_date,
                  gfs,qsa,Difference = diff,`As % GDP` = as_GDP) |>
    dplyr::arrange(time_period,id)



  if (nrow(gfs_nfsa) == 0) {
    cli::cli_alert_success("T0801SA and GFS fully consistent!")

  }
  return(gfs_nfsa)
}


