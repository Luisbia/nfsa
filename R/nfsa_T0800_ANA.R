#' Compares NFSA T0800 data with ANA data for consistency.
#'
#' This function retrieves and compares NFSA  T0800 data with ANA (Annual National Accounts) data.
#' It identifies discrepancies between the two datasets based on a specified threshold and generates an Excel file
#' containing the comparison results.
#'
#' @param data a data.frame containing T0800 data
#' @param ref_year A numeric value specifying the referencew year to which the data should be compared.
#' @param threshold A numeric value specifying the threshold for acceptable differences between NFSA and ANA data.
#'                  Defaults to 5.
#' @param non_validated Logical condition. If `FALSE`, which is the default option, only looks for files in validated. If set to `TRUE` also looks in non-validated.
#'
#' @return a dataframe comparing T0800 with ANA
#'
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' nfsa_T0800_ANA(data = dat, ref_year = 2025, threshold = 0.5)
#' }
#'
#' @import cli
#' @import dplyr
#' @import purrr
#' @import stringr
#' @import readsdmx
#' @import janitor
#' @import tibble
#' @export
nfsa_T0800_ANA <- function(data,
                           ref_year,
                           threshold = 5,
                           non_validated = FALSE){


  cli::cli_progress_message("Collecting NFSA...")

  limit_validation <- as.numeric(max(data$time_period)) -4

  nfsa_data <- data |>
    dplyr::rename(nfsa = obs_value)

  ## NAMA -----
  cli::cli_progress_message("Collecting ANA...")

  # Build country pattern for faster file filtering
  country <- sort(unique(data$ref_area))
  country <- stringr::str_replace(country,"EL", "GR")# EL is GR in nama file names
  country_pattern <- paste0("(", paste(country, collapse = "|"), ")")

  nama_files <- list.files(
    path = paste0("M:/nas/ASA10/Production/", ref_year,
                  "/(1) ASA/(1_2) Validation in progress/",
                  "(1_2_4) Consistency checks - ASA vs ANA/Input"),
    pattern = paste0(".*", country_pattern, ".*\\.xml$"),
    full.names = TRUE,
    recursive = non_validated
  ) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      ref_area = str_sub(value, -30, -29),
      update = as.numeric(str_sub(value, -18, -4))
    ) |>
    dplyr::filter(ref_area %in% country) |>
    dplyr::slice_max(update, n = 1, by = ref_area) |>
    dplyr::pull(value) |>
    purrr::set_names()


  read_nama_files <- function(file){
    file_timestamp <- stringr::str_extract(basename(file), "\\d{14}")
    nama_data <- readsdmx::read_sdmx(file) |>
      janitor::clean_names() |>
      dplyr::select(ref_area, ref_sector, sto, unit_measure, accounting_entry,
                    counterpart_area, time_period, nama = obs_value) |>
      dplyr::mutate(accounting_entry = if_else(sto == "EMP" & unit_measure == "PS" &
                                                 counterpart_area == "W2", "PS",
                                               accounting_entry),
                    accounting_entry = if_else(sto == "EMP" & unit_measure == "HW" &
                                                 counterpart_area == "W2", "HW",
                                               accounting_entry)) |>
      dplyr::select(-unit_measure, -counterpart_area) |>
      dplyr::mutate(nama = as.numeric(nama),
                    source_date = as.Date(file_timestamp, format = "%Y%m%d%H%M%S")) |>
      dplyr::distinct() |>
      nfsa::nfsa_unite_id()
    return(nama_data)
  }

  # Read XML files with progress feedback
  cli::cli_progress_message("Reading {length(nama_files)} XML file(s)...")
  nama_data <- purrr::map(nama_files, read_nama_files, .progress = TRUE) |>
    purrr::list_rbind(names_to = "file")  |>
    dplyr::mutate(status = dplyr::if_else(stringr::str_detect(file,"non-Validated"),"Non-Validated", "Validated")) |>
    dplyr::select(-file) |>
    dplyr::mutate(ref_area = dplyr::if_else(ref_area == "GR", "EL", ref_area))

  country <- stringr::str_replace(country,"GR", "EL")# EL is GR in nama file names

  nama_nfsa <- dplyr::full_join(nama_data, nfsa_data,by = join_by(ref_area, id,
                                                           time_period)) |>
    tidyr::drop_na() |>
    dplyr::mutate(diff = round(nfsa-nama,2)) |>
    dplyr::group_by(ref_area,time_period) |>
    dplyr::mutate(as_GDP = round(diff*100/nama[id == "S1.B1GQ.B"],3)) |>
    dplyr::ungroup() |>
    dplyr::filter(abs(diff)>threshold) |>
    dplyr::mutate(threshold = dplyr::if_else(abs(as_GDP) > 0.3, "More than 0.3% of GDP","Less than 0.3% of GDP")) |>
    dplyr::mutate(validate = dplyr::if_else(threshold == TRUE &
                                      id == "S1.B1GQ.B" &
                                      time_period>= limit_validation,
                                    "NOT VALIDATE","VALIDATE"))|>
    dplyr::select(ref_area,id,time_period,nama_date = source_date,nama_status = status,
                  nama,nfsa,Difference = diff,`As % GDP` = as_GDP,`Over 0.3 % GDP?` = threshold, `Validate?` =validate) |>
    dplyr::arrange(time_period,id)



  if (nrow(nama_nfsa) == 0) {
    cli::cli_alert_success("T0800 and ANA fully consistent!")

  }

  return(nama_nfsa)
}

