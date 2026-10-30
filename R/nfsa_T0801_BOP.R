#' @title Compare NFSA T0801 data with BOP data and identify discrepancies.
#'
#' @description This function compares NFSA T0801 data with Balance of Payments (BOP) data,
#' identifies discrepancies exceeding a specified threshold, and outputs the results
#' to an Excel file.
#'
#' @param data Data frame containing T0800 observations with `ref_area`, `id`,
#'   `time_period`, and `obs_value`.
#' @param ref_quarter Reference year as a number or character string (e.g., 2025).
#' @param threshold A numeric value specifying the threshold for the difference
#'   between NFSA and BOP data.  Differences exceeding this threshold are flagged.
#'   Default is 5.
#'
#' @return This function does not return a value. It generates an Excel file
#'   containing the comparison results in the specified output directory.  A
#'   console message indicates the file creation. If no inconsistencies are found,
#'   a success message is printed.
#'
#' @details The function reads NFSA data using `nfsa_get_data` and BOP data from
#'   XML files in a specified directory. It joins these datasets, calculates the
#'   difference between NFSA and BOP values, and filters for differences exceeding
#'   the provided threshold. The results are then written to an Excel file.

#'
#' @examples
#' \dontrun{
#' # Example usage:
#' nfsa_T0801_BOP(country = "AT", quarter = "2024Q2", threshold = 10)
#' }
#'
#' @importFrom cli cli_alert_success cli_progress_message
#' @importFrom dplyr distinct filter full_join if_else join_by mutate pull select
#' @importFrom tidyr drop_na
#' @importFrom janitor clean_names
#' @importFrom openxlsx write.xlsx
#' @importFrom purrr list_rbind map
#' @importFrom readsdmx read_sdmx
#' @importFrom stats na.omit
#' @importFrom stringr str_replace str_sub
#' @importFrom tibble as_tibble
#' @export
nfsa_T0801_BOP <- function(data,
                           ref_quarter,
                           threshold = 5
                          ){


country <- sort(unique(dat$ref_area))

  nfsa_data <- dat |>
    dplyr::select(ref_area,id,time_period,qsa = obs_value) |>
    dplyr::mutate(qsa = round(qsa,2))


  ## BOP

  cli::cli_progress_message("Collecting BOP...")
  country <- stringr::str_replace(country,"EL", "GR")# EL is GR in nama file names
  bop_files  <- list.files(path = paste0("M:/nas/QSA10/Production/",ref_quarter,"/(1) QSA/(1_2) Validation in progress/(1_2_6) Consistency checks - QSA vs BoP/Input"),
                           pattern = "xml$",
                           full.names = TRUE,
                           recursive = TRUE) |>
    tibble::as_tibble() |>
    dplyr::mutate(ref_area = stringr::str_sub(value,-22,-21)) |>
    dplyr::filter(ref_area %in% country) |>
    dplyr::select(value) |>
    dplyr::pull(value)

  read_bop_files <- function(file){
    bop_data <- readsdmx::read_sdmx(file) |>
    janitor::clean_names() |>
    dplyr::select(ref_area,ref_sector,sto,accounting_entry,time_period,bop = obs_value) |>
    dplyr::mutate(ref_sector = "S2") |>
    dplyr::mutate(bop = as.numeric(bop)) |>
    dplyr::distinct()
    return(bop_data)
  }

  bop_data <- map(bop_files,read_bop_files) |>
    purrr::list_rbind()|>
    dplyr::mutate(ref_area = dplyr::if_else(ref_area == "GR", "EL", ref_area)) |>
    nfsa::nfsa_unite_id()


  bop_nfsa <- dplyr::full_join(bop_data, nfsa_data,
                        by = join_by(ref_area,id, time_period)) |>
    tidyr::drop_na() |>
    dplyr::mutate(diff = round(bop-qsa,1)) |>
    dplyr::filter(abs(diff)>threshold)


  if (nrow(bop_nfsa) == 0) {
    cli::cli_alert_success("T0801 and BOP fully consistent!")
    return(NULL)

  }

  return(bop_nfsa)
}


