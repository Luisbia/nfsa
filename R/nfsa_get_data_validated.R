#' Retrieve Validated NFSA Data from Parquet Files
#'
#' @description
#' Reads validated National Financial and Sector Accounts (NFSA) data
#' from locally stored Parquet files. The function identifies the files
#' corresponding to the requested countries and table, selects only
#' files listed in an external validation register, and retrieves their
#' observations using Arrow.
#'
#' @details
#' The function supports three NFSA tables:
#' \itemize{
#'   \item \code{T0800}: Annual non-seasonally adjusted data.
#'   \item \code{T0801}: Quarterly non-seasonally adjusted data.
#'   \item \code{T0801SA}: Quarterly seasonally adjusted data.
#' }
#'
#' Data retrieval follows these steps:
#' \enumerate{
#'   \item Recursively searches the input directory for Parquet files
#'   corresponding to the requested table.
#'   \item Extracts the reporting country from each file path and
#'   retains files belonging to the requested countries.
#'   \item Reads the validation register located at
#'   \code{M:/nas/Rprod/assets/validated_files.csv}.
#'   \item Selects only Parquet files whose paths contain a filename
#'   recorded in the validation register for the corresponding
#'   country and table.
#'   \item Opens the selected files as an Arrow dataset.
#'   \item Optionally applies filters using Arrow before collecting
#'   the observations into memory.
#'   \item Joins the observations with \code{nfsa::nfsa_sto_lookup}
#'   to obtain the corresponding NFSA series identifiers.
#' }
#'
#' The structure of the returned data depends on \code{complete}:
#' \itemize{
#'   \item If \code{complete = FALSE}, observations with missing values
#'   in any column after the lookup join are removed, and only
#'   \code{ref_area}, \code{version}, \code{id}, \code{time_period},
#'   and \code{obs_value} are returned.
#'   \item If \code{complete = TRUE}, all columns resulting from the
#'   lookup join are retained, and observations are sorted by
#'   \code{time_period}, \code{ref_area}, and \code{id}.
#' }
#'
#' In both cases, the reporting country code \code{GR} is converted
#' to \code{EL}.
#'
#' @param input_sel Character string specifying the root directory
#'   containing the Parquet files. The search is recursive.
#'   Defaults to \code{"M:/nas/Rprod/copy_data/"}.
#'
#' @param country Character vector containing one or more reporting
#'   country codes (e.g., \code{"IT"} or \code{c("IT", "FR", "DE")}).
#'   Only validated files for these countries are selected.
#'
#' @param table Character string specifying the NFSA table to retrieve.
#'   Supported values are \code{"T0800"}, \code{"T0801"}, and
#'   \code{"T0801SA"}. Defaults to \code{"T0801"}.
#'
#' @param filters Optional character vector of expressions used to
#'   filter observations before they are loaded into memory.
#'   Expressions must refer to columns available in the Parquet files
#'   and must be compatible with Arrow's filtering capabilities.
#'   Multiple expressions are combined using logical AND.
#'   Defaults to \code{NULL}, meaning no additional filtering.
#'
#' @param complete Logical indicating whether to return the complete
#'   dataset, including all available columns and lookup information.
#'   If \code{FALSE} (default), returns only the standard NFSA
#'   observation columns and removes rows containing missing values.
#'   If \code{TRUE}, retains all columns and does not explicitly
#'   remove missing values.
#'
#' @return A data frame (tibble) containing the observations retrieved
#'   from validated Parquet files.
#'
#'   When \code{complete = FALSE}, the returned data frame contains:
#'   \describe{
#'     \item{ref_area}{Reporting country code.}
#'     \item{version}{Version of the data transmission.}
#'     \item{id}{NFSA series identifier derived from the lookup table.}
#'     \item{time_period}{Reference year or quarter of the observation.}
#'     \item{obs_value}{Numerical observation value.}
#'   }
#'
#'   When \code{complete = TRUE}, all available columns from the
#'   Parquet data and lookup join are returned.
#'
#'   Returns \code{NULL} if no matching files are found or no files
#'   satisfy the validation register.
#'
#' @seealso
#'   \code{\link[arrow]{open_dataset}},
#'   \code{\link[dplyr]{filter}},
#'   \code{\link[dplyr]{left_join}}
#'
#' @importFrom arrow open_dataset
#' @importFrom cli cli_abort cli_alert_danger
#' @importFrom data.table fread
#' @importFrom dplyr filter left_join select mutate arrange collect
#' @importFrom dplyr if_else join_by pull
#' @importFrom rlang parse_exprs
#' @importFrom stringr str_extract str_detect fixed
#' @importFrom tibble as_tibble
#' @importFrom tidyr drop_na
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Retrieve validated quarterly data for Italy
#' dat <- nfsa_get_data_validated(
#'   country = "IT"
#' )
#'
#' # Retrieve validated quarterly data for several countries
#' dat <- nfsa_get_data_validated(
#'   country = c("IT", "FR", "DE"),
#'   table = "T0801"
#' )
#'
#' # Retrieve annual data
#' dat <- nfsa_get_data_validated(
#'   country = "IT",
#'   table = "T0800"
#' )
#'
#' # Filter by reference period
#' dat <- nfsa_get_data_validated(
#'   country = "IT",
#'   table = "T0801",
#'   filters = "time_period >= '2020Q1'"
#' )
#'
#' # Apply multiple filters
#' dat <- nfsa_get_data_validated(
#'   country = "IT",
#'   table = "T0801",
#'   filters = c(
#'     "time_period >= '2020Q1'",
#'     "sto == 'B1GQ'"
#'   )
#' )
#'
#' # Retrieve the complete dataset
#' dat <- nfsa_get_data_validated(
#'   country = "IT",
#'   table = "T0801SA",
#'   complete = TRUE
#' )
#'
#' }
nfsa_get_data_validated <- function(input_sel = "M:/nas/Rprod/copy_data/",
                              country,
                              table = "T0801",
                              filters = NULL,
                              complete = FALSE) {

  # 1. Setup Configuration -------------------------------------------------
  cfg_map <- list(
    "T0800"   = list(freq_code = "_A_", pattern = "NASEC_T0800_A"),
    "T0801"   = list(freq_code = "_Q_", pattern = "NASEC_T0801_Q"),
    "T0801SA" = list(freq_code = "_Q_", pattern = "NASEC_T0801SA_Q")
  )

  if (!table %in% names(cfg_map)) {
    cli::cli_abort("Invalid table: {table}. Choose T0800, T0801, or T0801SA.")
  }
  cfg <- cfg_map[[table]]

  # 2. File Discovery (All Versions) ---------------------------------------
  target_files <- list.files(path = input_sel,
                             pattern = cfg$pattern,
                             recursive = TRUE,
                             full.names = TRUE) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      ref_area = stringr::str_extract(value, paste0("(?<=", cfg$freq_code, ").."))
    ) |>
    dplyr::filter(ref_area %in% !!country)

  if (length(target_files) == 0) {
    cli::cli_alert_danger("No files found matching {cfg$pattern} for {country}")
    return(NULL)
  }


  files_sel <- data.table::fread("M:/nas/Rprod/assets/validated_files.csv") |>
    filter(ref_area %in% country,
           table == table)



  target_files <- left_join(target_files,files_sel,
                          relationship = "many-to-many",
                          by = join_by(ref_area)) |>
    filter(str_detect(value, fixed(filename))) |>
    distinct(value) |>
    pull(value)



  if (length(target_files) == 0) {
    cli::cli_alert_danger("No files found matching {cfg$pattern} for {country}")
    return(NULL)
  }

  # 3. Data Processing Pipeline --------------------------------------------
  lookup <- nfsa::nfsa_sto_lookup

  ds <- arrow::open_dataset(target_files)
  # Apply Arrow filters (Push-down)
  if (!is.null(filters)) {
    parsed_filters <- rlang::parse_exprs(filters)
    ds <- ds |> dplyr::filter(!!!parsed_filters)
  }

  res <- ds |>
    dplyr::collect()

  if (complete == FALSE){
    res <-res |>
      dplyr::left_join(lookup, by = c("counterpart_area", "ref_sector", "counterpart_sector",
                                      "consolidation", "accounting_entry", "sto",
                                      "instr_asset", "unit_measure", "prices")) |>
      tidyr::drop_na() |>
      # Note: version is included here as requested
      dplyr::select(ref_area, version, id, time_period, obs_value) |>
      dplyr::mutate(ref_area = dplyr::if_else(ref_area == "GR", "EL", ref_area))
  }

  if (complete == TRUE){
    res <-res |>
      dplyr::left_join(lookup, by = c("counterpart_area", "ref_sector", "counterpart_sector",
                                      "consolidation", "accounting_entry", "sto",
                                      "instr_asset", "unit_measure", "prices")) |>
      dplyr::arrange(time_period,ref_area,id) |>
      dplyr::mutate(ref_area = dplyr::if_else(ref_area == "GR", "EL", ref_area))
  }

  return(res)
}




