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
#'   in `obs_value` are removed, and only
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
#'   observation columns and removes rows with missing `obs_value`.
#'   If \code{TRUE}, retains all columns and does not explicitly
#'   remove missing values.
#'
#' @param ref_period Optional character vector of transmission reference periods.
#'   Accepts quarterly periods such as `"2026Q2"` or `"2026_0002"`,
#'   annual periods such as `"2026"` or `"2026_0000"`, and vectors
#'   of periods. `NULL` (default) selects all reference periods.
#'   Internally, these are normalized to the filename format `YYYY_NNNN`.
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
#'
#' @importFrom arrow open_dataset
#' @importFrom cli cli_abort cli_alert_danger
#' @importFrom data.table fread
#' @importFrom dplyr arrange collect distinct filter if_else left_join mutate pull select
#' @importFrom rlang .data parse_exprs
#' @importFrom stringr fixed str_detect str_match str_replace
#' @importFrom tibble tibble
#' @importFrom tidyr drop_na
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
#' # Select one or several transmission reference periods
#' nfsa_get_data_validated(country = "IT", ref_period = "2026Q2")
#' nfsa_get_data_validated(country = "IT", ref_period = c("2025Q1", "2025Q2"))
#' nfsa_get_data_validated(country = "IT", table = "T0800", ref_period = "2026")
#'
#' }
nfsa_get_data_validated <- function(input_sel = "M:/nas/Rprod/db/",
                                    country,
                                    table = "T0801",
                                    filters = NULL,
                                    complete = FALSE,
                                    ref_period = NULL) {

  # Accept both readable periods (2026Q2, 2026) and filename periods
  # (2026_0002, 2026_0000). Keep filename convention internally.
  normalize_ref_period <- function(x) {
    if (is.null(x)) return(NULL)
    if (!is.character(x) || anyNA(x) ||
        any(!grepl("^\\d{4}(_000[0-4]|Q[1-4])?$", x, perl = TRUE))) {
      cli::cli_abort(
        "`ref_period` must contain YYYY, YYYYQ1-YYYYQ4, or YYYY_0000-YYYY_0004."
      )
    }
    x <- stringr::str_replace(x, "^([0-9]{4})Q([1-4])$", "\\1_000\\2")
    x <- stringr::str_replace(x, "^([0-9]{4})$", "\\1_0000")
    unique(x)
  }
  ref_period <- normalize_ref_period(ref_period)

  cfg_map <- list(
    T0800   = list(freq_code = "A", pattern = "NASEC_T0800_A"),
    T0801   = list(freq_code = "Q", pattern = "NASEC_T0801_Q"),
    T0801SA = list(freq_code = "Q", pattern = "NASEC_T0801SA_Q")
  )
  if (length(table) != 1L || is.na(table) || !table %in% names(cfg_map)) {
    cli::cli_abort("Invalid table: {table}. Choose T0800, T0801, or T0801SA.")
  }
  if (!is.logical(complete) || length(complete) != 1L || is.na(complete)) {
    cli::cli_abort("`complete` must be TRUE or FALSE.")
  }
  cfg <- cfg_map[[table]]

  # Discover local Parquet files, including those in subfolders.
  files <- list.files(
    path = input_sel,
    pattern = paste0("^", cfg$pattern, "_.*\\.parquet$"),
    recursive = TRUE,
    full.names = TRUE
  )

  # Filename convention: NASEC_T0801_Q_PL_2026_0002_V0002.parquet
  filename_regex <- paste0(
    "^", cfg$pattern,
    "_([A-Z]{2})_([0-9]{4}_[0-9]{4})_V([0-9]+)\\.parquet$"
  )
  matches <- stringr::str_match(basename(files), filename_regex)
  target_files <- tibble::tibble(
    value = files,
    ref_area = matches[, 2],
    ref_period_file = matches[, 3]
  ) |>
    dplyr::filter(.data$ref_area %in% .env$country,
                  !is.na(.data$ref_period_file))

  if (!is.null(ref_period)) {
    target_files <- target_files |>
      dplyr::filter(.data$ref_period_file %in% .env$ref_period)
  }

  if (nrow(target_files) == 0L) {
    cli::cli_alert_danger("No {table} Parquet files found for the requested countries/periods.")
    return(NULL)
  }

  # Retain only files recorded in the validation register.
  files_sel <- data.table::fread("M:/nas/Rprod/assets/validated_files.csv") |>
    dplyr::filter(.data$ref_area %in% .env$country,
                  .data$table == .env$table)

  # Match to registered filenames within each country. Keep the existing
  # substring-matching convention to support the validation register format.
  target_files <- target_files |>
    dplyr::left_join(files_sel, by = "ref_area", relationship = "many-to-many") |>
    dplyr::filter(!is.na(.data$filename),
                  stringr::str_detect(.data$value, stringr::fixed(.data$filename))) |>
    dplyr::distinct(.data$value) |>
    dplyr::pull(.data$value)

  if (length(target_files) == 0L) {
    cli::cli_alert_danger("No validated {table} files found for the requested countries/periods.")
    return(NULL)
  }

  # Apply filters in Arrow, before collecting.
  ds <- arrow::open_dataset(target_files)
  if (!is.null(filters)) {
    parsed_filters <- rlang::parse_exprs(filters)
    ds <- dplyr::filter(ds, !!!parsed_filters)
  }
  res <- dplyr::collect(ds) |>
    dplyr::left_join(
      nfsa::nfsa_sto_lookup,
      by = c("counterpart_area", "ref_sector", "counterpart_sector",
             "consolidation", "accounting_entry", "sto",
             "instr_asset", "unit_measure", "prices")
    ) |>
    dplyr::mutate(ref_area = dplyr::if_else(.data$ref_area == "GR", "EL", .data$ref_area))

  if (!complete) {
    # Do not drop rows due to optional metadata columns containing NA.
    res <- res |>
      tidyr::drop_na(.data$obs_value) |>
      dplyr::select(ref_area, version, id, time_period, obs_value)
  } else {
    res <- dplyr::arrange(res, time_period, ref_area, id)
  }
  res
}
