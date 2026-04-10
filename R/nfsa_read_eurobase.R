#' Read and parse Eurobase flat files for NFSA data
#'
#' Scans a directory for Eurobase flat files (plain or zipped) matching a given
#' type, filters them by file modification date, and parses them into a single
#' tidy tibble. Files that are malformed or contain no data rows are silently
#' skipped.
#'
#' @param type `character(1)`. Pattern used to match filenames in `path`.
#'   Matched as `*<type>*.zip`.
#'   Defaults to `"nasq_10_ki_partial_Y"`. Other options are `"nasq_10_ki_partial_N"`,
#'   `"nasq_10_nf_tr_N"`,`"nasq_10_nf_tr_Y"`, `"AT_nasa_10_nf_tr_N"`....
#' @param time_min `Date` or `character(1)` coercible to a Date via
#'   [lubridate::as_date()]. Only files with a modification date on or after
#'   this value are parsed. Defaults to [lubridate::today()].
#' @param time_max `Date` or `character(1)` coercible to a Date via
#'   [lubridate::as_date()]. Only files with a modification date on or before
#'   this value are parsed. Defaults to [lubridate::today()].
#' @param path `character(1)`. Directory to scan for matching files.
#'   Defaults to `"I:/econ/NFSA/ref/sent2eurobase/"`.
#'
#' @return A tibble with one row per data observation, containing:
#'   \describe{
#'     \item{`file`}{Full path of the source file.}
#'     \item{`sent`}{File modification time (`POSIXct`).}
#'     \item{`TIME`, `GEO`, `NA_ITEM`, ...}{Dimension columns parsed from the
#'       `FIELDS=` header line of each file. Column names vary by file type.}
#'     \item{`VALUE`}{Numeric observation value.}
#'     \item{`FLAG`}{Optional quality flag parsed from a `~`-delimited suffix
#'       on the value field (e.g. `48.44~p`); `NA` if absent.}
#'   }
#'   Returns an empty tibble (with a warning) if no matching files are found.
#'
#' @details
#' ## File format
#' The function expects Eurobase flat files with the following structure:
#' ```
#' FLAT_FILE=STANDARD
#' ...
#' UPDATE_MODE=RECORDS
#' FIELDS=TIME,GEO,NA_ITEM,...
#' 1999Q01 IT B2G_B3G_RAT_S11 ...  48.44
#' ...
#' END_OF_FLAT_FILE
#' ```
#' Data rows are read from the line after the last `FIELDS=` header to the line
#' before `END_OF_FLAT_FILE`. If `UPDATE_MODE=RECORDS` is present, the `FIELDS=`
#' line immediately following it is used; otherwise the last `FIELDS=` line in
#' the file is used as a fallback.
#'
#' ## Zip support
#' Files with a `.zip` extension are handled transparently via [base::unz()].
#' If the archive contains multiple files, the first one is used.
#'
#' ## Error handling
#' Files that cannot be parsed (e.g. missing markers, empty data sections) are
#' skipped via [purrr::possibly()] rather than aborting the entire run.
#'
#' @examples
#' \dontrun{
#' # Parse files sent today (default)
#' df <- nfsa_read_eurobase()
#'
#' # Parse files of a specific type sent on a given day
#' df <- nfsa_read_eurobase(
#'   type     = "nasq_10_ki_partial_Y",
#'   time_min = "2026-04-09",
#'   time_max = "2026-04-09"
#' )
#'}
#'
#'
#' @export
nfsa_read_eurobase <- function(
    type     = "nasq_10_ki_partial_Y",
    time_min = lubridate::today(),
    time_max = lubridate::today(),
    path     = "I:/econ/NFSA/ref/sent2eurobase/"
) {

  library(tidyverse)
  # --- Input validation -------------------------------------------------------
  time_min <- lubridate::as_date(time_min)  # accept both strings and Date objects
  time_max <- lubridate::as_date(time_max)
  if (time_min > time_max) stop("`time_min` must not be later than `time_max`.")

  # --- Inner parser -----------------------------------------------------------
  parse_flat_file <- function(filepath, zip_filename = NULL) {
    # Read lines from zip or plain file
    if (grepl("\\.zip$", filepath, ignore.case = TRUE)) {
      if (is.null(zip_filename)) {
        zip_filename <- unzip(filepath, list = TRUE)$Name[1]
      }
      con   <- unz(filepath, zip_filename)
      lines <- readLines(con, warn = FALSE)  # suppress incomplete final line warnings
      close(con)
    } else {
      lines <- readLines(filepath, warn = FALSE)
    }

    # Find boundaries
    records_marker <- which(lines == "UPDATE_MODE=RECORDS")
    fields_marker  <- if (length(records_marker) > 0) {
      records_marker + 1
    } else {
      tail(which(grepl("^FIELDS=", lines)), 1)
    }
    end_marker <- which(lines == "END_OF_FLAT_FILE")

    # Guard against malformed files
    if (length(fields_marker) == 0) stop("No FIELDS= line found in: ", filepath)
    if (length(end_marker)    == 0) stop("No END_OF_FLAT_FILE found in: ", filepath)

    col_names  <- c(strsplit(sub("^FIELDS=", "", lines[fields_marker]), ",")[[1]], "VALUE")
    data_lines <- lines[(fields_marker + 1):(end_marker - 1)]

    # Return NULL (instead of erroring) if the data section is empty
    if (length(data_lines) == 0) return(NULL)

    read.table(
      text      = data_lines,
      header    = FALSE,
      col.names = col_names,
      fill      = TRUE,
      quote     = ""
    ) |>
      as_tibble() |>
      separate_wider_delim(
        cols  = VALUE,
        delim = "~",
        names = c("VALUE", "FLAG"),
        too_few = "align_start"
      ) |>
      mutate(VALUE = as.numeric(VALUE)) |>  # convert here, once, not after unnest
      filter(!is.na(VALUE))
  }

  # --- File discovery ---------------------------------------------------------
  files <- list.files(
    path      = path,
    pattern   = glob2rx(paste0("*", type, "*.zip")),
    full.names = TRUE
  )

  if (length(files) == 0) {
    warning("No files found for type '", type, "' in: ", path)
    return(tibble())  # return empty tibble rather than crashing
  }

  # --- Build result -----------------------------------------------------------
  tibble(file = files) |>
    mutate(sent = file.mtime(file)) |>
    filter(
      as_date(sent) >= time_min,
      as_date(sent) <= time_max
    ) |>
    (\(df) { message("Parsing ", nrow(df), " file(s)..."); df })() |>  # print & pass through
    mutate(data = map(file, possibly(parse_flat_file, otherwise = NULL))) |>
    filter(!map_lgl(data, is.null)) |>
    unnest(cols = data)
}


