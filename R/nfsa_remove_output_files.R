#' Remove files from a directory while preserving folder structure
#'
#' @description
#' This function scans a specified directory recursively and deletes all files
#' found, while leaving the subfolder hierarchy intact. It provides a summary
#' of the number of files deleted and the total disk space cleared.
#'
#' @param path A character string representing the directory path.
#' Defaults to the "output" folder in the project root using `here::here()`.
#'
#' @return Returns the input path invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' nfsa_remove_output_files("output/plots")
#' }
#' Remove files from a directory with user confirmation
#'
#' @description
#' Scans a directory recursively and deletes all files while preserving
#' folder structure. Includes a safety prompt showing the file count
#' and total size before execution.
#'
#' @param path A character string. Defaults to `here::here("output")`.
#'
#' @return Returns the input path invisibly.
#' @export
nfsa_remove_output_files <- function(path = here::here("output")) {

  # 1. Existence Check
  if (!fs::dir_exists(path)) {
    cli::cli_alert_warning("Path {.path {path}} not found.")
    return(invisible(NULL))
  }

  # 2. Identify Files
  all_contents <- fs::dir_ls(path, recurse = TRUE, all = TRUE)
  files_to_delete <- all_contents[fs::is_file(all_contents)]

  # 3. Handle Empty Case
  if (length(files_to_delete) == 0) {
    cli::cli_alert_info("No files found in {.path {path}}.")
    return(invisible(path))
  }

  # 4. Calculate Stats
  total_bytes <- sum(fs::file_size(files_to_delete))
  total_mb <- round(as.numeric(total_bytes) / 1024^2, 2)

  # 5. Interactive Confirmation Gate
  if (interactive()) {
    msg <- sprintf(
      "Are you sure you want to delete %d files (%.2f MB) in '%s'?",
      length(files_to_delete), total_mb, path
    )

    # askYesNo returns TRUE, FALSE, or NA (Cancel)
    proceed <- utils::askYesNo(msg, default = FALSE)

    if (!isTRUE(proceed)) {
      cli::cli_alert_danger("Operation cancelled by user.")
      return(invisible(path))
    }
  }

  # 6. Execution
  fs::file_delete(files_to_delete)
  cli::cli_alert_success(
    "Successfully deleted {length(files_to_delete)} file(s) ({total_mb} MB)."
  )

  return(invisible(path))
}
