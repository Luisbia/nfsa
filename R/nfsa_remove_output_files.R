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
nfsa_remove_output_files <- function(path = here::here("output")) {

  # 1. Check if the directory exists
  if (!fs::dir_exists(path)) {
    cli::cli_alert_warning("Path {.path {path}} not found.")
    return(invisible(NULL))
  }

  # 2. List ONLY files (recurse to find files inside subfolders)
  all_contents <- fs::dir_ls(path, recurse = TRUE, all = TRUE)
  files_to_delete <- all_contents[fs::is_file(all_contents)]

  # 3. Handle empty case
  if (length(files_to_delete) == 0) {
    cli::cli_alert_info("No files found to delete in {.path {path}}.")
    return(invisible(path))
  }

  # 4. Calculate total size before deleting (1024^2 for MB)
  total_bytes <- sum(fs::file_size(files_to_delete))
  total_mb <- round(as.numeric(total_bytes) / 1024^2, 2)

  # 5. Delete the files
  fs::file_delete(files_to_delete)

  # 6. Report to user
  cli::cli_alert_success(
    "Deleted {length(files_to_delete)} file(s) totaling {.val {total_mb}} MB. Folders preserved."
  )

  return(invisible(path))
}
