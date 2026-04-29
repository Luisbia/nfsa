#' Synchtonise common files in `M:/nas/Rprod/assets` with local folder
#'Checks if newer versions are available and copied it to the local folder
#' @param source_dir "M:/nas/Rprod/assets"
#' @param target_dir here::here("assets")
#' @param recursive FALSE
#'
#' @returns copies more recent files in local folder
#' @export
#'
#' @examples
#' nfsa_asset_sync()
nfsa_asset_sync <- function(source_dir = "M:/nas/Rprod/assets" ,
                            target_dir = here::here("assets")  ,
                            recursive = FALSE) {

  library(fs)
  library(dplyr)
  library(cli)

  # --- Validate directories --------------------------------------------------
  if (!dir_exists(source_dir)) stop("Source directory does not exist: ", source_dir)
  if (!dir_exists(target_dir)) stop("Target directory does not exist: ", target_dir)

  cat("\n")
  cli_h1("Folder Sync Utility")
  cli_inform(c("i" = "Source : {.path {source_dir}}",
               "i" = "Target : {.path {target_dir}}"))
  cat("\n")

  # --- List files in both directories ----------------------------------------
  source_files <- dir_info(source_dir, recurse = recursive, type = "file") |>
    mutate(rel_path = path_rel(path, source_dir))

  target_files <- dir_info(target_dir, recurse = recursive, type = "file") |>
    mutate(rel_path = path_rel(path, target_dir))

  # --- Find files that exist in both and are newer in source -----------------
  candidates <- source_files |>
    inner_join(target_files, by = "rel_path", suffix = c("_src", "_tgt")) |>
    filter(modification_time_src > modification_time_tgt) |>
    select(
      rel_path,
      source_path     = path_src,
      target_path     = path_tgt,
      source_modified = modification_time_src,
      target_modified = modification_time_tgt
    )

  if (nrow(candidates) == 0) {
    cli_alert_success("No files to update - target is already up to date.")
    return(invisible(NULL))
  }

  # --- Display what was found ------------------------------------------------
  cli_alert_info("{nrow(candidates)} file{?s} in source {?is/are} newer than in target:\n")

  for (i in seq_len(nrow(candidates))) {
    row <- candidates[i, ]
    cli_bullets(c(
      "*" = "{.file {row$rel_path}}",
      " " = "  Source : {format(row$source_modified, '%Y-%m-%d %H:%M:%S')}",
      " " = "  Target : {format(row$target_modified, '%Y-%m-%d %H:%M:%S')}"
    ))
  }

  cat("\n")

  # --- Ask for global confirmation or file-by-file ---------------------------
  mode <- tolower(trimws(readline(
    "Copy all [A], choose file by file [F], or cancel [C]? "
  )))

  if (mode == "c") {
    cli_alert_warning("Operation cancelled. No files were copied.")
    return(invisible(NULL))
  }

  # --- Process each candidate ------------------------------------------------
  copied  <- character(0)
  skipped <- character(0)

  for (i in seq_len(nrow(candidates))) {
    row <- candidates[i, ]

    do_copy <- if (mode == "a") {
      TRUE
    } else {
      answer <- tolower(trimws(readline(
        sprintf("  Copy '%s'? [y/N] ", row$rel_path)
      )))
      answer == "y"
    }

    if (do_copy) {
      # Ensure target subdirectory exists (relevant when recursive = TRUE)
      dir_create(path_dir(row$target_path), recurse = TRUE)
      file_copy(row$source_path, row$target_path, overwrite = TRUE)
      copied <- c(copied, as.character(row$rel_path))
    } else {
      skipped <- c(skipped, as.character(row$rel_path))
    }
  }

  # --- Summary ---------------------------------------------------------------
  cat("\n")
  cli_h2("Summary")
  if (length(copied)  > 0) cli_alert_success("Copied  ({length(copied)})  : {.file {copied}}")
  if (length(skipped) > 0) cli_alert_info(   "Skipped ({length(skipped)}) : {.file {skipped}}")

  return(invisible(list(copied = copied, skipped = skipped)))
}


