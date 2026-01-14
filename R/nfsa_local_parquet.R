#' @title Identify and copy NFSA parquet files
#'
#' @description This function identifies and copies NFSA  parquet files from a server location to a local output folder.
#'
#' @param country A character vector specifying the country code (e.g., "BE").
#' @param output_folder A character string specifying the local folder to copy the files to.
#'
#' @details This function copies NSA, SCA and A files (both new and previous) from a server location to a local folder. The country code is used to filter the files to copy.
#' The overwrite argument is set to FALSE, and the copy.date argument is set to TRUE in `file.copy`.
#' The global `output_folder` variable needs to be defined outside the function
#' @return None. The function copies files as a side effect.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' # Assuming output_folder is already defined, like output_folder <- "C:/users/biedmlu/R/QNFSA/Rprod/data/"
#' nfsa_local_parquet(country = "BE", output_folder = "C:/users/biedmlu/R/QNFSA/Rprod/data/")
#' }
#'
#' @export
nfsa_local_parquet <- function(country,
                                    output_folder){

# 1. Define your folder mapping
# This table maps the relative path to the server path
folders <- data.frame(
  rel_path = c("q/new/nsa", "q/prev/nsa", "q/new/sca", "q/prev/sca", "a/new", "a/prev"),
  stringsAsFactors = FALSE
)

# 2. Loop through each row and perform the copy
for (i in 1:nrow(folders)) {
  
  # Construct full paths
  source_path <- paste0("M:/nas/Rprod/data/", folders$rel_path[i])
  target_path <- paste0(output_folder, folders$rel_path[i])
  
  # Find files
  files_to_copy <- list.files(
    path = source_path,
    pattern = paste0("_", country, "_"),
    full.names = TRUE,
    recursive = FALSE
  )
  
  # Check if files exist before trying to copy
  if (length(files_to_copy) > 0) {
    
    # Attempt copy
    results <- file.copy(files_to_copy, target_path, overwrite = FALSE, copy.date = TRUE)
    
    # Report back to user
    success_count <- sum(results)
    filenames <- basename(files_to_copy)
    
    # Print the specific files moved for this folder
    for (j in seq_along(results)) {
      if (results[j]) {
        message("COPIED: ", filenames[j])
      } 
    }
    
  } else {
    message("SKIP: No files found for ", folders$rel_path[i], " with country: ", country)
  }
}
}