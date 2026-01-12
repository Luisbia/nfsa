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

### NSA
new_nsa_server <- list.files("M:/nas/Rprod/data/q/new/nsa",
                             pattern = paste0("_",country,"_"),
                             full.names = TRUE,
                             recursive = FALSE )

file.copy(new_nsa_server,paste0(output_folder,"q/new/nsa"), 
          overwrite = FALSE, 
          copy.date = TRUE)

prev_nsa_server <- list.files("M:/nas/Rprod/data/q/prev/nsa",
                             pattern = paste0("_",country,"_"),
                             full.names = TRUE,
                             recursive = FALSE )

file.copy(prev_nsa_server,paste0(output_folder,"q/prev/nsa"), 
          overwrite = FALSE, 
          copy.date = TRUE)

### SCA
new_sca_server <- list.files("M:/nas/Rprod/data/q/new/sca",
                             pattern = paste0("_",country,"_"),
                             full.names = TRUE,
                             recursive = FALSE )

file.copy(new_sca_server,paste0(output_folder,"q/new/sca"), 
          overwrite = FALSE, 
          copy.date = TRUE)

prev_sca_server <- list.files("M:/nas/Rprod/data/q/prev/sca",
                              pattern = paste0("_",country,"_"),
                              full.names = TRUE,
                              recursive = FALSE )

file.copy(prev_sca_server,paste0(output_folder,"q/prev/sca"), 
          overwrite = FALSE, 
          copy.date = TRUE)

###A
new_a_server <- list.files("M:/nas/Rprod/data/a/new",
                             pattern = paste0("_",country,"_"),
                             full.names = TRUE,
                             recursive = FALSE )

file.copy(new_a_server,paste0(output_folder,"a/new"), 
          overwrite = FALSE, 
          copy.date = TRUE)

prev_a_server <- list.files("M:/nas/Rprod/data/a/prev",
                              pattern = paste0("_",country,"_"),
                              full.names = TRUE,
                              recursive = FALSE )

file.copy(prev_a_server,paste0(output_folder,"a/prev"), 
          overwrite = FALSE, 
          copy.date = TRUE)}
