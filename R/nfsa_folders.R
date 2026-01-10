#' @title Create NFSA Folders
#'
#' @description This function creates a directory structure for the NFSA project,
#' including folders for data, assets, output, and various subfolders within them.
#' The directory structure is created relative to the current working directory.
#'
#' @details The function uses the `fs` package to create the the directory structure:
#'
#' @return None. This function creates directories and does not return any value.
#'
#' @examples
#' \dontrun{
#' nfsa_folders()
#' # Creates the NFSA directory structure in the current working directory.
#' }
#'
#' @export
nfsa_folders <- function(){
library(fs)
dir_create(path= paste0(getwd(),"/assets"))
dir_create(path= paste0(getwd(),"/output"))
dir_create(path= paste0(getwd(),"/output/completeness"))
dir_create(path= paste0(getwd(),"/output/excel_template"))
dir_create(path= paste0(getwd(),"/output/flags"))
dir_create(path= paste0(getwd(),"/output/frequency"))
dir_create(path= paste0(getwd(),"/output/internal"))
dir_create(path= paste0(getwd(),"/output/inter_domain"))
dir_create(path= paste0(getwd(),"/output/logs"))
dir_create(path= paste0(getwd(),"/output/negatives_zeroes"))
dir_create(path= paste0(getwd(),"/output/plots"))
dir_create(path= paste0(getwd(),"/output/revisions"))
dir_create(path= paste0(getwd(),"/output/seas"))
dir_create(path= paste0(getwd(),"/output/sequence"))}
