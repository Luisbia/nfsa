#' @title Create NFSA Folders
#'
#' @description This function creates a directory structure for the NFSA project,
#' including folders for data, assets, output, and various subfolders within them.
#' The directory structure is created relative to the current working directory.
#'
#' @details The function uses the `fs` package to create the following directory structure:
#' \itemize{
#'   \item{\code{data/}}
#'   \item{\code{data/a/new/}}
#'   \item{\code{data/a/prev/}}
#'   \item{\code{data/q/new/}}
#'   \item{\code{data/q/prev/}}
#'   \item{\code{data/q/new/nsa/}}
#'   \item{\code{data/q/prev/nsa/}}
#'   \item{\code{data/q/new/sca/}}
#'   \item{\code{data/q/prev/sca/}}
#'   \item{\code{assets/}}
#'   \item{\code{output/}}
#'   \item{\code{output/completeness/}}
#'   \item{\code{output/frequency/}}
#'   \item{\code{output/inter_domain/}}
#'   \item{\code{output/logs/}}
#'   \item{\code{output/negatives_zeroes/}}
#'   \item{\code{output/plots/}}
#'   \item{\code{output/revisions/}}
#'   \item{\code{output/seas/}}
#'   \item{\code{output/sequence/}}
#' }
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
dir_create(path= paste0(getwd(),"/data"))
dir_create(path= paste0(getwd(),"/data/a"))
dir_create(path= paste0(getwd(),"/data/a/new"))
dir_create(path= paste0(getwd(),"/data/a/prev"))
dir_create(path= paste0(getwd(),"/data/q"))
dir_create(path= paste0(getwd(),"/data/q/new"))
dir_create(path= paste0(getwd(),"/data/q/prev"))
dir_create(path= paste0(getwd(),"/data/q/new/nsa"))
dir_create(path= paste0(getwd(),"/data/q/prev/nsa"))
dir_create(path= paste0(getwd(),"/data/q/new/sca"))
dir_create(path= paste0(getwd(),"/data/q/prev/sca"))
dir_create(path= paste0(getwd(),"/assets"))
dir_create(path= paste0(getwd(),"/output"))
dir_create(path= paste0(getwd(),"/output/completeness"))
dir_create(path= paste0(getwd(),"/output/flags"))
dir_create(path= paste0(getwd(),"/output/frequency"))
dir_create(path= paste0(getwd(),"/output/inter_domain"))
dir_create(path= paste0(getwd(),"/output/logs"))
dir_create(path= paste0(getwd(),"/output/negatives_zeroes"))
dir_create(path= paste0(getwd(),"/output/plots"))
dir_create(path= paste0(getwd(),"/output/revisions"))
dir_create(path= paste0(getwd(),"/output/seas"))
dir_create(path= paste0(getwd(),"/output/sequence"))}
