#' Read an xml file or several xml files
#'
#' Pass a list of files or select one interactively.
#'
#' @param file_sel the file(s) to read
#' @param interactive if the user choose the file via the menu
#' @param folder_sel the folder to look for xml files
#' @return a data frame
#' @importFrom rstudioapi selectFile
#' @importFrom janitor clean_names
#' @importFrom readsdmx read_sdmx
#' @importFrom purrr set_names
#' @importFrom purrr map
#' @importFrom purrr list_rbind
#' @import dplyr
#'
#' @export nfsa_read_xml
#'
#' @examples
#' tmp <- nfsa_read_xml()
#' tmp1 <- nfsa_read_xml(file_sel = "M:/Incoming_SDMX_files/NASEC_T0800_A_SK_2024_0000_V0003.xml",
#' interactive = FALSE)
nfsa_read_xml <- function(file_sel, folder_sel, interactive = TRUE){
library(dplyr)


  if (interactive == TRUE){

 file_sel <- rstudioapi::selectFile(caption = "Select ONE xml file",
                        label = "Select",
                        filter = "xml files (xml)")

 data <- readsdmx::read_sdmx(file_sel) |>
   janitor::clean_names() |>
   mutate(obs_value = as.numeric(obs_value))

  return(data)

  } else {
  data <-  purrr::set_names(file_sel) |>
    purrr::map(readsdmx::read_sdmx) |>
    purrr::list_rbind(names_to = "file") |>
    janitor::clean_names() |>
    mutate(obs_value = as.numeric(obs_value))

  rm(file_sel)

  return(data)
  }

}

