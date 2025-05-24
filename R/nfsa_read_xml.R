#' Read an xml file either passed as an argument or selected interactively
#'
#' The default path is "M:/Incoming_SDMX_files"
#'
#' @param file_sel the file to read
#' @param interactive if the user choose the file via the menu
#' @param folder_sel the default folder to look for xml files
#' @return a data frame
#' @importFrom rstudioapi selectFile
#' @importFrom janitor clean_names
#' @importFrom readsdmx read_sdmx
#' @import dplyr
#'
#' @export nfsa_read_xml
#'
#' @examples
#' tmp <- nfsa_read_xml()
#' tmp1 <- nfsa_read_xml(file_sel = "M:/Incoming_SDMX_files/NASEC_T0800_A_SK_2024_0000_V0003.xml",
#' interactive = FALSE)
nfsa_read_xml <- function(file_sel, folder_sel = "M:/Incoming_SDMX_files", interactive = TRUE){



  if (interactive == TRUE){

  file_sel <- rstudioapi::selectFile(caption = "Select an xml file ",
                                label = "Select",
                                path = folder_sel)

  data <-  readsdmx::read_sdmx(file_sel) |>
    janitor::clean_names() |>
    mutate(obs_value = as.numeric(obs_value))

  } else {
  data <-  readsdmx::read_sdmx(file_sel) |>
    janitor::clean_names() |>
    mutate(obs_value = as.numeric(obs_value))
  }
  return(data)
}

