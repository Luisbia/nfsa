#' Compare two xml files from the same country
#'
#' @param path_new_file  the most recent file
#' @param path_prev_file the file to compare with
#' @param interactive if the user choose the files via the menu
#' @importFrom rstudioapi selectFile
#' @importFrom janitor clean_names
#' @importFrom readsdmx read_sdmx
#' @import dplyr
#' @import tidyr
#'
#' @return a temporary excel file
#' @export nfsa_compare_xml
#'
nfsa_compare_xml <- function(path_new_file,
                             path_prev_file,
                             interactive = TRUE,
                             folder_sel = "M:/Incoming_SDMX_files"){

if (interactive == TRUE){

 path_new_file <- rstudioapi::selectFile(caption = "Select an xml file ",
                                       label = "Select",
                                       path = folder_sel,
                                       filter = ".xml")

 new_file <- readsdmx::read_sdmx(path_new_file) |>
    janitor::clean_names() %>%
    rename(new=obs_value) %>%
    mutate(new=as.numeric(new))

  path_prev_file <- rstudioapi::selectFile(caption = "Select an xml file ",
                                           label = "Select",
                                           path = folder_sel,
                                           filter = ".xml")

  prev_file <- readsdmx::read_sdmx(path_prev_file) |>
    janitor::clean_names() %>%
    rename(prev = obs_value) %>%
    mutate(prev = as.numeric(prev))

  full_join(new_file,prev_file) %>%
    mutate(change = new - prev) %>%
    filter(change !=0) |>
    nfsa::nfsa_to_excel()
} else {

  new_file <- readsdmx::read_sdmx(path_new_file) |>
    janitor::clean_names() %>%
    rename(new=obs_value) %>%
    mutate(new=as.numeric(new))

  prev_file <- readsdmx::read_sdmx(path_prev_file) |>
    janitor::clean_names() %>%
    rename(prev = obs_value) %>%
    mutate(prev = as.numeric(prev))

  full_join(new_file,prev_file) %>%
    mutate(change = new - prev) %>%
    filter(change !=0) |>
    nfsa::nfsa_to_excel()
}
}

