#' Reads and cleans NFSA data from an SDMX-ML file.
#'
#' This function reads an SDMX-ML file containing NFSA data using the `readsdmx` package,
#' cleans the column names using `janitor`, and converts the `obs_value` column to numeric.
#'
#' @param file The path to the SDMX-ML file.
#'
#' @return A data frame containing the cleaned NFSA data.
#'
#' @examples
#' \dontrun{
#' # Assuming you have an SDMX-ML file named "nfsa_data.xml" in your working directory
#' nfsa_data <- nfsa_read_xml("nfsa_data.xml")
#'
#' # Print the first few rows of the data
#' head(nfsa_data)
#' }
#'
#' @export
nfsa_read_xml <- function(file){
  library(readsdmx)

  data <- readsdmx::read_sdmx(file) |>
    janitor::clean_names() |>
    mutate(obs_value = as.numeric(obs_value))
}
