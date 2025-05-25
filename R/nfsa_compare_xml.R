#' Compare the content of different xml files
#'
#' @param paths_sel paths to the files
#' @importFrom nfsa nfsa_read_xml
#' @import dplyr
#'
#' @return a data frame
#' @export nfsa_compare_xml
#'
#'@examples
#' files_sel <- nfsa_find_xml(table_sel = "T0801_Q",country_sel = "BE",period_sel = "2024_0004")
#' tmp <- nfsa_compare_xml(files_sel)
#'
nfsa_compare_xml <- function(paths_sel){
library(dplyr)

dat <- nfsa::nfsa_read_xml(paths_sel, interactive = FALSE) |>
  group_by(across(c(-file,-obs_value))) |>
  arrange(file,.by_group = TRUE) |>
  mutate(change = obs_value-lag(obs_value)) |>
  filter(change != 0) |>
  ungroup()
reurn(dat)
}


