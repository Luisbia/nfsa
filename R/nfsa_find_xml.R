
#' Find xml files
#'
#'It works for single selection of tables, countries and periods but it is easy to loop through it
#'if needed.
#' @param table_sel individual table to look for T0800_A, T0801_Q, T0801SA_Q.
#' @param country_sel individual country to look for.
#' @param period_sel reporting period. For example, 2024 annual data:2024_0000,
#'                   first quarter of 2024: 2024_0001
#' @param path_sel folder where to search.
#' @param archived by default it does not look at archived files but it can if set to TRUE and look
#'                 into archived files (up to 2022?). That may be a bit slow and probably is better
#'                 to modify the path_sel argument.
#'
#' @return a vector of file paths
#' @export nfsa_find_xml
#'
#' @examples
#'
#'nfsa_find_xml(table_sel = "T0801_Q",
#'country_sel = "SK",
#'period_sel = "2024_0004")
#'
#' # Find a file in the archive
#' nfsa_find_xml(table_sel = "T0801SA_Q",
#'country_sel = "SK",
#'period_sel = "2023_0001",
#'archived = TRUE)
#'
#'# Find more than one country (table or period)
#'
#'countries <- c("SK","DE")
#'purrr::map(countries,~nfsa_find_xml(country_sel = .x,
#'                                    table_sel = "T0801_Q",
#'                                    period_sel = "2024_0004" ))
#' #Look for several countries and tables
#'tables <- c("T0801_Q", "T0801SA_Q")
#'country_tables <- expand.grid(countries,tables)
#'purrr::map2(.x = country_tables$Var1,
#'            .y = country_tables$Var2,
#'            ~nfsa_find_xml(country_sel = .x,
#'                           table_sel = .y,
#'                           period_sel = "2024_0004"))

nfsa_find_xml <- function(table_sel,
                          country_sel,
                          period_sel,
                          path_sel = "M:/Incoming_SDMX_files",
                          archived = FALSE) {

files <- list.files(path = path_sel,
           pattern = paste0("NASEC_",table_sel,"_",country_sel,"_",period_sel,"_"),
           full.names = TRUE,
           recursive = archived)

return(files)
}


