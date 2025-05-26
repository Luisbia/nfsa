

#' Report transmissions of a table for a period
#'
#' @param table_sel the table to select. Options are: "T0801_Q", "T0801SA_Q", "T0800_A"
#' @param country_sel country or countries for which to find transmissions. By default EU27.
#' @param period_sel the period to check.
#' @param output An excel file, or an html file or a chart
#'
#' @return an excel file, an html table or a ggplot
#' @export nfsa_report_transmissions
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @importFrom scales label_date
#' @importFrom forcats fct_rev
#' @examples
#' nfsa_report_transmissions (table_sel = "T0800_A", period_sel = "2024_0000")
nfsa_report_transmissions <- function(table_sel = "T0801_Q",
                                      country_sel= nfsa_eu27,
                                      period_sel = "2024_0004",
                                      output = "excel"){
library(dplyr)
library(ggplot2)



dat <- map(country_sel, ~nfsa::nfsa_find_xml(country_sel = .x, table_sel = table_sel, period_sel = period_sel,
  path_sel = "M:/Incoming_SDMX_files",archived = FALSE)) |>
  unlist() |>
  as_tibble() |>
  mutate(received = file.mtime(value)) |>
  tidyr::separate_wider_delim(value,
                              delim = "_",
                              names = c("root","SDMX","NASEC", "table", "period",
                                        "ref_area", "year", "quarter", "version"),
                              cols_remove = FALSE) |>
  mutate(version = stringr::str_remove_all(version,".xml")) |>
  select(file = value, table, ref_area, year, quarter,version,received) |>
  arrange(ref_area,version)

if (output == "excel") {
  nfsa_to_excel(dat)
}
if (output == "table") {
  nfsa_to_html(dat)
}

if (output == "chart") {
 ggplot(dat,aes(received,forcats::fct_rev(ref_area),colour = version))+
    geom_point(size = 5)+
    theme_minimal()+
    ylab("")+
    scale_x_datetime(labels = scales::label_date())
}

}
