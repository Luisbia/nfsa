#' Generate plots comparing unadjusted and seasonally adjusted data from two sources  for a given country.
#'
#' This function reads unadjusted and seasonally adjusted for a specified country, filters and processes the data, and generates time series plots
#' comparing the two sources for various statistical concepts (STO).
#' The plots are saved as a PDF file.
#'
#' @param country A character string specifying the country code (e.g., "AT"). This code is used to filter the data files.
#' @param output_sel A character string specifying the path to the directory where the generated PDF file should be saved. Defaults to `here("output", "plots")`.
#' @param time_min A character string specifying the earliest time period to include in the plots, in the format "YYYY-QX" (e.g., "1999-Q1"). Defaults to "1999-Q1".
#' @param my_theme A ggplot2 theme object to use for the plots. Defaults to `ggthemes::theme_fivethirtyeight()`.
#' @param my_colours Choose the colours for the lines, for example c("darkred","grey60")
#' @return None. The function generates and saves a PDF file containing the plots.
#'
#' @examples
#' \dontrun{
#' nfsa_q_plots_T_N_Y(country = "AT", output_sel = here("my_output"))
#' }
#'
#' @export
nfsa_q_plots_T_N_Y <- function(country,
                               output_sel = here::here("output", "plots"),
                               time_min = "1999-Q1",
                               my_theme = ggthemes::theme_fivethirtyeight(),
                               my_colours = c("#B656BD","#208486")){

  # Deprecation warning
  lifecycle::deprecate_soft(
    when = "0.2.0",
    what = "nfsa_q_plots_T_N_Y()",
    with = "nfsa_q_plots(comparison = 'adjusted_unadjusted')"
  )

  nfsa_q_plots(
    country = country,
    comparison = "adjusted_unadjusted",
    output_sel = output_sel,
    time_min = time_min,
    my_theme = my_theme,
    my_colours = my_colours
  )
}
