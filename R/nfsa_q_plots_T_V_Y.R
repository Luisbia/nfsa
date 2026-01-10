#' Generate plots comparing seasonally adjusted data from two sources (T and V) for a given country.
#'
#' This function reads seasonally adjusted data from two different sources ("T" and "V")
#' for a specified country, filters and processes the data, and generates time series plots
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
#' nfsa_q_plots_T_V_Y(country = "AT", output_sel = here("my_output"))
#' }
#'
#' @export
nfsa_q_plots_T_V_Y <- function(country,
                               output_sel = here("output", "plots"),
                               time_min = "1999-Q1",
                               my_theme = ggthemes::theme_fivethirtyeight(),
                               my_colours = c("#B656BD","#208486")){
  library(here)
  library(ggthemes)
  library(tidyverse)
  library(gridExtra)
  library(arrow)
  library(lubridate)


  lookup <- nfsa::nfsa_sto_lookup
  sto_label <- nfsa::nfsa_sto_label

my_scale <- scale_color_manual(values = c("T" = my_colours[1], "V" = my_colours[2]))


cli::cli_progress_message("Collecting data...")

tmp_t <- nfsa::nfsa_get_data(country = country, table = "T0801SA", type = "new") |>
  filter(obs_value != "NaN") %>%
  filter(time_period >= time_min) |>
  nfsa::nfsa_separate_id() |>
  mutate(source = "T",
         time_period = lubridate::yq(time_period))

if(nrow(tmp_t) == 0) stop(paste0("No seasonaly adjusted file for ", country))


tmp_v<- nfsa::nfsa_get_data(country = country, table = "T0801SA", type = "prev") |>
  filter(obs_value != "NaN") |>
  filter(time_period >= time_min) |>
  nfsa::nfsa_separate_id() |>
  mutate(source = "V",
         time_period = lubridate::yq(time_period))


if(nrow(tmp_v) == 0) stop(paste0("No seasonaly adjusted file for ", country))


tmp <- bind_rows(tmp_t, tmp_v) %>%
  na.omit() %>%
  left_join(.,sto_label, by = join_by(sto)) |>
  mutate(sto = paste0(sto,".",accounting_entry,"-",sto_label)) |>
  select(-sto_label) |>
  group_nest(ref_area,sto)

cli::cli_progress_message("Creating charts...")

charts <- tmp |>
  transmute(chart= map2(data,sto, ~ggplot(.x)+
                          geom_line(aes(time_period,obs_value, colour = source),linewidth = 0.65)+
                          facet_wrap(~ref_sector)+ #, scales="free_y"
                          my_theme+
                          my_scale+
                          scale_y_continuous(labels = scales::label_number(),position = "right")+
                          ggtitle(paste0(.y))+
                          ylab("")+ xlab("")+
                          theme(axis.ticks.y = element_blank())
  )
  ) |>
  deframe()

cli::cli_progress_message("Generating file...")


ggsave(
  filename = paste0(output_sel,"/", country,"_q_T_V_Y.pdf"),
  plot = marrangeGrob(charts, nrow=1, ncol=1),
  width = 15, height = 9
)
cli::cli_alert_success(paste0("Charts created in ",output_sel,"/", country,"_q_T_V_Y.pdf"))
}

