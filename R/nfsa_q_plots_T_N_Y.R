#' Generate plots comparing unadjusted and seasonally adjusted data from two sources  for a given country.
#'
#' This function reads unadjusted and seasonally adjusted for a specified country, filters and processes the data, and generates time series plots
#' comparing the two sources for various statistical concepts (STO).
#' The plots are saved as a PDF file.
#'
#' @param country_sel A character string specifying the country code (e.g., "AT"). This code is used to filter the data files.
#' @param output_sel A character string specifying the path to the directory where the generated PDF file should be saved. Defaults to `here("output", "plots")`.
#' @param time_min A character string specifying the earliest time period to include in the plots, in the format "YYYY-QX" (e.g., "1999-Q1"). Defaults to "1999-Q1".
#' @param my_theme A ggplot2 theme object to use for the plots. Defaults to `ggthemes::theme_fivethirtyeight()`.
#'
#' @return None. The function generates and saves a PDF file containing the plots.
#'
#' @examples
#' \dontrun{
#' nfsa_q_plots_T_N_Y(country_sel = "AT", output_sel = here("my_output"))
#' }
#'
#' @export
nfsa_q_plots_T_N_Y <- function(country_sel,
                               output_sel = here("output", "plots"),
                               time_min = "1999-Q1",
                               my_theme = ggthemes::theme_fivethirtyeight()){

  library(here)
  library(ggthemes)
  library(tidyverse)
  library(gridExtra)
  library(arrow)
  library(lubridate)
my_scale <- scale_color_manual(values = c("Y" = "#B656BD", "N" = "#208486"))
lookup <- nfsa::nfsa_sto_lookup
sto_label <- nfsa::nfsa_sto_label

cli::cli_progress_message("Collecting data...")

tmp_n <- nfsa::nfsa_get_data(country_sel = country_sel, table_sel = "T0801", type = "new") |>
  select(ref_area,id,time_period,obs_value) |>
  separate_wider_delim(id,delim = ".", names = c("ref_sector","sto","accounting_entry")) |>
  filter(obs_value != "NaN") |>
  filter(time_period >= time_min) |>
  mutate(source = "T",
         time_period = lubridate::yq(time_period),
         adjustment = "N")

tmp_y <- nfsa::nfsa_get_data(country_sel = country_sel, table_sel = "T0801SA", type = "new") |>
  select(ref_area,id,time_period,obs_value) |>
  separate_wider_delim(id,delim = ".", names = c("ref_sector","sto","accounting_entry")) |>
  filter(obs_value != "NaN") |>
  filter(time_period >= time_min) |>
  mutate(source = "T",
         time_period = lubridate::yq(time_period),
         adjustment = "Y")

if(nrow(tmp_y) == 0) stop(paste0("No seasonaly adjusted file for ", country_sel))



tmp <- bind_rows(tmp_n, tmp_y) %>%
  na.omit() %>%
  left_join(.,sto_label,by = join_by(sto)) |>
    mutate(sto = paste0(sto, ".",accounting_entry,"-",sto_label)) |>
  select(-sto_label,-accounting_entry) |>
  pivot_wider(names_from = adjustment,
              values_from = obs_value) |>
  na.omit() |>
  pivot_longer(cols = c(Y,N),
               names_to = "adjustment",
               values_to = "obs_value") |>
  group_nest(ref_area,sto)


cli::cli_progress_message("Creating charts...")

charts <- tmp |>
  transmute(chart= map2(data,sto, ~ggplot(.x)+
                          geom_line(aes(time_period,obs_value, colour = adjustment),linewidth = 0.65)+
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
  filename = paste0(output_sel,"/", country_sel,"_q_T_N_Y.pdf"),
  plot = marrangeGrob(charts, nrow=1, ncol=1),
  width = 15, height = 9
)
cli::cli_alert_success(paste0("Charts created in ",output_sel,"/", country_sel,"_q_T_N_Y.pdf"))
}

