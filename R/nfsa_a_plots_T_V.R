#' Generate plots comparing not adjusted data from two sources (T and V) for a given country.
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
#'
#' @return None. The function generates and saves a PDF file containing the plots.
#'
#' @examples
#' \dontrun{
#' nfsa_q_plots_T_V_N(country = "AT", output_sel = here("my_output"))
#' }
#'
#' @export
nfsa_a_plots_T_V <- function(country,
                             output_sel = here::here("output", "plots"),
                             time_min = 1995,
                             my_theme = ggthemes::theme_fivethirtyeight(),
                             my_colours = c("#B656BD","#208486")){

  lookup <- nfsa::nfsa_sto_lookup
  sto_label <- nfsa::nfsa_sto_label

  my_scale <- ggplot2::scale_color_manual(values = c("T" = my_colours[1], "V" = my_colours[2]))

  cli::cli_progress_message("Collecting data...")

tmp_t <- nfsa::nfsa_get_data(country = country,
                             table = "T0800",
                             type = "new") |>
 nfsa::nfsa_separate_id() |>
  dplyr::filter(obs_value != "NaN") |>
  dplyr::filter(time_period >= time_min) |>
  dplyr::mutate(source = "T",
         time_period = as.integer(time_period))

tmp_v<- nfsa::nfsa_get_data(country = country,
                            table = "T0800",
                            type = "prev") |>
  dplyr::filter(time_period >= time_min) |>
  nfsa::nfsa_separate_id() |>
  dplyr::filter(obs_value != "NaN") |>
  dplyr::mutate(source = "V",
         time_period = as.integer(time_period))

tmp <- dplyr::bind_rows(tmp_t, tmp_v) %>%
  stats::na.omit() %>%
  dplyr::left_join(.,sto_label, by = dplyr::join_by(sto)) |>
  dplyr::mutate(sto = paste0(sto,".",accounting_entry,"-",sto_label)) |>
  dplyr::select(-sto_label) |>
  tidyr::nest(.by = c(ref_area,sto))

cli::cli_progress_message("Creating charts...")
charts <- tmp |>
  dplyr::transmute(chart= purrr::map2(data,sto, ~ggplot2::ggplot(.x)+
                          ggplot2::geom_line(ggplot2::aes(time_period,obs_value, colour = source),linewidth = 0.65)+
                          ggplot2::facet_wrap(~ref_sector)+ #, scales="free_y"
                          my_theme+
                          my_scale+
                          ggplot2::scale_y_continuous(labels = scales::label_number(),position = "right")+
                          ggplot2::ggtitle(paste0(.y))+
                          ggplot2::ylab("")+ ggplot2::xlab("")+
                          ggplot2::theme(axis.ticks.y = ggplot2::element_blank())
  )
  ) |>
  tibble::deframe()


cli::cli_progress_message("Generating file...")
ggplot2::ggsave(
  filename = paste0(output_sel,"/", country,"_a_T_V.pdf"),
  plot = gridExtra::marrangeGrob(charts, nrow=1, ncol=1),
  width = 15, height = 9
)
cli::cli_alert_success(paste0("Charts created in ",output_sel,"/", country,"_a_T_V.pdf"))
}
