#' Generate plots comparing NFSA and BOP data  for a given country.
#'
#' This function reads T0801 and BOP data for a specified country, filters and processes the data, and generates time series plots
#' comparing the two sources for various statistical concepts (STO).
#' The plots are saved as a PDF file.
#'
#' @param country A character string specifying the country code (e.g., "AT"). This code is used to filter the data files.
#' @param quarter The quarter from which BOP data should be taken. NFSA by default takes the lates available data.
#' @param output_sel A character string specifying the path to the directory where the generated PDF file should be saved. Defaults to `here("output", "plots")`.
#' @param time_min A character string specifying the earliest time period to include in the plots, in the format "YYYY-QX" (e.g., "1999-Q1"). Defaults to "1999-Q1".
#' @param my_theme A ggplot2 theme object to use for the plots. Defaults to `ggthemes::theme_fivethirtyeight()`.
#' @param my_colours Choose the colours for the lines, for example c("darkred","grey60")
#' @return None. The function generates and saves a PDF file containing the plots.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' nfsa_remove_output_files("output/plots")
#' }
nfsa_q_plots_T0801_BOP <- function(country,
                                   quarter,
                                   output_sel = here::here("output", "plots"),
                                   time_min = "2015-Q1",
                                   my_theme = ggthemes::theme_fivethirtyeight(),
                                   my_colours = c("#B656BD","#208486", "#AF155C")){

  my_scale <- ggplot2::scale_color_manual(values = c("T0801" = my_colours[1],
                                            "BOP" = my_colours[2],
                                            "T0801-BOP" = my_colours[3]))

  cli::cli_progress_message("Collecting data...")

  tmp_n <- nfsa::nfsa_get_data(country = country) |>
    dplyr::select(ref_area,id,time_period,obs_value) |>
    nfsa::nfsa_separate_id() |>
    dplyr::filter(ref_sector == "S2") |>
    dplyr::select(-ref_sector) |>
    dplyr::filter(obs_value != "NaN") |>
    dplyr::filter(time_period >= time_min) |>
    dplyr::mutate(source = "T0801",
           time_period = lubridate::yq(time_period))



  tmp_y <- list.files(path = paste0("M:/nas/QSA10/Production/",quarter,"/(1) QSA/(1_2) Validation in progress/(1_2_6) Consistency checks - QSA vs BoP/Input"),
                      pattern = paste0("_",country,"_"),
                      full.names = TRUE,
                      recursive = TRUE) |>
    readsdmx::read_sdmx() |>
    janitor::clean_names() |>
    dplyr::filter(time_period >= time_min,
           counterpart_area == "W1" ) |>
    dplyr::select(ref_area,sto,accounting_entry,time_period,obs_value) |>
    dplyr::mutate(obs_value = as.numeric(obs_value),
           source = "BOP",
           time_period = lubridate::yq(time_period),) |>
    dplyr::filter(accounting_entry %in% c("C", "D", "B"))

  if(nrow(tmp_y) == 0) stop(paste0("No BOP file ", country))



  tmp <- dplyr::bind_rows(tmp_n, tmp_y) %>%
    tidyr::pivot_wider(names_from = source,
                values_from = obs_value) |>
    stats::na.omit() |>
    dplyr::mutate(`T0801-BOP` = T0801 - BOP) |>
    #filter(abs(`T0801-BOP`)>5) |>
    tidyr::pivot_longer(cols = c(T0801,BOP,`T0801-BOP`),
                 names_to = "source",
                 values_to = "obs_value") |>
    tidyr::nest(.by = c(ref_area,sto))


  cli::cli_progress_message("Creating charts...")

  charts <- tmp |>
    dplyr::transmute(chart= purrr::map2(data,sto, ~ggplot2::ggplot(.x)+
                            ggplot2::geom_line(ggplot2::aes(time_period,obs_value, colour = source),linewidth = 0.65)+
                            ggplot2::facet_wrap(~accounting_entry)+ #, scales="free_y"
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
    filename = paste0(output_sel,"/", country,"_T0801_BOP.pdf"),
    plot = gridExtra::marrangeGrob(charts, nrow=1, ncol=1),
    width = 15, height = 9
  )
  cli::cli_alert_success(paste0("Charts created in ",output_sel,"/", country,"_T0801_BOP.pdf"))
}
