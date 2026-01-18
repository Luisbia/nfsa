#' Generate quarterly plots comparing NFSA data
#'
#' This function creates time series plots comparing different versions and adjustments of NFSA quarterly data.
#' It supports three comparison modes:
#' - Comparing adjusted vs unadjusted data from the same version
#' - Comparing new vs previous versions of unadjusted data
#' - Comparing new vs previous versions of seasonally adjusted data
#'
#' @param country A character string specifying the country code (e.g., "AT").
#' @param comparison A character string specifying the type of comparison:
#'   - "adjusted_unadjusted": Compare adjusted (Y) vs unadjusted (N) from new version
#'   - "new_prev_unadjusted": Compare new vs prev versions of unadjusted data (T0801)
#'   - "new_prev_adjusted": Compare new vs prev versions of seasonally adjusted data (T0801SA)
#' @param output_sel A character string specifying the path to the directory where the generated
#'   PDF file should be saved. Defaults to `here("output", "plots")`.
#' @param time_min A character string specifying the earliest time period to include in the plots,
#'   in the format "YYYY-QX" (e.g., "1999-Q1"). Defaults to "1999-Q1".
#' @param my_theme A ggplot2 theme object to use for the plots.
#'   Defaults to `ggthemes::theme_fivethirtyeight()`.
#' @param my_colours A character vector of two colors for the lines.
#'   Defaults to c("#B656BD","#208486").
#' @param ... Additional filter arguments passed to `nfsa_get_data()`. Can be one or more filter expressions
#'   applied to columns in the original parquet files (e.g., `filters = "ref_sector == 'S13'"`).
#'
#' @return None. The function generates and saves a PDF file containing the plots.
#'
#' @examples
#' \dontrun{
#' # Compare adjusted vs unadjusted
#' nfsa_q_plots(country = "AT", comparison = "adjusted_unadjusted")
#'
#' # Compare new vs previous unadjusted
#' nfsa_q_plots(country = "AT", comparison = "new_prev_unadjusted")
#'
#' # Compare new vs previous adjusted
#' nfsa_q_plots(country = "AT", comparison = "new_prev_adjusted")
#' }
#'
#' @export
nfsa_q_plots <- function(country,
                         comparison = c("adjusted_unadjusted", "new_prev_unadjusted", "new_prev_adjusted"),
                         output_sel = here::here("output", "plots"),
                         time_min = "1999-Q1",
                         my_theme = ggthemes::theme_fivethirtyeight(),
                         my_colours = c("#B656BD", "#208486"),
                         ...) {

  comparison <- match.arg(comparison)

  lookup <- nfsa::nfsa_sto_lookup
  sto_label <- nfsa::nfsa_sto_label

  cli::cli_progress_message("Collecting data...")

  if (comparison == "adjusted_unadjusted") {
    # Compare N (unadjusted) vs Y (seasonally adjusted) from new version
    tmp_n <- nfsa::nfsa_get_data(country = country, table = "T0801",
                                 type = "new", ...) |>
      dplyr::select(ref_area, id, time_period, obs_value) |>
      nfsa::nfsa_separate_id() |>
      dplyr::filter(obs_value != "NaN") |>
      dplyr::filter(time_period >= time_min) |>
      dplyr::mutate(
        source = "new",
        time_period = lubridate::yq(time_period),
        adjustment = "N"
      )

    tmp_y <- nfsa::nfsa_get_data(country = country, table = "T0801SA",
                                 type = "new", ...) |>
      dplyr::select(ref_area, id, time_period, obs_value) |>
      nfsa::nfsa_separate_id() |>
      dplyr::filter(obs_value != "NaN") |>
      dplyr::filter(time_period >= time_min) |>
      dplyr::mutate(
        source = "new",
        time_period = lubridate::yq(time_period),
        adjustment = "Y"
      )

    if (nrow(tmp_y) == 0) stop(paste0("No seasonally adjusted file for ", country))

    tmp <- dplyr::bind_rows(tmp_n, tmp_y) |>
      stats::na.omit() |>
      dplyr::left_join(sto_label, by = dplyr::join_by(sto)) |>
      dplyr::mutate(sto = paste0(sto, ".", accounting_entry, "-", sto_label)) |>
      dplyr::select(-sto_label, -accounting_entry) |>
      tidyr::pivot_wider(names_from = adjustment, values_from = obs_value) |>
      stats::na.omit() |>
      tidyr::pivot_longer(cols = c(Y, N), names_to = "adjustment", values_to = "obs_value") |>
      tidyr::nest(.by = c(ref_area, sto))

    my_scale <- ggplot2::scale_color_manual(values = c("Y" = my_colours[1], "N" = my_colours[2]))
    filename <- paste0(output_sel, "/", country, "_q_new_N_Y.pdf")

    charts <- tmp |>
      dplyr::transmute(chart = purrr::map2(data, sto, ~ ggplot2::ggplot(.x) +
        ggplot2::geom_line(ggplot2::aes(time_period, obs_value, colour = adjustment), linewidth = 0.65) +
        ggplot2::facet_wrap(~ref_sector) +
        my_theme +
        my_scale +
        ggplot2::scale_y_continuous(labels = scales::label_number(), position = "right") +
        ggplot2::ggtitle(paste0(.y)) +
        ggplot2::ylab("") + ggplot2::xlab("") +
        ggplot2::theme(axis.ticks.y = ggplot2::element_blank()))) |>
      tibble::deframe()

  } else if (comparison == "new_prev_unadjusted") {
    # Compare new vs prev for unadjusted data
    tmp_new <- nfsa::nfsa_get_data(country = country, table = "T0801",
                                   type = "new", ...) |>
      nfsa::nfsa_separate_id() |>
      dplyr::filter(obs_value != "NaN") |>
      dplyr::filter(time_period >= time_min) |>
      dplyr::mutate(
        source = "new",
        time_period = lubridate::yq(time_period)
      )

    tmp_prev <- nfsa::nfsa_get_data(country = country, table = "T0801",
                                    type = "prev", ...) |>
      nfsa::nfsa_separate_id() |>
      dplyr::filter(obs_value != "NaN") |>
      dplyr::filter(time_period >= time_min) |>
      dplyr::mutate(
        source = "prev",
        time_period = lubridate::yq(time_period)
      )

    tmp <- dplyr::bind_rows(tmp_new, tmp_prev) |>
      stats::na.omit() |>
      dplyr::left_join(sto_label, by = dplyr::join_by(sto)) |>
      dplyr::mutate(sto = paste0(sto, ".", accounting_entry, "-", sto_label)) |>
      dplyr::select(-sto_label) |>
      tidyr::nest(.by = c(ref_area, sto))

    my_scale <- ggplot2::scale_color_manual(values = c("new" = my_colours[1], "prev" = my_colours[2]))
    filename <- paste0(output_sel, "/", country, "_q_new_prev_N.pdf")

    charts <- tmp |>
      dplyr::transmute(chart = purrr::map2(data, sto, ~ ggplot2::ggplot(.x) +
        ggplot2::geom_line(ggplot2::aes(time_period, obs_value, colour = source), linewidth = 0.65) +
        ggplot2::facet_wrap(~ref_sector) +
        my_theme +
        my_scale +
        ggplot2::scale_y_continuous(labels = scales::label_number(), position = "right") +
        ggplot2::ggtitle(paste0(.y)) +
        ggplot2::ylab("") + ggplot2::xlab("") +
        ggplot2::theme(axis.ticks.y = ggplot2::element_blank()))) |>
      tibble::deframe()

  } else if (comparison == "new_prev_adjusted") {
    # Compare new vs prev for seasonally adjusted data
    tmp_new <- nfsa::nfsa_get_data(country = country, table = "T0801SA",
                                   type = "new", ...) |>
      dplyr::filter(obs_value != "NaN") |>
      dplyr::filter(time_period >= time_min) |>
      nfsa::nfsa_separate_id() |>
      dplyr::mutate(
        source = "new",
        time_period = lubridate::yq(time_period)
      )

    if (nrow(tmp_new) == 0) stop(paste0("No seasonally adjusted file for ", country))

    tmp_prev <- nfsa::nfsa_get_data(country = country, table = "T0801SA",
                                    type = "prev", ...) |>
      dplyr::filter(obs_value != "NaN") |>
      dplyr::filter(time_period >= time_min) |>
      nfsa::nfsa_separate_id() |>
      dplyr::mutate(
        source = "prev",
        time_period = lubridate::yq(time_period)
      )

    if (nrow(tmp_prev) == 0) stop(paste0("No seasonally adjusted file for ", country))

    tmp <- dplyr::bind_rows(tmp_new, tmp_prev) |>
      stats::na.omit() |>
      dplyr::left_join(sto_label, by = dplyr::join_by(sto)) |>
      dplyr::mutate(sto = paste0(sto, ".", accounting_entry, "-", sto_label)) |>
      dplyr::select(-sto_label) |>
      tidyr::nest(.by = c(ref_area, sto))

    my_scale <- ggplot2::scale_color_manual(values = c("new" = my_colours[1], "prev" = my_colours[2]))
    filename <- paste0(output_sel, "/", country, "_q_new_prev_Y.pdf")

    charts <- tmp |>
      dplyr::transmute(chart = purrr::map2(data, sto, ~ ggplot2::ggplot(.x) +
        ggplot2::geom_line(ggplot2::aes(time_period, obs_value, colour = source), linewidth = 0.65) +
        ggplot2::facet_wrap(~ref_sector) +
        my_theme +
        my_scale +
        ggplot2::scale_y_continuous(labels = scales::label_number(), position = "right") +
        ggplot2::ggtitle(paste0(.y)) +
        ggplot2::ylab("") + ggplot2::xlab("") +
        ggplot2::theme(axis.ticks.y = ggplot2::element_blank()))) |>
      tibble::deframe()
  }

  cli::cli_progress_message("Generating file...")

  ggplot2::ggsave(
    filename = filename,
    plot = gridExtra::marrangeGrob(charts, nrow = 1, ncol = 1),
    width = 15, height = 9
  )

  cli::cli_alert_success(paste0("Charts created in ", filename))
}
