#' Plot and compare two annual or quarterly NFSA datasets
#'
#' @description
#' Creates a multipage PDF comparing two user-supplied NFSA datasets.
#' One page is produced per country and transaction/accounting-entry
#' combination, with institutional sectors displayed as facets. Within each
#' country, PDF pages are ordered by transaction prefix (P, D, B, then others)
#' and then by transaction number (e.g. P2 before P10).
#'
#' @details
#' Both inputs must have columns `ref_area`, `id`, `time_period`, and
#' `obs_value`. Series identifiers are expected to follow the NFSA
#' `sector.transaction.entry` convention (e.g. `S13.B9.B`).
#'
#' Annual periods may be written as `YYYY` or `YYYYA00`; quarterly periods
#' as `YYYY-Qn` or `YYYYQn` (`n` from 1 to 4). Both datasets must use the
#' same frequency. Observations are joined on `ref_area`, `id`, and
#' `time_period`, after standardizing period labels.
#'
#' With `chart_type = "lines"`, both versions are displayed as lines.
#' With `chart_type = "line_area"` or `chart_type = "line_col"`, version 1 is a line and the difference
#' `(version 1 - version 2)` is drawn as an area/column around zero. For a series
#' present only in version 2 and `show_all_series = TRUE`, the version 2
#' line is drawn so that the series is not represented by an empty panel.
#' The difference is only computed for matched observations.
#'
#' @param version1 Data frame for the first version, with `ref_area`, `id`,
#'   `time_period`, and `obs_value` columns.
#' @param version2 Data frame for the second version, with the same columns.
#' @param output_sel Directory for the generated PDF. Created if necessary.
#'   Defaults to `here::here("output", "plots")`.
#' @param time_min Earliest period to plot. Use `YYYY-Qn`, `YYYY`, or
#'   `YYYYA00`. Defaults to `"1999-Q1"`.
#' @param chart_type Either `"lines"` (two lines) or `"line_area"`,`"line_col"`
#'   (version 1 line and difference area).
#' @param facet_scales Either `"fixed"` or `"free_y"` for `facet_wrap()`.
#' @param show_all_series Logical. If `FALSE` (default), keep the
#'   observations/series in version 1 (`left_join`). If `TRUE`, retain
#'   observations/series from either dataset (`full_join`).
#' @param my_theme A ggplot2 theme. Defaults to
#'   `ggthemes::theme_fivethirtyeight()`.
#' @param my_colours Two colours: version 1, then version 2/difference.
#' @param version_labels Two labels used in legends. Defaults to
#'   `c("Version 1", "Version 2")`.
#' @param ... Additional ggplot2 components, such as `theme()`, `labs()`,
#'   `ggtitle()`, scales, or layers. Added to every chart in the supplied
#'   order after the defaults, so they can override existing settings.
#'
#' @return Invisibly returns the path to the generated multipage PDF.
#' @examples
#' \dontrun{
#
#' nfsa_q_plots_test(v1, v2, chart_type = "lines", facet_scales = "fixed")
#' nfsa_q_plots_test(v1, v2, chart_type = "line_area",
#'                   facet_scales = "free_y", show_all_series = TRUE)
#' nfsa_plots(v1, v2,
#'            theme = ggplot2::theme(legend.position = "bottom"),
#'            labels = ggplot2::labs(title = "NFSA comparison", caption = "Source: MATIS"))
#' }
#' @importFrom dplyr select mutate filter left_join full_join arrange group_by group_split
#' @importFrom ggplot2 ggplot aes geom_line geom_area geom_hline facet_wrap
#' @importFrom ggplot2 scale_y_continuous scale_colour_manual scale_fill_manual
#' @importFrom ggplot2 labs theme element_blank ggsave
#' @importFrom purrr map
#' @importFrom rlang .data
#' @export
nfsa_plots <- function(
    version1,
    version2,
    output_sel = here::here(),
    time_min = "1999-Q1",
    chart_type = "line_col",
    facet_scales = "free_y",
    show_all_series = FALSE,
    my_theme = ggthemes::theme_fivethirtyeight(),
    my_colours = c("#003399", "#AF155F"),
    version_labels = c("Version 1", "Version 2"),
    ...
) {
  plot_elements <- list(...)

  if (!is.logical(show_all_series) || length(show_all_series) != 1L ||
      is.na(show_all_series)) {
    stop("show_all_series must be a single TRUE or FALSE.", call. = FALSE)
  }
  if (length(my_colours) != 2L || anyNA(my_colours) ||
      length(version_labels) != 2L || anyNA(version_labels) ||
      any(!nzchar(version_labels)) || anyDuplicated(version_labels)) {
    stop("my_colours and version_labels must each contain two valid values; labels must differ.",
         call. = FALSE)
  }

  required_cols <- c("ref_area", "id", "time_period", "obs_value")
  for (nm in c("version1", "version2")) {
    dat <- get(nm)
    if (!is.data.frame(dat) || !all(required_cols %in% names(dat))) {
      stop(nm, " must be a data frame with columns: ",
           paste(required_cols, collapse = ", "), call. = FALSE)
    }
  }

  # Convert all supported period formats to a comparable quarter index.
  parse_period <- function(period) {
    period <- trimws(as.character(period))
    is_q <- grepl("^[0-9]{4}-?Q[1-4]$", period)
    is_a <- grepl("^[0-9]{4}(A00)?$", period)
    if (any(!is_q & !is_a)) {
      stop("Unexpected time_period format; use YYYY, YYYYA00, YYYY-Qn or YYYYQn.",
           call. = FALSE)
    }
    year <- as.integer(substr(period, 1L, 4L))
    quarter <- ifelse(is_q, as.integer(sub("^.*Q", "", period)), 1L)
    list(frequency = ifelse(is_q, "quarterly", "annual"),
         index = year * 4L + quarter - 1L,
         date = as.Date(sprintf("%04d-%02d-01", year, (quarter - 1L) * 3L + 1L)))
  }

  if (length(time_min) != 1L || is.na(time_min)) {
    stop("time_min must be a single valid period.", call. = FALSE)
  }
  first_period <- parse_period(time_min)$index

  prepare <- function(df, value_name) {
    df <- dplyr::select(df, dplyr::all_of(required_cols))
    if (nrow(df) == 0L) {
      df$plot_date <- as.Date(character())
      df$period_index <- integer()
      df[[value_name]] <- numeric()
      return(dplyr::select(df, ref_area, id, plot_date, period_index,
                           dplyr::all_of(value_name)))
    }
    period <- parse_period(df$time_period)
    df$plot_date <- period$date
    df$period_index <- period$index
    df$obs_value <- suppressWarnings(as.numeric(as.character(df$obs_value)))
    df <- dplyr::filter(df, .data$period_index >= first_period,
                        !is.na(.data$ref_area), !is.na(.data$id),
                        is.finite(.data$obs_value))
    df <- dplyr::select(df, ref_area, id, plot_date, period_index, obs_value)
    if (anyDuplicated(df[c("ref_area", "id", "period_index")])) {
      stop("Duplicate country/id/time_period observations detected in ",
           value_name, ".", call. = FALSE)
    }
    names(df)[names(df) == "obs_value"] <- value_name
    attr(df, "frequencies") <- unique(period$frequency)
    df
  }

  v1 <- prepare(version1, "value1")
  v2 <- prepare(version2, "value2")
  freq1 <- attr(v1, "frequencies")
  freq2 <- attr(v2, "frequencies")
  all_freq <- unique(c(freq1, freq2))
  if (length(all_freq) > 1L) {
    stop("Inputs contain mixed annual and quarterly frequencies.", call. = FALSE)
  }

  by_cols <- c("ref_area", "id", "plot_date", "period_index")
  combined <- if (show_all_series) {
    dplyr::full_join(v1, v2, by = by_cols)
  } else {
    dplyr::left_join(v1, v2, by = by_cols)
  }
  if (!nrow(combined)) {
    stop("No observations available to plot after filtering and joining.", call. = FALSE)
  }

  # Group by transaction and accounting entry, facet by institutional sector.
  if (any(!grepl("^[^.]+\\.[^.]+\\.[^.]+$", combined$id))) {
    stop("id values must follow sector.transaction.entry (e.g. S13.B9.B).",
         call. = FALSE)
  }
  combined$ref_sector <- sub("\\..*$", "", combined$id)
  combined$sto_entry <- sub("^[^.]+\\.", "", combined$id)
  combined$difference <- combined$value1 - combined$value2

  # PDF page order, within each country: P, D, B, then other entries.
  transaction <- sub("\\..*$", "", combined$sto_entry)
  prefix <- substr(transaction, 1L, 1L)
  order_group <- match(prefix, c("P", "D", "B"))
  order_group[is.na(order_group)] <- 4L

  # Natural numerical order (e.g. P2 before P10).
  order_number <- suppressWarnings(as.integer(
    sub("^([0-9]+).*$", "\\1", substring(transaction, 2L))
  ))

  combined <- combined[order(combined$ref_area, order_group,
                             order_number, combined$sto_entry,
                             na.last = TRUE), , drop = FALSE]

  # split() respects the level order of a factor (unlike group_by()).
  group_id <- paste(combined$ref_area, combined$sto_entry, sep = "\r")
  groups <- unname(split(combined, factor(group_id, levels = unique(group_id))))

  make_chart <- function(df) {
    df <- dplyr::arrange(df, .data$ref_sector, .data$plot_date)
    gg <- ggplot2::ggplot(df, ggplot2::aes(x = .data$plot_date)) +
      ggplot2::facet_wrap(~ref_sector, scales = facet_scales) +
      my_theme +
      ggplot2::scale_y_continuous(labels = scales::label_number(), position = "right") +
      ggplot2::labs(title = paste(df$ref_area[1], df$sto_entry[1], sep = " — "),
                    x = NULL, y = NULL) +
      ggplot2::theme(axis.ticks.y = ggplot2::element_blank())

    if (chart_type == "lines") {
      gg <- gg +
        ggplot2::geom_line(ggplot2::aes(y = .data$value1,
                                        colour = version_labels[1]), linewidth = 0.65,
                           na.rm = TRUE) +
        ggplot2::geom_line(ggplot2::aes(y = .data$value2,
                                        colour = version_labels[2]), linewidth = 0.65,
                           na.rm = TRUE) +
        ggplot2::scale_colour_manual(
          name = NULL, values = stats::setNames(my_colours, version_labels))
    }

    if (chart_type == "line_col") {
      gg <- gg +
        ggplot2::geom_hline(yintercept = 0, colour = "grey30") +
        ggplot2::geom_col(ggplot2::aes(y = .data$difference),
                           fill = my_colours[2], alpha = 0.6, na.rm = TRUE) +
        ggplot2::geom_line(ggplot2::aes(y = .data$value1),
                           colour = my_colours[1], linewidth = 0.65, na.rm = TRUE)
    }

      if (chart_type == "line_area") {

      gg <- gg +
        ggplot2::geom_hline(yintercept = 0, colour = "grey30") +
        ggplot2::geom_area(ggplot2::aes(y = .data$difference),
                           fill = my_colours[2], alpha = 0.6, na.rm = TRUE) +
        ggplot2::geom_line(ggplot2::aes(y = .data$value1),
                           colour = my_colours[1], linewidth = 0.65, na.rm = TRUE)
      }
       # If a series exists exclusively in version 2, show it as a line.
      if (show_all_series) {
        missing_v1 <- df |>
          dplyr::group_by(.data$ref_sector) |>
          dplyr::filter(all(is.na(.data$value1))) |>
          dplyr::ungroup()
        if (nrow(missing_v1)) {
          gg <- gg + ggplot2::geom_line(
            data = missing_v1, ggplot2::aes(y = .data$value2),
            colour = my_colours[2], linewidth = 0.65, na.rm = TRUE)
        }
      }
    }
    gg + plot_elements

  charts <- purrr::map(groups, make_chart)
  dir.create(output_sel, recursive = TRUE, showWarnings = FALSE)
  stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  filename <- file.path(output_sel, paste0("nfsa_comparison_", stamp, ".pdf"))
  ggplot2::ggsave(filename, plot = gridExtra::marrangeGrob(charts, nrow = 1, ncol = 1),
                  width = 15, height = 9)
  message("Charts created in ", filename)
  invisible(filename)
}
