#' @title Color scale functions for Eurostat palette
#'
#' @description This function generates a color scale using the Eurostat palette.
#'
#' @param ... Additional arguments to be passed to \code{\link[ggplot2]{scale_colour_manual}}.
#'
#' @return A \code{\link[ggplot2]{Scale}} object that can be added to a ggplot.
#' @export
nfsa_scale_colour <- function(...) {
  scale_colour_manual(
    values = c("#003399", "#FFB800", "#5694CA", "#9DC3E6",
               "#A6CE39", "#F47B20", "#C00000"),
    ...
  )
}

#' @title Fill scale functions for Eurostat palette
#'
#' @description This function generates a fill scale using the Eurostat palette.
#'
#' @param ... Additional arguments to be passed to \code{\link[ggplot2]{scale_fill_manual}}.
#'
#' @return A \code{\link[ggplot2]{Scale}} object that can be added to a ggplot.
#' @export
nfsa_scale_fill <- function(...) {
  scale_fill_manual(
    values = c("#003399", "#FFB800", "#5694CA", "#9DC3E6",
               "#A6CE39", "#F47B20", "#C00000"),
    ...
  )
}


