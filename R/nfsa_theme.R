#' Default theme options for charts
#'
#' @return a set of theme paremeters
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(mpg, disp)) + geom_point() + nfsa_theme()+ ggtitle("Test")

nfsa_theme <- function() {

  ggplot2::theme(

    # TEXT FORMAT
    # This sets the font, size, type and colour
    # of text for the chart's title
    plot.title = ggplot2::element_text(
      size = 16,
      face = "bold",
      color = "#262B38"
    ),
    # This sets the font, size, type and colour
    # of text for the chart's subtitle,
    # as well as setting a margin between the title and the subtitle
    plot.subtitle = ggplot2::element_text(
      color = "#262B38",
      size = 12,
      margin = ggplot2::margin(9, 0, 9, 0)
    ),
    # This leaves the caption text element empty,
    # because it is set elsewhere in the finalise plot function
    plot.caption = ggplot2::element_text(
      color = "#262B38",
      size = 12),

    # LEGEND FORMAT
    # This sets the position and alignment of the legend,
    # removes a title and background for it
    # and sets the requirements for any text within the legend.
    # The legend may often need some more manual tweaking
    # when it comes to its exact position based on the plot coordinates.
    legend.position = "right",
    legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(
      size = 12,
      color = "#262B38"
    ),

    # AXIS FORMAT
    # This sets the text font, size and colour for the axis test,
    # as well as setting the margins and removes lines and ticks.
    # In some cases, axis lines and axis ticks are things we would
    # want to have in the chart -
    # the cookbook shows examples of how to do so.
    axis.title = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(
      size = 12,
      color = "#262B38"
    ),
    axis.text.x = ggplot2::element_text(margin = ggplot2::margin(5, b = 10)),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_line(colour="#262B38"),

    # GRID LINES
    # This removes all minor gridlines and adds major y gridlines.
    # In many cases you will want to change this to remove
    # y gridlines and add x gridlines.
    # The cookbook shows you examples for doing so.
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color = "#D4D5D7"),
    panel.grid.major.x = ggplot2::element_blank(),

    # BLANK BACKGROUND
    # This sets the panel background as blank, removing the standard
    # grey ggplot background colour from the plot.
    panel.background = ggplot2::element_rect(fill = "#F3F6FC"),

    plot.background = element_rect(fill= "#FFFFFF",
                                   colour = "#262B38"),
    line = element_line(colour = "#262B38"),
    rect = element_rect(fill = "#FAFAFA",
                        linetype = 0,
                        colour = NA),

    # STRIP BACKGROUND
    # This sets the panel background for facet-wrapped plots to white,
    # removing the standard grey ggplot background colour and sets the
    # title size of the facet-wrap title to font size 22.
    strip.background = ggplot2::element_rect(fill = "#F3F6FC"),
    strip.text = ggplot2::element_text(size = 14, hjust = 0.1, color="#262B38",face="bold"),
    panel.spacing = unit(2, "lines"),
    strip.placement = "outside"
  )
}
