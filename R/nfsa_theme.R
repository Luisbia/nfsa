#' @title NFSA Theme for ggplot2
#'
#' @description A custom ggplot2 theme similar to Eurostats
#'
#' @param base_size Numeric. Base font size for the theme (default is 11).
#' @param base_family Character. Base font family for the theme (default is "sans").
#' @param grid Logical. Whether to display major grid lines (default is TRUE).
#' @param axis Logical. Whether to display axis lines (default is TRUE).
#'
#' @return A ggplot2 theme object.
#'
#' @import ggplot2
#'
#' @examples
#' # Apply the NFSA theme to a ggplot
#' library(ggplot2)
#'
#' ggplot(data = mtcars, aes(x = mpg, y = disp)) +
#'   geom_point() +
#'   nfsa_theme()
#'
#' # Customize the theme
#' ggplot(data = mtcars, aes(x = mpg, y = disp)) +
#'   geom_point() +
#'   nfsa_theme(base_size = 12, grid = FALSE, axis = FALSE)
#'
#' \dontrun{
#' # Example with different fonts (requires the font to be installed)
#' ggplot(data = mtcars, aes(x = mpg, y = disp)) +
#'   geom_point() +
#'   nfsa_theme(base_family = "serif")
#' }
#' @export
# Main theme function
nfsa_theme <- function(base_size = 11,
                       base_family = "sans",
                       grid = TRUE,
                       axis = TRUE) {

  library(ggplot2)

  # Define Eurostat color palette
  eurostat_colors <- c(
    blue = "#003399",      # Primary Eurostat blue
    yellow = "#FFB800",    # Eurostat yellow
    lightblue = "#5694CA", # Light blue for secondary data
    gray = "#707070",      # Gray for text
    lightgray = "#E5E5E5"  # Light gray for gridlines
  )

  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      # Plot background
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),

      # Grid lines - light gray, subtle
      panel.grid.major = if(grid) {
        element_line(color = "#E5E5E5", linewidth = 0.3)
      } else {
        element_blank()
      },
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(), # Typically only horizontal gridlines

      # Axes
      axis.line = if(axis) {
        element_line(color = "#707070", linewidth = 0.5)
      } else {
        element_blank()
      },
      axis.line.x = element_line(color = "#707070", linewidth = 0.5),
      axis.line.y = element_blank(), # No y-axis line typically
      axis.ticks = element_line(color = "#707070", linewidth = 0.5),
      axis.ticks.y = element_blank(),
      axis.text = element_text(color = "#707070", size = rel(0.9)),
      axis.title = element_text(color = "#333333", size = rel(1.0),
                                face = "plain"),
      axis.title.y = element_text(angle = 90, margin = margin(r = 10)),
      axis.title.x = element_text(margin = margin(t = 10)),

      # Plot title and subtitle
      plot.title = element_text(
        color = "#003399",
        size = rel(1.3),
        face = "bold",
        hjust = 0,
        margin = margin(b = 8)
      ),
      plot.subtitle = element_text(
        color = "#707070",
        size = rel(1.0),
        hjust = 0,
        margin = margin(b = 12)
      ),
      plot.caption = element_text(
        color = "#707070",
        size = rel(0.8),
        hjust = 0,
        margin = margin(t = 12)
      ),

      # Legend
      legend.background = element_rect(fill = "white", color = NA),
      legend.key = element_rect(fill = "white", color = NA),
      legend.text = element_text(color = "#707070", size = rel(0.9)),
      legend.title = element_text(color = "#333333", size = rel(1.0),
                                  face = "plain"),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box.spacing = unit(0.5, "cm"),

      # Faceting
      strip.background = element_rect(fill = "#F5F5F5", color = NA),
      strip.text = element_text(color = "#003399", size = rel(1.0),
                                face = "bold", margin = margin(5, 5, 5, 5)),

      # Margins
      plot.margin = margin(15, 15, 15, 15)
    )
}

