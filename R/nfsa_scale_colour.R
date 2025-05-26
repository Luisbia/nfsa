#' colour palette for charts
#'
#' @param name name of the palette. "full" by default. Other options ("primary","monoblue","monofuchsia") as defined in regacc_palettes()
#' @param direction of the scale. 1 by default, any other number to reverse it.
#' @return a colour palette
#' @export nfsa_scale_colour
#'
#' @examples
#' library(ggplot2)
#'ggplot(iris,aes(Sepal.Length, Sepal.Width, colour = Species))+
#'geom_point()+
#'nfsa_scale_colour()
nfsa_scale_colour <- function(name = "full", direction = 1) {
  if (direction == 1) {
    ggplot2::scale_colour_manual(values = nfsa_palettes(name, type = "discrete"))
  } else {
    ggplot2::scale_colour_manual(values = rev(nfsa_palettes(name, type = "discrete")))
  }
}


