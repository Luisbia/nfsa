#' fill continuous palette for charts
#'
#' @param name name of the palette. "full" by default. Other options ("primary","monoblue","monofuchsia")
#' @param direction of the scale. 1 by default, any other number to reverse it.
#'
#' @return a continuous fill palette
#' @export nfsa_scale_fill_c
#'
#' @examples a continuous fill palette
#' library(ggplot2)
#' ggplot(diamonds,aes(cut, color,fill = table))+
#'geom_tile() +
#'nfsa_scale_fill_c(name = "monofuchsia", direction = -1)
nfsa_scale_fill_c <- function(name = "monoblue", direction = 1) {
  if (direction == 1) {
    ggplot2::scale_fill_gradientn(colours = nfsa_palettes(name = name, type = "continuous"))
  } else {
    ggplot2::scale_fill_gradientn(colours = rev(nfsa_palettes(name = name, type = "continuous")))
  }
}
