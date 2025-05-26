#' fill discrete palette for charts
#'
#' @param name name of the palette. "full" by default. Other options ("primary","monoblue","monofuchsia")
#' @param direction of the scale. 1 by default, any other number to reverse it.
#'
#' @return a discrete fill palette
#' @export nfsa_scale_fill
#'
#' @examples a fill palette
#' library(ggplot2)
#'
#'ggplot(iris,aes(Sepal.Length, fill = Species))+
#'  geom_bar(position = "dodge")+
#'  scale_fill_nfsa()
nfsa_scale_fill <- function(name = "full", direction = 1) {
  if (direction == 1) {
    ggplot2::scale_fill_manual(values = nfsa_palettes(name, type = "discrete"))
  } else {
    ggplot2::scale_fill_manual(values = rev(nfsa_palettes(name = name, type = "discrete")))
  }
}



