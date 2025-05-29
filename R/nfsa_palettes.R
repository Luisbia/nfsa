#' Function to create palettes from colours
#'
#' @param name name of the palette ("full", "primary", "monoblue", "monofuchsia")
#' @param n number of colours
#' @param all_palettes list of all palettes
#' @param type continue or discrete
#'
#' @return a ready to use palette
#' @export nfsa_palettes
#'
#' @examples
#' scales::show_col(nfsa_palettes(name = "monofuchsia"))
nfsa_palettes <- function(name, n, all_palettes = nfsa_colours, type = c("discrete", "continuous")) {

  nfsa_colours = list(
    full = c("#0E47CB","#FFCC00", "#208486","#AF155C",  "#AA5F18", "#B656BD","#388AE2","#E04040", "#2644A7","#33A033","#B39421"),
    primary = c("#0E47CB","#FFCC00"),
    monoblue = c("#082B7A", "#0B39A2" , "#0E47CB", "#6E91E0", "#CFDAF5"),
    monofuchsia = c("#241125", "#6D3371", "#B656BD", "#D399D7", "#F0DDF1" )

  )

  palette = all_palettes[[name]]
  if (missing(n)) {
    n = length(palette)
  }
  type = match.arg(type)
  out = switch(type,
               continuous = grDevices::colorRampPalette(palette)(n),
               discrete = palette[1:n]
  )
  structure(out, name = name, class = "palette")
}
