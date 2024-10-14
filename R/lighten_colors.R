# Updated 2024-10-14

#' @name lighten_colors
#' @rdname lighten_colors
#' @title Lighten / Darken a color palette
#' @description Lighten or darken a color palette with mk::lighten_colors() or mk::darken_colors(). Inspired by / adapted from https://gist.github.com/Jfortin1/72ef064469d1703c6b30, 2024-10-13
#' 
#' @returns a character vector of hex codes
#' 
#' @examples
#' mk::lighten_colors(c("red", "green", "blue"), 0.65)
#' mk::darken_colors(mk::gg_colors(8), 0.75)
#' 
#' @param colors <chr> color names or hex codes
#' @param factor factor for lightening or darkening, where:
#' 
#' - for mk::lighten_colors(), 1 = original color and 0 = white
#'
#' - for mk::darken_colors(), 1 = original color and 0 = black
#' 
#' @export

lighten_colors <- function(colors, factor) {
  # factor = 1 = original color
  # factor = 0 = white
  sapply(
    X = colors,
    factor = factor,
    FUN = \(colors, factor) {
      rgb.colors <- grDevices::col2rgb(colors)
      # print(rgb.colors)
      rgb.colors <- 255 - (factor * (255 - rgb.colors))
      # print(rgb.colors)
      grDevices::rgb(
        t(rgb.colors[, 1]),
        maxColorValue = 255
      )
    }
  ) |> base::unname()
}

#' @rdname lighten_colors
#' @export

darken_colors <- function(colors, factor) {
  # factor = 1 = original color
  # factor = 0 = black
  sapply(
    X = colors,
    factor = factor,
    FUN = \(colors, factor) {
      rgb.colors <- grDevices::col2rgb(colors)
      # print(rgb.colors)
      rgb.colors <- rgb.colors * factor
      # print(rgb.colors)
      grDevices::rgb(
        t(rgb.colors[, 1]),
        maxColorValue = 255
      )
    }
  ) |> base::unname()
}
