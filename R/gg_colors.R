# Updated 2024-10-09

# Code adapted from:
# https://stackoverflow.com/questions/8197559

#' @title Generate ggplot2 default color palette
#' @description Pull values equally spaced from the color wheel
#' @returns a character vector of hex values
#' @details Code adapted from this Stack Overflow question: https://stackoverflow.com/questions/8197559
#' @examples
#' mk::gg_colors(8)
#' @param n number of colors to generate
#' @export

gg_colors <- function(n) {

  if (missing(n)) {
    stop("argument \"n\" is missing, provide a number of colors")
  }

  hues <- seq(15, 375, length = n + 1)
  grDevices::hcl(h = hues, l = 65, c = 100)[1:n]

}
