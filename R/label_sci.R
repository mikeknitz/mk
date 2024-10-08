# Updated 2024-06-04

#' @title
#' Axis labels for scientific notation
#' 
#' @description
#' Supply this function to the `labels` argument of ggplot scale function, e.g., `ggplot2::scale_y_continuous()`
#' 
#' @details
#' This function first forces scientific notation with `scales::label_scientific()` then beautifies it a bit with e.g., 5x10^2 instead of 5e+02
#' 
#' @returns
#' a vector of `expression()` results
#' 
#' @examples
#' \dontrun{
#' p + ggplot2::scale_y_continuous(labels = mk::label_sci)
#' }
#' 
#' @param x a numeric vector of breaks to format
#' @export

label_sci <- function(x) {
  lapply(x, \(x) {
    if (is.na(x)) return(NA)
    x_sci <- scales::label_scientific()(x)
    if (x_sci == "0e+00") return(expression(0))
    before <- sub("^(.+)e.\\d+$", "\\1", x_sci)
    after <- sub("^.+e(.\\d+)$", "\\1", x_sci)
    if (grepl("\\+", after)) after <- sub("^\\+(.*)$", "\\1", after)
    return({
      eval(parse(text = paste0(
        'expression(',
        before, ' * "\u00d7" * 10^', after, ')'
      )))
    })
  }) |>
    unlist() |>
    as.expression()
}
