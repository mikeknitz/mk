#' @title Exact sizes for single or faceted ggplots
#' @description Wrapper for `ggh4x::force_panelsizes()`
#' @returns Output of `ggh4x::force_panelsizes()`, a list object of class "forcedsize"
#' @details See examples for explanation of behavior
#' @examples
#' # Single plot / unfaceted
#' p1 <- ggplot2::ggplot(mtcars, ggplot2::aes(disp, mpg)) +
#'   ggplot2::geom_point()
#' 
#' # Faceted 2 x 2 grid
#' p2 <- p1 + ggplot2::facet_grid(vs ~ am)
#' 
#' # Specify exact size of a single plot
#' # units = "in" by default, these are the same
#' p1 + mk::size(4, 3, units = "in")
#' p1 + mk::size(4, 3)
#' 
#' # Specify exact sizes of facets
#' p2 + mk::size(c(4, 2), c(3, 1), units = "in")
#' 
#' # Length one numeric for recycling through each facet
#' # NOTE: this specifies sizes for EACH FACET, not for entire plot
#'   # for a faceted plot, must specify facet sizes/proportions individually
#' p2 + mk::size(3, 1.5, units = "in")
#' 
#' # NULL units will INDEPENDENTLY make widths proportionally sized
#' # to each other and heights proportionally sized, entire canvas filled
#' p2 + mk::size(c(4, 2), c(2, 1), units = NULL)
#' 
#' # NULL units with respect = TRUE, same idea, but unitless proportions
#' # will be proportional to each other, canvas may not be totally filled
#' # similar idea to ggplot2::theme(aspect.ratio = ...)
#' p2 + mk::size(c(4, 2), c(2, 1), units = NULL, respect = TRUE)
#' 
#' # NULL units only work as proportions individually for
#' # widths and heights, so these two appear to look the same
#' p1
#' p1 + mk::size(4, 3, units = NULL)
#' 
#' # Need respect = TRUE again here for unitless
#' # widths/heights to be proportional to each other
#' # These are similar but achieved a bit differently
#' p1 + mk::size(4, 2.5, units = NULL, respect = TRUE)
#' p1 + ggplot2::theme(aspect.ratio = 2.5 / 4)
#' @param cols width of single unfaceted ggplot or widths of multiple facets of a faceted ggplot, this is the cols argument of `ggh4x::force_panelsizes()`
#' @param rows height of single unfaceted ggplot or heights of multiple facets of a faceted ggplot, this is the rows argument of `ggh4x::force_panelsizes()`
#' @param units <string> the unit used for the numeric `cols` and `rows` arguments, default = "in". grid::unit() is used on these arguments prior to passing to `ggh4x::force_panelsizes()`.
#' 
#' If NULL, arguments `cols` and `rows` are instead supplied as unitless numeric vectors to `ggh4x::force_panelsizes()`. Proportions will instead be used to shape the facets instead of exact sizes.
#' @param ... arguments passed to `ggh4x::force_panelsizes()`
#' See examples for explanation of behavior and use of `respect` argument
#' @export

size <- function(

  # cols argument of ggh4x::force_panelsizes(),
  # widths of plots going across each column of facets
  # or width of a single plot / unfaceted plot
  cols = NULL,

  # rows argument of ggh4x::force_panelsizes(),
  # heights of plots going down each row of facets
  # or height of a single plot / unfaceted plot
  rows = NULL,

  # Unit applied to each numeric in rows and cols with grid::unit()
    # string supplied to grid::unit()
  # If NULL, supply unitless numeric vector to ggh4x::force_panelsizes()
    # which will use proportions to shape the facets instead of exact sizes,
    # (the plot will still fill the whole canvas)
  # Must supply respect = TRUE for unitless rows and cols to be proportional
    # to each other (similar behavior to ggplot2::theme(aspect.ratio = ...))
  # default "in"
  units = "in",

  # args passed to ggh4x::force_panelsizes()
  ...

) {

  # ----------------------------------------------------------------------------
  # Argument handling / aliases / deprecated arguments
  # ----------------------------------------------------------------------------
  # TODO
  
  other.args <- list(...)

  # ----------------------------------------------------------------------------
  # Tests
  # ----------------------------------------------------------------------------

  stopifnot(is.numeric(cols) || is.null(cols))
  stopifnot(is.numeric(rows) || is.null(rows))
  stopifnot(is.character(units) || is.null(units))

  # ----------------------------------------------------------------------------
  # ggh4x::force_panelsizes()
  # ----------------------------------------------------------------------------

  # Supply the rows/cols either as numeric (i.e., NULL units)
  # or supply them as the output of grid::unit()
  arg.rows <- NULL
  arg.cols <- NULL

  if (!is.null(rows)) {
    if (!is.null(units)) {
      arg.rows <- grid::unit(x = rows, units = units)
    } else {
      arg.rows <- rows
    }
  }
  if (!is.null(cols)) {
    if (!is.null(units)) {
      arg.cols <- grid::unit(x = cols, units = units)
    } else {
      arg.cols <- cols
    }
  }

  if (!is.null(arg.rows) || !is.null(arg.cols)) {
    return({
      list(
        rows = arg.rows,
        cols = arg.cols
      ) |>
        utils::modifyList(other.args) |>
        do.call(ggh4x::force_panelsizes, args = _)
    })
  } else {
    stop("Must specify at least one of \"rows\" or \"cols\"")
  }


}