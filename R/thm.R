# Updated 2024-10-28
# thm() = main theme function, flexible theme to add to any ggplot
# thm_gray() and thm_grey(), same function with minor tweaks that call on thm()
  # these call with the same intiial defaults, to thm(), with no missing() to thm()
  # then they make additional tweaks afterward
# Other helper functions
  # thm_bg()

#' @name thm
#' @title Custom theme
#' @description Result of `ggplot2::theme()` to add to a ggplot with `+`
#' @returns A `theme` class object (output of `ggplot2::theme()`)
#' @details Inspired and adapted by `cowplot::theme_cowplot()`. Argument names and order currently in development, subject to change (except for first argument `base.size`).
#' @examples
#' \dontrun{
#' # Use themes with `+`
#' p <- ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(hwy)) + ggplot2::geom_histogram()
#' p + mk::thm()
#' p + mk::thm_gray()
#' 
#' # Set the default theme at the top of a script
#' library(ggplot2)
#' theme_set(theme_get() + mk::thm())
#' theme_set(theme_get() + mk::thm(center.titles = TRUE))
#' }
#' @param base.size base size of text elements, also the text size of the title
#' @param bg color of background elements, default = "transparent". Determines background of panel, plot, legend, legend box, and strip background. For thm_gray(), it will not affect panel and strip background (which will be set gray)
#' @param title.face,subtitle.face,caption.face,tag.face face of main labs elements, e.g., "plain", "bold", "italic"
#' @param title.hjust,subtitle.hjust,caption.hjust,tag.hjust hjust of main labs elements, e.g., 0, 0.5, or 1
#' @param rel.title.text,rel.subtitle.text,rel.caption.text,rel.axis.title.text,rel.axis.text,rel.legend.title.text,rel.legend.text,rel.strip.text,rel.tag.text proportions for relative sizes of these elements relative to the `base.size`. E.g., base.size = 14 pts, then rel.subtitle.text = (12 / 14) * 14 = 12 pts
#' @param center.titles if TRUE, override other options and set hjust = 0 for title, subtitle, and caption
#' @export

thm <- function(

  # Base size of text elements (pt)
  base.size = 14,

  # Background elements
  bg = "transparent",

  # Faces of titles: plain, italic, or bold
  title.face = "plain",
  subtitle.face = "plain",
  caption.face = "plain",
  tag.face = "bold",

  # hjust of titles: 0, 0.5, or 1
  title.hjust = 0,
  subtitle.hjust = 0,
  caption.hjust = 1,
  tag.hjust = 0,

  # Sizes relative to base.size
  rel.title.text        = 14 / 14,
  rel.subtitle.text     = 12 / 14,
  rel.caption.text      = 10 / 14,
  rel.axis.title.text   = 12 / 14,
  rel.axis.text         = 10 / 14,
  rel.legend.title.text = 12 / 14,
  rel.legend.text       = 10 / 14,
  rel.strip.text        = 10 / 14,
  rel.tag.text          = 16 / 14,

  ## Misc other options
  # TRUE = Override other options and center the title, subtitle, and caption
  # NULL = do nothing
  center.titles = NULL


) {
  
  if (!is.null(center.titles)) {
    if (center.titles) {
      title.hjust <- 0.5
      subtitle.hjust <- 0.5
      caption.hjust <- 0.5
    }
  }
  
  ggplot2::theme(
    
    # Text base.size
    text = ggplot2::element_text(size = base.size),

    # Title, subtitle, caption, tag
    plot.title = ggplot2::element_text(
      face = title.face, hjust = title.hjust,
      size = rel.title.text * base.size
    ),
    plot.subtitle = ggplot2::element_text(
      face = subtitle.face, hjust = subtitle.hjust,
      size = rel.subtitle.text * base.size
    ),
    plot.caption = ggplot2::element_text(
      face = caption.face, hjust = caption.hjust,
      size = rel.caption.text * base.size
    ),
    plot.tag = ggplot2::element_text(
      face = tag.face, hjust = tag.hjust,
      size = rel.tag.text * base.size
    ),
    

    # Axis titles
    axis.title.x = ggplot2::element_text(
      size = rel.axis.title.text * base.size,
      margin = ggplot2::margin(3.5, 0, 0, 0, unit = "pt")
    ),
    axis.title.x.top = ggplot2::element_text(
      size = rel.axis.title.text * base.size,
      margin = ggplot2::margin(0, 0, 3.5, 0, unit = "pt")
    ),
    axis.title.y = ggplot2::element_text(
      size = rel.axis.title.text * base.size,
      margin = ggplot2::margin(0, 3.5, 0, 0, unit = "pt")
    ),
    axis.title.y.right = ggplot2::element_text(
      size = rel.axis.title.text * base.size,
      margin = ggplot2::margin(0, 0, 0, 3.5, unit = "pt")
    ),

    # Axis text
    axis.text = ggplot2::element_text(
      size = rel.axis.text * base.size,
      color = "black"
    ),
    axis.text.x = ggplot2::element_text(
      margin = ggplot2::margin(3, 0, 0, 0, unit = "pt")
    ),
    axis.text.x.top = ggplot2::element_text(
      margin = ggplot2::margin(0, 0, 3, 0, unit = "pt")
    ),
    axis.text.y = ggplot2::element_text(
      margin = ggplot2::margin(0, 3, 0, 0, unit = "pt")
    ),
    axis.text.y.right = ggplot2::element_text(
      margin = ggplot2::margin(0, 0, 0, 3, unit = "pt")
    ),

    # Axis ticks
    axis.ticks = ggplot2::element_line(
      color = "black", linewidth = 0.5
    ),
    axis.ticks.length = grid::unit(3.5, units = "points"),

    # Axis lines
    axis.line = ggplot2::element_line(
      color = "black", linewidth = 0.5
    ),

    # Panel grid
    panel.grid = ggplot2::element_blank(),

    # Legend
    legend.text = ggplot2::element_text(size = rel.legend.text * base.size),
    legend.title = ggplot2::element_text(size = rel.legend.title.text * base.size),
    legend.key.size = grid::unit(15, units = "points"),
    legend.justification = c("left", "center"),
    
    # Backgrounds
    panel.background = ggplot2::element_rect(fill = bg, color = NA),
    plot.background = ggplot2::element_rect(fill = bg, color = NA),
    legend.background = ggplot2::element_rect(fill = bg, color = NA),
    legend.box.background = ggplot2::element_rect(fill = bg, color = NA),

    # Faceting
    strip.clip = "off",
    strip.text = ggplot2::element_text(
      margin = ggplot2::margin(3.5, 3.5, 3.5, 3.5, unit = "pt"),
      size = rel.strip.text * base.size
    ),
    strip.background = ggplot2::element_rect(
      fill = "transparent", color = NA
    ),
    strip.placement = "outside"
    
  )

}

#' @rdname thm
#' @export

thm_gray <- function(

  base.size = 14,
  bg = "transparent",
  title.face = "plain",
  subtitle.face = "plain",
  caption.face = "plain",
  tag.face = "bold",
  title.hjust = 0,
  subtitle.hjust = 0,
  caption.hjust = 1,
  tag.hjust = 0,
  rel.title.text        = 14 / 14,
  rel.subtitle.text     = 12 / 14,
  rel.caption.text      = 10 / 14,
  rel.axis.title.text   = 12 / 14,
  rel.axis.text         = 10 / 14,
  rel.legend.title.text = 12 / 14,
  rel.legend.text       = 10 / 14,
  rel.strip.text        = 10 / 14,
  rel.tag.text          = 16 / 14,
  center.titles = NULL

) {

  thm(
    base.size = base.size,
    bg = bg,
    title.face = title.face,
    subtitle.face = subtitle.face,
    caption.face = caption.face,
    tag.face = tag.face,
    title.hjust = title.hjust,
    subtitle.hjust = subtitle.hjust,
    caption.hjust = caption.hjust,
    tag.hjust = tag.hjust,
    rel.title.text = rel.title.text,
    rel.subtitle.text = rel.subtitle.text,
    rel.caption.text = rel.caption.text,
    rel.axis.title.text = rel.axis.title.text,
    rel.axis.text = rel.axis.text,
    rel.legend.title.text = rel.legend.title.text,
    rel.legend.text = rel.legend.text,
    rel.strip.text = rel.strip.text,
    rel.tag.text = rel.tag.text,
    center.titles = center.titles
  ) +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(
        fill = "grey92",
        color = NA
      ),
      panel.grid = ggplot2::element_line(color = "white"),
      strip.background = ggplot2::element_rect(
        fill = "grey85", color = NA
      )
    )

}

#' @rdname thm
#' @export

thm_grey <- function(

  base.size = 14,
  bg = "transparent",
  title.face = "plain",
  subtitle.face = "plain",
  caption.face = "plain",
  tag.face = "bold",
  title.hjust = 0,
  subtitle.hjust = 0,
  caption.hjust = 1,
  tag.hjust = 0,
  rel.title.text        = 14 / 14,
  rel.subtitle.text     = 12 / 14,
  rel.caption.text      = 10 / 14,
  rel.axis.title.text   = 12 / 14,
  rel.axis.text         = 10 / 14,
  rel.legend.title.text = 12 / 14,
  rel.legend.text       = 10 / 14,
  rel.strip.text        = 10 / 14,
  rel.tag.text          = 16 / 14,
  center.titles = NULL

) {

  thm(
    base.size = base.size,
    bg = bg,
    title.face = title.face,
    subtitle.face = subtitle.face,
    caption.face = caption.face,
    tag.face = tag.face,
    title.hjust = title.hjust,
    subtitle.hjust = subtitle.hjust,
    caption.hjust = caption.hjust,
    tag.hjust = tag.hjust,
    rel.title.text = rel.title.text,
    rel.subtitle.text = rel.subtitle.text,
    rel.caption.text = rel.caption.text,
    rel.axis.title.text = rel.axis.title.text,
    rel.axis.text = rel.axis.text,
    rel.legend.title.text = rel.legend.title.text,
    rel.legend.text = rel.legend.text,
    rel.strip.text = rel.strip.text,
    rel.tag.text = rel.tag.text,
    center.titles = center.titles
  ) +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(
        fill = "grey92",
        color = NA
      ),
      panel.grid = ggplot2::element_line(color = "white"),
      strip.background = ggplot2::element_rect(
        fill = "grey85", color = NA
      )
    )

}

#' @title Set background colors in a ggplot
#' @description Set background of panel, plot, legend, legend box, and/or strip
#' @returns A `theme` class object (output of `ggplot2::theme()`)
#' @examples
#' \dontrun{
#' # Example plot
#' set.seed(123)
#' df <- ggplot2::mpg |>
#'   dplyr::group_by(model) |>
#'   dplyr::summarize(hwy = mean(hwy, na.rm = TRUE)) |>
#'   dplyr::slice_sample(n = 4) |>
#'   dplyr::mutate(model2 = model)
#' p <- ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = model, y = hwy)) +
#'   ggplot2::facet_wrap(
#'     ggplot2::vars(model), nrow = 1, scales = "free_x",
#'     labeller = \(x) df[, "model"] |> lapply(\(x) stringr::str_wrap(x, 10)),
#'     strip.position = "top"
#'   ) +
#'   ggplot2::geom_col(mapping = ggplot2::aes(fill = model)) +
#'   ggplot2::geom_point(mapping = ggplot2::aes(color = model2)) +
#'   ggplot2::labs(title = "Title", subtitle = "Subtitle", caption = "Caption")
#' 
#' # Original plot
#' p
#' 
#' # Transparency in all areas
#' p + mk::thm_bg("transparent")
#' 
#' # Transparency only in the overall plot background
#' p + mk::thm_bg(plot = "transparent")
#' 
#' # Transparency in all, but exclude panel and strip
#'   # from modification (FALSE instead of NULL)
#' p + mk::thm_bg("transparent", panel = FALSE, strip = FALSE)
#' 
#' # Transparency in all, but gray for panel and strip
#' p + mk::thm_bg("transparent", panel = "grey92", strip = "grey85")
#' 
#' # Indvidiually supply backgrounds to certain areas
#' p + mk::thm_bg(panel = "#2C3E50")
#' 
#' p + mk::thm_bg(
#'   panel = "maroon",
#'   plot = "purple",
#'   legend = "blue",
#'   legend.box = "pink",
#'   strip = "green"
#' )
#' }
#' @param all a color or "transparent" to apply to all elements, if NULL, need to specify one or more of the other more specific arguments
#' @param panel,plot,legend,legend.box,strip individually set backgrounds of these elements of the theme(), either or a color or "transparent", will invidiually override setting in `all`. NULL to not override anything. or if `!is.null(all)`, use FALSE to exclude a setting
#' @export

thm_bg <- function(
  all        = NULL,
  panel      = NULL,
  plot       = NULL,
  legend     = NULL,
  legend.box = NULL,
  strip      = NULL
) {
  
  args <- list()
  if (!is.null(all)) {
    args$panel <- all
    args$plot <- all
    args$legend <- all
    args$legend.box <- all
    args$strip <- all
  }

  if (!is.null(panel)) {
    if (is.logical(panel)) {
      stopifnot(length(panel) == 1)
      if (panel) {
        stop("Argument \"panel\" should either be a color, \"transparent\", or FALSE (if argument \"all\" is used)")
      } else {
        args$panel <- NULL
      }
    } else {
      args$panel <- panel
    }

  }
  if (!is.null(plot)) {
    if (is.logical(plot)) {
      stopifnot(length(plot) == 1)
      if (plot) {
        stop("Argument \"plot\" should either be NULL, a color, \"transparent\", or FALSE (if argument \"all\" is used)")
      } else {
        args$plot <- NULL
      }
    } else {
      args$plot <- plot
    }

  }
  if (!is.null(legend)) {
    if (is.logical(legend)) {
      stopifnot(length(legend) == 1)
      if (legend) {
        stop("Argument \"legend\" should either be NULL, a color, \"transparent\", or FALSE (if argument \"all\" is used)")
      } else {
        args$legend <- NULL
      }
    } else {
      args$legend <- legend
    }

  }
  if (!is.null(legend.box)) {
    if (is.logical(legend.box)) {
      stopifnot(length(legend.box) == 1)
      if (legend.box) {
        stop("Argument \"legend.box\" should either be NULL, a color, \"transparent\", or FALSE (if argument \"all\" is used)")
      } else {
        args$legend.box <- NULL
      }
    } else {
      args$legend.box <- legend.box
    }

  }
  if (!is.null(strip)) {
    if (is.logical(strip)) {
      stopifnot(length(strip) == 1)
      if (strip) {
        stop("Argument \"strip\" should either be NULL, a color, \"transparent\", or FALSE (if argument \"all\" is used)")
      } else {
        args$strip <- NULL
      }
    } else {
      args$strip <- strip
    }

  }

  if (length(args) == 0) {
    stop("must supply at least one non-NULL argument")
  }

  res <- ggplot2::theme()
  
  if (!is.null(args$panel)) {
    res <- res + ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = args$panel, color = NA)
    )
  }

  if (!is.null(args$plot)) {
    res <- res + ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = args$plot, color = NA)
    )
  }

  if (!is.null(args$legend)) {
    res <- res + ggplot2::theme(
     legend.background = ggplot2::element_rect(fill = args$legend, color = NA) 
    )
  }

  if (!is.null(args$legend.box)) {
    res <- res + ggplot2::theme(
      legend.box.background = ggplot2::element_rect(fill = args$legend.box, color = NA)   
    )
  }

  if (!is.null(args$strip)) {
    res <- res + ggplot2::theme(
      strip.background = ggplot2::element_rect(fill = args$strip, color = NA)
    )
  }
  
  res

}
