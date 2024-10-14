# Updated 2024-10-11
# TODO: deal with non-unique values in a vector input instead of list input
  # also should be able to handle a named vector and turn that into a named
  # list for proper input

#' @title Plot color palettes
#' @description Provide a vector of colors, or named list of multiple palettes, and return a plot
#' @returns a ggplot object
#' @details Each color gets a shape from ggplot2::geom_rect which is spaced at x-y coordinates
#' @examples
#' # Plot a single palette
#' mk::plot_colors(mk::gg_colors(8))
#'
#' # Customize the palette title shown
#' mk::plot_colors(list(`Default ggplot2 colors` = mk::gg_colors(8)))
#'
#' # Plot in multiple rows and multiple palettes
#' # ncol default is 8
#' mk::plot_colors(
#'   colors = list(
#'     `viridis::turbo(12)` = viridis::turbo(12),
#'     `RColorBrewer::brewer.pal(8, "Set2")` = RColorBrewer::brewer.pal(8, "Set2")
#'   ),
#'   ncol = 4
#' )
#'
#' # Put palette title above each palette instead of to the right
#' # Show color values / hex codes inside cells
#' mk::plot_colors(
#'   colors = list(
#'     `RColorBrewer::brewer.pal(9, "Reds")`      = RColorBrewer::brewer.pal(9, "Reds"),
#'     `RColorBrewer::brewer.pal(9, "Blues")`     = RColorBrewer::brewer.pal(9, "Blues"),
#'     `RColorBrewer::brewer.pal(11, "Spectral")` = RColorBrewer::brewer.pal(11, "Spectral")
#'   ),
#'   ncol = 11,
#'   text.size = 4,
#'   title.loc = "above"
#' )
#' @param colors colors to plot, <chr> vector or list of <chr> vectors (named or unnamed)
#' @param ncol number of colors per row plotted, default 8
#' @param text.size text size of color/hex values inside cells, default 0
#' @param pal.text.size text size of labels per each color palette, default 5
#' @param title.loc location of palette titles, one of "right" (default) or "above"
#' @param pal.spacing spacing between palettes (in units of cell height), default NULL is 0.25 for title.loc = "right" or 1.25 for title.loc = "above", otherwise provide numeric
#' @param right.margin margin to the right, e.g., for title.loc = "right". default NULL is 0 for title.loc = "above", or attempts to guess for title.loc = "right". Otherwise, provide <numeric> in inches
#' @param linewidth border linewidth, default 0.5
#' @param cell.aspect aspect ratio per color cell (height / width), <numeric>. default 0.5, NULL for no aspect.ratio applied.
#' @param dev Don't plot, just return the dataframe used for plotting, default FALSE
#' @export

plot_colors <- function(

  # Colors to plot
  colors,

  # How many colors per row?
  ncol = 8,

  # Size of text labels
  text.size = 0,

  # Size of palette text labels (numeric)
  # default slightly larger than text.size
  pal.text.size = 5,

  # location of palette titles
  # above or to the right of each palette
  title.loc = c("right", "above"),

  # Space between palettes (in units of cell heights)
  # NULL = 1.25 for title.loc = "above"
  # NULL = 0.25 for title.loc = "right"
  # otherwise, <numeric>
  pal.spacing = NULL,

  # margin to the right, e.g., for right palette titles
  # NULL = zero for title.loc = "above"
  # NULL = try to guess for title.loc = "right"
  # otherwise, <numeric> for inches
  right.margin = NULL,

  # Border linewidth
  linewidth = 0.5,

  # Aspect ratio per color cell (height / width)
  # NULL = no aspect.ratio applied
  cell.aspect = 0.5,

  # Don't plot, just return the dataframe used for plotting
  dev = FALSE

) {

  # If colors is atomic, change to length 1 named list
  if (is.atomic(colors)) {
    var.name <- deparse(substitute(colors))
    colors <- list(colors)
    names(colors) <- var.name
  }

  # If list has no names, give it empty string names
  if (is.null(names(colors))) {
    names(colors) <- rep(length(colors), "")
  }

  # Store list names here for later
  colors.names <- names(colors)

  # ----------------------------------------------------------------------------
  # Dataframe manipulation, creates a dataframe per each palette first
  #   Sets x and y coordinates in rows according to argument ncol
  #     and sets some gaps between the groups.
  #   Joins into a single dataframe before plotting
  # ----------------------------------------------------------------------------

  # Fill df to plot from list of color palettes
  df.list <- base::Map(colors, seq_len(length(colors)), f = \(palette, i) {
    n.colors <- length(palette)
    y.vals <- floor(seq(from = 1, by = 1 / ncol, length.out = n.colors))
    y.vals <- max(y.vals) + 1 - y.vals
    y.vals.rle <- rle(y.vals)
    x.vals <- lapply(y.vals.rle$lengths, \(len) {
      seq(from = 1, by = 1, length.out = len)
    }) |> unlist()
    hex.char <- as.character(palette)
    data.frame(
      x = x.vals,
      y = y.vals,
      hex = palette,
      hex.char = hex.char,
      group = rep(i, length(palette))
    )
  })

  # List of num rows per each df
  nrows.df.list <- lapply(colors, \(palette) {
    n.colors <- length(palette)
    y.vals <- floor(seq(from = 1, by = 1 / ncol, length.out = n.colors))
    y.vals <- max(y.vals) + 1 - y.vals
    length(rle(y.vals)$length)
  })
  # print(nrows.df.list) |> unlist()

  # Set spacing between palettes
  if (is.null(pal.spacing)) {
    if (title.loc[1] == "above") {
      pal.spacing <- 1.25
    } else if (title.loc[1] == "right") {
      pal.spacing <- 0.25
    }
  } else {
    stopifnot(is.numeric(pal.spacing))
  }

  # Adjust yvals so each palette in its own group spatially in the plot
  prev.min <- 1
  for (i in seq_along(df.list)[-1]) {
    this.max.y <- max(df.list[[i]]$y)
    offset <- this.max.y - prev.min
    df.list[[i]]$y <- df.list[[i]]$y - offset - 1 - pal.spacing
    prev.min <- min(df.list[[i]]$y)
  }

  # unique_id
  df.list <- lapply(df.list, \(df) {
    df$id <- paste0(df$group, ";", df$hex)
    df
  })

  # Join df's and add ggplot2::geom_rect() specifications
  df <- do.call(rbind, df.list)
  df$xmin <- df$x - 0.5
  df$xmax <- df$x + 0.5
  df$ymin <- df$y - 0.5
  df$ymax <- df$y + 0.5

  # scale fill arg
    # values = colors
    # names = unique id
  scale.fill.arg <- df$hex
  names(scale.fill.arg) <- df$id

  # Doesn't really matter, but factor order
  # for the ids for the non-visible legends
  df$id <- factor(df$id, levels = df$id)

  if (dev) return(df)

  # ----------------------------------------------------------------------------
  # Plot
  # ----------------------------------------------------------------------------

  aes <- ggplot2::aes

  p <- ggplot2::ggplot(
    data = df,
    mapping = aes(x = x, y = y)
  ) +
    ggplot2::geom_rect(
      mapping = aes(fill = id, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      color = "black",
      linewidth = linewidth,
      show.legend = FALSE
    ) +
    ggplot2::scale_fill_manual(
      values = scale.fill.arg,
      labels = df$id
    ) +
    ggplot2::geom_text(
      mapping = aes(label = hex),
      color = "black",
      size = text.size
    ) +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(add = c(0.5, 0.5))
    ) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(add = c(0.5, 0.5))
    ) +
    ggplot2::theme_void() +
    ggplot2::coord_cartesian(clip = "off")

  if (!is.null(cell.aspect)) {
    # Takes into account total y height and x width and
    # gaps in between and x/y axis expand values
    y.height <- (max(df$y) - min(df$y)) + 1 + 1
    plot.aspect <- (y.height / (ncol + 1)) * cell.aspect
    p <- p + ggplot2::theme(aspect.ratio = plot.aspect)
  }

  # Add some margin to the right for right palette titles
  if (title.loc[1] == "right") {
    # if null, try to guess right margin, works well for default pal.text.size
    if (is.null(right.margin)) {
      # Find longest palette title
      nchar.longest <- max(sapply(colors.names, nchar))
      p <- p + ggplot2::theme(
        plot.margin = ggplot2::margin(0, nchar.longest * 1.75 * pal.text.size, 0, 0, unit = "pt")
      )
    }
  }

  # Add margin to the right if specified, in inches
  if (!is.null(right.margin)) {
    stopifnot(is.numeric(right.margin))
    p <- p + ggplot2::theme(
      plot.margin = ggplot2::margin(0, right.margin, 0, 0, unit = "in")
    )
  }

  # ----------------------------------------------------------------------------
  # Label locations per group
  # ----------------------------------------------------------------------------

  # For each group in the dataframe, get x and y locations of palette titles
  label.locs <- lapply(seq_len(length(unique(df$group))), \(group.oi) {
    df.group <- subset(df, group %in% group.oi)
    if (title.loc[1] == "above") {
      return(list(x = 0.5, y = max(df.group$y + 1)))
    }
    if (title.loc[1] == "right") {
      return(list(
        x = ncol + 0.6,
        y = max(df.group$y) - ((max(df.group$y) - min(df.group$y)) / 2)
      ))
    }
  })

  # dev, keep nums
  # p <- p + ggplot2::theme(axis.text = ggplot2::element_text())

  stopifnot(identical(length(colors.names), length(label.locs)))

  # For each label location give add a geom_annotate
  for (i in seq_along(label.locs)) {
    p <- p +
      ggplot2::annotate(
        geom = "text",
        # x = 0.5, y = label.locs[i],
        x = label.locs[[i]]$x, label.locs[[i]]$y,
        label = colors.names[i],
        hjust = 0, size = pal.text.size,
        family = "mono"
      )
  }

  return(p)

  # avoid R CMD CHECK "no visible binding" messages
  x <- NULL; y <- NULL; id <- NULL; xmin <- NULL; xmax <- NULL
  ymin <- NULL; ymax <- NULL; hex <- NULL; group <- NULL

}
