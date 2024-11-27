# Updated 2024-11-27
# Depends on mk::thm()
# Depends on mk::thm_bg()

#' @title Waterfall plot for GSEA results
#' @description Take df of GSEA results and return waterfall plot
#' @returns A ggplot object
#' @examples
#' \dontrun{
#' mk::waterfall_plot(
#'   df = df |> dplyr::filter(qvalue < 0.05),
#'   sig = "qvalue",
#'   fill.preset = "0.05",
#'   title = "Hallmark Pathways",
#'   subtitle = "Group A vs. Group B",
#'   caption = "Pathways for q < 0.05"
#' )
#' }
#' @param df A dataframe of gsea results, must include columns: Description, NES, and a significance column e.g., qvalue
#' @param title,subtitle,caption ggplot titles, etc.
#' @param x,y xlab and ylab for ggplot, default NULL and "NES"
#' @param sig Column of `df` to use for significance, default = "qvalue"
#' @param type Type of waterfall plot, only current option is "gradient" to fill bars according to continuous significance value
#' @param fill.limits,fill.breaks,fill.labels Arguments to ggplot2::scale_fill_gradientn() when using `type = "gradient"`
#' @param fill.preset default NULL, or ignore the fill arguments above and use some preset values, one of: c("0.05", "0.15", "0.25")
#' @param colors, default NULL to use a red color scheme for `type = "gradient"`, or a vector of colors
#' @param fill.title default NA for default title, or use NULL or a custom title for the fill legend
#' @param base.size pass argument to mk::thm() to change default sizes of text elements in plot, default NULL to take the default value in mk::thm()
#' @param format TRUE (default) = consider the formatting arguments below in order, FALSE = turn all off
#' @param .gsub.hallmark TRUE (default) = Remove pattern ^HALLMARK_ from Description column
#' @param .gsub.underscore TRUE (default) = Replace pattern "_" with " " from Description column
#' @export

waterfall_plot <- function(

  # A dataframe of gsea results
  # Must include columns:
    # Description (names of pathways)
    # NES (normalized enrichment score)
    # The column for argument `sig` below, e.g., "qvalue"
  df,

  # Titles, etc.
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  x = NULL,
  y = "NES",
  
  # Which column of `df` to use for significance?
  sig = c("qvalue"),

  # How to fill the bars?
  # Only option `gradient` currently available
    # gradient = gradient fill for continuous significance variable
  type = c("gradient"),

  # Arguments to ggplot2::scale_fill_gradientn() when using `type = "gradient"`
  fill.limits = NULL,
  fill.breaks = NULL,
  fill.labels = NULL,

  # Or ignore these 3 arguments above and use a preset <chr>:
    # 0.05
    # 0.15
    # 0.25
  fill.preset = NULL,

  # NULL (default)
    # For type = gradient, uses a red color scheme
  # <chr>
    # Or provide a vector of colors to use as the gradient
  colors = NULL,

  # NA = default title for fill legend title
  # NULL = none, or a custom title
  fill.title = NA,

  # Pass argument to mk::thm(),
  base.size = NULL,

  # format = TRUE (default) --> use TRUE/FALSE options below
  # format = FALSE --> consider all format options below = FALSE
  format = TRUE,

  # Remove pattern ^HALLMARK from Description column
  .gsub.hallmark = TRUE,
  # Replace pattern "_" with " " from Description column
  .gsub.underscore = TRUE


) {
  
  
  # ----------------------------------------------------------------------------
  # Checks
  # ----------------------------------------------------------------------------
  
  # df = data.frame
  if (!inherits(df, "data.frame")) stop("df must be of class \"data.frame\"")
  
  # Columns of df
  check_col <- function(df, col) {
    if (!(col %in% colnames(df))) {
      stop(paste0("Column \"", col, "\" must be present in argument `df`"))
    } else {
      return(invisible(NULL))
    }
  }
  check_col(df, "Description")
  check_col(df, "NES")
  check_col(df, sig[1])

  # ----------------------------------------------------------------------------
  # Main plot
  # ----------------------------------------------------------------------------
  
  df <- df |> dplyr::arrange(NES)
  
  if (format) {
    if (.gsub.hallmark) df$Description <- sub("^HALLMARK_", "", df$Description)
    if (.gsub.underscore) df$Description <- gsub("_", " ", df$Description)
  }

  p <- ggplot2::ggplot(
    data = df,
    mapping = ggplot2::aes(x = Description, y = NES)
  ) +
    ggplot2::geom_col(mapping = ggplot2::aes(fill = .data[[sig[1]]])) +
    ggplot2::scale_x_discrete(limits = df$Description)
  
  # ----------------------------------------------------------------------------
  # Fill according to argument `type`
    # E.g., scale_fill_gradientn()
  # ----------------------------------------------------------------------------
  
  if (is.null(colors)) {
    if (type[1] %in% "gradient") {
      reds <- rev(RColorBrewer::brewer.pal(9, "Reds")) |>
        grDevices::colorRampPalette()
      colors.use <- reds(256)
    }
  } else {
    colors.use <- colors
    if (!is.character(colors.use)) {
      stop("Argument \"colors\" must be either NULL or a character vector of colors")
    }
  }

  if (type[1] %in% "gradient") {
    fill.args <- list()
    fill.args$colors <- colors.use
    
    if (is.null(fill.preset)) {
      if (!is.null(fill.limits)) fill.args$limits <- fill.limits
      if (!is.null(fill.breaks)) fill.args$breaks <- fill.breaks
      if (!is.null(fill.labels)) fill.args$labels <- fill.labels
    } else {
      fill.args <- switch(fill.preset,
        `0.05` = {
          fill.args |>
            (\(x) {
              x$limits <- c(0, 0.05)
              x$breaks <- c(0.01, 0.03, 0.05)
              x$labels <- c("0.01", "0.03", "0.05")
              x
            })()
        },
        `0.15` = {
          fill.args |>
            (\(x) {
              x$limits <- c(0, 0.15)
              x$breaks <- c(0.05, 0.10, 0.15)
              x$labels <- c("0.05", "0.10", "0.15")
              x
            })()
        },
        `0.25` = {
          fill.args |>
            (\(x) {
              x$limits <- c(0, 0.25)
              x$breaks <- c(0.05, 0.15, 0.25)
              x$labels <- c("0.05", "0.15", "0.25")
              x
            })()
        }
      )
    }

    p <- p + do.call(ggplot2::scale_fill_gradientn, fill.args)

  } else {
    stop("Argument \"type\" must be one of: gradient")
  }

  # ----------------------------------------------------------------------------
  # Plot labels, theme, etc.
  # ----------------------------------------------------------------------------
  
  p <- p +
    ggplot2::coord_flip()

  # base.size argument for adding mk::thm()
  if (!is.null(base.size)) {
    p <- p + thm(base.size = base.size)
  } else {
    p <- p + thm()
  }
      
  p <- p + ggplot2::theme(
      panel.grid = ggplot2::element_line(color = "lightgrey"),
      legend.title = ggplot2::element_text(margin = ggplot2::margin(b = 15))
    ) +
    thm_bg(panel = "white") +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      caption = caption,
      x = x,
      y = y
    )
 
  if (is.null(fill.title)) {
    p <- p + ggplot2::labs(fill = fill.title)
  } else {
    if (!is.na(fill.title)) {
      p <- p + ggplot2::labs(fill = fill.title)
    }
  }

  # ----------------------------------------------------------------------------

  return(p)
  
  # avoid R CMD CHECK "no visible binding" messages
  NES <- NULL; Description <- NULL; .data <- NULL

}
