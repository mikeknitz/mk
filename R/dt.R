#' @title Wrapper for DT::datatable()
#' @description Takes a dataframe and makes an interactive html table
#' @returns html widget
#' @details Re: arguments `cols.fac.color` and `lighten`: DT renders with CSS values of colors, so "green" may be different than R green, and !is.null(lighten) forces hex codes instead of color "green", so lighten = NULL and lighten = 1, may yield different results
#' @examples
#' # dataframe to summarize
#' df <- lapply(split(iris, iris$Species), \(df) {
#'   df[sample(nrow(df), 5), ]
#' }) |> do.call(rbind, args = _)
#' df$pval <- seq(0.01, 0.15, 0.01)
#' 
#' # Various options
#' mk::dt(
#'   df = df,
#'   title = "A title",
#'   subtitle = c("Line one of the subtitle", "and line two"),
#'   page.length = 15,
#'   plain = TRUE,
#'   row.names = FALSE,
#'   # Specify columns by indices or names
#'   cols.nowrap = colnames(df),
#'   cols.round.2 = "Petal.Width",
#'   cols.round.3 = 1,
#'   cols.round.4 = c("Sepal.Width", "Petal.Length", "pval"),
#'   cols.pval = "pval",
#'   cols.fac.color =  "Species"
#' )
#' 
#' # Explicit colors
#' mk::dt(
#'   df = df[, c("Species", "pval")],
#'   plain = TRUE,
#'   page.length = 15,
#'   width = 200,
#'   cols.round.4 = 2,
#'   cols.fac.color = list(
#'     Species = c("#EA4F0D", "#29EFA2", "#4490FE")
#'   )
#' )
#' @param df a dataframe
#' @param title,subtitle title and subtitle(s)
#' @param page.length length of each page
#' @param plain FALSE (default) = typical datatable with controls, search, sorting, etc.
#' 
#' TRUE = more simplified, less controls, sets `dom = "tr"` within `DT::datatable(options = ...)`
#' @param row.names Whether to show row.names() of data.frame, default = FALSE
#' @param width width e.g., "95%" or "500px" or 500, less than 95% helps with unnecessary scrollbars
#' @param cols.round.1 colnames/indices in which to round to 1 decimal place
#' @param cols.round.2 colnames/indices in which to round to 2 decimal places
#' @param cols.round.3 colnames/indices in which to round to 3 decimal places
#' @param cols.round.4 colnames/indices in which to round to 4 decimal places
#' @param cols.nowrap colnames/indices where content will not be wrapped
#' @param cols.pval column names/indices where cells with numbers [0, 0.05) will be shaded yellow and [0.05, 0.15) shaded orange
#' @param cols.pos.neg column names/indices where cells with numbers will be shaded green/red for positive/negative values
#' @param cols.fac.color default NULL for no coloring, or one of:
#' 
#' - character or numeric vector = provide colnames/indices for character columns
#' 
#' - list where list names = column names and list items = <chr> hex codes / colors for levels in the column, list items may or may not be named characters where names() refer to the levels
#' @param lighten lighten the provided colors to argument "cols.fac.color" by a factor where 1 = original color and 0 = white, default = 0.35, NULL for no lightening
#' @export

dt <- function(
  
  # A dataframe
  df,

  # Title and subtitle
  # Subtitle can be length > 1 for multiple lines
  title = "",
  subtitle = c(""),

  # nrow of each page of table
  page.length = 10,

  # FALSE (default) = typical datatable with controls, search, sorting, etc.
  # TRUE = more simplified, less controls, sets dom = "tr" within `DT::datatable(options = ...)`
  plain = FALSE,

  # Whether to show row.names() of the data.frame
  # default = FALSE
  row.names = FALSE,

  # Width, e.g., "95%" or "500px" or 500
  # Less than 95% helps with unnecessary scrollbars
  width = "95%",

  cols.round.1 = NULL,
  cols.round.2 = NULL,
  cols.round.3 = NULL,
  cols.round.4 = NULL,
  cols.nowrap = NULL,
  cols.pval = NULL,
  cols.pos.neg = NULL,

  # coloring based on factor levels
    # <chr> or <numeric> input
      # provide colnames/indices corresponding to character columns
      # in dataframe, uses lightened default ggplot colors
    # <list> input
      # list names = column names
      # list items = <chr> hex codes for the levels of the particular column,
        # or a named character vector where the names() map to the levels
  # default = NULL
  cols.fac.color = NULL,

  # If using cols.fac.color, apply mk::lighten_colors(factor = 0.35)
    # to each color before styling table, where factor is in [0, 1]
    # and 1 = original color and smaller = lighter
  # default = 0.35
  # NULL = do not apply any lightening
  # NOTE: DT renders with CSS values of colors, so "green"
    # may be different than R green, and !is.null(lighten)
    # forces hex codes instead of color "green", so lighten = NULL,
    # and lighten = 1, may yield different results
  lighten = 0.35

) {
  

  # ----------------------------------------------------------------------------
  # Start building arguments to DT::datatable()
  # ----------------------------------------------------------------------------
  
  args.list <- list(
    data = df,
    class = c("display", "compact"),
    rownames = row.names,
    fillContainer = FALSE,
    width = width
  )

  # ----------------------------------------------------------------------------
  # `options` argument to DT::datatable()
  # ----------------------------------------------------------------------------
  
  args.options <- list(
    pageLength = page.length,
    autoWidth = TRUE,
    initComplete = htmlwidgets::JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'font-size': '0.8em'});",
      "}"
    )
  )

  if (plain) {
    args.options$dom <- "tr"
  }
  
  # merge with main args.list
  args.list$options <- args.options

  # ----------------------------------------------------------------------------
  # `captions` argument to DT::datatable()
  # ----------------------------------------------------------------------------
  
  args.caption <- list(
    style = paste("caption-side: top;", "text-align: left;"),
    htmltools::tags$p(style = "font-size: 1.5em",
      title
    )
  )

  for (i in seq_along(subtitle)) {
    args.caption <- c(args.caption, list(
        htmltools::tags$p(
          style = "font-size: 1.1em; margin-left: 20px; margin-top: -10px",
          htmltools::tags$i(
            subtitle[i]
          )
        )
      )
    )
  }

  args.list$caption <- do.call(htmltools::tags$caption, args.caption)

  # ----------------------------------------------------------------------------
  # Call DT::datatable() and make further adjustments
  # ----------------------------------------------------------------------------
  
  res <- do.call(DT::datatable, args = args.list)
  
  # Font size of cells
  res <- res |> DT::formatStyle(seq_along(df), fontSize = "1em")

  # Line height of all rows
  res <- res |> DT::formatStyle(0, target = "row", lineHeight = "1em")
  
  # Round nums to 1 decimals
  if (!is.null(cols.round.1)) {
    res <- res |> DT::formatRound(cols.round.1, digits = 1)
  }
  
  # Round nums to 2 decimals
  if (!is.null(cols.round.2)) {
    res <- res |> DT::formatRound(cols.round.2, digits = 2)
  }
  
  # Round nums to 3 decimals
  if (!is.null(cols.round.3)) {
    res <- res |> DT::formatRound(cols.round.3, digits = 3)
  }

  # Round nums to 4 decimals
  if (!is.null(cols.round.4)) {
    res <- res |> DT::formatRound(cols.round.4, digits = 4)
  }

  # No wrap columns
  if (!is.null(cols.nowrap)) {
    res <- res |> DT::formatStyle(cols.nowrap, whiteSpace = "nowrap")
  }

  # pval coloring to columns
  if (!is.null(cols.pval)) {
    res <- res |>
      DT::formatStyle(cols.pval, backgroundColor = DT::styleInterval(
        c(0.05, 0.25), c("#F3DD86", "#F2CC9B", NA)
      ))
  }

  # green/red colors for pos/neg
  if (!is.null(cols.pos.neg)) {
    res <- res |>
      DT::formatStyle(cols.pos.neg, backgroundColor = DT::styleInterval(
        0, c("#ECBBB6", "#B2D3C4")
      ))
  }
  
  # ----------------------------------------------------------------------------
  # cols.fac.color argument
  # ----------------------------------------------------------------------------
  
  if (!is.null(cols.fac.color)) {

    # Either <chr> or <list> input for cols.fac.color
      # see argument descriptions
    
    cols.fac.color.err.mess <- paste0(
      "argument \"cols.fac.color\" must be one of:\n",
      "- a character vector of valid column names\n",
      "- a numeric vector of valid column indices\n",
      "- a list mapping multiple columns (list names) to hex codes (list items)\n"
    )
    
    if (is.list(cols.fac.color)) {

      #############################################
      #####   Tests for cols.fac.color arg    #####
      #############################################
      # Also creates named.cols.fac.color, which is
      # used for list form of cols.fac.color to
      # color the factor columns
      
      if (!all(sapply(cols.fac.color, \(x) {
        is.character(x) || is.factor(x)
      }))) {
        stop(paste0(
          "Not all list items are of class character or factor\n",
          cols.fac.color.err.mess
        ))
      }

      if (is.null(names(cols.fac.color))) {
        stop(paste0(
          "No list names found in list argument \"cols.fac.color\"\n",
          cols.fac.color.err.mess
        ))
      } else {
        if (any(is.na(names(cols.fac.color)))) {
          stop(paste0(
            "NA names found in list item names for argument \"cols.fac.color\"\n",
            cols.fac.color.err.mess
          ))
        }
      }
      
      if (!all(names(cols.fac.color) %in% colnames(df))) {
        stop(paste0(
          "Not all list item names in \"cols.fac.color\" found in colnames(df)\n",
          cols.fac.color.err.mess
        ))
      }
      
      if (any(duplicated(names(cols.fac.color)))) {
        stop(paste0(
          "Duplicated list items name found in list argument \"cols.fac.color\"\n",
          cols.fac.color.err.mess
        ))
      }

      # Tests each individual list item
      # Rewrites list items into `named.cols.fac.color` and ensures
        # that each list item is a named character if unnamed are provided
      dp.cols.fac.color <- deparse(substitute(cols.fac.color))
      named.cols.fac.color <- list()
      invisible(base::Map(
        cols.fac.color, names(cols.fac.color),
        f = \(colors, nm) {
          col.vec.matched <- df[[nm]] |> factor()
          if (length(colors) != nlevels(col.vec.matched)) {
            stop(paste0(
              "number of colors provided in ",  dp.cols.fac.color, "[[", nm, "]]\n",
              "  does not match number of levels in the dataframe column"
            ))
          }
          if (!is.null(names(colors))) {
            if (any(is.na(names(colors)))) {
              stop(paste0("NA names found in names(", dp.cols.fac.color, "[[", nm, "]])"))
            } else {
              if (!identical(sort(names(colors)), sort(levels(df[[nm]])))) {
                stop(paste0(
                  "Names in named vector in ", dp.cols.fac.color, "[[", nm, "]]\n",
                  "  do not match levels of the dataframe column. Fix the names\n",
                  "  or do not use named vectors in these list items" 
                ))
              }
            }
          } else {
            new.named.vec <- colors
            names(new.named.vec) <- levels(factor(df[[nm]]))
          }
          named.cols.fac.color[[nm]] <<- new.named.vec
        }
      ))
      
      # Now applies the styling with the processed
      # `named.cols.fac.color` where
        # list items = column names
        # list values = named character vectors
          # values = colors, names = levels within the columns
      base::Map(named.cols.fac.color, names(named.cols.fac.color), f = \(cat.colors, col.oi) {

        if (!is.null(lighten)) {
          names.before.lighten <- names(cat.colors)
          cat.colors <- lighten_colors(colors = cat.colors, factor = lighten)
          names(cat.colors) <- names.before.lighten
        }

        res <<- res |>
          DT::formatStyle(
            columns = col.oi,
            backgroundColor = DT::styleEqual(
              levels = names(cat.colors),
              values = cat.colors
            )
          )
      })

    # else we should be working with an atomic vector instead
    } else {
      
      if (is.numeric(cols.fac.color)) {
        stopifnot(all(cols.fac.color %in% seq_along(colnames(df))))
        cols.fac.color.names <- colnames(df)[cols.fac.color]
      } else if (is.character(cols.fac.color)) {
        stopifnot(all(cols.fac.color %in% colnames(df)))
        cols.fac.color.names <- cols.fac.color
      } else {
        stop(cols.fac.color.err.mess)
      }

      stopifnot(
        "columns supplied to argument \"cols.fac.color\" should be of class character or factor" =
        all(sapply(df[, cols.fac.color.names], \(col) {
          is.character(col) || is.factor(col)
        }))
      )

      for (column in cols.fac.color.names) {
        column_levels <- df[[column]] |> factor() |> levels()
        hex <- gg_colors(length(column_levels))
        if (!is.null(lighten)) {
          hex <- lighten_colors(colors = hex, factor = lighten)
        }
        res <- res |>
          DT::formatStyle(
            columns = column,
            backgroundColor = DT::styleEqual(
              levels = column_levels,
              values = hex
            )
          )
      }
    }
    
  }
  
  res

}