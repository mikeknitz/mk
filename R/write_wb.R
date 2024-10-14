#' @name write_wb
#' @rdname write_wb
#' @title Write to xlsx workbook with custom formatting
#' 
#' @description Quickly add dataframes to xlsx workbooks using `openxlsx` package. Add to a new workbook or a specific sheet of an existing workbook. Return value is a "Workbook" object from the `openxlsx` package that can then be written to a file with `openxlsx::saveWork()`, or `mk::save_wb()`
#' 
#' @returns a Workbook object
#' 
#' @examples
#' \dontrun{
#' df <- iris
#' wb <- write_wb(
#'   df = df,
#'   wb = NULL,
#'   auto.width = TRUE,
#'   freeze.rows = 1,
#'   freeze.cols = 1,
#'   cols.fac.color = list(Species = c("red", "blue", "green"))
#' )
#' save_wb(wb = wb, file = "iris.xlsx")
#' }
#' 
#' @param df dataframe to add to xlsx sheet, row.names will be ignored
#' @param wb a Workbook object to start from, or if NULL (default) create a new workbook
#' @param sheet xlsx sheet to write to, one of:
#' 
#' - <chr> specify a new sheet name or existing one to overwrite
#'
#' - <numeric> specify an EXISTING sheet index
#' 
#' - NULL (default) = write to a new sheet named "SheetX" where X is a number
#' 
#' @param bold.headers TRUE/FALSE whether to make header row bold, default = TRUE
#' @param auto.width one of:
#' 
#' - TRUE/FALSE = apply auto.width to all or no columns
#' 
#' - columns in which to apply auto width
#' 
#' - column indices in which to apply auto width
#' @param freeze.rows freeze X number of rows at the top (default NULL for none)
#' @param freeze.cols freeze X number of columns on the left (default NULL for none)
#' @param cols.pval coloring based [0, 0.05) = yellow, [0.05, 0.15) = orange, provide colnames/indices corresponding to numeric columns in dataframe, default = NULL for none
#' @param cols.pos.neg coloring based >0 = green, <0 = red, provide colnames/indices corresponding to numeric columns in dataframe, default = NULL for none
#' @param cols.fac.color default NULL for no coloring, or one of:
#' 
#' - character or numeric vector = provide colnames/indices for character columns
#' 
#' - list where list names = column names and list items = <chr> hex codes / colors for each column specified, list items may or may not be named characters
#' @param lighten lighten the provided colors to argument "cols.fac.color" by a factor where 1 = original color and 0 = white, default = 0.35, NULL for no lightening
#' @param row.names default = FALSE, row.names are not supported and this argument must be FALSE
#' @param silent whether to silence messages from this wrapper function (default FALSE)
#' @export

write_wb <- function(

  # R object to add to a fresh xlsx sheet, row.names ignored, one of a:
    # <data.frame>, add a dataframe table to a sheet, row.names ignored
  df,

  # workbook object to write to
  # NULL (default) = write to a new workbook
  wb = NULL,

  # which sheet index to write to?
  # <chr> specify a new sheet name or existing one to overwrite
  # <numeric> specify an EXISTING sheet index
  # NULL (default) = write to a new sheet named "SheetX" where X is a number
  sheet = NULL,

  # bold header row
  bold.headers = TRUE,

  # TRUE (default) = apply auto width to all columns
  # FALSE = don't apply any auto width
  # or colnames/indices corresponding to numeric columns in a dataframe
  auto.width = TRUE,

  # Freeze X number of rows or columns
  freeze.rows = NULL,
  freeze.cols = NULL,

  # coloring based [0, 0.05) = yellow, [0.05, 0.15) = orange
  # provide colnames/indices corresponding to numeric columns in dataframe
  cols.pval = NULL,

  # coloring based >0 = green, <0 = red
  # provide colnames/indices corresponding to numeric columns in dataframe
  cols.pos.neg = NULL,

  # coloring based on factor levels
    # <chr> or <numeric> input
      # provide colnames/indices corresponding to character columns in dataframe
      # uses lightened default ggplot colors, custom colors not implemented yet
    # <list> input
      # list names = column names
      # list items = <chr> hex codes for columns of each name,
        # or a named character vector where names() refer to df column names
      # list items = hex codes for columns of each name
  cols.fac.color = NULL,
  
  # If using cols.fac.color, apply lighten_colors(factor = 0.35)
    # to each color before styling workbook, where factor is in [0, 1]
    # and 1 = original color and smaller = lighter
  # default = 0.35
  # NULL = do not apply any lightening
  lighten = 0.35,

  # row.names from df are not supported
  # must be FALSE
  row.names = FALSE,

  # Hide messages, default FALSE
  silent = FALSE

) {

  if (missing(df)) {
    stop("Provide a dataframe to write")
  }

  # ----------------------------------------------------------------------------
  # Create new Workbook object if none provided
  # ----------------------------------------------------------------------------

  if (is.null(wb)) {
    if (!silent) message("No workbook provided, creating new Workbook object")
    wb <- openxlsx::createWorkbook()
  } else {
    if (!inherits(wb, "Workbook")) {
      stop("\"wb\" must be a \"Workbook\" object")
    }
  }

  # ----------------------------------------------------------------------------
  # Handle `sheet` argument
  # ----------------------------------------------------------------------------
  # `active.sheet` will be assigned here, a string referring
    # to the name of the sheet being written in this function

  # Change to TRUE if overwriting a previous sheet
  # This will turn on/off the openxlsx::addWorksheet() lines below this section
  sheet.exists <- FALSE

  sheet.err.mess <- paste0(
    "argument \"sheet\" should be one of:\n",
    " - the name of a single new or existing sheet\n",
    " - the index of a single new or existing sheet\n",
    " - or unspecified (a new sheet called SheetX ",
    "will be added, where X is a number)"
  )
  edge.case.mess <- paste0(
    "This function errors if you provide a workbook that already ",
    "has Sheet1 through Sheet1000 already taken, congratulations."
  )

  prev.sheet.names <- wb$sheet_names

  if (is.null(sheet))
    if (length(prev.sheet.names) == 0) {
      message("Writing to new sheet \"Sheet1\"")
      active.sheet <- "Sheet1"
    } else {
      active.sheet <- base::setdiff(
        paste0("Sheet", 1:1000),
        prev.sheet.names
      )[1]
      if (length(active.sheet) == 0) stop(edge.case.mess)
      message(paste0("Writing to new sheet \"", active.sheet, "\""))
  } else if (is.character(sheet)) {
    if (length(sheet) != 1) stop(sheet.err.mess)
    if (sheet %in% prev.sheet.names) {
      message(paste0("sheet \"", sheet, "\" already exists, overwriting..."))
      sheet.exists <- TRUE
    } else {
      active.sheet <- sheet
    }
  } else if (is.numeric(sheet)) {
    if (length(sheet) != 1) stop(sheet.err.mess)
    if (sheet %in% seq_along(prev.sheet.names)) {
      message(paste0(
        "sheet at index ", sheet, " named \"",
        prev.sheet.names[sheet], "\" already exists, overwriting..."
      ))
     active.sheet <- prev.sheet.names[sheet]
     sheet.exists <- TRUE
    } else {
      active.sheet <- base::setdiff(
        paste0("Sheet", 1:1000),
        prev.sheet.names
      )[1]
      if (length(active.sheet) == 0) stop(edge.case.mess)
      message(paste0("Writing to new sheet \"", active.sheet, "\""))
    }
  }

  # ----------------------------------------------------------------------------
  # Add new worksheet if necessary
  # ----------------------------------------------------------------------------

  if (!sheet.exists) {
    openxlsx::addWorksheet(
      wb = wb,
      sheetName = active.sheet
    )
  }

  # ----------------------------------------------------------------------------
  # Write df to the active.sheet with openxlsx::writeData()
  # ----------------------------------------------------------------------------

  args.write.data <- list(
    wb = wb,
    sheet = active.sheet,
    x = df,
    rowNames = row.names
  )

  if (bold.headers) {
    args.write.data$headerStyle <- openxlsx::createStyle(textDecoration = "bold")
  }

  if (args.write.data$rowNames) {
    stop("argument \"row.names\" (openxlsx::writeData(rowNames)) must be FALSE in mk::write_wb()")
  }

  do.call(openxlsx::writeData, args.write.data)

  # ----------------------------------------------------------------------------
  # cols.pval
  # ----------------------------------------------------------------------------

  if (!is.null(cols.pval)) {

    if (is.numeric(cols.pval)) {
      stopifnot(all(cols.pval %in% seq_along(colnames(df))))
      cols.pval.names <- colnames(df)[cols.pval]
    } else if (is.character(cols.pval)) {
      stopifnot(all(cols.pval %in% colnames(df)))
      cols.pval.names <- cols.pval
    } else {
      stop("argument \"cols.pval\" must be valid colname(s) or index(es) of df")
    }

    for (col.oi in cols.pval.names) {
      openxlsx::conditionalFormatting(
        wb = wb,
        sheet = active.sheet,
        cols = which(colnames(df) == col.oi),
        rule = "<0.15",
        rows = seq_len(nrow(df) + 1)[-1],
        style = openxlsx::createStyle(fontColour = "#833C0B", bgFill = "#FEC573")
      )
      openxlsx::conditionalFormatting(
        wb = wb,
        sheet = active.sheet,
        cols = which(colnames(df) == col.oi),
        rule = "<0.05",
        rows = seq_len(nrow(df) + 1)[-1],
        style = openxlsx::createStyle(fontColour = "#9C5700", bgFill = "#FFEB9C")
      )
    }
  }

  # ----------------------------------------------------------------------------
  # cols.pos.neg
  # ----------------------------------------------------------------------------

  if (!is.null(cols.pos.neg)) {

    if (is.numeric(cols.pos.neg)) {
      stopifnot(all(cols.pos.neg %in% seq_along(colnames(df))))
      cols.pos.neg.names <- colnames(df)[cols.pos.neg]
    } else if (is.character(cols.pos.neg)) {
      stopifnot(all(cols.pos.neg %in% colnames(df)))
      cols.pos.neg.names <- cols.pos.neg
    } else {
      stop("argument \"cols.pos.neg\" must be valid colname(s) or index(es) of df")
    }

    for (col.oi in cols.pos.neg.names) {
      openxlsx::conditionalFormatting(
        wb = wb,
        sheet = active.sheet,
        cols = which(colnames(df) == col.oi),
        rule = ">0",
        rows = seq_len(nrow(df) + 1)[-1],
        style = openxlsx::createStyle(fontColour = "#006100", bgFill = "#C6EFCE"),
      )

      openxlsx::conditionalFormatting(
        wb = wb,
        sheet = active.sheet,
        cols = which(colnames(df) == col.oi),
        rule = "<0",
        rows = seq_len(nrow(df) + 1)[-1],
        style = openxlsx::createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
      )
    }
  }
  
  # ----------------------------------------------------------------------------
  # cols.fac.color
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
      # Also creates named.cols.fac.color which is used
      # for list form of cols.fac.color to color the
      # factor columns
      if (!all(sapply(cols.fac.color, \(x) is.character(x) || is.factor(x)))) {
        stop(paste0(
          "Not all list items are of class character or factor\n",
          cols.fac.color.err.mess
        ))
      }
      
      if (is.null(names(cols.fac.color))) {
        message("No list names found in list argument \"cols.fac.color\"")
        stop(cols.fac.color.err.mess)
      } else {
        if (any(is.na(names(cols.fac.color)))) {
          message("NA names found in list item names for argument \"cols.fac.color\"")
          stop(cols.fac.color.err.mess)
        }
      }

      if (!all(names(cols.fac.color) %in% colnames(df))) {
        message("Not all list item names in \"cols.fac.color\" found in colnames(df)")
        stop(cols.fac.color.err.mess)
      }

      if (any(duplicated(names(cols.fac.color)))) {
        message("Duplicated list items name found in list argument \"cols.fac.color\"")
        stop(cols.fac.color.err.mess)
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

      #############################################
      #############################################
      #############################################

      base::Map(named.cols.fac.color, names(named.cols.fac.color), f = \(cat.colors, col.oi) {
        if (!is.null(lighten)) {
          cat.colors <- lighten_colors(colors = cat.colors, factor = lighten)
        }
        for (i in seq_along(cat.colors)) {
          openxlsx::conditionalFormatting(
            wb = wb,
            sheet = active.sheet,
            cols = which(colnames(df) == col.oi),
            rule = paste0("==\"", names(cat.colors)[i], "\""),
            rows = seq_len(nrow(df) + 1)[-1],
            style = openxlsx::createStyle(bgFill = cat.colors[i])
          )
        }
      })

    # code for list.form of cols.fac.color done
    # below is where cols.fac.color is an atomic vector of colors
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
      
      for (col.oi in cols.fac.color.names) {

        # cat.colors = list
          # list items = color hex codes
          # list names = column factor levels
        cat.colors <- df[[col.oi]] |> factor() |> nlevels()
        cat.colors <- gg_colors(n = cat.colors)
        if (!is.null(lighten)) {
          cat.colors <- lighten_colors(colors = cat.colors, factor = lighten)
        }
        names(cat.colors) <- df[[col.oi]] |> factor() |> levels()
        
        for (i in seq_along(cat.colors)) {
          openxlsx::conditionalFormatting(
            wb = wb,
            sheet = active.sheet,
            cols = which(colnames(df) == col.oi),
            rule = paste0("==\"", names(cat.colors)[i], "\""),
            rows = seq_len(nrow(df) + 1)[-1],
            style = openxlsx::createStyle(bgFill = cat.colors[i])
          )        
        }

      }

    }

  }

  # ----------------------------------------------------------------------------
  # auto.width
  # ----------------------------------------------------------------------------
  
  stopifnot(!is.null(auto.width))
  
  if (is.logical(auto.width)) {
    stopifnot(length(auto.width) == 1)
    if (auto.width) {
      openxlsx::setColWidths(
        wb = wb,
        sheet = active.sheet,
        cols = seq_len(ncol(df)),
        widths = "auto"
      )
    }
  } else if (is.character(auto.width) || is.factor(auto.width) || is.numeric(auto.width)) {

    if (is.numeric(auto.width)) {
      stopifnot(all(auto.width %in% seq_along(colnames(df))))
      auto.width.names <- colnames(df)[auto.width]
    } else if (is.character(auto.width)) {
      stopifnot(all(auto.width %in% colnames(df)))
      auto.width.names <- auto.width
    } else if (is.factor(auto.width)) {
      stopifnot(all(as.character(auto.width) %in% colnames(df)))
      auto.width.names <- as.character(auto.width)
    } else {
      stop("auto.width should be one of: TRUE/FALSE or a character vector of colnames or a numeric vector of column indices")
    }

    for (col.oi in auto.width.names) {
      openxlsx::setColWidths(
        wb = wb,
        sheet = active.sheet,
        cols = which(colnames(df) == col.oi),
        widths = "auto"
      )
    }
    
  } else {
    stop("auto.width must be TRUE/FALSE or class character")
  }

  # ----------------------------------------------------------------------------
  # freeze.rows, freeze.cols
  # ----------------------------------------------------------------------------
  
  if (!is.null(freeze.rows) || !is.null(freeze.cols)) {
    if (!is.null(freeze.rows)) {
      stopifnot(is.numeric(freeze.rows))
      stopifnot(length(freeze.rows) == 1)
    }
    if (!is.null(freeze.cols)) {
      stopifnot(is.numeric(freeze.cols))
      stopifnot(length(freeze.cols) == 1)
    }
    freeze.args <- list(
      wb = wb,
      sheet = active.sheet,
      firstActiveRow = NULL,
      firstActiveCol = NULL
    )
    
    if (!is.null(freeze.rows)) freeze.args$firstActiveRow <- freeze.rows + 1
    if (!is.null(freeze.cols)) freeze.args$firstActiveCol <- freeze.cols + 1

    do.call(openxlsx::freezePane, freeze.args)

  }

  # ----------------------------------------------------------------------------

  message("Done.")

  message("To save to .xlsx, use mk::save_wb()")

  # Return the workbook object
  wb

}


# ==============================================================================

#' @title Save a Workbook object to file
#' 
#' @description Wrapper around `openxlsx::saveWorkbook()`, mostly just to use `overwrite = TRUE` as the default functionality
#' 
#' @returns `invisible(NULL)`
#' 
#' @examples
#' \dontrun{
#' df <- iris
#' wb <- write_wb(
#'   df = df,
#'   wb = NULL,
#'   auto.width = TRUE,
#'   freeze.rows = 1,
#'   freeze.cols = 1,
#'   cols.fac.color = list(Species = c("red", "blue", "green"))
#' )
#' save_wb(wb = wb, file = "iris.xlsx")
#' }
#' 
#' @param wb a Workbook object (from package `openxlsx`)
#' @param file a file path to an .xlsx file to write to
#' @param overwrite whether to overwrite if file exists (default = TRUE)
#' @param silent whether to silence messages from this function (default = FALSE)
#' @export

save_wb <- function(wb, file, overwrite = TRUE, silent = FALSE) {
  openxlsx::saveWorkbook(wb, file, overwrite)
  if (!silent) {
    message(paste0("Done. \"", file, "\" written."))
  }
  return(invisible(NULL))
}
