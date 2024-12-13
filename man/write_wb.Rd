% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_wb.R
\name{write_wb}
\alias{write_wb}
\title{Write to xlsx workbook with custom formatting}
\usage{
write_wb(
  df,
  wb = NULL,
  sheet = NULL,
  bold.headers = TRUE,
  auto.width = TRUE,
  freeze.rows = NULL,
  freeze.cols = NULL,
  cols.pval = NULL,
  cols.pos.neg = NULL,
  cols.fac.color = NULL,
  lighten = 0.35,
  row.names = FALSE,
  silent = FALSE
)
}
\arguments{
\item{df}{dataframe to add to xlsx sheet, row.names will be ignored}

\item{wb}{a Workbook object to start from, or if NULL (default) create a new workbook}

\item{sheet}{xlsx sheet to write to, one of:
\itemize{
\item \if{html}{\out{<chr>}} specify a new sheet name or existing one to overwrite
\item \if{html}{\out{<numeric>}} specify an EXISTING sheet index
\item NULL (default) = write to a new sheet named "SheetX" where X is a number
}}

\item{bold.headers}{TRUE/FALSE whether to make header row bold, default = TRUE}

\item{auto.width}{one of:
\itemize{
\item TRUE/FALSE = apply auto.width to all or no columns
\item columns in which to apply auto width
\item column indices in which to apply auto width
}}

\item{freeze.rows}{freeze X number of rows at the top (default NULL for none)}

\item{freeze.cols}{freeze X number of columns on the left (default NULL for none)}

\item{cols.pval}{coloring based [0, 0.05) = yellow, [0.05, 0.15) = orange, provide colnames/indices corresponding to numeric columns in dataframe, default = NULL for none}

\item{cols.pos.neg}{coloring based >0 = green, <0 = red, provide colnames/indices corresponding to numeric columns in dataframe, default = NULL for none}

\item{cols.fac.color}{default NULL for no coloring, or one of:
\itemize{
\item character or numeric vector = provide colnames/indices for character columns
\item list where list names = column names and list items = \if{html}{\out{<chr>}} hex codes / colors for levels in the column, list items may or may not be named characters where names() refer to the levels
}}

\item{lighten}{lighten the provided colors to argument "cols.fac.color" by a factor where 1 = original color and 0 = white, default = 0.35, NULL for no lightening}

\item{row.names}{default = FALSE, row.names are not supported and this argument must be FALSE}

\item{silent}{whether to silence messages from this wrapper function (default FALSE)}
}
\value{
a Workbook object
}
\description{
Quickly add dataframes to xlsx workbooks using \code{openxlsx} package. Add to a new workbook or a specific sheet of an existing workbook. Return value is a "Workbook" object from the \code{openxlsx} package that can then be written to a file with \code{openxlsx::saveWork()}, or \code{mk::save_wb()}
}
\examples{
\dontrun{
df <- iris
wb <- write_wb(
  df = df,
  wb = NULL,
  auto.width = TRUE,
  freeze.rows = 1,
  freeze.cols = 1,
  cols.fac.color = list(Species = c("red", "blue", "green"))
)
save_wb(wb = wb, file = "iris.xlsx")
}

}
