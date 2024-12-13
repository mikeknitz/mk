% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_wb.R
\name{save_wb}
\alias{save_wb}
\title{Save a Workbook object to file}
\usage{
save_wb(wb, file, overwrite = TRUE, silent = FALSE)
}
\arguments{
\item{wb}{a Workbook object (from package \code{openxlsx})}

\item{file}{a file path to an .xlsx file to write to}

\item{overwrite}{whether to overwrite if file exists (default = TRUE)}

\item{silent}{whether to silence messages from this function (default = FALSE)}
}
\value{
\code{invisible(NULL)}
}
\description{
Wrapper around \code{openxlsx::saveWorkbook()}, mostly just to use \code{overwrite = TRUE} as the default functionality
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
