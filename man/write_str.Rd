% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_str.R
\name{write_str}
\alias{write_str}
\title{Write str() output to a file}
\usage{
write_str(object, file = "temp.R")
}
\arguments{
\item{object}{object to inspect with \code{str()}}

\item{file}{path to file to write to, defaults to "temp.R"}
}
\value{
\code{invisible(NULL)} if \code{!is.null(file)}, otherwise a character vector
}
\description{
Captures \code{str()} with \code{capture.output()} and writes to a file
}
\details{
Uses a large number (9999) for argument \code{list.len} of \code{str()} to avoid truncation of \code{str()} output. Works more reliably than using \code{sink()} in my experience, though \code{capture.output()} apparently also uses \code{sink()} but clearly better.
}
\examples{
\dontrun{
mk::write_str(ggplot2::mpg, file = "temp.R")
}
}
