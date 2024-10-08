% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validate.R
\name{validate}
\alias{validate}
\title{Validate R objects from cache}
\usage{
validate(
  object,
  name,
  f = base::identical,
  dir = "cache",
  revalidate = FALSE,
  silent = FALSE,
  create.dir = FALSE,
  func,
  loc = NULL
)
}
\arguments{
\item{object}{R object to validate}

\item{name}{Unique string with NO extension
\itemize{
\item If \verb{<dir>/<name>.rds} present, validate against it
\item If \verb{<dir>/<name>.rds} not present, create \verb{<dir>/<name>.rds} and validate against it
}}

\item{f}{Function to validate with. i.e., this function is provided the arguments \code{object} and \verb{<dir>/<name>.rds} from cached version.}

\item{dir}{Directory where cached .rds are stored}

\item{revalidate}{\code{TRUE} = If file \verb{<dir>/<name>.rds} exists, overwrite it anew

\code{FALSE} (default) = validate against existing file if exists

Only use TRUE here temporarily if new result for object expected

NOTE: if FALSE, function will check \code{options()$f_global_revalidate} for a TRUE value to override this option

NOTE: deprecated, check GLOBAL_REVALIDATE for a TRUE value to override this option, lower priority than options()$f_global_revalidate}

\item{silent}{Whether to suppress messages/warnings (default = FALSE)}

\item{create.dir}{\itemize{
\item TRUE = create \code{dir} if does not already exist
\item FALSE (default) = return an error instead if dir does not exist
}}

\item{func}{alias for argument \code{f}}

\item{loc}{DEPCREATED, use argument \code{dir} instead
\itemize{
\item NULL or unsupplied (default) = does nothing, uses \code{dir} argument instead
\item \if{html}{\out{<chr>}} path to dir = If \code{dir} not provided, sets the \code{dir} argument above to this \code{loc} value
}

Error if both \code{dir} and \code{loc} provided}
}
\value{
Function will return the result of calling \code{f} with arguments \code{object} and \verb{readRDS(<cached-object>)}

i.e., a TRUE/FALSE value for \code{f = identical}
}
\description{
Compare an R object to a cached .rds version, or create cached version. Used for validating specific objects in a big script, used so I can run a big script all over and know if important objects have changed or not, as an alternative to a more involved workflow software with more overhead.
}
\examples{
\dontrun{
mk::validate(object, "2410282010255", base::identical)
stopifnot(mk::validate(object, "2410282050709", base::identical))
stopifnot(isTRUE(mk::validate(object, "2410282050833", base::all.equal)))
}
}
