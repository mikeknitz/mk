# Updated 2024-10-11

#' @title Write str() output to a file
#' @description Captures `str()` with `capture.output()` and writes to a file
#' @param object object to inspect with `str()`
#' @param file path to file to write to, defaults to "temp.R"
#' @returns `invisible(NULL)` if `!is.null(file)`, otherwise a character vector
#' @details Uses a large number (9999) for argument `list.len` of `str()` to avoid truncation of `str()` output. Works more reliably than using `sink()` in my experience, though `capture.output()` apparently also uses `sink()` but clearly better.
#' @examples
#' \dontrun{
#' mk::write_str(ggplot2::mpg, file = "temp.R")
#' }
#' @export

write_str <- function(object, file = "temp.R") {
  utils::capture.output(
    utils::str(object, list.len = 9999),
    file = file
  )
}
