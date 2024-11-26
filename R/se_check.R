# Updated 2024-10-24

#' @title Check SE object structure
#' @description Check agreement in dimnames of the assays and the rowData()/colData()
#' @returns TRUE/FALSE
#' @details This function checks:
#' 
#' - that `row.names()` of all assays identical to `row.names(rowData(se))`
#'
#' - that `colnames()` of all assays identical to `row.names(colData(se))`
#' 
#' For strict = TRUE
#' 
#' - that `as.character(rowData(se)[[1]])` identical to `row.names(rowData(se))`
#'
#' - that `as.character(colData(se)[[1]])` identical to `row.names(colData(se))`
#' 
#' - that 
#' @examples
#' \dontrun{
#' mk::se_check(se)
#' mk::se_check(se, silent = TRUE)
#' mk::se_check(se, strict = TRUE)
#' }
#' @param se a SummarizedExperiment object
#' @param silent suppress messages, default FALSE
#' @param strict more opinionated checks, default FALSE
#' 
#' @export

se_check <- function(se, silent = FALSE, strict = FALSE) {
  
  # Print info if silent = FALSE
  if (!silent) {
    message("INFO:")
    num.assays <- length(SummarizedExperiment::assays(se))
    message(paste0("  ", num.assays, " assay in SE object"))
    if (is.null(names(SummarizedExperiment::assays(se)))) {
      message("  No names found for assay list")
    } else {
      if (any(is.na(names(SummarizedExperiment::assays(se))))) {
        message("  NA names found in assay list")
      }
      names.assays <- names(SummarizedExperiment::assays(se))
      names.assays <- sub("^(.*)$", "\"\\1\"", names.assays)
      names.assays <- paste(names.assays, collapse = ", ")
      message(paste0("  Assay names:\n    ", names.assays))
    }
  }
  
  return_false <- FALSE
  
  if (!silent) message("DEFAULT CHECKS:")
  
  # Iterate through assays for Checks 1 and 2
  for (i in seq_len(length(SummarizedExperiment::assays(se)))) {
    # Check 1
    cond <- identical(
      row.names(SummarizedExperiment::assays(se)[[i]]),
      row.names(SummarizedExperiment::rowData(se))
    )
    if (!cond) {
      message(paste0(
        "  ERROR: row.names() in assay ", i, " not identical to row.names(rowData(se))"
      ))
      return_false <- TRUE
    } else {
      if (!silent) {
        message(paste0(
          "  row.names() in assay ", i, " identical to row.names(rowData(se))"
        ))
      }
    }
    cond <- identical(
      colnames(SummarizedExperiment::assays(se)[[i]]),
      row.names(SummarizedExperiment::colData(se))
    )
    # Check 2
    if (!cond) {
      message(paste0(
        "  ERROR: colnames() in assay ", i, " not identical to row.names(colData(se))"
      ))
      return_false <- TRUE
    } else {
      if (!silent) {
        message(paste0(
          "  colnames() in assay ", i, " identical to row.names(colData(se))"
        ))
      }
    }
  }

  if (!silent && strict) message("STRICT CHECKS:")
  # Strict check 1
  if (strict) {
    cond <- identical(
      row.names(SummarizedExperiment::rowData(se)),
      as.character(SummarizedExperiment::rowData(se)[[1]])
    )
    if (!cond) {
      message(paste0(
        "  ERROR: row.names(rowData(se)) not identical to as.character(rowData(se)[[1]])"
      ))
      return_false <- TRUE
    } else {
      if (!silent) {
        message(paste0(
          "  row.names(rowData(se)) identical to as.character(rowData(se)[[1]])"
        ))
      }
    }

    # Strict check 2
    cond <- identical(
      row.names(SummarizedExperiment::colData(se)),
      as.character(SummarizedExperiment::colData(se)[[1]])
    )
    if (!cond) {
      message(paste0(
        "  ERROR: row.names(colData(se)) not identical to as.character(colData(se)[[1]])"
      ))
      return_false <- TRUE
    } else {
      if (!silent) {
        message(paste0(
          "  row.names(colData(se)) identical to as.character(colData(se)[[1]])"
        ))
      }
    }
  }

  if (return_false) stop()
  TRUE

}
