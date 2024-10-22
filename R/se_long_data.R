#' @title Transform SummarizedExperiment object to long form
#' 
#' @description Supply a SummarizedExperiment object and an assay and get a dataframe that is left-joined with information from `colData(se)` and `rowData(se)`, useful for plotting
#' 
#' @returns a dataframe in long format
#' 
#' @examples
#' \dontrun{
#' # Subset an SE object to a gene of interest, then convert to long form
#' library(SummarizedExperiment)
#' se.sub <- subset(se, subset = gene %in% "FOXP3")
#' df <- mk::se_long_data(se.sub, "cpm", "FOXP3")
#' }
#' @details "se_rownames" and "se_colnames" are used internally for joining operations, which in the case of `dplyr::left_join()` require a column to join on
#' 
#' @param se a SummarizedExperiment object
#' @param assay an assay name or index for expression values
#' @param values.to name of long-format dataframe column to store expression values in, default = "expr"
#' @param silent suppress message about not explicitly supplying an assay argument, default FALSE
#' @param strict if TRUE (default), error if,
#' 
#' - "se_rownames" is a column in rowData(se) or colData(se)
#' 
#' - "se_colnames" is a column in colData(se) or rowData(se)
#' 
#' - any column names are shared between colData(se) or rowData(se)
#' 
#' - "values.to" in columns of colData(se) or rowData(se)
#' 
#' if FALSE, attempt to execute, but may have unintended effects in the left join operations
#' 
#' @export

se_long_data <- function(

  # SummarizedExperiment object
  se,

  # <chr> for assay name or <int/num> for assay index
  assay = 1,

  # <chr> name of column in which to store the
  # matrix expression values
  values.to = "expr",

  # Suppress messages, default FALSE
  silent = FALSE,

  # if TRUE, error if:
    # se_rownames is a column in rowData(se) or colData(se)
    # se_colnames is a column in colData(se) or rowData(se)
    # any column names are shared between colData(se) or rowData(se)
    # values.to in columns of colData(se) or rowData(se)
  # if FALSE, attempt to execute, but may have some unintended
    # effects in the left join operations
  strict = TRUE

) {
  
  # ----------------------------------------------------------------------------
  # Check arguments
  # ----------------------------------------------------------------------------
  
  if (missing(assay)) {
    if (!silent) {
      message("No assay specified, defaulting to assays(se)[[1]]")
    }
  }

  if (!is.character(assay) && !is.numeric(assay)) {
    stop("\"assay\" must be the name or index of a single SummarizedExperiment assay")
  }

  if (length(assay) != 1) {
    stop("\"assay\" must be the name or index of a single SummarizedExperiment assay")
  }
  
  if (strict) {
    if (values.to %in% colnames(rowData(se))) {
      stop("STRICT: \"values.to\" cannot be already present in colnames(rowData(se))")
    }
    if (values.to %in% colnames(colData(se))) {
      stop("STRICT: \"values.to\" cannot be already present in colnames(colData(se))")
    }
    if ("se_colnames" %in% colnames(SummarizedExperiment::colData(se))) {
      stop("STRICT: colData(se) cannot include a column named \"se_colnames\"")
    }
    if ("se_colnames" %in% colnames(SummarizedExperiment::rowData(se))) {
      stop("STRICT: rowData(se) cannot include a column named \"se_colnames\"")
    }
    if ("se_rownames" %in% colnames(SummarizedExperiment::rowData(se))) {
      stop("STRICT: rowData(se) cannot include a column named \"se_rownames\"")
    }
    if ("se_rownames" %in% colnames(SummarizedExperiment::colData(se))) {
      stop("STRICT: colData(se) cannot include a column named \"se_rownames\"")
    }
    combined.colnames <- c(colnames(SummarizedExperiment::colData(se)), colnames(SummarizedExperiment::rowData(se)))
    dupe.vec <- duplicated(combined.colnames)
    if (any(dupe.vec)) {
      message("STRICT: Shared columns found")
      message(paste0("SHARED: ", paste(combined.colnames[dupe.vec], collapse = ", ")))
      stop("STRICT: colData(se) and rowData(se) cannot share column names")
    }
  }

  # ----------------------------------------------------------------------------
  # Joining operations
  # ----------------------------------------------------------------------------
  
  df <- SummarizedExperiment::assays(se)[[assay]] |>
    as.data.frame() |>
    tibble::rownames_to_column(var = "se_rownames") |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(colnames(SummarizedExperiment::assays(se)[[assay]])),
      names_to = "se_colnames",
      values_to = values.to,
      names_repair = "minimal"
    ) |>
    dplyr::left_join(
      by = "se_colnames",
      y = SummarizedExperiment::colData(se) |>
        as.data.frame() |>
        tibble::rownames_to_column(var = "se_colnames") |>
        as.data.frame()
    ) |>
    dplyr::left_join(
      by = "se_rownames",
      y = SummarizedExperiment::rowData(se) |>
        as.data.frame() |>
        tibble::rownames_to_column(var = "se_rownames") |>
        as.data.frame()
    ) |>
    as.data.frame()
  
  return(df)

  # avoid R CMD CHECK "no visible binding" messages
  rowData <- NULL; colData <- NULL

  
}