# Updated 2024-10-06

#' @title Convert gene symbols to names using AnnotationDbi
#' @description Takes vector of HGNC human gene symbols or MGI mouse gene symbols and returns gene names/descriptions from the GENENAME column from `org.Hs.eg.db` or `org.Mm.eg.db`
#' 
#' Must install these packages from BioConductor (not automatically installed with this package)
#' 
#' `BiocManager::install("AnnotationDbi")`
#' 
#' `BiocManager::install("org.Hs.eg.db")`
#' 
#' `BiocManager::install("org.Mm.eg.db")`
#' @returns A character vector of gene names / descriptions
#' @details
#' - `AnnotationDbi:mapIds()` is run here with the `multiVals = "first"` argument, can override this by supplying a different value in `...`
#' 
#' - An error from AnnotationDbi is different from it just not finding a gene symbol in its database. By default it will return NA values for genes not found if it finds at least one match in the `genes` vector. The "na" argument here just aligns this behavior in the case that you supply a single invalid gene symbol (e.g., for use in loops).
#' @examples
#' \dontrun{
#' genes <- c("CD3D", "CD4", "CD8A")
#' mk::gene_symbol_to_name(genes)
#' }
#' @param genes a character vector of gene symbols
#' @param org one of "human", "mouse", or an `OrgDb` class object
#' @param error.behavior See details section:
#' 
#' - `"error"` (default) = fail on error from call to AnnotationDbi::mapIds()
#' 
#' - `"na"` = return vector of length `length(genes)` of NA values
#' @param message TRUE(default) = suppress messages from AnnotationDbi::mapIds()
#' @param warning TRUE(default) = suppress warnings from AnnotationDbi::mapIds()
#' @param error TRUE(default) = suppress errors from AnnotationDbi::mapIds()
#' @param silent Override message, warning, and error arguments above TRUE/FALSE
#' 
#' NULL (default) = no effect
#' @param ... Arguments passed/overridden to `AnnotationDbi:mapIds()`
#' @export

#### Packages required

# BiocManager::install("AnnotationDbi")
# BiocManager::install("org.Hs.eg.db")
# BiocManager::install("org.Mm.eg.db")

#### Examples

# gene_symbol_to_name("CD3D", "human")
# gene_symbol_to_name("CD8A", "human", column = "UNIPROT")
# gene_symbol_to_name("CD8A", "human", multiVals = "list")
# gene_symbol_to_name(c("CD3D", "CD4"), "human")
# gene_symbol_to_name(c("CD3D", "XXX"), "human")
# gene_symbol_to_name("XXX", "human")
# gene_symbol_to_name("XXX", "human", error.behavior = "na")
# gene_symbol_to_name("XXX", "human", error.behavior = "na", silent = TRUE)

gene_symbol_to_name <- function(

  # gene symbol(s)
  genes,

  # one of these, or an OrgDb object
  org = c("human", "mouse"),

  # For error from call to AnnotationDbi
    # error (default) = fail on error
    # na              = return vector of length `length(genes)` of NA values
  error.behavior = c("error", "na"),

  # Show AnnotationDbi messages/warnings/errors in console = TRUE (default)
  # These do not suppress errors from this function gene_symbol_to_name()
  message = TRUE,
  warning = TRUE,
  error   = TRUE,

  # Override message, warning, and error arguments above
  # Can either be NULL (default, no effect) or TRUE/FALSE
  silent = NULL,

  # Arguments passed/overridden to AnnotationDbi::mapIds()
  ...

) {

  if (!is.null(silent)) {
    if (!silent) {
      message <- TRUE; warning <- TRUE; error <- TRUE;
    } else if (silent) {
      message <- FALSE; warning <- FALSE; error <- FALSE;
    }
  }

  # Assign org_db from args
  if (inherits(org, "OrgDb")) {
    org_db <- org
  } else {
    # Check class of org
    if (!is.character(org[1]) && !is.factor(org[1])) {
      stop("org should be of class \"character\" or \"OrgDb\"")
    }
    # Check acceptable org argument
    if (!(org[1] %in% c("human", "mouse"))) {
      stop("org should be one of c(\"human\", \"mouse\") or class \"OrgDb\"")
    }
    # Assign corresponding OrgDb
    org_db <- switch(org[1],
      human = org.Hs.eg.db::org.Hs.eg.db,
      mouse = org.Mm.eg.db::org.Mm.eg.db
    )
  }

  # Args to AnnotationDbi::mapIds()
  func.args <- list(
    x = org_db,
    keys = genes,
    keytype = "SYMBOL",
    column = "GENENAME",
    multiVals = "first"
  ) |>
    # Merge in / overwrite from args in ...
    utils::modifyList(as.list(list(...)))

  # Call AnnotationDbi::mapIds()
  mess <- c("message", "warning", "error")[c(!message, !warning, !error)]
  if (length(mess) == 0) {
    gene.names <- try(do.call(AnnotationDbi::mapIds, func.args))
  } else {
    gene.names <- try(suppressMessages(
      do.call(AnnotationDbi::mapIds, func.args),
      classes = mess
    ), silent = !error)
  }

  # Return gene.names depending on error.behavior
  if (inherits(gene.names, "try-error")) {
    if (error.behavior[1] == "na") {
      return(as.character(rep(NA, length(genes))))
    } else if (error.behavior[1] == "error") {
      stop("Error in AnnotationDbi::mapIds()")
    } else {
      stop("error.behavior must be one of c(\"error\", \"na\")")
    }
  } else {
    return(gene.names)
  }

}
