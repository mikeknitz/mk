# Updated 2024-10-06

#' @title
#' Seurat number-label pairs for DimPlots
#' 
#' @description
#' Take a Seurat object and a labeled ident (e.g., cell_type) and produce two columns:
#'
#' - `labels.num`: vector of numbers to label inside a DimPlot
#'
#' - `labels.char`: vector of numbered labels to match these numbers, to use in the legend for a DimPlot
#'
#' E.g.,
#'
#' ```
#' labels.num   labels.char
#'     0         0: T cell
#'     1         1: B cell
#'     2         2: NK cell
#' ```
#'
#' @returns
#' a Seurat object with modified `meta.data` slot with `labels.num` and `labels.char` overwritten
#'
#' @details
#' As written, this will overwrite any previously stored information in the `labels.num` or `labels.char` vectors in the `meta.data` slot of the Seurat object
#' 
#' TODO: Haven't tested for edge cases with NA factor levels in the idents
#' 
#' TODO: See if can store this information elsewhere in the object instead of taking up room in the `meta.data` slot
#'
#' @examples
#' \dontrun{
#' so <- mk::so_label_pair(so, "cell_type")
#' }
#'
#' @param so a Seurat object
#' @param ident an ident from which to create a number-label pair
#' @param start.zero TRUE (default) = start numbering at zero
#'
#' FALSE = start numbering at one
#'
#' @export

so_label_pair <- function(

  # Seurat object
  so,

  # Ident from which to create number-label pair
  ident,

  # Start numbering at zero
  start.zero = TRUE

) {


  # Get the vector for the ident
  # Coerce to factor if not already done so
  ident_vec <- so@meta.data[[ident]] |> factor()

  # recode_args_num = list:
    # list values = factor levels of the ident
    # list names = numbers associated with these levels
  recode_args_num <- as.list(levels(ident_vec))
  if (start.zero) {
    names(recode_args_num) <- as.character(seq_len(length(recode_args_num)) - 1)
  } else {
    names(recode_args_num) <- as.character(seq_len(length(recode_args_num)))
  }

  # Create labels.num
  labels.num <- do.call(forcats::fct_recode, c(list(ident_vec), recode_args_num))

  # recode_args_char = list:
    # list values = factor levels of the ident
    # list names = factor levels of the ident, prepended with "0: ", "1: ", etc.
  recode_args_char <- as.list(levels(ident_vec))
  if (start.zero) {
    names(recode_args_char) <- paste0(
      as.character(seq_len(length(recode_args_char)) - 1), ": ", unlist(recode_args_char)
    )
  } else {
    names(recode_args_char) <- paste0(
      as.character(seq_len(length(recode_args_char))), ": ", unlist(recode_args_char)
    )
  }

  # Create labels.char
  labels.char <- do.call(forcats::fct_recode, c(list(ident_vec), recode_args_char))

  # Assign these back to the Seurat object
  so@meta.data[["labels.num"]] <- labels.num
  so@meta.data[["labels.char"]] <- labels.char

  # Return modified Seurat object
  so

}
