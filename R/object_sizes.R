# Updated 2024-05-30

#' @title Get sizes of objects in memory
#' @details This reports sizes in GB or MB in character vector output, not suitable from this output for further calculations.
#' @param sort "desc" (default) to sort by descending size or NULL for default order from `ls()`
#' @return a dataframe of object sizes in Gb or Mb for all items retrieved from ls(envir = .GlobalEnv).
#' 
#' if nothing found from ls(), return(NULL)
#' @examples
#' mk::object_sizes()
#'
#' @export

object_sizes <- function(sort = "desc") {
  
  # Get dataframe of information ---------------------------------------------

  objs <- ls(envir = .GlobalEnv)

  if (length(objs) == 0) {

    return(NULL)

  } else {

    names(objs) <- objs
    objs <- as.list(objs)

    res <- lapply(objs, \(x) {
      res <- format(utils::object.size(get(x, envir = .GlobalEnv)), units = "Gb")
      res_num <- stringr::str_replace(res, "^(.+) .+$", "\\1") |> as.numeric()
      if (res_num <= 1) {
        res <- format(utils::object.size(get(x, envir = .GlobalEnv)), units = "Mb")
      }
      res
    })
    res <- data.frame(obj = names(res), size = unlist(res))
    row.names(res) <- NULL


    res$class <- lapply(res$obj, \(x) {
      x <- class(get(x, envir = .GlobalEnv))
      if (length(x) > 1) {
        x <- paste(x, collapse = ", ")
      }
      x
    }) |> unlist()


    # Sort on size column ------------------------------------------------------
    if (!is.null(sort)) {

      if (sort == "desc") {
        res <- res |>
          dplyr::mutate(size_num = stringr::str_replace(size, "^(.+) .+$", "\\1")) |>
          dplyr::mutate(size_num = as.numeric(size_num)) |>
          dplyr::mutate(size_ext = stringr::str_replace(size, "^.+ (.+)$", "\\1")) |>
          dplyr::arrange(size_ext, dplyr::desc(size_num)) |>
          dplyr::select(-c(size_num, size_ext))
      }

    }

    return(res)

  }
  
  # avoid R CMD CHECK "no visible binding" messages
  size <- NULL; size_num <- NULL; size_ext <- NULL

}

