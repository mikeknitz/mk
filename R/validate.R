# Updated 2024-10-08

#' @title Validate R objects from cache
#' @description Compare an R object to a cached .rds version, or create cached version. Used for validating specific objects in a big script, used so I can run a big script all over and know if important objects have changed or not, as an alternative to a more involved workflow software with more overhead.
#' @returns
#' Function will return the result of calling `f` with arguments `object` and `readRDS(<cached-object>)`
#' 
#' i.e., a TRUE/FALSE value for `f = identical`
#' @examples
#' \dontrun{
#' mk::validate(object, "2410282010255", base::identical)
#' stopifnot(mk::validate(object, "2410282050709", base::identical))
#' stopifnot(isTRUE(mk::validate(object, "2410282050833", base::all.equal)))
#' }
#' @param object R object to validate
#' @param name Unique string with NO extension
#' 
#' - If `<dir>/<name>.rds` present, validate against it
#' 
#' - If `<dir>/<name>.rds` not present, create `<dir>/<name>.rds` and validate against it 
#' @param f Function to validate with. i.e., this function is provided the arguments `object` and `<dir>/<name>.rds` from cached version.
#' @param dir Directory where cached .rds are stored
#' @param revalidate
#' `TRUE` = If file `<dir>/<name>.rds` exists, overwrite it anew 
#' 
#' `FALSE` (default) = validate against existing file if exists
#' 
#' Only use TRUE here temporarily if new result for object expected
#' 
#' NOTE: if FALSE, function will check `options()$f_global_revalidate` for a TRUE value to override this option
#' 
#' NOTE: deprecated, check GLOBAL_REVALIDATE for a TRUE value to override this option, lower priority than options()$f_global_revalidate
#' @param silent Whether to suppress messages/warnings (default = FALSE)
#' @param create.dir
#' - TRUE = create `dir` if does not already exist
#' - FALSE (default) = return an error instead if dir does not exist
#' @param func alias for argument `f`
#' @param loc DEPCREATED, use argument `dir` instead
#' 
#' - NULL or unsupplied (default) = does nothing, uses `dir` argument instead
#' 
#' - <chr> path to dir = If `dir` not provided, sets the `dir` argument above to this `loc` value
#' 
#' Error if both `dir` and `loc` provided
#' @export

validate <- function(

  # Object to validate
  object,

  # UNIQUE name/string with NO extension
  # If `<dir>/<name>.rds` present, validate against it
  # If `<dir>/<name>.rds` not present, create `<dir>/<name>.rds` and validate against it
  name,

  # Function to validate with
  # Should return TRUE/FALSE, or something coercible with isTRUE()
  # base::all.equal good alternative (use isTRUE() afterward)
  f = base::identical,

  # Location of .rds to validate
  # E.g., validating against `<dir>/<name>.rds`
  dir = "cache",

  # TRUE = If file `<dir>/<name>.rds` exists, overwrite it anew
  # FALSE (default) = validate against existing file
  # Only use TRUE temporarily if new result for object expected
  # NOTE: if FALSE, check `options()$f_global_revalidate` for a TRUE value to override this option
  # NOTE: deprecated, check GLOBAL_REVALIDATE for a TRUE value to override this option, lower priority than options()$f_global_revalidate
  revalidate = FALSE,

  # Whether to supress messages/warnings (default = FALSE)
  # NOTE: overridden by `options()$f_validate_silent`
  silent = FALSE,

  # ----- Other options ----- #
  
  # TRUE            = create `dir` if does not already exist
  # FALSE (default) = return an error instead
  create.dir = FALSE,

  # Alias for `f`
  func,

  # DEPRECATED, use argument `dir` instead
  # NULL or unsupplied (default) = does nothing, function uses `dir` argument instead
  # <chr> path to dir            = If `dir` not provided, sets the `dir` argument above
  # Error if both `loc` and `dir` provided
  loc = NULL

) {
  
  # Detect arguments
  missing.dir  <- base::missing(dir)
  missing.loc  <- base::missing(loc)
  missing.func <- base::missing(func)
  missing.f    <- base::missing(f)

  # ----------------------------------------------------------------------------
  # Handle alias func for f
  # ----------------------------------------------------------------------------
  
  if (missing.f  && missing.func) {
    invisible(NULL)
  }
  else if (!missing.f && missing.func) {
    invisible(NULL)
  }
  else if (missing.f  && !missing.func) {
    f <- func
  }
  else if (!missing.f && !missing.func) {
    stop(paste0(
      "Cannot accept values for both arguments `f` and alias `func`\n",
      "Aborting... no changes made"
    ))
  } else {
    stop("internal error")
  }

  # ----------------------------------------------------------------------------
  # Check for `options()$f_validate_silent` to set `silent` argument
  # ----------------------------------------------------------------------------
  if (!is.null(options()$f_validate_silent)) {
    stopifnot(is.logical(options()$f_validate_silent))
    silent <- options()$f_validate_silent
  }

  # ----------------------------------------------------------------------------
  # Check for `options()$f_global_revalidate` to set `revalidate` argument
  # ----------------------------------------------------------------------------
  # Also check for global variable GLOBAL_REVALIDATE (deprecated)
  if (!is.null(options()$f_global_revalidate)) {
    stopifnot(is.logical(options()$f_global_revalidate))
    stopifnot(length(options()$f_global_revalidate) == 1)
    revalidate <- options()$f_global_revalidate
  } else if (exists("GLOBAL_REVALIDATE", envir = globalenv())) {
    if (!silent) {
      base::warning(paste0(
        "setting the revalidate option with a global variable ",
        "GLOBAL_REVALIDATE is deprecated"
      ))    
    }
    if (isTRUE(globalenv()$GLOBAL_REVALIDATE)) {
      stopifnot(is.logical(globalenv()$GLOBAL_REVALIDATE))
      stopifnot(length(globalenv()$GLOBAL_REVALIDATE) == 1)
      revalidate <- globalenv()$GLOBAL_REVALIDATE
    }
  }

  # ----------------------------------------------------------------------------
  # Handle deprecated argument `loc` logic
  # ----------------------------------------------------------------------------
  
  if (missing.dir && missing.loc) {
    invisible(NULL)
  } else if (!missing.dir && missing.loc) {
    invisible(NULL)
  } else if (missing.dir && !missing.loc) {
    if (!is.null(loc)) {
      if (!silent) {
        base::warning("Argument `loc` is deprecated, use `dir` instead")  
        base::message("Setting argument `dir` to supplied `loc` value")
      }
      dir <- loc
    } else if (is.null(loc)) {
      invisible(NULL)
      if (!silent) {
        base::warning("Argument `loc` is deprecated, use `dir` instead")  
        base::message("Using default value \"cache\" for argument `dir`")
      }
    }
  } else if (!missing.dir && !missing.loc) {
    stop(paste0(
      "Cannot accept values for both arguments `dir` and `loc` (deprecated)\n",
      "Aborting... no changes made"
    ))
  } else {
    stop("internal error")
  }

  # ----------------------------------------------------------------------------
  # Check that dir exists, create if need be
  # ----------------------------------------------------------------------------
  
  if (!dir.exists(dir)) {
    if (create.dir) {
      dir.create(dir, recursive = TRUE)
    } else {
      stop(paste0(
        "`dir` does not exist, and `create.dir = FALSE`, Aborting..."
      ))
    }
  }
  if (!silent) {
    message(paste0("Using dir = ", dir))
  }

  # ----------------------------------------------------------------------------
  # Check for previous file, e.g., "cache/<name>.rds"
  # ----------------------------------------------------------------------------
  # If file does not exist, create it
  # If it does exist, optionally overwrite the cached value
    # if revalidate = TRUE
  
  rds.file.full.path <- file.path(dir, paste0(name, ".rds"))
  file_exists <- base::file.exists(rds.file.full.path)

  if (!file_exists) {
    if (!silent) {
      base::message("Validation file does not exist")
      base::message("Running saveRDS() ...")
    }
    base::saveRDS(object, rds.file.full.path)
  } else {
    if (!silent) {
      base::message("Validation file exists")
    }
    if (revalidate) {
      if (!silent) {
        base::message("Overwriting validation file because `revalidate = TRUE` ...")
      }
      base::saveRDS(object, rds.file.full.path)
    }
  }

  # ----------------------------------------------------------------------------
  # Validate existing file with provided object
  # ----------------------------------------------------------------------------
  
  if (!silent) {
    base::message("Validating...")
  }

  base::do.call(f, list(object, base::readRDS(rds.file.full.path)))

}