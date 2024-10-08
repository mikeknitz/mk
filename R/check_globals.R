# Updated 2024-06-06

#' @title Check if functions use global variables
#' @description Checks all the defined functions in a specified environment whether they reference global variables, using `codetools::findGlobals()`.
#' 
#' Ideally most functions would be pure, but quick one-off functions might be expected to reference global variables.
#' @returns a `data.frame` with columns `Function`, `Env`, and `HasGlobals`
#' @examples
#' mk::check_globals()
#' @param env a specified environment to check, default `.GlobalEnv`
#' @export

check_globals <- function(env = .GlobalEnv) {
    
    # Get list of functions in the environment
    func_list <- utils::ls.str(mode = "function", envir = env) |> as.list()

    # Check one func
    check_one_func <- function(f, env = env) {
        # Get <char> vector of globals in this function
        # Inspired by:
            # https://stackoverflow.com/questions/28970939
        res <- base::intersect(codetools::findGlobals(get(f, envir = env)),
            ls(envir = env))
        # TRUE if has globals
        !identical(res, character(0))
    }
    
    # Apply to func_list and return <data.frame>
    res <- lapply(func_list, check_one_func, env = env) |>
        stats::setNames(unlist(func_list)) |>
        unlist()

    res <- data.frame(
        "Function" = names(res),
        "Env"      = rep(deparse(base::substitute(env)), length(func_list)),
        "HasGlobals" = res
    )
    row.names(res) <- NULL
    res
    
}
