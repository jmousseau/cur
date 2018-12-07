#' Curry a function.
#'
#' Creates a curried version of \code{func} that executes upon a parameterless
#' call.
#'
#' @param func The function to curry.
#'
#' @return A curried version of \code{func}.
#'
#' @examples
#'
#' # Curry a function with labeled parameters. Note that a parameterless call
#' # must be made in order to execute the function.
#' sd_rm <- curry(sd)(na.rm = TRUE)
#' sd_rm(c(1, 2, 3, NA))()
#'
#' # Curry a function with an ellipsis.
#' add_1_and_3_to <- curry(sum)(1, 3)
#' add_1_and_3_to(70)()
#'
#' @export
curry <- function(func) {
  root_env <- new.env()
  root_env$defined_parameters <- list()

  func_curried <- function(...) {
    parameters <- as.list(match.call())
    parameters[[1]] <- NULL

    if (length(parameters) == 0) {
      return(do.call(func, root_env$defined_parameters))
    }

    root_env$defined_parameters <- c(root_env$defined_parameters, parameters)

    return(func_curried)
  }

  return(func_curried)
}

#' An alias of curry.
#'
#' Meant for use with the package's namespace.
#'
#' @examples
#'
#' add_1_and_3_to <- cur::ry(sum)(1, 3)
#' add_1_and_3_to(70)()
#'
#' @export
ry <- curry
