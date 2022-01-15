#' Pipe operator
#'
#' See \code{magrittr::\`\link[magrittr:pipe]{\%>\%}}\` for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL



#' @title Find the "lhs" in the pipeline
#'
#' @description This function finds the left-hand sided symbol in a magrittr pipe and returns it as a character.
#'
#' @return Left-hand sided symbol as string in the magrittr pipe.
#'
#' @references \url{https://github.com/tidyverse/magrittr/issues/115#issuecomment-173894787}
#'
#' @export
#'
#' @examples
#' library(magrittr)
#'
#' ## example
#' blah <- function(x) the_lhs()
#' adtte %>%
#'   blah()

the_lhs <- function() {
  parents <- lapply(sys.frames(), parent.env)

  is_magrittr_env <-
    vapply(parents, identical, logical(1), y = environment(`%>%`))

  if (any(is_magrittr_env)) {
    left <- deparse(get("lhs", sys.frames()[[max(which(is_magrittr_env))]]))
  }

  return(as.character(gsub(" %.*$", "", left)))
}

# copy of rlang::`%||%` operator
#' @title Default value for `NULL`
#'
#' @description This infix function makes it easy to replace NULLs with a
#' default value. This is a copy of the rlang function.
#' @noRd
`%||%` <- function (x, y) if (rlang::is_null(x)) y else x
