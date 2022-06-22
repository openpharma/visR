
#' @title Find the "lhs" in the pipeline
#' @description This function finds the left-hand sided symbol in a magrittr pipe and returns it as a character.
#' @return Left-hand sided symbol as string in the magrittr pipe.
#' @references \url{https://github.com/tidyverse/magrittr/issues/115#issuecomment-173894787}
#'
#' @examples
#' blah <- function(x) the_lhs()
#' adtte %>%
#'   blah()
#' @export
#'
the_lhs <- function() {
  parents <- lapply(sys.frames(), parent.env)

  is_magrittr_env <-
    vapply(parents, identical, logical(1), y = environment(`%>%`))

  if (any(is_magrittr_env)) {
    left <- deparse(get("lhs", sys.frames()[[max(which(is_magrittr_env))]]))
  }

  return(as.character(gsub(" %.*$", "", left)))
}

#' @title Find the character that represents the data argument in a call list
#'
#' @description This function returns character that represents the data argument in a call list.
#' @param call_list A list from a call
#' @return Character representing the data.
#' @noRd

.call_list_to_name <- function(call_list) {
  call_list[["data"]]
  if (length(base::deparse(call_list[["data"]])) == 1 &&
    deparse(call_list[["data"]]) %in% c(".", ".x", "..1")) {
    df <- the_lhs()
    call_list[["data"]] <- as.symbol(df) %>% as.character()
  } else {
    df <- as.character(sub("\\[.*$", "", deparse(call_list[["data"]]))[1])
  }
}

#' @title Is visR survfit?
#'
#' @return logical
#' @noRd
is_visr_survfit <- function(x) {
  # the visr survift object saves a quosure instead of a call
  inherits(x, "survfit") && rlang::is_quosure(x$call)
}
