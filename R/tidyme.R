#' @title Extended tidy cleaning of selected objects using S3 method
#'
#' @description S3 method for extended tidying of selected model outputs. Note
#'   that the visR method retains the original nomenclature of the objects,
#'   and adds the one of broom::tidy to ensure compatibility with tidy workflows.
#'   The default method relies on \code{broom::tidy} to return a tidied object
#'
#' @seealso \code{\link[broom]{tidy}}
#'
#' @param x An S3 object
#' @param ... other arguments passed on to the method
#'
#' @examples
#'
#' ## Extended tidying for a survfit object
#' surv_object <- visR::estimate_KM(data = adtte, strata = "TRTA")
#' tidied <- visR::tidyme(surv_object)
#'
#' ## Tidyme for non-included classes
#' data <- cars
#' lm_object <- stats::lm(data = cars, speed ~ dist)
#' lm_tidied <- visR::tidyme(lm_object)
#' lm_tidied
#'
#' @return Data frame containing all list elements of the S3 object as columns.
#'   The column 'strata' is a factor to ensure that the strata are sorted
#'   in agreement with the order in the `survfit` object
#'
#' @rdname tidyme
#'
#' @export

tidyme <- function(x, ...) {
  UseMethod("tidyme")
}

#' @rdname tidyme
#' @method tidyme default
#' @export

tidyme.default <- function(x, ...) {
  base::message("tidyme S3 default method (broom::tidy) used.")
  return(as.data.frame(broom::tidy(x)))
}

#' @rdname tidyme
#' @method tidyme survfit
#' @export

tidyme.survfit <- function(x, ...) {
  if (inherits(x, "survfit")) {

    ## keep source
    survfit_object <- x

    ## Change class to perform list manipulations. The survfit class was throwing errors.
    class(x) <- ("list")

    ## Prepare for cleaning
    reps <- as.vector(length(x$time))

    ## Lists to vectors
    cleaner <- function(x) {
      if (length(x) == 1) {
        rep(x, reps)
      } else {
        x
      }
    }

    ## Cleanit: strata will always be filled out based off the estimation function from which it is called
    retme <-
      lapply(x[names(x) %in% c("n", "strata", "call", "data_name", "na.action", "strata_lbls") == FALSE], cleaner) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(
        time = time,
        n.risk = as.integer(n.risk),
        n.event = as.integer(n.event),
        n.censor = as.integer(n.censor),
        call = list(x[["call"]]),
        estimate = surv,
        std.error = std.err,
        conf.low = lower,
        conf.high = upper
      )

    if (!is.null(x[["strata"]])) {
      retme[["strata"]] <- rep(names(x[["strata"]]), x[["strata"]])
      retme[["n.strata"]] <- rep(x[["n"]], x[["strata"]])
    }
  }

  attr(retme, "survfit_object") <- survfit_object
  strata <- .extract_strata_varlist(survfit_object)
  # modify strata label, removing ref to raw variable name
  if (!is.null(strata)) {
    for (stratum in strata) {
      retme[["strata"]] <-
        gsub(
          pattern = paste0(stratum, "="),
          replacement = "",
          x = retme[["strata"]],
          fixed = TRUE
        )
    }
  }

  retme[["strata"]] <- factor(retme[["strata"]], levels = unique(retme[["strata"]]))

  return(as.data.frame(retme))
}
