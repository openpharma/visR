#' @title Wrapper around quantile methods
#'
#' @description S3 method for extracting quantiles.
#'   No default method is available at the moment.
#'
#' @seealso \code{\link[survival]{quantile.survfit}}
#'
#' @param x An object of class \code{survfit}
#' @param probs  probabilities Default = c(0.25,0.50,0.75)
#' @inheritParams survival::quantile.survfit
#' @param ... other arguments passed on to the method
#'
#' @examples
#'
#' ## Kaplan-Meier estimates
#' survfit_object <- visR::estimate_KM(data = adtte, strata = c("TRTP"))
#'
#' ## visR quantiles
#' visR::get_quantile(survfit_object)
#'
#' ## survival quantiles
#' quantile(survfit_object)
#'
#' @return A data frame with quantiles of the object
#'
#' @rdname get_quantile
#' @export
#'
get_quantile <- function(x, ...) {
  UseMethod("get_quantile", x)
}

#' @rdname get_quantile
#' @method get_quantile survfit
#' @export
get_quantile.survfit <- function(x,
                                 ...,
                                 probs = c(0.25, 0.50, 0.75),
                                 conf.int = TRUE,
                                 tolerance = sqrt(.Machine$double.eps)) {


  # User input validation ---------------------------------------------------

  if (conf.int == TRUE & !base::all(c("lower", "upper") %in% names(x))) {
    stop("Confidence limits were not part of original estimation.")
  }

  if (!base::all(is.numeric(probs) == TRUE) | (!base::all(probs < 1))) {
    stop("probs should be a numeric vector.")
  }

  if (!is.numeric(tolerance)) {
    stop("tolerance should be numeric")
  }

  # Extract quantiles -------------------------------------------------------

  q <- quantile(x,
    probs = probs,
    conf.int = conf.int,
    tolerance = tolerance,
    type = 3
  )

  qdf <- do.call(rbind.data.frame, q)

  strata <- as.character(unlist(lapply(q, rownames)))
  quantity <- unlist(lapply(strsplit(rownames(qdf), "\\.", fixed = FALSE), `[[`, 1))

  final <- data.frame(
    cbind(strata, quantity, qdf),
    row.names = NULL,
    check.names = FALSE
  )

  final <- final[order(final[, "strata"], final[, "quantity"]), ]

  return(final)
}
