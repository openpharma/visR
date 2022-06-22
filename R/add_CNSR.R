#' @title Add censoring symbols to a visR object
#'
#' @description Add censoring symbols to a visR ggplot through an S3 method.
#'   The S3 method is for adding censoring symbols to a visR ggplot.
#'   The method is set up to use the pipe `%>%`.
#'
#'   No default method is available at the moment.
#'
#' @param gg A ggplot created with visR
#' @param shape aesthetic of ggplot2 \code{\link[ggplot2]{geom_point}}. Default is 3.
#' @param size aesthetic of ggplot2 \code{\link[ggplot2]{geom_point}}. Default is 2.
#' @param ... other arguments passed on to the method to modify \code{\link[ggplot2]{geom_point}}
#'
#' @examples
#'
#' library(visR)
#'
#' # Estimate KM curves by treatment group
#' survfit_object <- survival::survfit(data = adtte, survival::Surv(AVAL, 1 - CNSR) ~ TRTP)
#'
#' ## plot without confidence intervals
#' p <- visR::visr(survfit_object)
#' p
#'
#' # add censoring to plot
#' p %>% visR::add_CNSR()
#'
#' # change censor symbol shape
#' p %>% visR::add_CNSR(shape = 1)
#'
#' # change size and shape
#' p %>% visR::add_CNSR(size = 4, shape = 2)
#'
#' @return Censoring symbols overlayed on a visR ggplot
#'
#' @rdname add_CNSR
#'
#' @export

add_CNSR <- function(gg, ...) {
  UseMethod("add_CNSR", gg)
}

#' @rdname add_CNSR
#' @method add_CNSR ggsurvfit
#' @export

add_CNSR.ggsurvfit <- function(gg, shape = 3, size = 2, ...) {
  if (!base::is.numeric(size)) {
    if (base::is.list(size)) {

      # ggplot technically allows a list of the same length as the elements to
      # be plotted. However, we don't sanity check this and let ggplot deal with
      # it: https://github.com/openpharma/visR/wiki/Don't-do-this
    } else {
      warning("Invalid `size` specified. Setting it to 2.")
      size <- 2
    }
  }

  if (!base::is.numeric(shape)) {
    if (base::is.list(shape)) {

      # ggplot technically allows a list of the same length as the elements to
      # be plotted. However, we don't sanity check this and let ggplot deal with
      # it: https://github.com/openpharma/visR/wiki/Don't-do-this
    } else if (base::is.character(shape)) {
      if (base::nchar(shape) > 1) {
        warning("Invalid `shape` specified. If specifiyng a symbol, it must be a single character. Setting it to 3.")
        shape <- 3
      }
    } else if ((base::is.na(shape)) || (base::is.null(shape))) {
      warning("Invalid `shape` specified. Setting it to 3.")
      shape <- 3
    }
  } else if ((shape < 0) | (shape > 25)) {
    warning("Invalid `shape` specified. Values between [0-25] are supported. Setting it to 3.")
    shape <- 3
  }

  gg <- gg +
    ggplot2::geom_point(
      data = base::subset(gg$data, n.censor >= 1),
      ggplot2::aes(
        x = time,
        y = est,
        color = strata
      ),
      shape = shape,
      size = size,
      show.legend = FALSE
    )

  return(gg)
}

#' @export
#' @method add_CNSR ggtidycuminc
#' @rdname add_CNSR
add_CNSR.ggtidycuminc <- add_CNSR.ggsurvfit
