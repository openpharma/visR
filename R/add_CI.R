#' @title Add confidence interval (CI) to visR object
#'
#' @description Method to add pointwise confidence intervals to a an object
#'   created by visR through an S3 method. The method is set up to use
#'   the pipe `%>%`. There are two options to display CI's, a "ribbon" or
#'   as "step" lines.
#'
#'   No default method is available at the moment.
#'
#' @param gg A ggplot created with visR
#' @param alpha aesthetic of ggplot2 \code{\link[ggplot2]{geom_ribbon}}. Default is 0.1.
#' @param style aesthetic of ggplot2 \code{\link[ggplot2]{geom_ribbon}}. Default is "ribbon". An alternative option is "step" that uses a line to display interval bounds.
#' @param linetype aesthetic of ggplot2 \code{\link[ggplot2]{geom_ribbon}}.
#' @param ... other arguments passed on to the method to modify \code{\link[ggplot2]{geom_ribbon}}
#'
#' @examples
#'
#' library(visR)
#'
#' # Estimate KM curves by treatment group
#' survfit_object <- survival::survfit(data = adtte, survival::Surv(AVAL, 1 - CNSR) ~ TRTP)
#'
#' ## plot without confidence intervals (CI)
#' p <- visR::visr(survfit_object)
#' p
#'
#' # add CI to plot with default settings
#' p %>% add_CI()
#'
#' # change transparency of CI ribbon
#' p %>% add_CI(alpha = 0.9, style = "ribbon")
#'
#' # plot CI as a step line instead of ribbon
#' p %>% add_CI(alpha = 0.1, style = "step")
#'
#' # change linetype of CI
#' p %>% add_CI(style = "step", linetype = 1)
#'
#' @return Pointwise confidence interval overlayed on a visR ggplot
#'
#' @rdname add_CI
#'
#' @export

add_CI <- function(gg, ...) {
  UseMethod("add_CI", gg)
}

#' @rdname add_CI
#' @method add_CI ggsurvfit
#' @export

add_CI.ggsurvfit <- function(gg,
                             alpha = 0.1,
                             style = "ribbon",
                             linetype, ...) {
  if (!base::all(c("est.lower", "est.upper") %in% colnames(gg$data))) {
    stop("Confidence limits were not part of original estimation.")
  }

  if ((alpha > 1) | (alpha < 0)) {
    warning("Invalid `alpha` argument, must be between 0 and 1. Setting it to 0.1.")
    alpha <- 0.1
  }

  if (!base::any(c("ribbon", "step") %in% style)) {
    warning("Invalid `style` argument. Setting `style` to `ribbon`.")
    style <- "ribbon"
  }

  gg_gb <- ggplot2::ggplot_build(gg)
  strata_colours <- unique(gg_gb$data[[1]]$colour)

  if (style == "ribbon") {
    if (!missing(linetype)) {
      warning("Argument `linetype` not used for style ribbon.")
    }

    gg <- gg +
      ggplot2::geom_ribbon(ggplot2::aes(
        ymin = est.lower,
        ymax = est.upper
      ),
      stat = StatStepribbon,
      na.rm = TRUE,
      show.legend = FALSE
      ) +
      ggplot2::scale_fill_manual(values = ggplot2::alpha(strata_colours, alpha))
  }

  if (style == "step") {
    if (missing(linetype)) {

      # Set a default linetype of solid (2) if the user didn't specify any
      linetype <- 2
    }

    gg <- gg +
      ggplot2::geom_ribbon(ggplot2::aes(
        ymin = est.lower,
        ymax = est.upper,
        colour = strata
      ),
      outline.type = "both",
      linetype = linetype,
      show.legend = FALSE,
      na.rm = TRUE
      ) +
      ggplot2::scale_fill_manual(values = ggplot2::alpha(strata_colours, 0))
  }

  return(gg)
}

#' @rdname add_CI
#' @method add_CI ggtidycuminc
#' @export
add_CI.ggtidycuminc <- add_CI.ggsurvfit
