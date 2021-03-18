#' @title Add pointwise Confidence Interval to a an object created by visR through an S3 method
#'
#' @description S3 method for adding a pointwise confidence interval to a object created with visR.
#'     No default method is available at the moment.
#'
#' @author Steven Haesendonckx
#'
#' @param gg visR object
#' @param ... other arguments passed on to the method to modify \code{\link[ggplot2]{geom_ribbon}}
#'
#' @examples
#' library(survival)
#' library(dplyr)
#' library(tidyr)
#' library(ggplot2)
#'
#' survfit_object <- survival::survfit(data = adtte, Surv(AVAL, 1-CNSR) ~ TRTP)
#' vr_plot(survfit_object) %>%
#'   add_CI(alpha = 0.1, style = "step", linetype = 3)
#'
#' @return Pointwise confidence interval overlayed on a visR ggplot
#'
#' @rdname add_CI
#'
#' @export

add_CI <- function(gg, ...){
  UseMethod("add_CI")
}

#' @param gg A ggplot created with visR
#' @param alpha aesthetic of ggplot2 \code{\link[ggplot2]{geom_ribbon}}. Default is 0.1.
#' @param style aesthetic of ggplot2 \code{\link[ggplot2]{geom_ribbon}}. Default is "ribbon".
#' @param linetype aesthetic of ggplot2 \code{\link[ggplot2]{geom_ribbon}}.
#'
#' @rdname add_CI
#' @method add_CI ggsurvfit
#' @export

add_CI.ggsurvfit <- function(gg, 
                             alpha = 0.1, 
                             style = "ribbon", 
                             linetype, ...){
  
  if (! base::all(c("est.lower", "est.upper") %in% colnames(gg$data))) {
    
    stop("Confidence limits were not part of original estimation.")
  
  }
  
  if (! base::any(c("ribbon", "step") %in% style)) {
    
    warning("Invalid `style` argument. Setting `style` to `ribbon`.")
    style <- "ribbon"
    
  }
  
  if (style == "ribbon"){
    
    if (!missing(linetype)) {
      
      warning("Argument `linetype` not used for style ribbon.")
      
    }
    
    gg <- gg +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = est.lower, 
                                        ymax = est.upper, 
                                        fill = strata), 
                           alpha = alpha, 
                           na.rm = TRUE)
  }
  
  if (style == "step"){
    
    if (missing(linetype)) {
      
      # Set a default linetype of solid (2) if the user didn't specify any
      linetype <- 2
      
    }
    
    gg <- gg +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = est.lower, 
                                        ymax = est.upper, 
                                        fill = NA, 
                                        colour = strata),
                           alpha = alpha, 
                           outline.type = "both",
                           linetype = linetype, 
                           show.legend = FALSE,
                           na.rm = TRUE)
  }
  
  return(gg)
}