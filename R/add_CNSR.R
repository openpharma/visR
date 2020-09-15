#' @title Add censoring symbols to a visR ggplot through an S3 method
#'
#' @description S3 method for adding censoring symbols to visR ggplot.
#'     No default method is available at the moment.
#'     
#' @author Steven Haesendonckx
#' 
#' @param gg visR object
#' @param ... other arguments passed on to the method to modify \code{\link[ggplot2]{geom_point}}
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
#' @return Censoring symbols overlayed on a visR ggplot
#'  
#' @rdname add_CNSR
#' 
#' @export

add_CNSR <- function(gg, ...){
  UseMethod("add_CNSR")
} 

#' @param gg A ggplot created with visR
#' @param shape aesthetic of ggplot2 \code{\link[ggplot2]{geom_point}}. Default is 3.
#' @param size aesthetic of ggplot2 \code{\link[ggplot2]{geom_point}}. Default is 2.
#'
#' @rdname add_CNSR
#' @method add_CNSR ggsurvfit
#' @export

add_CNSR.ggsurvfit <- function(gg, shape = 3, size = 2, ...){
  
  if (!inherits(gg, "ggsurvfit")){
    stop("Function can only be applied to an object of class `ggsurvfit`.")
  }
  
  gg <- gg +
    geom_point(data = base::subset(gg$data, n.censor >= 1), aes(x = time, y = est, color = strata), shape = shape, size = size)

  return(gg)
}