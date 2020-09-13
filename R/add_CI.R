#' @title Add pointwise Confidence Interval to a plot created through visR using S3 method
#'
#' @description S3 method for adding a pointwise confidence interval to a visR plot.
#'     No default method is available at the moment.
#'     
#' @author Steven Haesendonckx {shaesen2@@its.jnj.com}
#' 
#' @param gg visR plot
#' @param ... other arguments passed on to the method
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
#' @return Pointwise confidence interval overlayed on a visR plot
#'  
#' @rdname add_CI
#' 
#' @export

add_CI <- function(gg, ...){
  UseMethod("add_CI")
} 

#' @param gg A plot created with visR
#' @inheritparams ggplot2::geom_ribbon
#'
#' @rdname add_CI
#' @method add_CI ggsurvfit
#' @export

add_CI.ggsurvfit <- function(gg, alpha = 0.1, style = "ribbon", linetype = 2, ...){

  if (! base::all(c("est.lower", "est.upper") %in% colnames(gg$data))) {
    warning("Confidence limits were not part of original estimation.")
    return(NULL)
  }
  
  if (! base::any(c("ribbon", "step") %in% style)) {
    warning("Invalid `step` argument.")
    return(NULL)
  } 
  
  if (style == "ribbon"){
    gg <- gg +
      geom_ribbon(aes(ymin = est.lower, ymax = est.upper, fill = strata), alpha = alpha, na.rm = TRUE) 
  }
  
  if (style == "step"){
    gg <- gg +
      geom_ribbon(aes(ymin = est.lower, ymax = est.upper, fill = NA, colour = strata), alpha = alpha, outline.type = "both", linetype = linetype, show.legend = FALSE, na.rm = TRUE)
  }
  
  return(gg)
}


add_CI.tblKM <- function(gg){
  stop("this object is not yet part of the scope")
}