#' @title Create a `ggplot` directly from an object through an S3 method
#'
#' @description S3 method for creating plots directly from objects using `ggplot2`, similar to base plot function.
#'     The default method is base::plot.
#'     
#' @author Steven Haesendonckx
#' 
#' @seealso \code{\link[ggplot2]{ggplot}}
#' 
#' @param x object to be passed on to the method
#' @param ... other arguments passed on to the method
#'  
#' @rdname plot
#' 
#' @export

plot <- function(x, ...){
  UseMethod("plot")
} 

#' @rdname plot
#' @method plot default
#' @export

plot.default <- function(x, ...){
  base::plot(x)
}

#' @param survfit_object Object of class `survfit`
#' @param y_label \code{character} Label for the y-axis. When not specified, the default will do a proposal, depending on the `fun` argument.
#' @param x_label \code{character} Label for the x-asis. When not specified, the algorithm will look for "PARAM" information inside the list structure of the `survfit` object.
#'   Note that this information is automatically added when using visR::KM_est and when the input data has the variable "PARAM". If no "PARAM" information is available
#'   "time" is used as label.
#' @param x_units Unit to be added to the x_label (x_label (x_unit)). Default is NULL.
#' @param x_ticks Ticks for the x-axis. When not specified, the default will do a proposal. 
#' @param y_ticks Ticks for the y-axis. When not specified, the default will do a proposal based on the `fun` argument.
#' @param legend_position Specifies the legend position in the plot. Character values allowed are "top" "left" "bottom" "right". Numeric coordinates are also allowed.
#'   Default is "right".
#' 
#' 
#' @examples
#' library(survival)
#' library(dplyr)
#' library(tidyr)
#' library(ggplot2)
#' 
#' survfit_object <- KM_est(data = adtte, strata = "TRTP")
#'
#' ## Plot survival probability
#' plot(survfit_object = survfit_object, fun = "surv")
#' plot(survfit_object, fun = "pct")
#' 
#' ## Plot cumulative hazard
#' plot(survfit_object, fun = "cloglog")
#'  
#' @return Object of class \code{ggplot}  \code{ggsurvplot}.
#'  
#' @rdname plot
#' @method plot survfit
#' @export
#
plot.survfit <- function(
  survfit_object = NULL
 ,y_label = NULL
 ,x_label = NULL
 ,x_units = NULL
 ,x_ticks = NULL
 ,y_ticks = NULL
 ,fun = "surv"
 ,legend_position = "right"
 ){
  
  #### Input validation ####
  
  if (!inherits(survfit_object, "survfit")) stop("survfit object is not of class `survfit`")
  if (is.character(legend_position) && ! legend_position %in% c("top", "bottom", "right", "left", "none")){
    stop("Invalid legend position given.")
  } else if (is.numeric(legend_position) && length(legend_position) != 2) {
    stop("Invalid legend position coordinates given.")
  }

  #### Y-label ####
  
  if (is.null(y_label) & is.character(fun)){
    y_label <- base::switch(
      fun,
      surv = "Survival probability",
      log = "log(Survival probability)",
      event = "Failure probability",
      cloglog = "log(-log(Survival probability))",
      pct = "Survival probability (%)",
      logpct = "log(Survival probability (%))",
      cumhaz = "cumulative hazard",
      stop("Unrecognized fun argument")
    )
  } else if (is.null(y_label) & is.function(fun)) {
    stop("Error in plot: No Y label defined. No default is available when `fun` is a function.")
  }  
  
  tidy_object <- prepare_suvfit(survfit_object, fun)
    
  #### Obtain alternatives for X-axis ####
  
  if (is.null(x_label)){
    if ("PARAM" %in% names(survfit_object)) x_label = survfit_object[["PARAM"]]
    if (! "PARAM" %in% names(survfit_object)) x_label = "time"
    if (!is.null(x_units)) x_label = paste0(x_label, " (", x_units, ")")
  }
  if (is.null(x_ticks)) x_ticks = pretty(survfit_object$time, 10)
  
  #### Obtain alternatives for Y-axis ####
  
  if (is.null(y_ticks) & is.character(fun)){
    y_ticks <- base::switch(
      fun,
      surv = pretty(c(0,1), 5),
      log =  pretty(round(c(ymin,ymax), 0), 5),
      event = pretty(c(0,1), 5),
      cloglog = pretty(round(c(ymin,ymax), 0), 5),
      pct = pretty(c(0,100), 5),
      logpct = pretty(c(0,5), 5),
      cumhaz =  pretty(round(c(ymin,ymax), 0), 5),
      stop("Unrecognized fun argument")
    )
  } else if (is.null(y_label) & is.function(fun)) {
    stop("Error in plot: No Y label defined. No default is available when `fun` is a function.")
  }  

  #### Plotit ####
  
  yscaleFUN <- function(x) sprintf("%.2f", x)
  
  gg <- ggplot2::ggplot(tidy_object, aes(x = time, group = strata)) +
    ggplot2::geom_step(aes(y = est, col = strata)) + 
    ggsci::scale_color_nejm() + 
    ggsci::scale_fill_nejm() + 
    ggplot2::scale_x_continuous(name = paste0("\n", x_label),
                                breaks = x_ticks,
                                limits = c(min(x_ticks), max(x_ticks))) +
    ggplot2::scale_y_continuous(name = paste0(y_label, "\n"),
                                breaks = y_ticks,
                                labels = yscaleFUN,
                                limits = c(min(y_ticks), max(y_ticks))) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = legend_position) +
    ggplot2::guides(color=guide_legend(override.aes=list(fill=NA))) +
    NULL
  
  class(gg) <- append(class(gg), "ggsurvfit")
  
  return(gg)
}
