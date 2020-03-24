#' Tidy a survival survfit object
#'
#' This function tidies a Surv object using broom tidy function. 
#' The function expects a data frame and the equation for the survival model. 
#'
#' @param data input data frame for the KM analysis. 
#' @param euqation Survival equation that defines the analysis.         
#'
#' @return tidy data frame (tibble) using the broom tidy function. 
#' @export
#'
#' @examples
#' library(survival)
#' vr_est_kaplan_meier(lung, "Surv(time, status) ~ sex")
#' 
vr_est_kaplan_meier <- function(data, equation) {
  
    survfit_object <- survival::survfit(
      eval(parse(text = equation)), data = data
      )
    
    broom_object <- broom::tidy(survfit_object)
    return(broom_object)
}
