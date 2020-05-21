#' Tidy a Kaplan-Meier survfit object
#'
#' This function tidies a Kaplan-Meier Surv object using broom tidy function. 
#' The function expects a data frame, tibble or data.table and the equation for the survival model. 
#'
#' @param data input data frame, tibble or data.table for the Kaplan-Meier analysis. 
#' @param equation Survival equation that defines the analysis.         
#'
#' @return tidy data frame (tibble) using the broom tidy function. 
#' @export
#'
#' @examples
#' library(survival)
#' 
#' ## No stratification
#' vr_est_kaplan_meier(data = lung, equation = "Surv(time, status) ~ 1")
#' 
#' ## Stratified Kaplan-Meier analysis by `sex`
#' vr_est_kaplan_meier(data = lung, equation = "Surv(time, status) ~ sex")

 
vr_est_kaplan_meier <- function(data, equation) {
  
  data <- as.data.frame(data)
  
  survfit_object <- survival::survfit(
    eval(parse(text = equation)), data = data
  )

  # No strata: Create an artificial one for compatibility with downstream processing
  if (is.null(survfit_object$strata)){
    survfit_object$strata <- as.vector(length(survfit_object$time))
    attr(survfit_object$strata, "names") <- "Overall"
  } 

  broom_object <- broom::tidy(survfit_object)
  return(broom_object)
}
