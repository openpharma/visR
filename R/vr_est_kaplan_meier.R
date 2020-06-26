#' @title Tidy a Kaplan-Meier survfit object
#'
#' @description This function tidies a Kaplan-Meier Surv object using broom tidy function for downstream processing. 
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
#' 
#' ## Stratification with one level
#' df <- lung
#' df$blah <- "One level present"
#' vr_est_kaplan_meier(data = df, equation = "Surv(time, status) ~ blah")


vr_est_kaplan_meier <- function(data, equation) {
  data <- as.data.frame(data)
  formula <- stats::as.formula(equation)
  
  survfit_object <- survival::survfit(
   formula, data = data
  )

  # No strata: Create an artificial one for compatibility with downstream processing
  if (is.null(survfit_object$strata)){
    survfit_object$strata <- as.vector(length(survfit_object$time))
    
    strat_var <- deparse(formula[[3]])
    #strat_var <- base::trimws(base::sub('.*~', '', base::gsub(' ', '', equation)), which="both")
    
    if (strat_var == "1"){
      # overall analysis
      attr(survfit_object$strata, "names") <- "Overall"
    } else {
      # ~ x with One level in variable present
      attr(survfit_object$strata, "names") <- data[1, strat_var]
    }
  } 

  broom_object <- broom::tidy(survfit_object)
  return(broom_object)
}

### Ideas for improvement
 ## rename function vr_KM_estimation
 ## Use of CNSR AVAL CHG. Only information required is input df and main effect (overall = 1, TRT01P, ..)


