get_COX_HR <- function(x, ...){
  UseMethod("add_COX_HR")
} 

update_formula = ". ~ . + SEX"

get_COX_HR.survfit <- function(survfit_object = survfit_object, update_formula = NULL, ...){
  
  if (!base::inherits(gg, "ggsurvfit")) stop("Error in add_COX_HR: Object gg not of class `ggsurvfit`.")
  
  #### Update ####
  
  if (!is.null(update_formula)){
    updated_object <- update(survfit_object,  formula = eval(update_formula), evaluate = TRUE)
  }
  else updated_object <- survfit_object
  
  #### Change call ####
  
  SurvCall <- as.list(updated_object$call)
  CoxArgs <- base::formals(survival::coxph)
  CoxCall <- append(as.symbol("coxph"), SurvCall[names(SurvCall) %in% names(CoxArgs)])
  
  cox <- tidyme(eval(as.call(CoxCall)))
      

  return(cox)
}


add_COX_HR.tblKM <- function(gg){
  stop("this object is not yet part of the scope")
}
