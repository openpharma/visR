add_COX_HR <- function(gg, ...){
  UseMethod("add_COX_HR")
} 

add_COX_HR.ggsurvfit <- function(gg, ...){
  
  ### Question: how do we want to use this function?
  
   ## or we pass along a coxph object
  
  
  
   ## or we allow the user to update the formula
    # update(survfit_object,  formula = . ~ . + SEX, evaluate = TRUE))
  
  
  
   ## or we assume no changes to the formula
      SurvCall <- as.list(gg$data$call[[1]])
  
      CoxArgs <- (base::formals(survival::coxph))
      CoxCall <- append(as.symbol("survdiff"), SurvCall[names(SurvCall) %in% names(CoxArgs)])
  
      cox <- eval(as.call(CoxCall))
      
      tidycox <- tidyme(cox)
      
  return(list(gg, tidycox))
}

add_COX_HR.tblKM <- function(gg){
  stop("this object is not yet part of the scope")
}
