#' @title Summarize Hazard Ratio from a survival object using S3 method
#'
#' @description S3 method for extracting information regarding Hazard Ratios.
#'     No default method is available at the moment.
#'
#' @author Steven Haesendonckx
#'
#' @seealso \code{\link[survival]{coxph}} \code{\link[stats]{update.formula}}
#'
#' @param x S3 object
#' @param ... other arguments passed on to the method survival::coxph
#'
#' @examples
#' library(survival)
#' library(dplyr)
#' library(tidyr)
#' library(ggplot2)
#' library(broom)
#'
#' ## treatment effect
#' survfit_object_trt <- estimate_KM(data = adtte, strata = c("TRTP"))
#' get_COX_HR(survfit_object_trt)
#'
#' ## treatment and gender effect
#' survfit_object_trt_sex <- estimate_KM(data = adtte, strata = c("TRTP", "SEX"))
#' get_COX_HR(survfit_object_trt_sex)
#'
#' ## update formula of KM estimates by treatment to include "SEX" for HR estimation
#' get_COX_HR(survfit_object_trt, update_formula = ". ~ . + SEX")
#'
#' ## update formula of KM estimates by treatment to include "AGE" for
#' ## HR estimation with ties considered via the efron method
#' get_COX_HR(survfit_object_trt, update_formula = ". ~ . + strata(AGE)", ties = "efron")
#'
#' @return A tibble with Hazard Ratios
#'
#' @rdname get_COX_HR
#'
#' @export

get_COX_HR <- function(x, ...){
  UseMethod("get_COX_HR")
}

#' @param survfit_object An object of class \code{survfit}
#' @param update_formula Template which specifies how to update the formula of the survfit_object \code{\link[stats]{update.formula}}
#'
#' @rdname get_COX_HR
#' @method get_COX_HR survfit
#' @export

get_COX_HR.survfit <- function(
  survfit_object = survfit_object,
  update_formula = NULL,
  ...
){

  if (!base::inherits(survfit_object, "survfit")){
    stop("Error in add_COX_HR: Object gg not of class `ggsurvfit`.")}

  #### Update ####

  if (!is.null(update_formula)){
    updated_object <- update(survfit_object,  formula = eval(update_formula), evaluate = TRUE)
  }
  else updated_object <- survfit_object

  #### Change call ####

  SurvCall <- as.list(updated_object$call)
  CoxArgs <- base::formals(survival::coxph)
  CoxCall <- append(as.symbol("coxph"), SurvCall[names(SurvCall) %in% names(CoxArgs)])
  CoxCall <- append(CoxCall, list(...))

  cox <- tidyme(eval(as.call(CoxCall)))


  return(cox)
}


add_COX_HR.tblKM <- function(gg){
  stop("this object is not yet part of the scope")
}
