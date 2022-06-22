#' @title Summarize Hazard Ratio from a survival object using S3 method
#'
#' @description S3 method for extracting information regarding Hazard Ratios.
#' The function allows the survival object's formula to be updated.
#' No default method is available at the moment.
#'
#' @seealso \code{\link[survival]{coxph}} \code{\link[stats]{update.formula}}
#'
#' @param x An object of class \code{survfit}
#' @param ... other arguments passed on to the method survival::coxph
#'
#' @rdname get_COX_HR
#' @export

get_COX_HR <- function(x, ...) {
  UseMethod("get_COX_HR", x)
}


#' @param update_formula Template which specifies how to update the formula of the survfit object \code{\link[stats]{update.formula}}
#'
#' @examples
#' ## treatment effect
#' survfit_object_trt <- visR::estimate_KM(data = adtte, strata = c("TRTP"))
#' visR::get_COX_HR(survfit_object_trt)
#'
#' ## treatment and gender effect
#' survfit_object_trt_sex <- visR::estimate_KM(data = adtte, strata = c("TRTP", "SEX"))
#' visR::get_COX_HR(survfit_object_trt_sex)
#'
#' ## update formula of KM estimates by treatment to include "SEX" for HR estimation
#' visR::get_COX_HR(survfit_object_trt, update_formula = ". ~ . + SEX")
#'
#' ## update formula of KM estimates by treatment to include "AGE" for
#' ## HR estimation with ties considered via the efron method
#' visR::get_COX_HR(survfit_object_trt,
#'   update_formula = ". ~ . + survival::strata(AGE)", ties = "efron"
#' )
#'
#' @return A tidied object of class \code{coxph} containing Hazard Ratios
#'
#' @rdname get_COX_HR
#' @method get_COX_HR survfit
#' @export

get_COX_HR.survfit <- function(x,
                               update_formula = NULL,
                               ...) {

  # Update formula ----------------------------------------------------------
  updated_call <- rlang::quo_squash(x$call)
  updated_call[["data"]] <- rlang::inject(!!updated_call[["data"]], env = attr(x$call, ".Environment"))
  updated_object <- eval(updated_call, envir = attr(x$call, ".Environment"))
  if (!is.null(update_formula)) {
    updated_object <-
      stats::update(updated_object, formula = stats::as.formula(update_formula), evaluate = TRUE)
  }

  # Change Call -------------------------------------------------------------
  SurvCall <- as.list(updated_object$call)
  CoxArgs <- base::formals(survival::coxph)
  CoxCall <- append(quote(survival::coxph), SurvCall[names(SurvCall) %in% names(CoxArgs)])
  CoxCall <- append(CoxCall, list(...))

  # Tidy output -------------------------------------------------------------
  cox <-
    eval(as.call(CoxCall), envir = attr(x$call, ".Environment")) %>%
    tidyme()

  return(cox)
}

# END OF CODE -------------------------------------------------------------
