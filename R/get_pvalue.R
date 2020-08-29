#' @title Summarize equality across strata from a survival object using S3 method
#'
#' @description S3 method for extracting information regarding equality across strata.
#'     No default method is available at the moment.
#'     
#' @author Steven Haesendonckx {shaesen2@@its.jnj.com}
#' 
#' @seealso \code{\link[survival]{survdiff}
#' 
#' @param x S3 object
#' @param ... other arguments
#' @examples
#' 
#' ## Extended tidying for a survfit object
#' surv_object <- survival::survfit(data = adtte, Surv(AVAL, 1-CNSR) ~ TRTP)
#' get_pvalue(surv_object)
#'  
#' @rdname get_pvalue
#' @export get_pvalue

get_pvalue <- function(x, ...){
  UseMethod("get_pvalue")
}

#' @return \code{NULL}
#' 
#' @author Steven Haesendonckx {shaesen2@@its.jnj.com}
#' 

#' @param survfit_object An object of class `survfit`
#' @param ptype Character vector containing the type of p-value desired. Current options are "Log-Rank" "Wilcoxon" "Tarone-Ware" "Custom" "All".
#'   "Custom" allows the user to specify the weights on the Kaplan-Meier estimates using the argument `rho`.
#'   The default is "All" displaying all types possible. When `rho` is specified in context of "All", also a custom p-value is displayed. 
#' @inheritParams survival::survdiff
#'
#' @rdname get_pvalue
#' @method get_pvalue.survfit
#' @S3method get_pvalue

get_pvalue.survfit <- function(
  survfit_object,
  ptype = "All",
  rho   = NULL,
  ...
) {
  
  ## Input Validation

  if(! inherits(survfit_object, "survfit"))
    stop("Error in get_pvalue: x is not of class `survfit`.")
  if (length(names(survfit_object[["strata"]])) == 1)
    stop("Error in get_pvalue: Main effect has only 1 level. Test of equality over strata can't be determined")
  if (is.null(ptype))
    stop("Error in get_pvalue: Specify a valid ptype.")
  if (! base::any(c("Log-Rank", "Wilcoxon", "Tarone-Ware", "Custom", "All") %in% ptype))
    stop("Error in get_pvalue: Specify a valid type")
  if ("Custom" %in% ptype & is.null(rho))
    stop("Error in get_pvalue: ptype = `Custom`. Please, specify rho.")
  
  ## Re-use Call from survival object
  
  Call <- as.list(survfit_object$call)
  NewCall <- append(as.list(as.symbol("survdiff")), Call[names(Call) %in% names(formals(survival::survdiff))])
  
  if ("All" %in% ptype){
    ptype = c("Log-Rank", "Wilcoxon", "Tarone-Ware")
    if (!is.null(rho)){
      ptype = c(ptype, "Custom")
    }
  }
  
  ## Summary list
  
  psummary <- list(
    `Log-Rank`    = rlang::expr(eval(as.call(append(NewCall, list(rho = 0))))),
    `Wilcoxon`    = rlang::expr(eval(as.call(append(NewCall, list(rho = 1))))),
    `Tarone-Ware` = rlang::expr(eval(as.call(append(NewCall, list(rho = 1.5))))),
    `Custom`      = rlang::expr(eval(as.call(append(NewCall, list(rho = rho)))))
  )[ptype]
  
  .pvalformat <- function(x){
    options(scipen=999)
    if (x < 0.001) "<0.001"
    else if (x > 0.999) ">0.999"
    else format(round(x, 3), digits = 3, justify = "right", width = 6)
  }

  psummary_eval <- lapply(psummary, eval, env = environment())
  Nm <- names(psummary_eval)
  if ("Custom" %in% ptype) Nm <- base::sub("Custom", paste0("Harrington and Fleming test (rho = ", rho, ")"), Nm, fixed = TRUE)
  Chisq <- unlist(lapply(psummary_eval, function(x) x$chisq))
  df <- unlist(lapply(psummary_eval, function(x) length(x$n)-1))
  pval <- unlist(lapply(psummary_eval, function(x) .pvalformat(stats::pchisq(x$chisq, length(x$n)-1, lower.tail = FALSE))))
  
  ## Output to tibble
  equality <- 
    data.frame(
      `Equality across strata` = Nm,
      Chisq = Chisq,
      df = df,
      `p-value` = pval,
      check.names = FALSE,
      stringsAsFactors = FALSE,
      row.names = NULL
    ) %>%
    as_tibble()
  
  return(equality)
} 