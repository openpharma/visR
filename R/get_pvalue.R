#' @title Summarize the test for equality across strata from a survival object using S3 method
#'
#' @description S3 method for extracting information regarding equality across strata.
#'     No default method is available at the moment.
#'     
#' @author Steven Haesendonckx
#' 
#' @seealso \code{\link[survival]{survdiff}}
#' 
#' @param x S3 object
#' @param ... other arguments passed on to the method
#' 
#' @examples
#' library(survival)
#' library(dplyr)
#' library(tidyr)
#' library(ggplot2)
#' 
#' survfit_object <- estimate_KM(data = adtte, strata = "TRTP")
#' get_pvalue(survfit_object)
#'  
#' @return A tibble with summary measures for the Test of Equality Across Strata
#'  
#' @rdname get_pvalue
#' 
#' @export

get_pvalue <- function(x, ...){
  UseMethod("get_pvalue")
}

#' @param survfit_object An object of class \code{survfit}
#' @param ptype Character vector containing the type of p-value desired. Current options are "Log-Rank" "Wilcoxon" "Tarone-Ware" "Custom" "All".
#'    "Custom" allows the user to specify the weights on the Kaplan-Meier estimates using the argument `rho`.
#'    The default is "All" displaying all types possible. When `rho` is specified in context of "All", also a custom p-value is displayed. 
#' @param statlist Character vector containing the desired information to be displayed. The order of the arguments determines the order in which
#'    they are displayed in the final result. Default is the test name ("test"), Chisquare test statistic ("Chisq"), degrees of freedom ("df") and
#'    p-value ("p").
#' @inheritParams survival::survdiff
#' 
#'
#' @rdname get_pvalue
#' @method get_pvalue survfit
#' @export

get_pvalue.survfit <- function(survfit_object,
                               ptype = "All",
                               rho   = NULL,
                               statlist = c("test", "Chisq", "df", "p"),
                               ...) {
  ## Input Validation
  
  if (!inherits(survfit_object, "survfit"))
    stop("Error in get_pvalue: x is not of class `survfit`.")
  if (length(names(survfit_object[["strata"]])) == 1)
    stop(
      "Error in get_pvalue: Main effect has only 1 level. Test of equality over strata can't be determined"
    )
  if (is.null(ptype))
    stop("Error in get_pvalue: Specify a valid ptype.")
  if (!base::any(c("Log-Rank", "Wilcoxon", "Tarone-Ware", "Custom", "All") %in% ptype))
    stop("Error in get_pvalue: Specify a valid type")
  if ("Custom" %in% ptype & is.null(rho))
    stop("Error in get_pvalue: ptype = `Custom`. Please, specify rho.")
  if (is.null(statlist) |
      !base::all(statlist %in% c("test", "df", "Chisq", "p")))
    stop("Error in get_pvalue: Specify valid `statlist` arguments.")
  
  ## Re-use Call from survival object
  
  Call <- as.list(survfit_object$call)
  NewCall <-
    append(as.list(as.symbol("survdiff")), Call[names(Call) %in% names(formals(survival::survdiff))])
  
  if ("All" %in% ptype) {
    ptype = c("Log-Rank", "Wilcoxon", "Tarone-Ware")
    if (!is.null(rho)) {
      ptype = c(ptype, "Custom")
    }
  }
  
  ## Summary list
  
  survdifflist <- list(
    `Log-Rank`    = rlang::expr(eval(as.call(
      append(NewCall, list(rho = 0))
    ))),
    `Wilcoxon`    = rlang::expr(eval(as.call(
      append(NewCall, list(rho = 1))
    ))),
    `Tarone-Ware` = rlang::expr(eval(as.call(
      append(NewCall, list(rho = 1.5))
    ))),
    `Custom`      = rlang::expr(eval(as.call(
      append(NewCall, list(rho = rho))
    )))
  )[ptype]
  
  
  .pvalformat <- function(x) {
    options(scipen = 999)
    if (x < 0.001)
      "<0.001"
    else if (x > 0.999)
      ">0.999"
    else
      format(round(x, 3),
             digits = 3,
             justify = "right",
             width = 6)
  }
  
  survdifflist_eval <-
    lapply(survdifflist, eval, env = environment())
  
  ## statlist
  
  statlist <- unique(statlist)
  statlist <-
    base::sub("test", "Equality across strata", statlist, fixed = TRUE)
  statlist <- base::sub("p", "p-value", statlist, fixed = TRUE)
  Nms <- names(survdifflist_eval)
  
  stat_summary <- list(
    `Equality across strata` = rlang::expr(base::sub(
      "Custom",
      paste0("Harrington and Fleming test (rho = ", rho, ")"),
      Nms,
      fixed = TRUE
    )),
    `Chisq`    = rlang::expr(unlist(
      lapply(survdifflist_eval, function(x)
        x$chisq)
    )),
    df        = rlang::expr(unlist(
      lapply(survdifflist_eval, function(x)
        length(x$n) - 1)
    )),
    `p-value` = rlang::expr(unlist(
      lapply(survdifflist_eval, function(x)
        .pvalformat(
          stats::pchisq(x$chisq, length(x$n) - 1, lower.tail = FALSE)
        ))
    ))
  )[statlist]
  
  
  ## output to tibble
  
  equality <- data.frame(
    lapply(stat_summary, eval, env = environment()),
    check.names = FALSE,
    stringsAsFactors = FALSE,
    row.names = NULL
  ) %>%
    tibble::as_tibble()
  
  return(equality)
} 
