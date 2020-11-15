#' @title Summarize p-values from null hypothesis
#'
#' @description S3 method for extracting information regarding null hypothesis testing.
#'     No default method is available at the moment.
#'     
#' @author Steven Haesendonckx
#' 
#' @param x S3 object
#' @param ... other arguments passed on to the method
#' 
#' @examples
#' library(visR)
#' library(survival)
#' library(survMisc)
#' 
#' survfit_object <- vr_KM_est(data = adtte, strata = "TRTP")
#' get_pvalue(survfit_object)
#'  
#' @return A tibble with summary measures for the Test of Equality Across Strata
#'  
#' @rdname get_pvalue
#' 
#' @export


# T1. The function accepts only a survival object
# T1.1 An error when an object of a different type is used
# T1.2 The function accepts a survival object
# 
# T2. The function excecutes the test the null hypothesis of no equality across strata or a linear trend test across strata
# T2.1 An error when another method is requested eg method = "blah"
# T2.2 No error when method = "Equality"
# T2.3 No error when method = "Trend"
# 
# T3. The function tests the null hypothesis of no difference across strata when the requested ptype is: method = "Equality"
# T3.1 An error When the number of strata < 2
# T3.2 An error when `ptype` is out of scope
# T3.3 An error when `ptype` is NULL
# T3.4 An error when the requested `ptype` is "Custom" and `rho` is not specified
# T3.5 An error when the requested `statlist` is not part of the default
# T3.6 An error when the requested `statlist` is NULL
# 
# T4. The function tests the null hypothesis of a linear trend across strata when the requested ptype is: method = "Trend"
# T4.1 An error When the number of strata < 3
# T4.2 An error when `ptype` is out of scope
# T4.3 An error when `ptype` is NULL
# T4.4 An error when the number of strata in the survfit_object does not match the length of the scores vector
# T4.5 An error when the requested `statlist` is not part of the default
# T4.6 An error when the requested `statlist` is NULL


get_pvalue <- function(x, ...){
  UseMethod("get_pvalue")
}

#' @description S3 method for extracting information regarding null hypothesis testing for differences among groups.
#'     No default method is available at the moment.
#'     
#' @seealso \code{\link[survival]{survdiff}} \code{\link[survMisc]{comp}}
#' 
#' @param survfit_object An object of class \code{survfit}
#' 
#' @param method Method for evaluating the difference between survival curves. "Equality" uses \code{\link[survival]{survdiff}} and 
#'   tests the null hypothesis of no difference among strata.\cr
#'   "Trend" uses \code{\link[survMisc]{comp}} and tests the null hypothesis of a linear trend across strata.\cr
#'    Default is "Equality".
#' 
#' @param ptype Character vector containing the type of test desired for calculation of the p-value.
#'   Current options are:
#'     \itemize{
#'       \item{
#'         `method` = "Equality" allows for "Log-Rank" (rho = 0) "Peto-Peto Gehan-Wilcoxon" (rho = 1) "Tarone-Ware" (rho = 1.5) "Custom"
#'         (rho = user defined).
#'       }
#'       \item{
#'         `method` = "Trend" allows for "Log-Rank" (weight = 1) "Gehan and Breslow" (weight = n) "Tharone-Ware" (weight = sqrt(N))
#'          "Peto-Peto" (weight = S1) "Andersen" (weight = S2) "Fleming-Harrington" (p=1 q=1 which can be changed by the user).
#'       }
#'   }
#'   
#' @param statlist Character vector containing the desired information to be displayed. The order of the arguments determines the order in which
#'    they are displayed in the final result. Default is the test name ("test") and the p-value ("p-value"). In additional to the default,
#'    "Chisq" and "df" can be requested for `method` = "Equality". For `method` = "Trend", also "Weights" and "Scores" are available.
#'    
#'     
#' @inheritParams survival::survdiff
#' @inheritParams survMisc::comp
#' 
#' @rdname get_pvalue
#' @method get_pvalue survfit
#' @export

get_pvalue.survfit <- function(survfit_object,
                               method = c("Equality"),
                               ptype = c("Log-Rank", "Wilcoxon"),
                               statlist = c("Test", "p-value"),
                               p = 1,
                               q = 1,
                               scores = NULL,
                               rho = NULL
                              ) {


# User Input Validation ---------------------------------------------------

  wt_test <- c("Log-Rank", "Gehan and Breslow", "Tharone-Ware", "Peto-Peto", "Andersen", "Fleming-Harrington")
  rho_test <- c("Log-Rank", "Peto-Peto Gehan-Wilcoxon", "Tarone-Ware", "Custom")

  if (!inherits(survfit_object, "survfit"))
    stop("Error in get_pvalue: x is not of class `survfit`.")
  
  if (length(names(survfit_object[["strata"]])) == 1)
    stop("Error in get_pvalue: Main effect has only 1 level. A difference in survival requires > 1 strata.")
  
  if (!is.null(scores) & length(names(survfit_object[["strata"]])) != length(scores))
    stop("Error in get_pvalue: Test of Trend requires equal length of strata and scores.")
  
  if (length(names(survfit_object[["strata"]])) < 3 & !is.null(scores))
    stop("Error in get_pvalue: Main effect has less than 3 levels. Test of Trend can't be performed.")
  
  if (is.null(ptype))
    stop("Error in get_pvalue: Specify a valid ptype.")
  
  if (is.null(method) | !base::any(method) %in% c("Equality", "Trend"))
    stop("Error in get_pvalue: Specify a valid method")
  
  if (!is.null(method) | length(method) > 1)
    stop("Error in get_pvalue: Only one method can be requested.")
  
# Custom function for pvalue formatting -----------------------------------
  
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

# Statlist ----------------------------------------------------------------

  statlist <- unique(statlist)

# Summary List for Trend --------------------------------------------------

  if ("Trend" %in% method & length(survfit_object)[["n"]] > 2){
    
    ## Statlist
    dstatlist <- c("Test", "Scores", "Weights", "p-value")
    
    if (is.null(statlist)) statlist <- dstatlist

    ## Additional user input validation
    if (!base::any(c(wt_test) %in% ptype))
      stop("Error in get_pvalue: Specify a valid ptype")
  
    if (!base::all(statlist %in% dstatlist))
      stop("Error in get_pvalue: Specify valid `statlist` arguments.")

    ## Reset scores
      if (is.null(scores)) scores = base::rep(1, length(survfit_object[["n"]]))
      Scores <- rep(paste(scores, collapse = ", "), 6)
      
    ## Capture objects and wrangle into dataframe
      tenfit <- capture.output(
        comp(
          ten(
               survfit_object
               ,p=p
               ,q=q
               ,scores=scores
             )
          )
      )
  
      splitters <- tenfit[grepl("\\$", tenfit)]
      tenfit <- tenfit[which(grepl("\\$tft", tenfit)):which(grepl("\\$scores", tenfit))]
      tenfit <- tenfit[-which(grepl("\\$", tenfit))]
      tenfit <- tenfit[which(!grepl("Signif", tenfit))]
      tenfit <- tenfit[tenfit != ""]
      tenfit <- tenfit[which(!grepl("---", tenfit))]
      tenfitlist <- strsplit(tenfit, "\\s+")
      minlength <- min(unlist(lapply(tenfitlist, length)))
      tenfitlist <- lapply(tenfitlist, function(x) x[1:minlength])
      tenfitlist[[1]] <- NULL

      tft <- as.data.frame(matrix(unlist(tenfitlist), nrow=length(tenfitlist), byrow=T), stringsAsFactors = FALSE)
      tft <- cbind(wt_test, Scores, tft)

      colnames(tft) <- c("Test", "Scores", "Weights", "Q", "var", "z", "p-value")

    ## Final Selection and formatting
      final_trend <- tft[which(tft[,"Test"] %in% ptype), statlist[which(stalist %in% colnames(tft))]]
      if ("p-value" %in% statlist) final_trend[["p-value"]] <- .pvalformat(final_trend[["p-value"]])
  }



# Summary List for Equality across strata  ------------------------------------------

  if ("Equality" %in% method & length(survfit_object)[["n"]] > 1){

    ## Statlist
      dstatlist <- c("Test", "df", "Chisq", "p-value")
    
      if (is.null(statlist)) statlist <- dstatlist

    ## Additional user input validation
      if (!base::any(c(rho_test) %in% ptype))
        stop("Error in get_pvalue: Specify a valid ptype")
  
      if (!base::all(statlist %in% dstatlist))
        stop("Error in get_pvalue: Specify valid `statlist` arguments.")
    
      if ("Custom" %in% ptype & is.null(rho))
        stop("Error in get_pvalue: ptype = `Custom`. Please, specify `rho`.")
    
    ## Re-use Call from survival object
      Call <- as.list(survfit_object$call)
      NewCall <- append(as.list(as.symbol("survdiff")), Call[names(Call) %in% names(formals(survival::survdiff))])
  
      if (!is.null(rho)) {
        ptype = c(ptype, "Custom")
      }
  
  ## Summary list
  
    survdifflist <- list(
      `Log-Rank`    = rlang::expr(eval(as.call(
        append(NewCall, list(rho = 0))
      ))),
      `Peto-Peto Gehan-Wilcoxon`    = rlang::expr(eval(as.call(
        append(NewCall, list(rho = 1))
      ))),
      `Tarone-Ware` = rlang::expr(eval(as.call(
      append(NewCall, list(rho = 1.5))
      ))),
      `Custom`      = rlang::expr(eval(as.call(
        append(NewCall, list(rho = rho))
      )))
    )[ptype]
  
  
    survdifflist_eval <- lapply(survdifflist, eval, env = environment())
  
  ## statlist
    Nms <- names(survdifflist_eval)
  
    stat_summary <- list(
      `Test` = rlang::expr(base::sub(
        "Custom",
        paste0("Harrington and Fleming test (rho = ", rho, ")"),
        Nms,
        fixed = TRUE
      )),
      `Chisq`    = rlang::expr(unlist(
        lapply(survdifflist_eval, function(x) x$chisq)
      )),
      df        = rlang::expr(unlist(
        lapply(survdifflist_eval, function(x) length(x$n) - 1)
      )),
      `p-value` = rlang::expr(unlist(
        lapply(survdifflist_eval, function(x)
          .pvalformat(
            stats::pchisq(x$chisq, length(x$n) - 1, lower.tail = FALSE)
          ))
      ))
    )[statlist]
  
  
  ## output to tibble
    final <- data.frame(
      lapply(stat_summary, eval, env = environment()),
      check.names = FALSE,
      stringsAsFactors = FALSE,
      row.names = NULL
    ) 

  }
  
# Formatted Results -----------------------------------------------------------------
  
  return(final)
} 