#' @title Summarize p-values from null hypothesis
#'
#' @description S3 method for extracting information regarding null hypothesis testing.
#'     
#' @author Steven Haesendonckx
#' 
#' @examples
#' library(visR)
#' library(survival)
#' library(survMisc)
#' 
#' survfit_object <- vr_KM_est(data = adtte, strata = "TRTP")
#' get_pvalue(survfit_object)
#' get_pvalue(survfit_object, method="Trend")
#'  
#' @return A data.frame with summary measures for the requested `method`
#'  
#' @rdname get_pvalue
#' 
#' @export

get_pvalue <- function(x, ...){
  UseMethod("get_pvalue")
}

#' @seealso \code{\link[survival]{survdiff}} \cr
#'          \code{\link[survMisc]{comp}}
#' 
#' @param survfit_object An object of class \code{survfit}
#' 
#' @param method Method for evaluating the difference between survival curves. "Equality" uses survival::\code{\link[survival]{survdiff}} and 
#'   tests the null hypothesis of no difference among strata. The method is based on the G-rho family of tests. The weight, defined by `rho`,
#'   can be user-defined.\cr\cr
#'   "Trend" uses survMisc::\code{\link[survMisc]{comp}} and tests the null hypothesis of a linear trend across strata using weighted
#'   "Log-Rank" tests.\cr\cr
#'    Default is "Equality".
#' 
#' @param ptype Character vector containing the type of test desired for calculation of the p-value.
#'   Current options are:
#'     \itemize{
#'       \item{
#'         `method` = "Equality" allows for "Log-Rank" (rho = 0), "Peto-Peto Gehan-Wilcoxon" (rho = 1), "Tarone-Ware" (rho = 1.5) and "Custom"
#'         (rho = user-defined).
#'       }
#'       \item{
#'         `method` = "Trend" allows for "Log-Rank" (weight = 1), "Gehan and Breslow" (weight = n), "Tharone-Ware" (weight = sqrt(N)),
#'          "Peto-Peto" (weight = S1), "Andersen" (weight = S2) and "Fleming-Harrington" (p=1 q=1 which can be changed by the user).\cr
#'          The scores for the linear trend can be defined via the `scores` argument.
#'       }
#'   }
#'   
#' @param statlist Character vector containing the desired information to be displayed. The order of the arguments determines the order in which
#'    they are displayed in the final result. Default is the test name ("test") and the p-value ("p-value"). In additional to the default,
#'    "Chisq" and "df" can be requested for `method` = "Equality". For `method` = "Trend", also "Weights" and "Scores" are available.
#'    
#' @param p numeric, used for Fleming-Harrington test in `method` = "Trend" 
#' @param q numeric, used for Fleming-Harrington test in `method` = "Trend" 
#' @param scores numeric vector, used in `method` = "Trend" 
#' @param rho numeric, used in `method` = "Equality" 
#' 
#' @rdname get_pvalue
#' @method get_pvalue survfit
#' @export

get_pvalue.survfit <- function(survfit_object,
                               method = c("Equality"),
                               ptype = NULL,
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
  
  if (length(names(survfit_object[["strata"]])) < 3 & method == "Trend")
    stop("Error in get_pvalue: Main effect has less than 3 levels. Test of Trend can't be performed.")
  
  if (!method %in% c("Equality", "Trend"))
    stop("Error in get_pvalue: Specify a valid method")
  
  if (length(method) > 1)
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

  if ("Trend" %in% method & length(survfit_object[["n"]]) > 2){
    
    ## Statlist
    dstatlist <- c("Test", "Scores", "Weights", "p-value")
    
    if (is.null(statlist)) statlist <- dstatlist
    if (is.null(ptype)) ptype <- wt_test

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
      final<- tft[which(tft[,"Test"] %in% ptype), statlist[which(statlist %in% colnames(tft))]]
      if ("p-value" %in% statlist) final[["p-value"]] <- sapply(final[["p-value"]], .pvalformat)
  }


# Summary List for Equality across strata  ------------------------------------------

  if ("Equality" %in% method & length(survfit_object[["n"]]) > 1){

    ## Statlist
      dstatlist <- c("Test", "df", "Chisq", "p-value")
      if (is.null(statlist)) statlist <- dstatlist
      
    ## Rho and ptype
      dptype <- NULL
      
      if (!is.null(rho)){
        dptype <- base::switch(
          as.character(rho),
          `0` = "Log-Rank",
          `1` = "Peto-Peto Gehan-Wilcoxon",
          `1.5` = "Tarone-Ware",
          "Custom"
        )
      }

      ptype <- unique(c(ptype, dptype))
      
      if (is.null(ptype)) ptype <- base::setdiff(rho_test, "Custom")

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