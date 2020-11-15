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


# T1. Calculate rho-family requested ptypes
# T1.1 Logranks: specify ptype
# T1.2 Wilcoxon: specify ptype
# T1.3 Rho: 
# T2. Calculate weighted log rank
# 
# T3. Calculate combinations of 
# T3.1 Recognize valid ptypes
# T3.2 Combinations but also independent



get_pvalue <- function(x, ...){
  UseMethod("get_pvalue")
}

#' @param survfit_object An object of class \code{survfit}
#' 
#' @param ptype Character vector containing the type of test desired for calculation of the p-value. The difference in survival curves is calculated
#'   based on weighted Log-Rank tests using \code{\link[survMisc]{comp}}. Options are:
#'     "Log-Rank" "Gehan and Breslow" "Tharone-Ware" "Peto-Peto" "Andersen" "Fleming-Harrington".
#'      The "Fleming-Harrington" method can be adjusted by specifying p and q.  \cr
#'      The default is Log-Rank. A trend test can be done by including scores for the different strata.
#'      
#' @param statlist Character vector containing the desired information to be displayed. The order of the arguments determines the order in which
#'    they are displayed in the final result. Default is the test name ("test"), Chisquare test statistic ("Chisq"), degrees of freedom ("df") and
#'    p-value ("p").
#'    
#' @inheritParams survMisc::comp
#' 
#' @rdname get_pvalue
#' @method get_pvalue survfit
#' @export

get_pvalue.survfit <- function(survfit_object,
                               ptype = c("Log-Rank"),
                               statlist = c("Test", "Scores", "Weights", "p-value"),
                               p = 1,
                               q = 1,
                               scores = NULL,
                               ...) {
  

# Input Validation --------------------------------------------------------
  
  wt_test <- c("Log-Rank", "Gehan and Breslow", "Tharone-Ware", "Peto-Peto", "Andersen", "Fleming-Harrington")

  if (!inherits(survfit_object, "survfit"))
    stop("Error in get_pvalue: x is not of class `survfit`.")
  
  if (length(names(survfit_object[["strata"]])) == 1)
    stop("Error in get_pvalue: Main effect has only 1 level. Test of equality over strata can't be determined")
  
  if (!is.null(scores) & length(names(survfit_object[["strata"]])) != length(scores))
    stop("Error in get_pvalue: Test of Trend requires equal length of strata and scores.")
  
  if (length(names(survfit_object[["strata"]])) < 3 & !is.null(scores))
    stop("Error in get_pvalue: Main effect has less than 3 levels. Test of Trend can't be performed.")
  
  if (is.null(ptype))
    stop("Error in get_pvalue: Specify a valid ptype.")
  
  if (!base::any(c(wt_test) %in% ptype))
    stop("Error in get_pvalue: Specify a valid ptype")
  
  if (is.null(statlist) | !base::all(statlist %in% c("Test", "Scores", "Weights", "p-value")))
    stop("Error in get_pvalue: Specify valid `statlist` arguments.")
  

# Reset scores to default if NULL -----------------------------------------

  if (is.null(scores)) scores = base::rep(1, length(survfit_object[["n"]]))
  
  Scores <- rep(paste(scores, collapse = ", "), 6)

# Summary List ------------------------------------------------------------
  
  tenfit <- capture.output(comp(ten(survfit_object)))
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


# Statlist ----------------------------------------------------------------

  statlist <- unique(statlist)

# Selection ---------------------------------------------------------------

  final <- tft[which(tft[,"Test"] %in% ptype), statlist]
  
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
  
  
# Formatted Results -----------------------------------------------------------------
  
  if ("p-value" %in% statlist) final[["p-value"]] <- .pvalformat(final[["p-value"]])

  return(final)
} 