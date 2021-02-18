#' @title Wrapper for Kaplan Meier analysis for an ADaM Basic Data Structure (BDS) for Time-to-Event analysis
#'  
#' @description This function is a wrapper around \code{survival::survfit.formula} to perform a Kaplan-Meier analysis,
#'    based on the expected ADaM Basic Data Structure (BDS) for Time-to-Event analysis and assuming right-censored data.
#'    The function expects that the data has been filtered on the PARAM/PARAMCD of interest.
#'    Alternatively, PARAM/PARAMCD can be used in the \code{strata} argument. \cr
#'    The result is an object of class \code{survfit} which can be used in downstream functions and methods that rely on the \code{survfit} class.
#'    By default:
#'    \itemize{
#'      \item{The Kaplan Meier estimate is estimated directly (stype = 1).}
#'      \item{The cumulative hazard is estimated using the Nelson-Aalen estimator (ctype = 1): H.tilde = cumsum(x$n.event/x$n.risk).
#'      The MLE (H.hat(t) = -log(S.hat(t))) can't be requested.}
#'      \item{A two-sided pointwise 0.95 confidence interval is estimated using a log transformation (conf.type = "log").}
#'    }
#'
#' @author Steven Haesendonckx
#' 
#' @seealso \code{\link[survival]{survfit.formula} \link[survival]{survfitCI}}
#' 
#' @param data The name of the ADaM Basic Data Structure (BDS) for Time-to-Event analysis eg ADTTE. Rows in which AVAL or CNSR contain NA, are removed during analysis. 
#' @param strata Character vector, representing the strata for Time-to-Event analysis eg TRT01P. When NULL, an overall analysis is performed.
#'   Default is NULL.
#' @param ... additional arguments passed on to the ellipsis of the call \code{survival::survfit.formula(data = data, formula = Surv(AVAL, 1-CNSR) ~ strata), ...)} .
#' Use \code{?survival::survfit.formula} and \code{?survival::survfitCI} for more information.    
#'
#' @return survfit object, extended by elements PARAM/PARAMCD, ready for downstream processing in estimation or visualization functions and methods.
#' 
#' @references \url{https://https://github.com/therneau/survival}
#' 
#' @export
#'
#' @examples
#' library(survival)
#' library(dplyr)
#' library(tidyr)
#' 
#' ## No stratification
#' estimate_KM(data = adtte)
#' 
#' ## Stratified Kaplan-Meier analysis by `TRTP`
#' estimate_KM(data = adtte, strata = "TRTP")
#' 
#' ## Stratified Kaplan-Meier analysis by `TRTP` and `SEX` 
#' estimate_KM(data = adtte, strata = c("TRTP", "SEX"))
#' 
#' ## Stratification with one level
#' estimate_KM(data = adtte, strata = "PARAMCD")
#' 
#' ## Analysis on subset of adtte
#' estimate_KM(data = adtte[adtte$SEX == "F", ])
#' 
#' ## Modify the default analysis by using the ellipsis
#' estimate_KM(data = adtte, strata = NULL, type = "kaplan-meier", conf.int = F, timefix = TRUE)
#' estimate_KM(data = adtte, strata = NULL, type = "kaplan-meier", conf.int = FALSE, timefix = TRUE)

estimate_KM <- function(
   data = NULL
  ,strata = NULL
  ,...
){

# Capture input to validate user input for data argument -----------------
  
 ## Get actual data name as symbol
 ### Magrittre pipe returns "." which inactivates recalls to survfit in downstream functions
 ### map passes .x as Call$data
 ### df: catch expressions that represent base R subsets
  
  Call <- as.list(match.call())
  dots <- list(...)
  dfExpr <- Call[["data"]]
  
 ## Validate `data` and capture data name
 
  if (is.null(data)) stop(paste0("Data can't be NULL."))   

  if (base::length(base::deparse(Call[["data"]])) == 1 && base::deparse(Call[["data"]]) %in% c(".", ".x")){
    df <- the_lhs()
    Call[["data"]] <- as.symbol(df)
  } else {
     df <- as.character(sub("\\[.*$", "", deparse(dfExpr))[1])
  } 

  if (!(inherits(data, "data.frame") | inherits(data, "tibble") | inherits(data, "data.table"))) stop(paste0("Data can be of `class` dataframe or tibble or data.table."))   

  data <- as.data.frame(data)

# Validate columns --------------------------------------------------------

  reqcols <- c(strata, "CNSR", "AVAL")
  
  if (! base::exists(df)){
    stop(paste0("Data ", df, " not found."))
  }
  
  if (! all(reqcols %in% colnames(data))){
    stop(paste0("Following columns are missing from `data`: ", paste(setdiff(reqcols, colnames(data)), collapse = " "), "."))
  }
  
  if (! is.numeric(data[["AVAL"]])){
    stop("Analysis variable, AVAL, is not numeric.")
  }
  
  if (! is.numeric(data[["CNSR"]])){
    stop("Censor variable, CNSR, is not numeric.")
  }
  
# Ensure the presence of at least one strata -----------------------------
  
  if (is.null(strata)){
    main <- "1"
  } else {
    main <- paste(strata, collapse = " + ")
  }
  
# Remove NA from the analysis --------------------------------------------
  
  data <- as.data.frame(data)%>%
    tidyr::drop_na(AVAL, CNSR)
  
  if (!is.null(strata)){
    data <- data%>%
      tidyr::drop_na(any_of({{strata}}))
  }
  
# Calculate survival and add time = 0 to survfit object -------------------
  
 ## Reverse censoring: see ADaM guidelines versus R survival KM analysis
  
  formula <- stats::as.formula(glue::glue("survival::Surv(AVAL, 1-CNSR) ~ {main}"))
  
  survfit_object <- survival::survfit(
    formula, data = data, ...
  )

  survfit_object <- survival::survfit0(
    survfit_object, start.time = 0
  )
  

# Update Call with original info and dots, similar as update.default ------

  survfit_object$call[["formula"]] <- formula
  survfit_object$call[["data"]] <- Call$data
  if (length(dots) > 0){
    names(survfit_object$call)
    names(dots)
    for (i in seq_along(dots)){
      survfit_object$call[[names(dots)[i]]] <- unlist(dots[i], use.names = F)
    }
  }
  
# Add additional metadata -------------------------------------------------
  
  if ("PARAM" %in% colnames(data) && length(setdiff(c("PARAMCD", "PARAM"), strata)) == 2){
    # we expect only one unique value => catch mistakes
    survfit_object[["PARAM"]] <- paste(unique(data[["PARAM"]]), collapse = ", ")
  }
    
  if ("PARAMCD" %in% colnames(data) && length(setdiff(c("PARAMCD", "PARAM"), strata)) == 2){
    # we expect only one unique value => catch mistakes
    survfit_object[["PARAMCD"]] <- paste(unique(data[["PARAMCD"]]), collapse = ", ")
  } 
  
# Artificial strata for easy downstream processing when strata=NULL ------
  
  if (is.null(survfit_object$strata)){
    survfit_object$strata <- as.vector(length(survfit_object$time))
    
    if (main == "1"){
      # overall analysis
      attr(survfit_object$strata, "names") <- "Overall"
    } else {
      # ~ x with One level in variable present
      attr(survfit_object$strata, "names") <- as.character(data[1, main])
    }
  }

# Return ------------------------------------------------------------------

  return(survfit_object)
}