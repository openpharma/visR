#' @title Kaplan Meier survival (and failure?) probability for each unique time point 
#'
#' @description This function tidies a Kaplan-Meier Surv object using broom tidy function for downstream processing 
#'  where the survival probability, number at risk or number censored at each unique time point are required. \cr
#'
#' @author ??
#' @author Steven Haesendonckx {shaesen2@@its.jnj.com}
#' 
#' @param data ADaM Basic Data Structure (BDS) for Time-to-Event analysis. 
#' @param aval Character Analysis variable. Default is AVAL.
#' @param strata Character vector, representing the strata for Time-to-Event analysis eg TRT01P. When NULL, an overall analysis is performed.
#'   Default is NULL.       
#'
#' @return Tidy data frame (tibble) containing the KM summary table. 
#' @export
#'
#' @examples
#' library(survival)
#' library(glue)
#' library(dplyr)
#' library(tidyr)
#'  
#' ## No stratification
#' vr_KM_est(data = adtte)
#' 
#' ## Stratified Kaplan-Meier analysis by `TRTP`
#' vr_KM_est(data = adtte, strata = "TRTP")
#' 
#' ## Stratified Kaplan-Meier analysis by `TRTP` and `SEX` 
#' vr_KM_est(data = adtte, strata = c("TRTP", "SEX"))
#' 
#' ## Stratification with one level
#' vr_KM_est(data = adtte, strata = "PARAMCD")

vr_KM_est <- function(data = NULL
                     ,aval = "AVAL"
                     ,strata = NULL
                    )
{  
  ## Ensure to have data frame and remove missing aval, strata
  data <- as.data.frame(data)%>%
    tidyr::drop_na({{aval}}, CNSR)
  
  if (!is.null(strata)){
    data <- data%>%
      tidyr::drop_na(any_of({{strata}}))
  }

  ## Validate input
  reqcols <- c(aval, "CNSR", strata)

  if (!sum(reqcols %in% colnames(data)) == length(reqcols))  {
    stop(paste0("The following columns are missing from the dataset: ", paste(setdiff(reqcols, colnames(data)), collapse = " "), "."))
  }
  
  if (! is.character(aval) | ! is.numeric(data[[aval]])){
    stop("Analysis variable, aval, is not numeric.")
  }
  if (! is.numeric(data[["CNSR"]])){
    stop("Censor variable, CNSR, is not numeric.")
  }
  
  ## Reverse censoring: see ADaM guidelines versus R survival KM analysis
  data$status <- abs(1-data$CNSR)
  
  if (is.null(strata)){
    main <- "1"
  } else {
    main <- paste(strata, collapse = " + ")
  }
  
  ## Calculate survival and add starting point (time 0) to the survfit object.
  formula <- stats::as.formula(glue::glue("Surv({aval}, status) ~ {main}"))
  
  survfit_object <- survival::survfit(
   formula, data = data
  )
  
  survfit_object <- survfit0(
    survfit_object, start.time = 0
  )

  ## No strata: Create an artificial one for compatibility with downstream processing
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

  broom_object <- broom::tidy(survfit_object)%>%
    mutate( time = as.integer(time)
           ,n.risk = as.integer(n.risk)
           ,n.event = as.integer(n.event)
           ,n.censor = as.integer(n.censor)
           )

    # dplyr::rename(survival = estimate)%>%
    # dplyr::mutate(failure = 1-survival) #add CI. Would this simple be 1-conf of survival?
  
  return(broom_object)
}
