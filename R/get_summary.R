#' @title Summarize the descriptive statistics across strata from a survival object using S3 method
#'
#' @description S3 method for extracting descriptive statistics across strata.
#'     No default method is available at the moment.
#'     
#' @author Pelagia Alexandra Papadopoulou 
#' 
#' @param x S3 object
#' @param ... other arguments passed on to the method
#' 
#' @examples
#' library(survival) 
#' library(dplyr)
#' library(tidyr)
#' 
#' survfit_object <- survival::survfit(data = adtte, Surv(AVAL, 1-CNSR) ~ TRTP)
#' get_summary(survfit_object)
#'  
#' @return A tibble with summary measures for the Overall Survival Across Strata
#'  
#' @rdname get_summary
#' 
#' @export

get_summary <- function(x, ...){
  UseMethod("get_summary")
}

#' @param survfit_object An object of class `survfit`
#' @param statlist Character vector containing the desired information to be displayed. The order of the arguments determines the order in which
#'    they are displayed in the final result. Default is the strata ("strata"), number of subjects ("records"), number of events ("events"),
#'    the median survival time ("median"), the Confidence Interval ("CI"), the Lower Confidence Limit ("UCL") and the Upper Confidence Limit ("UCL").
#' 
#' @rdname get_summary
#' @method get_summary survfit
#' @export

get_summary.survfit <- function(survfit_object,
                                statlist = c("strata", "records", "events", "median", "LCL", "UCL", "CI"),
                                ...) {
  #### statlist ####
  
  statlist <- unique(statlist)
  
  #### Input Validation ####
  
  if (!inherits(survfit_object, "survfit"))
    stop("Error in get_summary: x is not of class `survfit`.")
  if (is.null(statlist) |
      !base::all(statlist %in% c("strata", "records", "events", "median", "LCL", "UCL", "CI")))
    stop("Error in get_summary: Specify valid `statlist` arguments.")
  
  #### Adjust or Remove UCL/LCL in statlist based on conf.int was estimated in survfit object ####
  
  if ("conf.int" %in% names(survfit_object) &
      survfit_object[["conf.type"]] != "none" &
      ((base::any(grepl(
        "CL", statlist, fixed = TRUE
      ))) | (base::any(grepl(
        "CI", statlist, fixed = TRUE
      ))))) {
    CI <-
      paste0(survfit_object[["conf.int"]], statlist[grepl("CL", statlist, fixed = TRUE)])
    statlist[grepl("CL", statlist, fixed = TRUE)] <- CI
    CI_Varname <-
      paste0(survfit_object[["conf.int"]], statlist[grepl("CI", statlist, fixed = TRUE)])
    statlist[grepl("CI", statlist, fixed = TRUE)] <- CI_Varname
  } else if (!"conf.int" %in% names(survfit_object) |
             survfit_object[["conf.type"]] == "none") {
    statlist <- statlist[-which(grepl("CL", statlist, fixed = TRUE))]
    statlist <-
      statlist[-which(grepl("CI", statlist, fixed = TRUE))]
    warning("get_summary_fit: No conf.int estimated in survfit_object.")
  }
  
  #### Summary list: define strata and CI string to make next steps easier to read ####
  
  if ("strata" %in% names(survfit_object)) {
    strata <- names(survfit_object[["strata"]])
  } else
    strata <- "Overall"
  
  .CIpaste <- function(df) {
    if (base::any(grepl("CI", statlist, fixed = TRUE))) {
      paste0("(", apply(df[, CI, drop = FALSE] , 1 , paste , collapse = ";"), ")")
    } else {
      NULL
    }
  }
  
  summary_survfit <-
    as.data.frame(
      base::rbind(summary(survfit_object)[["table"]]),
      check.names = F,
      stringsAsFactors = F,
      row.names = NULL
    ) %>%
    dplyr::mutate(strata = strata) %>%
    dplyr::mutate(!!CI_Varname := .CIpaste(.)) %>%
    dplyr::select(tidyselect::all_of(statlist))
  
  #### Display ####
  statlist <-
    base::sub("records", "No. of subjects", statlist, fixed = TRUE)
  statlist <-
    base::sub("events", "No. of events", statlist, fixed = TRUE)
  statlist <-
    base::sub("median", "Median(surv.time)", statlist, fixed = TRUE)
  
  colnames(summary_survfit) <- statlist
  
  ## output to tibble
  
  return(summary_survfit)
}