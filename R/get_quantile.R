#' @title Extended quantile S3 method - EXPERIMENTAL NEEDS TESTING AND COMPARING WITH SAS SOFTWARE
#'
#' @description S3 method for extracting quantiles. For survival objects, it takes into account the maximal time of survival as limit instead of NA
#'   If the quantile requested is larger than the minimal quantile available, the result is NA. 
#'   No default method is available at the moment.
#'     
#' @author Steven Haesendonckx
#' 
#' @seealso \code{\link[survival]{quantile.survfit}}
#' 
#' @param x S3 object
#' @param ... other arguments passed on to the method
#' 
#' @examples
#' library(survival)
#' library(dplyr)
#' library(tidyr)
#' 
#' survfit_object_trt <- vr_KM_est(data = adtte, strata = c("TRTP"))
#' get_quantile(survfit_object)
#' quantile(survfit_object)
#'
#' @return A tibble with quantiles
#'  
#' @rdname get_quantile
#' 
#' @export

get_quantile <- function(x, ...){
  UseMethod("get_quantile")
} 

#' @param survfit_object An object of class \code{survfit}
#' @inheritParams survival::quantile.survfit
#'
#' @rdname get_quantile
#' @method get_quantile survfit
#' @export

get_quantile.survfit <- function(  survfit_object = survfit_object
                                 , probs = c(0.25,0.50,0.75)
                                 , conf.int = TRUE
                                 , tolerance = sqrt(.Machine$double.eps)
                                 , debug = FALSE
                                )
{

  if (debug == TRUE) browser()
   
  if(! inherits(survfit_object, "survfit")) stop("x is not of class `survfit`.")
  if(conf.int == TRUE & ! base::all(c("lower", "upper") %in% names(survfit_object))) stop("Confidence limits were not part of original estimation.")
  
  q <- quantile(survfit_object, probs = probs, conf.int = conf.int, tolerance = tolerance, type = 3)
  qdf <- do.call(rbind.data.frame, q)

  qdf$strata <- as.character(unlist(lapply(q, rownames)))
  qdf$quantity <- unlist(lapply(strsplit(rownames(qdf), "\\.", fixed=FALSE), `[[`, 1))
  
  qdf <- reshape2::melt(qdf, id.vars = c("strata", "quantity"), value.name = "value")

  ## focus on fun = surv, but we could do this for more funs
  ## get maximal 1-S(t) and max time
  
  cutoff <- tidyme.survfit(survfit_object) %>%
     dplyr::rename(quantile = surv) %>%
     dplyr::arrange(strata, time) %>%
     dplyr::select(strata, time, quantile, lower, upper) %>%
     reshape2::melt(id.vars = c("strata", "time"), value.name = "value") %>%
     dplyr::group_by(strata, variable) %>%
     dplyr::filter(!is.na(value)) %>%
     dplyr::filter(value == min(value)) %>%
     summarize(tmax = max(time), tmin = min(time),  vmin = min(value), qmax = 1-(min(value))) %>%
     dplyr::ungroup() %>%
     dplyr::mutate(quantity = as.character(variable),
                   variable = NULL,
                   strata = unlist(lapply(strsplit(strata, "\\.", fixed=FALSE), function(x) {x[[length(x)]]}))
                  )
  
  
  ## merge info
  correct <- qdf %>%
     dplyr::left_join(cutoff, by = base::intersect(colnames(cutoff), colnames(.))) %>%
     dplyr::mutate(
        variable = as.numeric(as.character(variable)),
        diff = abs(variable - qmax*100),
        value = ifelse(  as.numeric(variable) > 100*qmax | value > tmax | (abs(as.numeric(variable)- 100*qmax) <= tolerance & tmin != tmax)
                                  , NA_real_
                                  , value
                                 )
                   ) %>%
     dplyr::arrange(strata, quantity, variable) %>%
     dplyr::select(-diff, -qmax, -vmin, -tmin, -tmax)%>%
     tidyr::pivot_wider(id_cols = c(strata, quantity), names_from = variable, values_from = value)

  return(correct)
}
