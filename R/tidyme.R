#' @title Extended tidy cleaning of selected objects using S3 method
#'
#' @description S3 method for extended tidying of selected model outputs.
#'     The default method relies on \code{broom::tidy} to return a tidied object
#'     
#' @author Steven Haesendonckx
#' 
#' @seealso \code{\link[broom]{tidy}}
#' 
#' @param x S3 object
#' @param ... other arguments passed on to the method
#' 
#' @examples
#' \donttest{
#' ## Extended tidying for a survfit object
#' ## TODO: Check the error in running the example
#' #library(survial)
#' #surv_object <- survival::survfit(data = adtte, Surv(AVAL, 1-CNSR) ~ TRTP)
#' #tidied <- tidyme(surv_object)
#' 
#' ## Tidyme for non-included classes
#' #data <- cars
#' #lm_object <- lm(data = cars, speed ~ dist)
#' #lm_tidied <- tidyme(lm_object)
#' }
#' 
#' @return Tibble containing all list elements of the S3 object as columns
#' 
#' @rdname tidyme
#' 
#' @export

tidyme <- function(x, ...){
  UseMethod("tidyme")
} 

#' @rdname tidyme
#' @method tidyme default
#' @export

tidyme.default <- function(x, ...){
  #library(broom)
  base::writeLines("tidyme S3 default method (broom::tidy) used.")
  return(broom::tidy(x))
}
 
#' @rdname tidyme
#' @method tidyme survfit
#' @export

tidyme.survfit <- function(x, ...) {
  if (inherits(x, "survfit")) {
    
    ## Change class to perform list manipulations. The survfit class was throwing errors.
    class(x) <-  ("list")
    
    ## Prepare for cleaning
    reps <- as.vector(length(x$time))

    ## Lists to vectors
    cleaner <- function (x) {
      if (length(x) == 1){
        rep(x,reps)
      } else {
        x
      }
    }
    
    ## Cleanit: strata will always be filled out based off the estimation function from which it is called
    retme <- dplyr::bind_rows(base::lapply(x[names(x) %in% c("n", "strata", "call") == FALSE], cleaner))%>%
      dplyr::mutate( time = as.integer(time)
             ,n.risk = as.integer(n.risk)
             ,n.event = as.integer(n.event)
             ,n.censor = as.integer(n.censor)
             ,call = list(x$call) 
            )

    if (!is.null(x$strata)) {
      retme[["strata"]] <- rep(names(x$strata), x$strata)
      retme$n.strata <- rep(x$n, x$strata)
    } 
  }
  
  
  return(as_tibble(retme))
}