
tidyme <- function(x, ...){
  UseMethod("tidyme")
}  


tidyme.survfit <- function(x, ...) {
  if (inherits(x, "survfit")) {
    
    ## Change class to perform list manipulations. The survfit class was throwing errors.
    class(x) <-  ("list")
    
    ## Prepare for cleaning
    reps <- as.vector(length(x$time))
    callme <- x$call
  
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
      mutate( time = as.integer(time)
             ,n.risk = as.integer(n.risk)
             ,n.event = as.integer(n.event)
             ,n.censor = as.integer(n.censor)
             ,call = rep(base::paste(x$call, collapse = " "), reps) 
            )

    if (!is.null(x$strata)) {
      retme[["strata"]] <- rep(names(x$strata), x$strata)
      retme$n.strata <- rep(x$n, x$strata)
    } 
  }
  
  return(as_tibble(retme))
}
