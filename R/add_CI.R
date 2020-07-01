add_CI <- function(gg, ...){
  UseMethod("add_CI")
} 

add_CI.ggKMsurv <- function(gg, ...){

  if (! base::all(c("est.lower", "est.upper")  %in% colnames(gg$data))) {
    warning("Confidence limits were not part of original estimation.")
    return(NULL)
  }
  
  gg <- gg +
    geom_ribbon(aes(ymin = est.lower, ymax = est.upper, fill = strata), alpha = 0.1) 
    
  return(gg)
}

add_CI.tblKM <- function(gg){
  stop("this object is not yet part of the scope")
}