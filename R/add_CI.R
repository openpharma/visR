add_CI <- function(gg, ...){
  UseMethod("add_CI")
} 

add_CI.ggKMsurv <- function(gg, ...){

  if (!"conf.int" %in% colnames(gg$data)){
    warning("Confidence limits were not part of original estimation.")
    return(NULL)
  }
  
 ## CI associated with this:
    gg <- gg +
      geom_ribbon(aes(ymin = est.lower, ymax = est.upper, fill = strata), alpha = 0.1) 
    
  return(gg)
}

add_CI.tblKM <- function(gg){
  stop("this object is not yet part of the scope")
}