add_KM_CI <- function(gg, ...){
  UseMethod("add_KM_CI")
} 

add_KM_CI.ggKMsurv <- function(gg, ...){

  if (!"conf.int" %in% colnames(gg$data)){
    warning("Confidence limits were not part of original estimation.")
    return(NULL)
  }
  
 ## find out what was plotted
  y <- gg$labels$y
  
 ## CI associated with this:
  if (y == "surv"){
    
    gg <- gg +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1) 
    
  } else if (y == "cumhaz"){
    
  }
  
  return(gg)
}

add_KM_CI.tblKM <- function(gg){
  stop("this object is not yet part of the scope")
}