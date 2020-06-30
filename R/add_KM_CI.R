add_KM_CI <- function(gg, ...){
  UseMethod("add_KM_CI")
} 

add_KM_CI.ggKMsurv <- function(gg, ...){

  if (!"conf.int" %in% colnames(gg$data)){
    warning("Confidence limits were not part of original estimation.")
    return(NULL)
  }
  
 ## CI associated with this:
    gg <- gg +
<<<<<<< HEAD
      geom_ribbon(aes(ymin = est.lower, ymax = est.upper, fill = strata), alpha = 0.1) 
=======
      geom_ribbon(aes(ymin = lower, ymax = upper, fill = strata), alpha = 0.1) 
    
  } else if (y == "cumhaz"){
>>>>>>> 026ffff029aab733ae1d4fdd31b77b62f00ac333
    
  return(gg)
}

add_KM_CI.tblKM <- function(gg){
  stop("this object is not yet part of the scope")
}