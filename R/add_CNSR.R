
add_CNSR <- function(gg, shape = 3, size = 2, ...){
  
  if (!inherits(gg, "ggKMsurv")){
    stop("Function can only be applied to an object of class `ggKMsurv`.")
  }
  
  gg <- gg +
    geom_point(data = base::subset(gg$data, n.censor >= 1), aes(x = time, y = est, color = strata), shape = shape, size = size)

  return(gg)
}