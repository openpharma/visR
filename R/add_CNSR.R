add_CNSR <- function(gg, ...){
  UseMethod("add_CNSR")
} 

add_CNSR.ggsurvfit <- function(gg, shape = 3, size = 2, ...){
  
  if (!inherits(gg, "ggsurvfit")){
    stop("Function can only be applied to an object of class `ggsurvfit`.")
  }
  
  gg <- gg +
    geom_point(data = base::subset(gg$data, n.censor >= 1), aes(x = time, y = est, color = strata), shape = shape, size = size)

  return(gg)
}

add_CNSR.ggsurvfit_ly <- function(gg, shape = 3, size = 2, ...){

  # ## do some validation
  # gg <- gg %>%
  #   plotly::add_markers(x=~x, y=~y, color=~strata, data=censored, 
  #               marker=list(symbol="line-ns-open", size=10),
  #               name=~strata,
  #               showlegend=T,
  #               legendgroup=~strata,
  #               hoverinfo="none")

}