add_CI <- function(gg, ...){
  UseMethod("add_CI")
} 

add_CI.ggsurvfit <- function(gg, alpha = 0.1, style = "ribbon", linetype = 2, ...){

  if (! base::all(c("est.lower", "est.upper") %in% colnames(gg$data))) {
    warning("Confidence limits were not part of original estimation.")
    return(NULL)
  }
  
  if (! base::any(c("ribbon", "step") %in% style)) {
    warning("Invalid `step` argument.")
    return(NULL)
  } 
  
  if (style == "ribbon"){
    gg <- gg +
      geom_ribbon(aes(ymin = est.lower, ymax = est.upper, fill = strata), alpha = alpha, na.rm = TRUE) 
  }
  
  if (style == "step"){
    gg <- gg +
      geom_ribbon(aes(ymin = est.lower, ymax = est.upper, fill = NA, colour = strata), alpha = alpha, outline.type = "both", linetype = linetype, show.legend = FALSE, na.rm = TRUE)
  }
  
  return(gg)
}

add_CI.ggsurvfit_ly <- function(gg){
  
  # ## do some checks
  # 
  #  gg <- gg %>%
  #   plotly::add_ribbons(x=~x, ymin=~low, ymax=~up, color=~strata, 
  #               opacity=0.25, 
  #               line=list(width=0),
  #               name=~strata,
  #               showlegend=T,
  #               legendgroup=~strata,
  #               hoverinfo="none") 
  # 
  #  return(gg)  
}

add_CI.tblKM <- function(gg){
  stop("this object is not yet part of the scope")
}