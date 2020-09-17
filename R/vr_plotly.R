## EXPERIMENTAL - WILL BE REWRITTEN TO TAKE THE GGPLOT FROM VISR AND GGPLOTLY IT

vr_plotly <- function(x, ...){
  UseMethod("vr_plotly")
} 


vr_plotly.default <- function (x, ...){
    p <- plotly::ggplotly(
      p = ggplot2::last_plot(),
      width = NULL,
      height = NULL,
      tooltip = "all",
      dynamicTicks = TRUE,
      layerData = 1,
      originalData = TRUE,
      source = "A"
     ) 
    
    return(p)
}

vr_plotly.survfit  <- function(
  x,
  legend_position = "right",
  legend_orientation = NULL,
  ...
  ){
  
  if (! base::inherits(x, "survfit")) stop("Object is not of class survfit.")

  legendoptions <- legendopts(legend_position = legend_position, legend_orientation = legend_orientation)
  
  p <- plotly::ggplotly(
    p = vr_plot(
      x, 
      legend_position = legend_position,
      ...
    ),
    width = NULL,
    height = NULL,
    tooltip = "all",
    dynamicTicks = TRUE,
    layerData = 1,
    originalData = TRUE,
    source = "A"
   ) 
  
  Nm <- names(survfit_object$strata)

  ## Adjust the legend consistently with ggplot2
  if (legendoptions$showlegend  == TRUE) {
    leg_opts <- legendoptions$leg_opts
    showlegend <- legendoptions$showlegend 

    
    p$x$layout$annotations[[1]]$text <- ""
    p$x$layout$annotations[[1]]$legendTitle <- FALSE

    leg_opts <- append(leg_opts, list(title=list(text='strata')))
    
    p <- p %>%
      plotly::layout(
      legend = leg_opts
      )
    
    ## Change legend style
    for (i in seq_along(Nm)){
      p <- plotly::style(p, name = Nm[i], traces = i, showlegend = TRUE)
      # p$x$data[[i]]$name
      # p$x$data[[i]]$legendgroup
    }
  }
    
  return(p)
  ## change legend pos
}


# vr_plotly.survfit <- function(
#   x,
#   legend_position = "right",
#   legend_orientation = NULL,
#   x_label = "time",
#   y_label = "blah"
#   
# ){
# 
#   leg_opts <- legendopts(legend_position = legend_position, legend_orientation = legend_orientation)
# 
#   tidy_survobj <- tidyme.survfit(survfit_object)
# 
# ## Template
#   plotly::plot_ly(
#     data = tidy_survobj,
#     x = ~ time,
#     y = ~ surv,
#     split = ~ strata,
#     hoverinfo = "x+y+z+text",
#     text = ~ strata
#     ) %>%
#   
# ## Survival lines
#   plotly::add_lines(
#     y = ~ surv,
#     type = "line",
#     name = ~ strata,
#     mode = 'lines',
#     showlegend = TRUE,
#     legendgroup = ~ strata,
#     line = list(color = ~ strata,
#                 width = 2,
#                 shape = "hvh"
#                 )
#     ) %>%
# 
#   
# ## Legend position
#   plotly::layout(
#     showlegend=showlegend,
#     legend = leg_opts,
#     xaxis = list(title = x_label,
#                  hoverformat=".2f"),
#     yaxis = list(title = y_label,
#                  hoverformat=".2f")
#   )
#   
# }