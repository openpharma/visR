vr_plotly <- function(x, ...){
  UseMethod("vr_plotly")
} 

vr_plotly.default <- function(x, ...){
  plotly::ggplotly(
    p = vr_plot(x),
    width = NULL,
    height = NULL,
    tooltip = "all",
    dynamicTicks = TRUE,
    layerData = 1,
    originalData = TRUE,
    source = "A"
   )
}


x<- survfit_object
x_label = "time"
y_label = "blah"


  ## replace default eg "h" if user specified something else
  ucoalesce <- function(x, default){
    ifelse(is.null(x), default, x)
  }

  showlegend <- TRUE
  legend.position = "bottom"
  legend.position = "top"
  legend.position = "right"
  legend.position = "left"
  legend.position = "none"
  showlegend <- TRUE
  legend.position = c(0.1,0.8)


  ### common prep for both plots: perhaps we can put this in a separate function?

  
  
  
  if (is.character(legend.position)){
    if (legend.position == "bottom"){
      leg_opts <- list(xanchor = "center",
                       x = 0.5,
                       y = -0.2,
                       orientation = ucoalesce(legend.orientation, "h")
                      )
    } else if (legend.position == "right"){
      leg_opts <- list(yanchor = "center",
                       x = 1.2,
                       y = 0.5,
                       orientation = ucoalesce(legend.orientation, "v")
                      )
    } else if (legend.position == "top"){
      leg_opts <- list(xanchor = "center",
                       x = 0.5,
                       y = 1.2,
                       orientation = ucoalesce(legend.orientation, "h")
                      )
    } else if (legend.position == "left"){
      leg_opts <- list(yanchor = "center",
                       x = -1.0,
                       y = 0.5,
                       orientation = ucoalesce(legend.orientation, "v")
                      )
    } else if (legend.position == "none"){
     showlegend <-  FALSE
     leg_opts <- NULL
    }
  } else {
    leg_opts <- list(x = legend.position[1],
                     y = legend.position[2]
                    )
  }


  tidy_survobj <- tidyme.survfit(survfit_object)

## Template
  plotly::plot_ly(
    data = tidy_survobj,
    x = ~ time,
    y = ~ surv,
    split = ~ strata,
    hoverinfo = "x+y+z+text",
    text = ~ strata
    ) %>%
  
## Survival lines
  plotly::add_lines(
    y = ~ surv,
    type = "line",
    name = ~ strata,
    mode = 'lines',
    showlegend = TRUE,
    legendgroup = ~ strata,
    line = list(color = ~ strata,
                width = 2,
                shape = "hvh"
                )
    ) %>%

  
## Legend position
  plotly::layout(
    showlegend=showlegend,
    legend = leg_opts,
    xaxis = list(title = x_label,
                 hoverformat=".2f"),
    yaxis = list(title = y_label,
                 hoverformat=".2f")
  )
  
  
  class(p) <- ggsurvfit_ly


  
  return(p)