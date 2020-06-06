#' Plot Kaplan-Meier Curve for Existing Tidy Survival Object
#'
#' TODO: Define and describe the purpose of this function. 
#' 
#' @param broom_object, 
#'
#' @return ggplot object 
#' @export
#'
#' @examples
#' # TODO: Define an example for this function
#' library(visR)
#' library(dplyr)
#' library(survival)
#' library(ggplot2)
#' 
#' data("veteran")
#' data <-  veteran %>% 
#'     mutate(trt = as.factor(case_when(
#'        trt == 1 ~ "standard therapy", 
#'        trt == 2 ~ "test chemotherapy"
#'        ))
#'        , blah = "Only level")
#'
#' vr_KM_plot(
#'     broom_object = vr_est_kaplan_meier(data, "survival::Surv(time, status) ~ trt")
#'    ,title = "Time to First Dermatologic Event"
#'    ,y_label = "Suvival Probability"
#'    ,x_label = NULL                    
#'    ,xaxistable=FALSE
#'    ,conf_limits = T
#'  )
#'  
#' vr_KM_plot(
#'     broom_object = vr_est_kaplan_meier(data, "survival::Surv(time, status) ~ blah")
#'    ,title = "Time to First Dermatologic Event"
#'    ,y_label = "Suvival Probability"
#'    ,x_label = NULL                    
#'    ,xaxistable=FALSE
#'    ,conf_limits = T
#'  )



## minimal proposal
  vr_KM_plot <- function(
     broom_object = NULL                # object: does it need to be broom? Can we input KM object instead? (keeping in mind validation and possible breaking updates in broom)
    ,title = NULL
    ,subtitle=NULL               
    ,conf_limits=FALSE                  # Alpha level needs to be taken care off in estimation function
    ,y_label = "Suvival Probability"
    ,x_label = "time"                   # take PARAMCD if present in broom object => use label of AVAL/CHG, ... Time (days) is not sufficient We would need "Time to resolution of xxx"
    ,xaxistable=FALSE
    ,theme=NULL
  ){

  #validation and verification: eg are we dealing with tidy object or list of objects?
    
  #obtain alternative defaults such as xaxislabel, title, subtitle
    
  #look for defined theme inside global environment, if not present look for theme argument, otherwise use bw
    
    plot <- ggplot2::ggplot(broom_object, aes(x = time)) +
      geom_step(aes(y = estimate, col = strata)) + 
      ggsci::scale_color_nejm() + 
      ggsci::scale_fill_nejm() + 
      ggplot2::ylab(y_label) + 
      ggplot2::xlab(sprintf("%s", x_label)) + 
      NULL
      
    if (conf_limits == T){
      plot <- plot + pammtools::geom_stepribbon(aes(ymin = conf.low, ymax = conf.high, fill = strata), alpha = 0.25)
    }
    
    if (!is.null(title)){
      plot <- plot +  ggplot2::labs(title = title)
    } 
    
    if (!is.null(subtitle)){
      plot <- plot +  ggplot2::labs(subtitle = subtitle)
    } 
    
    ## add theme to graph
    if (is.null(theme) & exists("theme", envir = .GlobalEnv) & !is.null(get("theme", envir = .GlobalEnv)) ){
      plot <- plot + theme
    } else if (is.null(theme) & (!exists("theme", envir = .GlobalEnv) | is.null(get("theme", envir = .GlobalEnv)))) {
      plot <- plot +  ggplot2::theme_bw()
    } else if (!is.null(theme)){
      #catherror to see if exist
      plot <- plot + theme
    } else {
      warning("Variable 'theme' can't be found or non-conform to ggplot2 guidelines. Function will apply default theme_bw.")
      plot <- plot +  ggplot2::theme_bw()
    }
  
    

    ## legend position
    
    
    ## Risk table
    
    
    ## ggrob everything into grid
 

    
    return(plot)
}
