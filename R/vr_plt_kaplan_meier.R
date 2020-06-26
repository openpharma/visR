#' Plot Kaplan-Meier Curve for Existing Tidy Survival Object
#'
#' TODO: Define and describe the purpose of this function. 
#' 
#' @param broom_object, 
#'
#' @param broom_object An object via broom
#' @param title = ""
#' @param abbreviations = NULL
#' @param variable_definitions = NULL
#' @param N = NULL
#' @param N_unit = "patients"
#' @param time_unit = "days"
#' @param data_source = NULL
#' @param estimate_name = "survival probability"
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
    
    
    # ## Risk table
    #  # use broom object as input
    # risk_table_data <- vr_est_km_risk_table(data, equation)
    # 
    # # Create plot
    # table <- vr_plt_km_risk_table(risk_table_data,
    #                               time_unit = time_unit)
    # curve <- vr_plt_kaplan_meier(
    #     broom_object, 
    #     title = title, 
    #     abbreviations = abbreviations, 
    #     variable_definitions = variable_definitions, 
    #     N = N, 
    #     N_unit = N_unit, 
    #     time_unit = time_unit, 
    #     data_source = data_source
    # ) + ggplot2::xlim(c(0, max(risk_table_data$time)))
    # 
    # plot_object <- ggpubr::ggarrange(plotlist = list(curve, table), nrow = 2, ncol = 1, 
    #     heights = c(4, 1), align = "v")
    # 
    
    ## ggrob everything into grid
 

    
    return(plot)
}
