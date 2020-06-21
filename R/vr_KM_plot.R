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
#' library(survival)
#' library(glue)
#' library(dplyr)
#' library(tidyr)
#' library(ggplot2)
#'
#' ## No stratification
#' vr_KM_plot(
#'     survfit_object = vr_KM_est(data = adtte)
#'    ,title = "Time to First Dermatologic Event"
#'    ,y_label = "Suvival Probability"
#'    ,x_label = NULL                    
#'    ,xaxistable=FALSE
#'    ,conf_limits = T
#'  )
#'  
#' ## Stratified Kaplan-Meier analysis by `TRTP`
#' vr_KM_plot(
#'     survfit_object = vr_KM_est(data = adtte, strata = "TRTP")
#'    ,title = "Time to First Dermatologic Event"
#'    ,y_label = "Suvival Probability"
#'    ,x_label = NULL                    
#'    ,xaxistable=FALSE
#'    ,conf_limits = T
#'  )
#' 
#' ## Stratified Kaplan-Meier analysis by `TRTP` and `SEX` 
#' vr_KM_plot(
#'     survfit_object = vr_KM_est(data = adtte, strata = c("TRTP", "SEX"))
#'    ,title = "Time to First Dermatologic Event"
#'    ,y_label = "Suvival Probability"
#'    ,x_label = NULL                    
#'    ,xaxistable=FALSE
#'    ,conf_limits = T
#'  )
#' 
#' ## Stratification with one level
#' vr_KM_plot(
#'     survfit_object = vr_KM_est(data = adtte, strata = "PARAMCD")
#'    ,title = "Time to First Dermatologic Event"
#'    ,y_label = "Suvival Probability"
#'    ,x_label = NULL                    
#'    ,xaxistable=FALSE
#'    ,conf_limits = T
#'  )


## minimal proposal
  vr_KM_plot <- function(
     survfit_object = NULL
    ,title = NULL
    ,subtitle=NULL                                
    ,conf_limits=FALSE                  
    ,y_label = "Suvival Probability"
    ,x_label = "time"                   # take PARAMCD (put it inside broom) => "Time to resolution of xxx" How to get units (days/hours)
    ,xaxistable=FALSE
    ,theme=NULL                         # feed it company-specific theme
    #,footers =                         # I would not use it. We put footers in RTF itself.
    ,min_at_risk = 0
    ,time_ticks = NULL
  ){
    
  # Extended tidy of survfit class
  tidy_object <- tidyme.survfit(survfit_object)

  #validation and verification: eg are we dealing with tidy object or list of objects?
    
  #obtain alternative defaults such as xaxislabel, title, subtitle
    
  #look for defined theme inside global environment, if not present look for theme argument, otherwise use bw
    
    plot <- ggplot2::ggplot(tidy_object, aes(x = time)) +
      geom_step(aes(y = surv, col = strata)) + 
      ggsci::scale_color_nejm() + 
      ggsci::scale_fill_nejm() + 
      ggplot2::ylab(y_label) + 
      ggplot2::xlab(sprintf("%s", x_label)) + 
      NULL
      
    if (conf_limits == T){
      if (survfit_object$conf.type != "none"){
        plot <- plot + pammtools::geom_stepribbon(aes(ymin = lower, ymax = upper, fill = strata), alpha = 0.25)
      } else {
        warning("Confidence limits will not be drawn as they were not estimated.")
      }
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
    
    ## risk table
    if (!is.null(risk_table)){
      vr_KM_risktable(tidy_object, min_at_risk = min_at_risk, time_ticks = time_ticks)
    }
    
    
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
