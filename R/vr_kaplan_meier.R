#' Generate Kaplan-Meier plot for survival data
#'
#' @param data input data set
#' @param title plot title
#' @param abbreviations 
#' @param variable_definitions 
#' @param N 
#' @param N_unit unit label for big N (i.e. patients, subjects, etc.)
#' @param time_unit unit label for the x-axis (i.e. minutes, days, weeks, years)
#' @param data_source label for the data source 
#'
#' @return ggplot object for the KM plot
#' @export
#'
#' @examples
#' library(survival)
#' library(dplyr)
#' library(magrittr)
#' library(ggplot2)
#' # Load data from the Veteran's Cancer Study
#' data(veteran)
#' 
#' data <-  veteran %>% 
#'   mutate(trt = as.factor(case_when(
#'   trt == 1 ~ "standard therapy", 
#'   trt == 2 ~ "test chemotherapy"
#'   )))
#'   
#'   vr_kaplan_meier(
#'     data = data, 
#'     data_source = "Veteran's Lung Cancer Study", 
#'     title = "Comparison of survival in male patients having advanced inoperable lung cancer \nunder standard therapy vs. test chemotherapy"
#'     )

vr_kaplan_meier <- function(
    data, 
    equation = "survival::Surv(time, status) ~ trt",
    title = "", 
    abbreviations = NULL, 
    variable_definitions = NULL, 
    N = NULL, 
    N_unit = "patients", 
    time_unit = "days", 
    data_source = NULL
) {

    # Create interim data models
    broom_object <- vr_est_kaplan_meier(data, equation)
    risk_table_data <- vr_est_km_risk_table(data, equation)
    
    # Create plot
    table <- vr_plt_km_risk_table(risk_table_data,
                                  time_unit = time_unit)
    curve <- vr_plt_kaplan_meier(
        broom_object, 
        title = title, 
        abbreviations = abbreviations, 
        variable_definitions = variable_definitions, 
        N = N, 
        N_unit = N_unit, 
        time_unit = time_unit, 
        data_source = data_source
    ) + ggplot2::xlim(c(0, max(risk_table_data$time)))
    
    plot_object <- ggpubr::ggarrange(plotlist = list(curve, table), nrow = 2, ncol = 1, 
        heights = c(4, 1), align = "v")
    return(plot_object)
}
