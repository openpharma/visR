#' Create Kaplan-Meier Curve for Survival Data
#'
#' @param data 
#' @param title 
#' @param abbreviations 
#' @param variable_definitions 
#' @param N 
#' @param N_unit 
#' @param time_unit 
#' @param data_source 
#'
#' @return
#' @export
#'
#' @examples
vr_kaplan_meier <- function(
    data, 
    title = "", 
    abbreviations = NULL, 
    variable_definitions = NULL, 
    N = NULL, 
    N_unit = "patients", 
    time_unit = "days", 
    data_source = NULL
) {

    # Create interim data models
    broom_object <- vr_est_kaplan_meier(data)
    risk_table_data <- vr_est_km_risk_table(data)
    
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
