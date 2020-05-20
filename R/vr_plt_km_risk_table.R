#' Plot (Kaplan-Meier) Risk Table for an existing tidy risk object
#'
#' @param risk_table_data Input is risk table or output of vr_est_km_risk_table
#' @param time_unit What unit is the time unit
#'
#' @return ggplot object for the risk table
#' @export
#'
#' @examples
#' # Create interim data models
#' library(survival)
#' library(ggplot2)
#' library(dplyr)
#' data("veteran")
#' data <-  veteran %>%
#'     mutate(trt = as.factor(case_when(
#'        trt == 1 ~ "standard therapy",
#'        trt == 2 ~ "test chemotherapy"
#'    )))
#'
#' equation <- "survival::Surv(time, status) ~ trt"
#' risk_table_data <- vr_est_km_risk_table(data, equation)
#' time_unit = "days"
#'
#' # Create plot
#' vr_plt_km_risk_table(risk_table_data,
#'                      time_unit = time_unit)
#'
vr_plt_km_risk_table <- function(risk_table_data, time_unit) {
    plot <-
        ggplot(risk_table_data, aes(x = time)) +
        geom_text(aes(y = strata_variable, col = strata, label = value)) +
        xlab(sprintf("time (%s)", time_unit)) +
        ylab("") +
        ggsci::scale_color_nejm() +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


    return(plot)
}
