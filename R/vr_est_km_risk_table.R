#' Generate a tidied risk table
#' 
#' TODO: Describe purpose of function and inputs / outputs
#'
#' @param data input data set
#' @param equation  Survival function to be estimated in the format of Survfit.
#' @param min_at_risk default = 3
#'
#' @return tibble that contains the tidied risk table
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
#' vr_est_km_risk_table(data, equation) 
vr_est_km_risk_table <- function(data, equation, min_at_risk = 3) {
    survfit_object <- survival::survfit(
        eval(parse(text = equation)), data = data
    )
    survfit_summary <- summary(survfit_object)
    
    # Get time limit
    limit_data <- data.frame(
        time = survfit_summary$time, 
        n.risk = survfit_summary$n.risk, 
        strata = survfit_summary$strata
    )
    max_time <- 
        limit_data %>% 
        filter(n.risk >= min_at_risk) %>% 
        group_by(strata) %>% 
        summarize(max_time = max(time)) %>% 
        ungroup() %>% 
        summarize(min_time = min(max_time)) %>% 
        pull(min_time)
    
    # Get time tick mark positions
    times <- data$time[data$time <= max_time]
    time_ticks <- pretty(times, 5)
    
    # Build risk table
    survfit_summary <- summary(survfit_object, times = time_ticks, extend = TRUE)
    table_data <- data.frame(
        time = survfit_summary$time, 
        n.risk = survfit_summary$n.risk, 
        n.event = survfit_summary$n.event, 
        strata = survfit_summary$strata
    )
    table_data <- 
        table_data %>% 
        dplyr::mutate(n.censor = lag(n.risk) - (n.risk + n.event)) %>% 
        dplyr::mutate(n.censor = case_when(
            n.censor >= 0 ~ n.censor, 
            TRUE ~ 0
        )) %>% 
        tidyr::gather(key = "variable", value = "value", n.risk, n.event, n.censor) %>% 
        dplyr::mutate(strata_variable = sprintf("%s, %s", strata, variable))
    
    return(table_data)
}