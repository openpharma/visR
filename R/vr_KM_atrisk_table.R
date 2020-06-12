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

library(survival)
data = lung
equation = "Surv(time, status) ~ sex"
broom_object <- vr_est_kaplan_meier(data = lung, equation = "Surv(time, status) ~ sex")


vr_est_km_risk_table <- function(broom_object, min_at_risk = 3) {

    time_column_name <- colnames(broom_object)[1]
    
    # Get time limit: minimum time where only 3 subjects remain at risk
    limit_data <- broom_object %>%
        dplyr::select({{ time_column_name }}, n.risk, strata)

    max_time <- 
        limit_data %>% 
        filter(n.risk >= min_at_risk) %>% 
        group_by(strata) %>% 
        summarize(max_time = max(time)) %>% 
        ungroup() %>% 
        summarize(min_time = min(max_time)) %>% 
        pull(min_time)
    
    # Get time tick mark positions
    # SHAESEN2: we should have more control over this eg, based on tickvalues
    
    times <- data[[time_column_name]][data[[time_column_name]] <= max_time]
    time_ticks <- pretty(times, 5)
    
    # 
    # x00<-c(nsclc.KM$time[1:nsclc.KM$strata[1]])
    # x10<-nsclc.KM$time[(nsclc.KM$strata[1]+1):(nsclc.KM$strata[1]+nsclc.KM$strata[2])]
    # x01 <- table(cut(x = x00, breaks = c(-1,toi)))
    # x11 <- table(cut(x = x10, breaks = c(-1,toi))) 
    # xt0 <- nrow(nsclc[nsclc$trt01an==0,])-cumsum(x01)
    # xt1 <- nrow(nsclc[nsclc$trt01an==1,])-cumsum(x11)
  
    # # Build risk table
    # survfit_summary <- summary(survfit_object, times = time_ticks, extend = TRUE)
    # table_data <- data.frame(
    #     time = survfit_summary$time,
    #     n.risk = survfit_summary$n.risk,
    #     n.event = survfit_summary$n.event,
    #     strata = survfit_summary$strata
    # )
    # table_data <-
    #     table_data %>%
    #     dplyr::mutate(n.censor = lag(n.risk) - (n.risk + n.event)) %>%
    #     dplyr::mutate(n.censor = case_when(
    #         n.censor >= 0 ~ n.censor,
    #         TRUE ~ 0
    #     )) %>%
    #     tidyr::gather(key = "variable", value = "value", n.risk, n.event, n.censor) %>%
    #     dplyr::mutate(strata_variable = sprintf("%s, %s", strata, variable))

    return(table_data)
}
