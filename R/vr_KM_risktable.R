library(survival)
library(dplyr)
library(tidyr)
library(broom)

broom_object <- vr_KM_est(data = adtte, strata = c("TRTP"))
min_at_risk = 3
time_ticks = seq(50, 200, 50)

vr_KM_risktable <- function(
   broom_object
  ,min_at_risk = 0
  ,time_ticks = NULL
){
  ## pull out max time to consider
  max_time <- 
    broom_object %>% 
    filter(n.risk >= min_at_risk) %>% 
    group_by(strata) %>% 
    summarize(max_time = max(time)) %>% 
    ungroup() %>% 
    summarize(min_time = min(max_time)) %>% 
    pull(min_time)
  
  ## time_ticks
  if (is.null(time_ticks)){
    times <- broom_object[["time"]][broom_object[["time"]] <= max_time]
    time_ticks <- pretty(times, 5)
  }
  
  ## Create bins for at risk calculation => allows us to select max time where time <= time_ticks
  binned_object <- broom_object %>%
    mutate(bins = as.character(cut(time, c(-Inf, time_ticks), right = FALSE, include.lowest = TRUE)))%>%
    mutate(lim = sapply(strsplit(bins, ","), function(x) {
        y <- as.numeric(  gsub("\\D", "", x) )
        return(max(y[is.finite(y)]))
      }))
 
  ## Ensure all combo's are present so at risk at every time tick for every strata can be calculated.
  ## Include time to avoids missing times replaced by 0 when merged.     
  filler <- expand(binned_object, strata, nesting(time, bins, lim))

  pre_risk_table <- binned_object%>%
    right_join(filler, intersect(colnames(filler), colnames(.))) %>%
    arrange(strata, time)%>%
    replace(., is.na(.), 0)
  
  ## Select time 0 to create lower limit record
  time0_risk_table <- pre_risk_table%>%
    group_by(strata)%>%
    filter(row_number() == 1)%>%
    mutate(lim = sapply(strsplit(bins, ","), function(x) {
      y <- as.numeric(  gsub("\\D", "", x) )
      return(min(y[is.finite(y)]))
    })
    , bins = "[-Inf, 0)")
    
  risk_table <- bind_rows(time0_risk_table, pre_risk_table)%>%
    group_by(strata) %>%
    mutate(n = max(n.risk)) %>%
    group_by(strata, lim) %>%
    summarise(n=min(n), n.event = sum(n.event), n.censor = sum(n.censor), time=min(lim)) %>%
    arrange(strata, time) %>%
    mutate(at.risk = n-cumsum(n.event)-cumsum(n.censor))

    # # Build risk table
    # survival:::summary.survfit
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
    # 
    return(risk_table)
}
