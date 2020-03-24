#' Plot Kaplan-Meier Summary Table for Existing
#'
#' TODO: Check the warnings thrown in the examples. 
#' 
#' @param data Dataset as dataframe/tibble containing with event data (one row per subject)
#' @param equation Formula to create survival object
#'
#' @return List containing two summary tables: 1) Summary Table with persons at risk, events, median survival times along with 95% CIs over strata and 2) Summary table with test of equality over strata
#' @export
#'
#' @examples
#' #' # Create interim data models
#' library(survival)
#' library(survminer)
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
#' vr_kaplan_meier_summary(data, equation) 
#' 
#' # Example
#' output <- vr_kaplan_meier_summary(data=BRCAOV.survInfo, equation="Surv(times, patient.vital_status) ~ admin.disease_code")
#' output[1] # Summary Table with persons at risk, events, median survival times along with 95% CIs over strata
#' output[2] # Summary table with test of equality over strata
#' 
vr_kaplan_meier_summary <- function(data, equation) {

# Run survival function
fit <- survival::survfit(eval(parse(text = equation)), data = data)

# Summary Table with persons at risk, events, median survival times along with 95% CIs over strata
median_survival_time_summary <- 
  dplyr::as_tibble(summary(fit)$table, rownames = "strata") %>%
  dplyr::mutate(strata = sub(".*=", "", names(fit$strata))) %>%
  dplyr::select(strata,records,events,median,'0.95LCL','0.95UCL') %>%
  dplyr::rename('# persons' = records, '# events' = events, 'median survival time' = median)
  
# test of equality over strata
log_rank    <- cbind(Test = 'Log-rank',    broom::glance(survdiff(eval(parse(text = equation)), data = data, rho = 0)), Description = 'Log-rank gives more weight on higher values of time', stringsAsFactors = F)
wilcoxon    <- cbind(Test = 'Wilcoxon',    broom::glance(survdiff(eval(parse(text = equation)), data = data, rho = 1)),   Description = 'Wilcoxon gives more weight on lower values of time', stringsAsFactors = F)
tarone_ware <- cbind(Test = 'Tarone-Ware', broom::glance(survdiff(eval(parse(text = equation)), data = data, rho = 1.5)), Description = 'Tarone-Ware is in between', stringsAsFactors = F)

# Summary table with test of equality over strata
equality_of_strata <- 
  log_rank  %>%
  dplyr::bind_rows(wilcoxon) %>%
  dplyr::bind_rows(tarone_ware)   

return(list(median_survival_time_summary, equality_of_strata))

}

