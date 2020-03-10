library(survminer)
library(survival)
library(broom)
library(RTCGA.clinical)

survivalTCGA(BRCA.clinical, OV.clinical, extract.cols = "admin.disease_code") -> BRCAOV.survInfo
options(scipen=999)

vr_kaplan_meier_summary <- function(data, equation) {

# Run survival function
fit <- survfit(eval(parse(text=equation)) , data = data)

# Summary Table with persons at risk, events, median survival times along with 95% CIs over strata
median_survival_time_summary <- 
  dplyr::as_tibble(summary(fit)$table, rownames="strata") %>%
  dplyr::mutate(strata=sub(".*=", "", names(fit$strata))) %>%
  dplyr::select(strata,records,events,median,'0.95LCL','0.95UCL') %>%
  dplyr::rename('# persons'=records, '# events'=events, 'median survival time'=median)
  
# test of equality over strata
log_rank    <- cbind(Test='Log-rank',    broom::glance(survdiff(eval(parse(text=equation)), data=data, rho=0)), Description='Log-rank gives more weight on higher values of time', stringsAsFactors=F)
wilcoxon    <- cbind(Test='Wilcoxon',    broom::glance(survdiff(eval(parse(text=equation)), data=data, rho=1)),   Description='Wilcoxon gives more weight on lower values of time', stringsAsFactors=F)
tarone_ware <- cbind(Test='Tarone-Ware', broom::glance(survdiff(eval(parse(text=equation)), data=data, rho=1.5)), Description='Tarone-Ware is in between', stringsAsFactors=F)

# Summary table with test of equality over strata
equality_of_strata <- 
log_rank  %>%
dplyr::bind_rows(wilcoxon) %>%
dplyr::bind_rows(tarone_ware)   

return(list(median_survival_time_summary,equality_of_strata))

}


# Example
output <- vr_kaplan_meier_summary(data=BRCAOV.survInfo, equation="Surv(times, patient.vital_status) ~ admin.disease_code")

output[1] # Summary Table with persons at risk, events, median survival times along with 95% CIs over strata
output[2] # Summary table with test of equality over strata