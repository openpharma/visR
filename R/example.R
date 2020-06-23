library(dplyr)
library(tidyr)
library(survival)
library(ggplot2)


## Estimation function
survfit_object <- vr_KM_est(data = adtte, strata = "TRTP", aval = "AVAL", conf.int = T)
survfit_object <- vr_KM_est(data = adtte, strata = "TRTP", aval = "AVAL", conf.int = F,  timefix=TRUE)

## low level plotting depends on list structure of surv object
plot(survfit_object)

## clean survfit
tidy_survobj <- tidyme.survfit(survfit_object)

## high level plotting
vr_KM_plot(
   survfit_object = survfit_object
  ,conf_limits = TRUE                  
  ,y_label = "Suvival Probability"
  ,x_label = "time"                   
  ,xaxistable=T
  ,min_at_risk = 0
  ,time_ticks = seq(50,200,50)
)

## rerun the call
tidy_survobj$call
eval(tidy_survobj$call[1][[1]])

## Update formula of survival object
getCall(survfit_object)
eval(survfit_object)
(new_survfit_object <- update(survfit_object,  formula = . ~ . + SEX, evaluate = TRUE))
