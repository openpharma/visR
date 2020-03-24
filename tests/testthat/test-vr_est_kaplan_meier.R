context("KM helper functions")

skip_if_not_installed("survival")
library(survival)


## three models. 
## test how flexible the method is for various TTE methods
fit <- survfit(Surv(time, status) ~ age + sex, lung)
fit2 <- coxph(Surv(time, status) ~ age + sex, lung, robust = TRUE)
fit3 <- coxph(Surv(time, status) ~ age + sex + frailty(inst), lung)

## TODO: 
## test the inputs to the function and if it fails
## with appropriate error messages 

test_that("check vr_est_kaplan_meier.R inputs", {
  
  td <- vr_est_kaplan_meier(lung, "Surv(time, status) ~ age + sex")
  
})

