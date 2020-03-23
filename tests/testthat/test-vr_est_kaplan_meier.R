context("KM helper functions")

skip_if_not_installed("survival")
library(survival)


fit <- survfit(Surv(time, status) ~ age + sex, lung)

fit2 <- coxph(Surv(time, status) ~ age + sex, lung, robust = TRUE)
vr_est_kaplan_meier(lung, "Surv(time, status) ~ age + sex")

fit3 <- coxph(Surv(time, status) ~ age + sex + frailty(inst), lung)


test_that("check vr_est_kaplan_meier.R inputs", {
  
  td <- vr_est_kaplan_meier(lung, "Surv(time, status) ~ age + sex")
  
})

