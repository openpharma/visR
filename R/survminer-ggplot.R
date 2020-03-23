library(survival)
library(survminer)
model <- coxph( Surv(time, status) ~ sex + rx + adhere,
                data = colon )
ggforest(model)
