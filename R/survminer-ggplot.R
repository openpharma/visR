library(survival)
library(survminer)
model <- coxph( Surv(time, status) ~ sex + rx + adhere,
                data = colon )
ggforest(model)

colon <- within(colon, {
  sex <- factor(sex, labels = c("female", "male"))
  differ <- factor(differ, labels = c("well", "moderate", "poor"))
  extent <- factor(extent, labels = c("submuc.", "muscle", "serosa", "contig."))
})
bigmodel <-
  coxph(Surv(time, status) ~ sex + rx + adhere + differ + extent + node4,
        data = colon )
ggforest(bigmodel)