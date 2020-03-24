context("vr_kaplan_meier function")

skip_if_not_installed("survival")
library(survival)

## three models. 
fit <- survfit(Surv(time, status) ~ age + sex, lung)
fit2 <- coxph(Surv(time, status) ~ age + sex, lung, robust = TRUE)
fit3 <- coxph(Surv(time, status) ~ age + sex + frailty(inst), lung)

## TODO: 

library(dplyr)
library(magrittr)
library(ggplot2)

# Load data from the Veteran's Cancer Study
data(veteran)
data <- 
  veteran %>% 
  mutate(trt = as.factor(case_when(
    trt == 1 ~ "standard therapy", 
    trt == 2 ~ "test chemotherapy"
  )))

vr_kaplan_meier(
  data = data, 
  data_source = "Veteran's Lung Cancer Study", 
  title = "Comparison of survival in male patients having advanced inoperable lung cancer \nunder standard therapy vs. test chemotherapy"
)

vr_kaplan_meier(
  data = lung,
  equation = "survival::Surv(time, status) ~ sex",
  data_source = "NCCTG Lung Cancer Study", 
  title = "Comparison of survival in male and female patients."
)

test_that("check input args", {
  
  library(dplyr)
  library(magrittr)
  library(ggplot2)
  
  # Load data from the Veteran's Cancer Study
  data(veteran)
  data <- 
    veteran %>% 
    mutate(trt = as.factor(case_when(
      trt == 1 ~ "standard therapy", 
      trt == 2 ~ "test chemotherapy"
    )))
  
  vr_kaplan_meier(
    data = data, 
    data_source = "Veteran's Lung Cancer Study", 
    title = "Comparison of survival in male patients having advanced inoperable lung cancer \nunder standard therapy vs. test chemotherapy"
  )
  
  
  vr_kaplan_meier(
    data = lung,
    equation = "survival::Surv(time, status) ~ sex",
    data_source = "NCCTG Lung Cancer Study", 
    title = "Comparison of survival in male and female patients."
  )
  
  
  
})

