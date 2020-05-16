## code to prepare `DATASET` dataset goes here



usethis::use_data("DATASET")

# Breast survival data

library(dplyr)
library(RTCGA)
library(RTCGA.clinical)

brca_cohort <- survivalTCGA(BRCA.clinical,
                            extract.cols=c("admin.disease_code",
                                           "patient.breast_carcinoma_estrogen_receptor_status",
                                           "patient.breast_carcinoma_progesterone_receptor_status",
                                           "patient.clinical_cqcf.tumor_type",
                                           "patient.drugs.drug.therapy_types.therapy_type",
                                           "patient.age_at_initial_pathologic_diagnosis")) %>%
  rename(tumor_type = patient.clinical_cqcf.tumor_type,
         therapy = patient.drugs.drug.therapy_types.therapy_type,
         er_status = patient.breast_carcinoma_estrogen_receptor_status,
         progesterone_status = patient.breast_carcinoma_progesterone_receptor_status,
         dx_age = patient.age_at_initial_pathologic_diagnosis,
         followup_time = times
  ) %>%
  mutate(er_status = factor(er_status),
         progesterone_status = factor(progesterone_status),
         dx_age = as.numeric(dx_age),
         dx_age_group = factor(case_when(dx_age < 30 ~ "< 30y",
                                         dx_age >= 30 & dx_age <= 50 ~ "30-50y",
                                         dx_age > 50 & dx_age <= 70 ~ "51-70y",
                                         dx_age > 70 ~ "> 70y"),
                               levels=c("< 30y", "30-50y", "51-70y", "> 70y")))

usethis::use_data(brca_cohort)
