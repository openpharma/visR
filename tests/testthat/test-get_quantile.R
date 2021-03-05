#' @title Specifications get_quantile
#' @section Last updated by:
#' Tim Treis
#' @section Last update date:
#' 03-MAR-2021

# Specifications ----------------------------------------------------------

library(testthat)
library(visR)
library(survival)

#' T1.  The function accepts a survival object
#' T1.1 No error when `survfit_object` is of class `survfit`
#' T1.2 An error when `survfit_object` is not of class `survfit`
#' T1.3 An error when `survfit_object` is NULL
#' T1.4 An error when `survfit_object` does not exist in the global environment
#' T2.  The function properly calculates the quantiles
#' T2.1 No error when comparing output to the equivalent of the survival package

reformat_survival_quantiles_to_tibble <- function(data) {
  
  survival_quantiles_format <- data.frame()
  
  for (q in names(data)) {
    
    tmp <- data[q][[1]]
    tmp <- cbind(tmp, quantity = rep(q, nrow(tmp)))
    tmp <- cbind(tmp, strata = rownames(tmp))
    survival_quantiles_format <- rbind(survival_quantiles_format, tmp)
    
  }
  survival_quantiles_format <- tibble::as_tibble(survival_quantiles_format) %>%
    dplyr::relocate("strata", "quantity") %>%
    dplyr::arrange(strata, quantity) %>%
    dplyr::mutate_at(3, as.double) %>%
    dplyr::mutate_at(4, as.double) %>%
    dplyr::mutate_at(5, as.double)
  
  return(survival_quantiles_format)
  
}

# Requirement T1 ---------------------------------------------------------------

context("plot - T1. The function accepts a survival object")

testthat::test_that("T1.1 No error when `survfit_object` is of class `survfit`",{
  
  # Modify `lung` dataset to be ADAM compatible
  
  survfit_object <- lung %>% 
    dplyr::rename(AVAL = time, CNSR = status, SEX = sex) %>%
    dplyr::mutate(CNSR = dplyr::case_when(CNSR == 2 ~ 0,  # didn't reach end
                                          CNSR == 1 ~ 1)) %>%
    dplyr::mutate(SEX = dplyr::case_when(SEX == 1 ~ "male",  # didn't reach end
                                         SEX == 2 ~ "female")) %>%
    visR::estimate_KM(strata = "SEX") 
  
  testthat::expect_error(visR::get_quantile(survfit_object = survfit_object), NA)
  
})

testthat::test_that("T1.2 An error when `survfit_object` is not of class `survfit`",{
  
  adtte_as_list = adtte
  adtte_as_dataframe = base::as.data.frame(adtte)
  adtte_as_tibble = dplyr::as_tibble(adtte)
  adtte_as_datatable = data.table::as.data.table(adtte)
  
  testthat::expect_error(visR::get_quantile(survfit_object = adtte_as_list))
  testthat::expect_error(visR::get_quantile(survfit_object = adtte_as_dataframe))
  testthat::expect_error(visR::get_quantile(survfit_object = adtte_as_tibble))
  testthat::expect_error(visR::get_quantile(survfit_object = adtte_as_datatable))
  
})

testthat::test_that("T1.3 An error when `survfit_object` is NULL",{
  
  testthat::expect_error(visR::get_quantile(survfit_object = NULL))
  
})

testthat::test_that("T1.4 An error when `survfit_object` does not exist in the global environment",{
  
  testthat::expect_error(blah %>% visR::get_quantile())
  testthat::expect_error(visR::get_quantile(survfit_object = blah))
  
})


# Requirement T2 ---------------------------------------------------------------

context("plot - T2. The function properly calculates the quantiles")

testthat::test_that("T2.1 No error when comparing output to the equivalent of the survival package",{
  
  #-----------------------------------------------------------------------------
  # Test `lung` data from survival ---------------------------------------------

  adam_lung <- lung %>% 
    dplyr::rename(AVAL = time, CNSR = status, SEX = sex) %>%
    dplyr::mutate(CNSR = dplyr::case_when(CNSR == 2 ~ 0,  # didn't reach end
                                          CNSR == 1 ~ 1))
  
  visR_survfit_object <- adam_lung %>% visR::estimate_KM(strata = "SEX") 
  
  visR_quantiles <- visR::get_quantile(survfit_object = visR_survfit_object)
  
  # Generate equivalent data using the survival package
  survival_survfit_object <- survival::survfit(survival::Surv(AVAL, 1 - CNSR) ~ SEX, 
                                               data = adam_lung)
  survival_quantiles <- quantile(survival_survfit_object)
  
  # Reformat quantile data to have the same format and sorting
  survival_quantiles <- reformat_survival_quantiles_to_tibble(survival_quantiles)
  visR_quantiles <- visR_quantiles %>% dplyr::arrange(strata, quantity)
  
  testthat::expect_equal(visR_survfit_object$call, survival_survfit_object$call)
  testthat::expect_equal(survival_quantiles, visR_quantiles)
  
  #-----------------------------------------------------------------------------
  # Test `cancer` data from survival -------------------------------------------
  
  adam_cancer <- cancer %>% 
    dplyr::rename(AVAL = time, CNSR = status, SEX = sex) %>%
    dplyr::mutate(CNSR = dplyr::case_when(CNSR == 2 ~ 0,  # didn't reach end
                                          CNSR == 1 ~ 1))
  
  visR_survfit_object <- adam_cancer %>% visR::estimate_KM(strata = "SEX") 
  
  visR_quantiles <- visR::get_quantile(survfit_object = visR_survfit_object)
  
  # Generate equivalent data using the survival package
  survival_survfit_object <- survival::survfit(survival::Surv(AVAL, 1 - CNSR) ~ SEX, 
                                               data = adam_cancer)
  survival_quantiles <- quantile(survival_survfit_object)
  
  # Reformat quantile data to have the same format and sorting
  survival_quantiles <- reformat_survival_quantiles_to_tibble(survival_quantiles)
  visR_quantiles <- visR_quantiles %>% dplyr::arrange(strata, quantity)
  
  testthat::expect_equal(visR_survfit_object$call, survival_survfit_object$call)
  testthat::expect_equal(survival_quantiles, visR_quantiles)
  
  #-----------------------------------------------------------------------------
  # Test `kidney` data from survival -------------------------------------------
  
  adam_kidney <- kidney %>% 
    dplyr::rename(AVAL = time, CNSR = status, SEX = sex) 
  
  visR_survfit_object <- adam_kidney %>% visR::estimate_KM(strata = "SEX") 
  
  visR_quantiles <- visR::get_quantile(survfit_object = visR_survfit_object)
  
  # Generate equivalent data using the survival package
  survival_survfit_object <- survival::survfit(survival::Surv(AVAL, 1 - CNSR) ~ SEX, 
                                               data = adam_kidney)
  survival_quantiles <- quantile(survival_survfit_object)
  
  # Reformat quantile data to have the same format and sorting
  survival_quantiles <- reformat_survival_quantiles_to_tibble(survival_quantiles)
  visR_quantiles <- visR_quantiles %>% dplyr::arrange(strata, quantity)
  
  testthat::expect_equal(visR_survfit_object$call, survival_survfit_object$call)
  testthat::expect_equal(survival_quantiles, visR_quantiles)
  
  #-----------------------------------------------------------------------------
  # Test `adtte` data from visR ------------------------------------------------
  
  visR_survfit_object <- adtte %>% visR::estimate_KM(strata = "SEX") 
  
  visR_quantiles <- visR::get_quantile(survfit_object = visR_survfit_object)
  
  # Generate equivalent data using the survival package
  survival_survfit_object <- survival::survfit(survival::Surv(AVAL, 1 - CNSR) ~ SEX, 
                                               data = adtte)
  survival_quantiles <- quantile(survival_survfit_object)
  
  # Reformat quantile data to have the same format and sorting
  survival_quantiles <- reformat_survival_quantiles_to_tibble(survival_quantiles)
  visR_quantiles <- visR_quantiles %>% dplyr::arrange(strata, quantity)
  
  testthat::expect_equal(visR_survfit_object$call, survival_survfit_object$call)
  testthat::expect_equal(survival_quantiles, visR_quantiles)
  
})

# END OF CODE ----------------------------------------------------------

