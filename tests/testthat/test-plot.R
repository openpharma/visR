#' @title Specifications plot
#' @section Last updated by:
#' Tim Treis
#' @section Last update date:
#' 03-MAR-2021

# Specifications ----------------------------------------------------------

library(testthat)
library(visR)
library(survival)

#' T1. The function accepts a `survfit` object and passes other objects to base
#' T1.1 No error when `survfit_object` is of class `survfit`
#' T1.2 An error when `survfit_object` is not of class `survfit`
#' T1.3 No error when a non-`survfit` object is passed to it
#' T1.4 An error when `survfit_object` is NULL
#' T1.5 An error when `survfit_object` does not exist in the global environment


# Requirement T1 ----------------------------------------------------------

context("plot - T1. The function accepts a `survfit` object and passes other objects to base")

testthat::test_that("T1.1. No error when `survfit_object` is of class `survfit`",{
  
  survfit_object <- adtte %>% visR::estimate_KM(strata = "TRTP")
  
  testthat::expect_error(visR::plot(survfit_object = survfit_object), NA)
  
})

testthat::test_that("T1.2 An error when `survfit_object` is not of class `survfit`",{
  
  # visR evaluates the object
  adtte_as_list = adtte
  adtte_as_dataframe = base::as.data.frame(adtte)
  adtte_as_tibble = dplyr::as_tibble(adtte)
  adtte_as_datatable = data.table::as.data.table(adtte)
  
  testthat::expect_error(visR::plot(survfit_object = adtte_as_list))
  testthat::expect_error(visR::plot(survfit_object = adtte_as_dataframe))
  testthat::expect_error(visR::plot(survfit_object = adtte_as_tibble))
  testthat::expect_error(visR::plot(survfit_object = adtte_as_datatable))
  
})

testthat::test_that("T1.3 No error when a non-`survfit` object is passed to it",{
  
  # base evaluates the object
  adtte_as_list = adtte
  adtte_as_dataframe = base::as.data.frame(adtte)
  adtte_as_tibble = dplyr::as_tibble(adtte)
  adtte_as_datatable = data.table::as.data.table(adtte)
  
  testthat::expect_error(adtte_as_list %>% visR::plot(), NA)
  testthat::expect_error(adtte_as_dataframe %>% visR::plot(), NA)
  testthat::expect_error(adtte_as_tibble %>% visR::plot(), NA)
  testthat::expect_error(adtte_as_datatable %>% visR::plot(), NA)
  
})

testthat::test_that("T1.4 An error when `survfit_object` is NULL",{
  
  testthat::expect_error(visR::plot(survfit_object = NULL))
  
})

testthat::test_that("T1.5 An error when `survfit_object` does not exist in the global environment",{
  
  testthat::expect_error(blah %>% visR::plot())
  testthat::expect_error(visR::plot(blah))
  
})



# Requirement T2 ----------------------------------------------------------



# END OF CODE ----------------------------------------------------------

