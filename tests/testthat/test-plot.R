#' @title Specifications plot
#' @section Last updated by:
#' Tim Treis
#' @section Last update date:
#' 03-MAR-2021

# Specifications ----------------------------------------------------------

library(testthat)
library(visR)
library(survival)

#' T1.  The function accepts a `survfit` object and passes other objects to base
#' T1.1 No error when `survfit_object` is of class `survfit`
#' T1.2 An error when `survfit_object` is not of class `survfit`
#' T1.3 No error when a non-`survfit` object is passed to it
#' T1.4 An error when `survfit_object` is NULL
#' T1.5 An error when `survfit_object` does not exist in the global environment
#' T2.  The function produces a survival ggplot2 survival plot 
#' T2.1 The class vector of the generated output contains `gg`
#' T2.2 The class vector of the generated output contains `ggplot`
#' T2.3 The class vector of the generated output contains `ggsurvfit`


# Requirement T1 ---------------------------------------------------------------

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



# Requirement T2 ---------------------------------------------------------------

context("plot - T2. The function produces a survival ggplot2 survival plot")

testthat::test_that("T2.1 The class vector of the generated output contains `gg`",{
  
  survfit_object <- adtte %>% visR::estimate_KM(strata = "TRTP")
  p <- visR::plot(survfit_object = survfit_object)
  
  testthat::expect_true("gg" %in% class(p))
  
})

testthat::test_that("T2.2 The class vector of the generated output contains `ggplot`",{
  
  survfit_object <- adtte %>% visR::estimate_KM(strata = "TRTP")
  p <- visR::plot(survfit_object = survfit_object)
  
  testthat::expect_true("ggplot" %in% class(p))
  
})

testthat::test_that("T2.3 The class vector of the generated output contains `ggsurvfit`",{
  
  survfit_object <- adtte %>% visR::estimate_KM(strata = "TRTP")
  p <- visR::plot(survfit_object = survfit_object)
  
  testthat::expect_true("ggsurvfit" %in% class(p))
  
})

testthat::test_that("T2.4 The resulting plot contains the correct amount of strata with correct endpoints",{
  
  survfit_object <- adtte %>% visR::estimate_KM(strata = "SEX")
  p <- visR::plot(survfit_object = survfit_object)
  p_data <- ggplot2::ggplot_build(p)$data[[1]]
  p_layout <- ggplot2::ggplot_build(p)$layout
  
  n_sex <- length(unique(p_data$colour))
  endpoint_group1 <- min(p_data[p_data$group == "1",]$y)
  endpoint_group2 <- min(p_data[p_data$group == "2",]$y)
  
  testthat::expect_equal(n_sex, 2)
  testthat::expect_equal(round(endpoint_group1, 2), 0.34)
  testthat::expect_equal(round(endpoint_group2, 2), 0.26)
  
})

# END OF CODE ----------------------------------------------------------

