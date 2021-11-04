#' @title Specifications add_CI
#' @section Last updated by:
#' Tim Treis
#' @section Last update date:
#' 10-MAY-2021

# Specifications ---------------------------------------------------------------

# Different surv functions?

# Requirement T1 ---------------------------------------------------------------

testthat::context("add_quantiles - T1. tbd.")

testthat::test_that("T1.1 No error when the default parameters are used.", {
  
  gg <- adtte %>%
    visR::estimate_KM() %>%
    visR::visr() 
  
  gg %>% 
    visR::add_quantiles() %>%
    testthat::expect_error(NA) 
  
})

testthat::test_that("T1.2 No error when more than one quantile is specified.", {
  
  gg <- adtte %>%
    visR::estimate_KM() %>%
    visR::visr() 
  
  gg %>% 
    visR::add_quantiles(quantiles = c(0.25, 0.50)) %>%
    testthat::expect_error(NA) 
  
})

testthat::test_that("T1.3 No error when `linetype` is a non-default character string.", {
  
  gg <- adtte %>%
    visR::estimate_KM() %>%
    visR::visr() 
  
  gg %>% 
    visR::add_quantiles(quantiles = c(0.25, 0.50),
                        linetype = "solid") %>%
    testthat::expect_error(NA) 
  
})

testthat::test_that("T1.4 No error when `linetype` is `mixed`.", {
  
  gg <- adtte %>%
    visR::estimate_KM() %>%
    visR::visr() 
  
  gg %>% 
    visR::add_quantiles(quantiles = c(0.25, 0.50),
                        linetype = "mixed") %>%
    testthat::expect_error(NA) 
  
})

testthat::test_that("T1.5 No error when `linecolour` is a non-default character string.", {
  
  gg <- adtte %>%
    visR::estimate_KM() %>%
    visR::visr() 
  
  gg %>% 
    visR::add_quantiles(quantiles = c(0.25, 0.50),
                        linetype = "solid",
                        linecolour = "red") %>%
    testthat::expect_error(NA) 
  
})

testthat::test_that("T1.6 No error when `linecolour` is `strata`.", {
  
  gg <- adtte %>%
    visR::estimate_KM() %>%
    visR::visr() 
  
  gg %>% 
    visR::add_quantiles(quantiles = c(0.25, 0.50),
                        linetype = "solid",
                        linecolour = "strata") %>%
    testthat::expect_error(NA) 
  
})

# Requirement T2 ---------------------------------------------------------------

testthat::context("add_quantiles - T1. tbd.")

testthat::test_that("T1.1 No error when the default parameters are used.", {
  
  gg <- adtte %>%
    visR::estimate_KM() %>%
    visR::visr() 
  
  gg %>% 
    visR::add_quantiles() %>%
    testthat::expect_error(NA) 
  
})