#' @title Specifications add_quantiles.R
#' @section Last updated by: Tim Treis
#' @section Last update date: 16-Nov-2021

# Specifications ---------------------------------------------------------------

#' T1. No error when quantile lines are added to a plot.
#' T1.1 No error when the default parameters are used.
#' T1.2 No error when `quantiles` is a single number.
#' T1.3 No error when `quantiles` is a numeric vector.
#' T1.4 An error when quantiles is NULL or not a numeric.
#' T1.5 No error when `linetype` is a non-default character string.
#' T1.6 No error when `linetype` is `mixed`.
#' T1.7 An error when `linetype` is NULL or not a string.
#' T1.8 No error when `linecolour` is a non-default character string.
#' T1.9 No error when `linecolour` is `strata`.
#' T1.10 An error when `linecolour` is NULL or not a string.
#' T2. The quantile lines are y-axis-transformation dependent.
#' T2.1 No error when the default `surv` option is used.
#' T2.2 No error when a function is passed as a string.
#' T2.3 No error when a `.Primitive` function is used.
#' T2.4 No error when a custom function is used.

# Requirement T1 ---------------------------------------------------------------

testthat::context("add_quantiles - T1. No error when quantile lines are added to a plot.")

testthat::test_that("T1.1 No error when the default parameters are used.", {
  
  gg <- adtte %>%
    visR::estimate_KM() %>%
    visR::visr() 
  
  gg %>% 
    visR::add_quantiles() %>%
    testthat::expect_error(NA) 
  
})

testthat::test_that("T1.2 No error when `quantiles` is a single number.", {
  
  gg <- adtte %>%
    visR::estimate_KM() %>%
    visR::visr() 
  
  gg %>% 
    visR::add_quantiles(quantiles = 0.5) %>%
    testthat::expect_error(NA) 
  
})

testthat::test_that("T1.3 No error when `quantiles` is a numeric vector.", {
  
  gg <- adtte %>%
    visR::estimate_KM() %>%
    visR::visr() 
  
  gg %>% 
    visR::add_quantiles(quantiles = c(0.25, 0.50)) %>%
    testthat::expect_error(NA) 
  
})

testthat::test_that("T1.4 An error when quantiles is NULL or not a numeric.", {
  
  gg <- adtte %>%
    visR::estimate_KM() %>%
    visR::visr() 
  
  gg %>% 
    visR::add_quantiles(quantiles = c()) %>%
    testthat::expect_error() 
  
  gg %>% 
    visR::add_quantiles(quantiles = "visR") %>%
    testthat::expect_error() 
  
  gg %>% 
    visR::add_quantiles(quantiles = NULL) %>%
    testthat::expect_error() 
  
})

testthat::test_that("T1.5 No error when `linetype` is a non-default character string.", {
  
  gg <- adtte %>%
    visR::estimate_KM() %>%
    visR::visr() 
  
  gg %>% 
    visR::add_quantiles(quantiles = c(0.25, 0.50),
                        linetype = "solid") %>%
    testthat::expect_error(NA) 
  
})

testthat::test_that("T1.6 No error when `linetype` is `mixed`.", {
  
  gg <- adtte %>%
    visR::estimate_KM() %>%
    visR::visr() 
  
  gg %>% 
    visR::add_quantiles(quantiles = c(0.25, 0.50),
                        linetype = "mixed") %>%
    testthat::expect_error(NA) 
  
})

testthat::test_that("T1.7 An error when `linetype` is NULL or not a string.", {
  
  gg <- adtte %>%
    visR::estimate_KM() %>%
    visR::visr() 
  
  gg %>% 
    visR::add_quantiles(linetype = NULL) %>%
    testthat::expect_error() 
  
  gg %>% 
    visR::add_quantiles(linetype = 2) %>%
    testthat::expect_error() 
  
})

testthat::test_that("T1.8 No error when `linecolour` is a non-default character string.", {
  
  gg <- adtte %>%
    visR::estimate_KM() %>%
    visR::visr() 
  
  gg %>% 
    visR::add_quantiles(quantiles = c(0.25, 0.50),
                        linetype = "solid",
                        linecolour = "red") %>%
    testthat::expect_error(NA) 
  
})

testthat::test_that("T1.9 No error when `linecolour` is `strata`.", {
  
  gg <- adtte %>%
    visR::estimate_KM() %>%
    visR::visr() 
  
  gg %>% 
    visR::add_quantiles(quantiles = c(0.25, 0.50),
                        linetype = "solid",
                        linecolour = "strata") %>%
    testthat::expect_error(NA) 
  
})

testthat::test_that("T1.10 An error when `linecolour` is NULL or not a string.", {
  
  gg <- adtte %>%
    visR::estimate_KM() %>%
    visR::visr() 
  
  gg %>% 
    visR::add_quantiles(linecolour = NULL) %>%
    testthat::expect_error() 
  
})

# Requirement T2 ---------------------------------------------------------------

testthat::context("add_quantiles - T2. The quantile lines are y-axis-transformation dependent.")

testthat::test_that("T2.1 No error when the default `surv` option is used.", {
  
  adtte %>%
    visR::estimate_KM() %>%
    visR::visr() %>% # defaults to fun = "surv" 
    visR::add_quantiles() %>%
    testthat::expect_error(NA) 
  
})

testthat::test_that("T2.2 No error when a function is passed as a string.", {
  
  adtte %>%
    visR::estimate_KM() %>%
    visR::visr(fun = "log") %>% 
    visR::add_quantiles() %>%
    testthat::expect_error(NA) 
  
  adtte %>%
    visR::estimate_KM() %>%
    visR::visr(fun = "event") %>% 
    visR::add_quantiles() %>%
    testthat::expect_error(NA) 
  
  suppressWarnings(
    adtte %>%
      visR::estimate_KM() %>%
      visR::visr(fun = "cloglog")
  ) %>%
    visR::add_quantiles() %>%
    testthat::expect_error(NA) 
  
  adtte %>%
    visR::estimate_KM() %>%
    visR::visr(fun = "pct") %>% 
    visR::add_quantiles() %>%
    testthat::expect_error(NA) 
  
  adtte %>%
    visR::estimate_KM() %>%
    visR::visr(fun = "logpct") %>% 
    visR::add_quantiles() %>%
    testthat::expect_error(NA) 
  
  adtte %>%
    visR::estimate_KM() %>%
    visR::visr(fun = "cumhaz") %>% 
    visR::add_quantiles() %>%
    testthat::expect_error(NA) 
  
})

testthat::test_that("T2.3 No error when a `.Primitive` function is used.", {
  
  adtte %>%
    visR::estimate_KM() %>%
    visR::visr(fun = log, y_label = "neded_since_fun_not_string") %>% 
    visR::add_quantiles() %>%
    testthat::expect_error(NA) 
  
})

testthat::test_that("T2.4 No error when a custom function is used.", {
  
  suppressWarnings(
    adtte %>%
      visR::estimate_KM() %>%
      visR::visr(fun = function(x) log(-log(x)), 
                 y_label = "neded_since_fun_not_string") 
  ) %>% 
    visR::add_quantiles() %>%
    testthat::expect_error(NA) 
  
})
