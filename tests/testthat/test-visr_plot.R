#' @title Specifications visr_plot
#' @section Last updated by:
#' Tim Treis
#' @section Last update date:
#' 25-MAY-2021

# Specifications ----------------------------------------------------------

#' T1. `visR::visr()` only accepts `survfit` or `attrition`.
#' T1.1 No error when applied to a `survfit` object.
#' T1.2 No error when applied to a `attrition` object.
#' T1.3 An error when applied to an object that is not `survfit` or `attrition`.
#' T2. No invalid parameters are used when applying `visR::visr()` to a `survfit` object.
#' T2.1 No error when `NULL`, a `character` string or an `expression` is passed to `x_label`.
#' T2.2 No error when `NULL`, a `character` string or an `expression` is passed to `y_label`.
#' T2.3 No error when `NULL` or a `character` string is passed to `x_units`.
#' T2.4 No error when `NULL` or a `numeric` is passed to `x_ticks`.
#' T2.5 No error when `NULL` or a `numeric` is passed to `y_ticks`.
#' T2.6 No error when a valid option is passed to `legend_position`.
#' T2.7 An error when `x_label` is not `NULL`, a `character` string or an `expression`.
#' T2.8 An error when `y_label` is not `NULL`, a `character` string or an `expression`.
#' T2.9 An error when `x_units` is not `NULL` or a `character` string.
#' T2.10 An error when `x_ticks` is not `NULL` or a `numeric`.
#' T2.11 An error when `y_ticks` is not `NULL` or a `numeric`.
#' T2.12 No error when a valid option is passed to `legend_position`.
#' T2.13 An error when the string is not amongst the valid options for `legend_position`.
#' T2.14 An error when an invalid option is passed to `legend_position`.
#' T3. The y-axis label is correctly deducted from the provided `fun` when applying `visR::visr()` to a `survfit` object.
#' T3.1 No error when `y_label` is `NULL` and `fun` is one of the valid string options.
#' T3.2 An error when `y_label` is `NULL`, `fun` is a string but not one of the valid options.
#' T3.3 No error when `y_label` is a string and `fun` is a function.
#' T3.4 An error when `y_label` is `NULL` and `fun` is a function.
#' T3.5 A warning when the provided function causes undefined values, f.e. log(-log(2)).


# Requirement T1 ----------------------------------------------------------

testthat::context("visr_plot - T1. visR::visr() only accepts `survfit` or `attrition`.")

testthat::test_that("T1.1 No error when applied to a `survfit` object.", {
  
  survfit_object <- adtte %>%
    visR::estimate_KM("SEX") 
  
  testthat::expect_error(survfit_object %>% visR::visr(), NA)
  
})

testthat::test_that("T1.2 No error when applied to a `attrition` object.", {
  
  attrition_object <- adtte %>%
    visR::get_attrition(criteria_descriptions = c("1. Placebo Group", 
                                                  "2. Be 75 years of age or older.",
                                                  "3. White", 
                                                  "4. Site 709"),
                        criteria_conditions   = c("TRTP=='Placebo'",
                                                  "AGE>=75",
                                                  "RACE=='WHITE'", 
                                                  "SITEID==709"),
                        subject_column_name   = 'USUBJID') 
  
  testthat::expect_error(attrition_object %>% visR::visr("Criteria", "Remaining N"), NA)
  
})

testthat::test_that("T1.3 An error when applied to an object that is not `survfit` or `attrition`.", {
  
  testthat::expect_error("visR" %>% visR::visr())
  testthat::expect_error(1 %>% visR::visr())
  testthat::expect_error(NA %>% visR::visr())
  testthat::expect_error(TRUE %>% visR::visr())
  testthat::expect_error(list() %>% visR::visr())
  testthat::expect_error(stats::lm(AGE ~ TRTDUR, adtte) %>% visR::visr())
  
})

# Requirement T2 ----------------------------------------------------------

testthat::context("visr_plot - No invalid parameters are used when applying `visR::visr()` to a `survfit` object.")

testthat::test_that("T2.1 No error when `NULL`, a `character` string or an `expression` is passed to `x_label`.", {
  
  survfit_object <- adtte %>%
    visR::estimate_KM("SEX") 
  
  testthat::expect_error(survfit_object %>% visR::visr(), NA) # Equal to x_label = NULL
  testthat::expect_error(survfit_object %>% visR::visr(x_label = NULL), NA)
  testthat::expect_error(survfit_object %>% visR::visr(x_label = "visR"), NA)
  testthat::expect_error(survfit_object %>% visR::visr(x_label = expression(sum(x, a, b))), NA)
  
})

testthat::test_that("T2.2 No error when `NULL`, a `character` string or an `expression` is passed to `y_label`.", {
  
  survfit_object <- adtte %>%
    visR::estimate_KM("SEX") 
  
  testthat::expect_error(survfit_object %>% visR::visr(), NA) # Equal to y_label = NULL
  testthat::expect_error(survfit_object %>% visR::visr(y_label = NULL), NA)
  testthat::expect_error(survfit_object %>% visR::visr(y_label = "visR"), NA)
  testthat::expect_error(survfit_object %>% visR::visr(y_label = expression(sum(x, a, b))), NA)
  
})

testthat::test_that("T2.3 No error when `NULL` or a `character` string is passed to `x_units`.", {
  
  survfit_object <- adtte %>%
    visR::estimate_KM("SEX") 
  
  testthat::expect_error(survfit_object %>% visR::visr(), NA) # Equal to y_label = NULL
  testthat::expect_error(survfit_object %>% visR::visr(x_units = NULL), NA)
  testthat::expect_error(survfit_object %>% visR::visr(x_units = "visR"), NA)
  
})

testthat::test_that("T2.4 No error when `NULL` or a `numeric` is passed to `x_ticks`.", {
  
  survfit_object <- adtte %>%
    visR::estimate_KM("SEX") 
  
  testthat::expect_error(survfit_object %>% visR::visr(), NA) # Equal to y_label = NULL
  testthat::expect_error(survfit_object %>% visR::visr(x_ticks = NULL), NA)
  testthat::expect_error(survfit_object %>% visR::visr(x_ticks = 1), NA)
  testthat::expect_error(survfit_object %>% visR::visr(x_ticks = c(0, 100)), NA)
  testthat::expect_error(survfit_object %>% visR::visr(x_ticks = seq(0, 100, 20)), NA)
  
})

testthat::test_that("T2.5 No error when `NULL` or a `numeric` is passed to `y_ticks`.", {
  
  survfit_object <- adtte %>%
    visR::estimate_KM("SEX") 
  
  testthat::expect_error(survfit_object %>% visR::visr(), NA) # Equal to y_label = NULL
  testthat::expect_error(survfit_object %>% visR::visr(y_ticks = NULL), NA)
  testthat::expect_error(survfit_object %>% visR::visr(y_ticks = 1), NA)
  testthat::expect_error(survfit_object %>% visR::visr(y_ticks = c(0, 100)), NA)
  testthat::expect_error(survfit_object %>% visR::visr(y_ticks = seq(0, 100, 20)), NA)
  
})

testthat::test_that("T2.6 No error when a valid option is passed to `legend_position`.", {
  
  survfit_object <- adtte %>%
    visR::estimate_KM("SEX") 
  
  testthat::expect_error(survfit_object %>% visR::visr(legend_position = "top"), NA)
  testthat::expect_error(survfit_object %>% visR::visr(legend_position = "bottom"), NA)
  testthat::expect_error(survfit_object %>% visR::visr(legend_position = "right"), NA)
  testthat::expect_error(survfit_object %>% visR::visr(legend_position = "left"), NA)
  testthat::expect_error(survfit_object %>% visR::visr(legend_position = "none"), NA)
  testthat::expect_error(survfit_object %>% visR::visr(legend_position = c(0.5, 0.5)), NA)
  
})

testthat::test_that("T2.7 An error when `x_label` is not `NULL`, a `character` string or an `expression`.", {
  
  survfit_object <- adtte %>%
    visR::estimate_KM("SEX") 
  
  testthat::expect_error(survfit_object %>% visR::visr(x_label = 1))
  testthat::expect_error(survfit_object %>% visR::visr(x_label = NA))
  testthat::expect_error(survfit_object %>% visR::visr(x_label = TRUE))
  testthat::expect_error(survfit_object %>% visR::visr(x_label = list()))
  testthat::expect_error(survfit_object %>% visR::visr(x_label = stats::lm(AGE ~ TRTDUR, adtte)))
  
})

testthat::test_that("T2.8 An error when `y_label` is not `NULL`, a `character` string or an `expression`.", {
  
  survfit_object <- adtte %>%
    visR::estimate_KM("SEX") 
  
  testthat::expect_error(survfit_object %>% visR::visr(y_label = 1))
  testthat::expect_error(survfit_object %>% visR::visr(y_label = NA))
  testthat::expect_error(survfit_object %>% visR::visr(y_label = TRUE))
  testthat::expect_error(survfit_object %>% visR::visr(y_label = list()))
  testthat::expect_error(survfit_object %>% visR::visr(y_label = stats::lm(AGE ~ TRTDUR, adtte)))
  
})

testthat::test_that("T2.9 An error when `x_units` is not `NULL` or a `character` string.", {
  
  survfit_object <- adtte %>%
    visR::estimate_KM("SEX") 
  
  testthat::expect_error(survfit_object %>% visR::visr(x_units = 1))
  testthat::expect_error(survfit_object %>% visR::visr(x_units = NA))
  testthat::expect_error(survfit_object %>% visR::visr(x_units = TRUE))
  testthat::expect_error(survfit_object %>% visR::visr(x_units = list()))
  testthat::expect_error(survfit_object %>% visR::visr(x_units = stats::lm(AGE ~ TRTDUR, adtte)))
  
})

testthat::test_that("T2.10 An error when `x_ticks` is not `NULL` or a `numeric`.", {
  
  survfit_object <- adtte %>%
    visR::estimate_KM("SEX") 
  
  testthat::expect_error(survfit_object %>% visR::visr(x_ticks = "visR"))
  testthat::expect_error(survfit_object %>% visR::visr(x_ticks = NA))
  testthat::expect_error(survfit_object %>% visR::visr(x_ticks = TRUE))
  testthat::expect_error(survfit_object %>% visR::visr(x_ticks = list()))
  testthat::expect_error(survfit_object %>% visR::visr(x_ticks = stats::lm(AGE ~ TRTDUR, adtte)))
  
})

testthat::test_that("T2.11 An error when `y_ticks` is not `NULL` or a `numeric`.", {
  
  survfit_object <- adtte %>%
    visR::estimate_KM("SEX") 
  
  testthat::expect_error(survfit_object %>% visR::visr(y_ticks = "visR"))
  testthat::expect_error(survfit_object %>% visR::visr(y_ticks = NA))
  testthat::expect_error(survfit_object %>% visR::visr(y_ticks = TRUE))
  testthat::expect_error(survfit_object %>% visR::visr(y_ticks = list()))
  testthat::expect_error(survfit_object %>% visR::visr(y_ticks = stats::lm(AGE ~ TRTDUR, adtte)))
  
})

testthat::test_that("T2.12 No error when a valid option is passed to `legend_position`.", {
  
  survfit_object <- adtte %>%
    visR::estimate_KM("SEX") 
  
  testthat::expect_error(survfit_object %>% visR::visr(legend_position = "top"), NA)
  testthat::expect_error(survfit_object %>% visR::visr(legend_position = "bottom"), NA)
  testthat::expect_error(survfit_object %>% visR::visr(legend_position = "right"), NA)
  testthat::expect_error(survfit_object %>% visR::visr(legend_position = "left"), NA)
  testthat::expect_error(survfit_object %>% visR::visr(legend_position = "none"), NA)
  testthat::expect_error(survfit_object %>% visR::visr(legend_position = c(0.5, 0.5)), NA)
  
})

testthat::test_that("T2.13 An error when the string is not amongst the valid options for `legend_position`.", {
  
  survfit_object <- adtte %>%
    visR::estimate_KM("SEX") 
  
  testthat::expect_error(survfit_object %>% visR::visr(legend_position = "visR"))
  
})

testthat::test_that("T2.14 An error when an invalid option is passed to `legend_position`.", {
  
  survfit_object <- adtte %>%
    visR::estimate_KM("SEX") 
  
  testthat::expect_error(survfit_object %>% visR::visr(legend_position = c(0)))
  testthat::expect_error(survfit_object %>% visR::visr(legend_position = c(0.5, 0.5, 0.5)))
  testthat::expect_error(survfit_object %>% visR::visr(legend_position = data.frame(1,2)))
  
})

# Requirement T3 ----------------------------------------------------------

testthat::context("visr_plot - T3. The y-axis properties are correctly deducted from the provided `fun` when applying `visR::visr()` to a `survfit` object.")

testthat::test_that("T3.1 No error when `y_label` is `NULL` and `fun` is one of the valid string options.", {
  
  survfit_object <- adtte %>%
    visR::estimate_KM("SEX") 
  
  testthat::expect_error(survfit_object %>% visR::visr(y_label = NULL, fun = "surv"), NA)
  testthat::expect_error(survfit_object %>% visR::visr(y_label = NULL, fun = "log"), NA)
  testthat::expect_error(survfit_object %>% visR::visr(y_label = NULL, fun = "event"), NA)
  testthat::expect_error(survfit_object %>% visR::visr(y_label = NULL, fun = "cloglog"), NA)
  testthat::expect_error(survfit_object %>% visR::visr(y_label = NULL, fun = "pct"), NA)
  testthat::expect_error(survfit_object %>% visR::visr(y_label = NULL, fun = "logpct"), NA)
  testthat::expect_error(survfit_object %>% visR::visr(y_label = NULL, fun = "cumhaz"), NA)
  
})

testthat::test_that("T3.2 An error when `y_label` is `NULL`, `fun` is a string but not one of the valid options.", {
  
  survfit_object <- adtte %>%
    visR::estimate_KM("SEX") 
  
  testthat::expect_error(survfit_object %>% visR::visr(y_label = NULL, fun = "visR"))
  
})

testthat::test_that("T3.3 No error when `y_label` is a string and `fun` is a function.", {
  
  survfit_object <- adtte %>%
    visR::estimate_KM("SEX") 
  
  testthat::expect_error(survfit_object %>% visR::visr(y_label = "visR", fun = log), NA)
  
})

testthat::test_that("T3.4 An error when `y_label` is `NULL` and `fun` is a function.", {
  
  survfit_object <- adtte %>%
    visR::estimate_KM("SEX") 
  
  testthat::expect_error(survfit_object %>% visR::visr(y_label = NULL, fun = log))
  
})

testthat::test_that("T3.5 A warning when the provided function causes undefined values, f.e. log(-log(2)).", {
  
  survfit_object <- adtte %>%
    visR::estimate_KM("SEX") 
  
  testthat::expect_warning(survfit_object %>% visR::visr(y_label = NULL, fun = "cloglog"))
  
})

# END ---------------------------------------------------------------------

a <- adtte %>%
  visR::estimate_KM("SEX") 

a$surv[140] <- a$surv[140]*-1

a %>% visR::visr(y_label = NULL, fun = "cloglog")
