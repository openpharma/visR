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
#' T2.1 No error when a valid option is passed to `legend_position`.
#' T2.2 An error when the string is not amongst the valid options for `legend_position`.
#' T2.3 An error when an invalid option is passed to `legend_position`.
#' T3. The y-axis label is correctly deducted from the provided `fun` when applying `visR::visr()` to a `survfit` object.


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

testthat::test_that("T2.1 No error when a valid option is passed to `legend_position`.", {
  
  survfit_object <- adtte %>%
    visR::estimate_KM("SEX") 
  
  testthat::expect_error(survfit_object %>% visR::visr(legend_position = "top"), NA)
  testthat::expect_error(survfit_object %>% visR::visr(legend_position = "bottom"), NA)
  testthat::expect_error(survfit_object %>% visR::visr(legend_position = "right"), NA)
  testthat::expect_error(survfit_object %>% visR::visr(legend_position = "left"), NA)
  testthat::expect_error(survfit_object %>% visR::visr(legend_position = "none"), NA)
  testthat::expect_error(survfit_object %>% visR::visr(legend_position = c(0.5, 0.5)), NA)
  testthat::expect_error(survfit_object %>% visR::visr(legend_position = matrix(1,2)), NA)
  
})

testthat::test_that("T2.2 An error when the string is not amongst the valid options for `legend_position`.", {
  
  survfit_object <- adtte %>%
    visR::estimate_KM("SEX") 
  
  testthat::expect_error(survfit_object %>% visR::visr(legend_position = "visR"))
  
})

testthat::test_that("T2.3 An error when an invalid option is passed to `legend_position`.", {
  
  survfit_object <- adtte %>%
    visR::estimate_KM("SEX") 
  
  testthat::expect_error(survfit_object %>% visR::visr(legend_position = c(0)))
  testthat::expect_error(survfit_object %>% visR::visr(legend_position = c(0.5, 0.5, 0.5)))
  testthat::expect_error(survfit_object %>% visR::visr(legend_position = data.frame(1,2)))
  
})

testthat::test_that("T2.4 `.", {
  
  survfit_object <- adtte %>%
    visR::estimate_KM("SEX") 
  
  testthat::expect_error(survfit_object %>% visR::visr(legend_position = c(0)))
  testthat::expect_error(survfit_object %>% visR::visr(legend_position = c(0.5, 0.5, 0.5)))
  testthat::expect_error(survfit_object %>% visR::visr(legend_position = data.frame(1,2)))
  
})

# Requirement T3 ----------------------------------------------------------

testthat::context("visr_plot - T3. The y-axis label is correctly deducted from the provided `fun` when applying `visR::visr()` to a `survfit` object.")

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

# END ---------------------------------------------------------------------