#' @title tidyme
#' @section Last updated by:
#' Tim Treis
#' @section Last update date:
#' 19-MAR-2021

# Specifications ---------------------------------------------------------------

#' T1.  The function tidies up an associated object
#' T1.1 The default method returns a tibble, tidies with broom
#' T1.2 The default method does not affect the content of the object eg `visR::tidyme(stats::lm())`
#' T1.3 The default method throws a warning and relies on broom::tidy
#' T2 The S3 method, associated with a survival object, returns a tidied survival object
#' T2.1 The S3 method, associated with a survival object, transforms the survival object into a data frame
#' T2.2 The tidied dataframe has the same content as the original survival object
#' T2.3 The tidied dataframe contains the call of the original function
#' T2.4 The tidied dataframe contains ... what else??

# Requirement T1 ---------------------------------------------------------------

context("tidyme - The function properly converts \"messy output\" into tidy tibbles")

testthat::test_that("T1.1 The `visR::tidyme(stats::lm())` transformation doesn't change the values",{
  
  lm_object <- stats::lm(data = adtte, TRTDUR ~ AVAL)
  intercept <- lm_object$coefficients[[1]]
  slope <- lm_object$coefficients[[2]]
  
  lm_tidied <- suppressMessages(visR::tidyme(lm_object))
  tidy_intercept <- lm_tidied[lm_tidied$term == "(Intercept)", "estimate"][[1]]
  tidy_slope <- lm_tidied[lm_tidied$term == "AVAL", "estimate"][[1]]
  
  testthat::expect_equal(79.62, round(intercept, 2))
  testthat::expect_equal(round(intercept, 2), round(tidy_intercept, 2))
  
  testthat::expect_equal(0.55, round(slope, 2))
  testthat::expect_equal(round(slope, 2), round(tidy_slope, 2))
  
})

testthat::test_that("T1.1 The `visR::tidyme(survival::survfit())` transformation doesn't change the values",{
  
  surv_object <- survival::survfit(data = adtte, Surv(AVAL, 1-CNSR) ~ TRTP)
  surv <- surv_object$surv[c(5, 10, 50, 100)]
  cumhaz <- surv_object$cumhaz[c(5, 10, 50, 100)]
  
  surv_object_tidied <- visR::tidyme(surv_object)
  surv_tidied <- surv_object_tidied$surv[c(5, 10, 50, 100)]
  cumhaz_tidied <- surv_object_tidied$cumhaz[c(5, 10, 50, 100)]
  
  testthat::expect_equal(c(0.94, 0.92, 0.63, 0.11), round(surv, 2))
  testthat::expect_equal(round(surv, 2), round(surv_tidied, 2))
  
  testthat::expect_equal(c(0.06, 0.09, 0.46, 2.07), round(cumhaz, 2))
  testthat::expect_equal(round(cumhaz, 2), round(cumhaz_tidied, 2))
  
})


# Requirement T2 ---------------------------------------------------------------

context("tidyme - T2. Warnings and errors are thrown when neccecary")

testthat::test_that("T2.1 No error when a `stats::lm()` model is transformed",{
  
  lm_object <- stats::lm(data = adtte, TRTDUR ~ AVAL)
  
  testthat::expect_error(visR::tidyme(lm_object), NA)
  
})

testthat::test_that("T2.2 No error when a `survival::survfit()` model is transformed",{
  
  surv_object <- survival::survfit(data = adtte, Surv(AVAL, 1-CNSR) ~ TRTP)
  
  testthat::expect_error(visR::tidyme(surv_object), NA)
  
})

testthat::test_that("T2.3 Message when a non-survfit object is transformed",{
  
  lm_object <- stats::lm(data = adtte, TRTDUR ~ AVAL)
  
  testthat::expect_message(visR::tidyme(lm_object))
  
})

# END OF CODE ------------------------------------------------------------------