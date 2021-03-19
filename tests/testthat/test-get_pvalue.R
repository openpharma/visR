#' @title Specifications get_pvalue
#' @section Last updated by:
#' Steven Haesendonckx
#' @section Last update date:
#' 19-MAR-2021

# Specifications ----------------------------------------------------------

#' T1. The function accepts a `survfit` object with at least 2 strata
#' T1.1 No error when a `survfit` object is passed to the function with at least 2 strata
#' T1.2 An error when a `survfit` object is passed to the function with 1 strata
#' T1.3 An error when a non-`survfit` object is passed to the function
#'
#'
#' T2.1 An error when colname specified through `AVAL` is not present in `data`
#' T2.2 An error when colname specified through `AVAL` is not numeric
#' T2.3 No error when the colname specified through `AVAL` is not the proposed default
#' T2.4 An error when colname specified through `CNSR` is not present in `data`
#' T2.5 An error when colname specified through `CNSR` is not numeric
#' T2.6 No error when the colname specified through `CNSR` is not the proposed default
#' T3. The user can specify strata
#' T3.1 An error when the columns, specifying the strata are not available in `data`
#' T3.2 No error when strata is NULL
#' T3.3 When no strata are specified, an artificial strata is created 'Overall'
#' T4. The function removes all rows with NA values inside any of the strata
#' T5. The function does not alter the calculation of survival::survfit
#' T5.1 The function gives the same results as survival::survfit
#' T5.2 The function adds timepoint = 0
#' T5.3 The function allows additional arguments to be passed, specific for survival::survfit
#' T5.4 The function returns an object of class `survfit`
#' T6. The function adds additional information to the survfit object when available
#' T6.1 The calculation is not affected by the addition of additional parameters
#' T6.2 The function add PARAM/PARAMCD when available
#' T7. The function call supports traceability
#' T7.1 The function updates call$data when magrittr pipe is used

# Requirement T1 ----------------------------------------------------------

context("get_pvalue - T1. The function accepts a `survfit` object")

testthat::test_that("T1.1. No error when a `survfit` object is passed to the function with at least 2 strata",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_error(visR::get_pvalue(survfit_object), NA)

})

testthat::test_that("TT1.2 An error when a `survfit` object is passed to the function with 1 strata",{

  survfit_object <- visR::estimate_KM(adtte, strata = "STUDYID")
  testthat::expect_error(visR::get_pvalue(survfit_object))

  names(survfit_object$strata)

})

testthat::test_that("TT1.3 An error when a non-`survfit` object is passed to the function",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  class(survfit_object) <- "blah"
  testthat::expect_error(visR::get_pvalue(survfit_object))

})




# END OF CODE ----------------------------------------------------------

