#' @title Specifications get_COX_HR
#' @section Last updated by:
#' Steven Haesendonckx
#' @section Last update date:
#' 19-APR-2021

# Specifications ----------------------------------------------------------

#' T1. The function accepts a `survfit` object
#' T1.1 No error when the input is a `survfit` object
#' T1.2 An error when the input is a non-`survfit` object
#' T1.3 An error when the input is NULL
#' T2. The function accepts an update to the original formula of the `survfit` object
#' T2.1 No error when the update_formula argument is a `formula`
#' T2.2 An error when the update_formula argument is not a `formula`
#' T3. The function calculate the COX Hazard Ratio
#' 
#' 


# Requirement T1 ----------------------------------------------------------

context("get_COX_HR - T1. The function accepts a `survfit` object")

testthat::test_that("T1.1 No error when the input is a `survfit` object",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_error(visR::get_COX_HR(survfit_object), NA)

})

testthat::test_that("T1.2 An error when the input is a non-`survfit` object",{

  survfit_object <- as.list("blah")
  testthat::expect_error(visR::get_COX_HR(survfit_object))

})

testthat::test_that("T1.3 An error when the input is NULL",{

  survfit_object <- NULL
  testthat::expect_error(visR::get_COX_HR(survfit_object))
})

# END OF CODE ----------------------------------------------------------

