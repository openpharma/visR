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
#' T3. The function calculates the COX Hazard Ratio
#' T3.1 The function produces the same results as survival::coxph
#' T3.2 The function returns a tidied data frame 

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

# Requirement T2 ----------------------------------------------------------

context("get_COX_HR - T1. The function accepts a `survfit` object")

testthat::test_that("T2.1 No error when the update_formula argument is a `formula`",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  formula <- ". ~ . + SEX"
  testthat::expect_error(visR::get_COX_HR(survfit_object, update_formula = formula), NA)

})

testthat::test_that("T2.2 An error when the update_formula argument is not a `formula`",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  formula <- as.list("&?")
  testthat::expect_error(visR::get_COX_HR(survfit_object, update_formula = formula))

})

# Requirement T3 ----------------------------------------------------------

context("get_COX_HR - T3. The function calculates the COX Hazard Ratio")

testthat::test_that("T3.1 No error when the update_formula argument is a `formula`",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  formula <- ". ~ . + SEX"
  tidy_coxph_visR <- visR::get_COX_HR(survfit_object, update_formula = formula)
  
  coxph <- survival::coxph(formula = survival::Surv(AVAL, 1 - CNSR) ~ TRTA + SEX, data = adtte)
  tidy_coxph_traditional <- broom::tidy(coxph)
  
  testthat::expect_equal(tidy_coxph_traditional, tidy_coxph_visR)

})

testthat::test_that("T3.2 The function returns a tidied data frame ",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  formula <- ". ~ . + SEX"
  tidy_coxph_visR <- visR::get_COX_HR(survfit_object, update_formula = formula)
  
  testthat::expect_true(inherits(tidy_coxph_visR, c("data.frame", "tbl_df", "tbl")))
})

# END OF CODE ----------------------------------------------------------

