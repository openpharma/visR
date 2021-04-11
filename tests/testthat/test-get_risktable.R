#' @title Specifications get_risktable.survfit
#' @section Last updated by:
#' Steven Haesendonckx
#' @section Last update date:
#' 10-APR-2021

# Specifications ----------------------------------------------------------

#' T1. The function accepts a `survfit` object
#' T1.1 No error when a `survfit` object is passed to the function
#' T1.2 An error when a non-`survfit` object is passed to the function
#' T2. The function accepts an argument that specifies the minimum at risk
#' T2.1 An error when the minimum at risk is negative
#' T2.2 An error when the minimum at risk is not an integer
#' T2.3 An error when the minimum at risk is larger than the maximum at risk in the data set
#' T2.4 No error when the minimum at risk is lower than the minimum at risk in the data set
#' T2.5 The minimum at risk is calculated as 0 when the minimum at risk specified is lower that the minimum at risk in the data set
#' T3. The function accepts an argument that specifies the time at which the risk set is calculated
#' T3.1 An error when the times specified are negative
#' T3.2 The function orders the times argument internally to avoid errors
#' T4. The function accepts a `statlist` to be displayed  for which labels can be specified
#' T4.1 An error when the `statlist` is not a character vector
#' T4.2 An error when the `statlist` contains non-allowed strings eg "blah"
#' T4.3 Duplicate entries are removed from `statlist`
#' T4.4 An error when the `label` is not a character vector
#' T5. The function matches the length of the `label` vector with that of the `statlist` vector
#' T5.1 The function supplies defaults to increase the length of the `label` vector to same length as the `statlist` vector 
#' T5.2 The supplied defaults for the `label` vector match the arguments specified in the `statlist`
#' T5.3 The function limits the length of the `label` vector to the length of the `statlist` vector
#' T6. The function groups the calculation by strata, by statlist or overall
#' T6.1 An error when the `group` arguments does not contain `strata` or `statlist`
#' T6.2 The calculations are grouped by strata when group = "strata"
#' T6.3 The calculations are grouped by statlist when group = "statlist"
#' T7. The function allows the calculations to be grouped overall 
#' T7.1 An error when the argument collapse is not boolean
#' T7.2 The calculations are grouped overall when collapse = TRUE
#' T7.3 No error when there is only one strata available
#' T8. The output dataset is a data.frame
#' T8.1 which columns? + addd attributes

# Requirement T1 ----------------------------------------------------------

context("get_risktable.survfit - T1. The function accepts a `survfit` object")

testthat::test_that("T1.1. No error when a `survfit` object is passed to the function",{

  survfit_object <- survival::survfit(formula = survival::Surv(AVAL, 1 - CNSR) ~ TRTA, data = adtte)
  
  testthat::expect_error(visR::get_risktable(survfit_object), NA)

})

testthat::test_that("TT1.2 An error when a non-`survfit` object is passed to the function",{

  survfit_object <- visR::estimate_KM(adtte, strata = "STUDYID")
  class(survfit_object) <- "blah"
  testthat::expect_error(visR::get_risktable(survfit_object))

})

# Requirement T2 ----------------------------------------------------------

context("get_risktable.survfit - T2. The function accepts an argument that specifies the minimum at risk")

testthat::test_that("T2.1 An error when the minimum at risk is negative",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_error(visR::get_risktable(survfit_object, min_at_risk = -5))

})

testthat::test_that("T2.2 An error when the minimum at risk is not an integer",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_error(visR::get_risktable(survfit_object, min_at_risk = 2.3))

})

testthat::test_that("T2.3 An error when the minimum at risk is larger than the maximum at risk in the data set",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_error(visR::get_risktable(survfit_object, min_at_risk = 100))

})

testthat::test_that("T2.4 No error when the minimum at risk is lower than the minimum at risk in the data set",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_error(visR::get_risktable(survfit_object, min_at_risk = 0), NA)

})

# Requirement T3 ----------------------------------------------------------

context("get_risktable.survfit - T3. The function accepts an argument that specifies the time at which the risk set is calculated")

testthat::test_that("T3.1 An error when the times specified are negative",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_error(visR::get_risktable(survfit_object, breaks = c(0,5,-20,9)))

})

testthat::test_that("T3.2 The function orders the times argument internally to avoid errors",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  breaks <- c(0,9,5,10,8,3)
  risktable <- visR::get_risktable(survfit_object, breaks = breaks)
  testthat::expect_equal(unique(risktable[["time"]]), breaks[order(breaks)])
})





# END OF CODE ----------------------------------------------------------

