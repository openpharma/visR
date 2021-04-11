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
#' T2.3 No error when the minimum at risk is larger than the maximum at risk in the data set
#' T2.4 No error when the minimum at risk is lower than the minimum at risk in the data set
#' T2.5 The minimum at risk is calculated as 0 when the minimum at risk specified is lower that the minimum at risk in the data set
#' T3. The function accepts an argument that specifies the time at which the risk set is calculated
#' T3.1 An error when the times specified are negative
#' T3.2 The function orders the times argument internally to avoid errors
#' T4. The function accepts a `statlist` to be displayed  for which labels can be specified
#' T4.1 An error when the `statlist` is not a character vector
#' T4.2 An error when the `statlist` contains non-allowed strings eg "blah"
#' T4.3 An error when the `label` is not a character vector
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



   ,group = "strata"
   ,collapse = FALSE
){


#' 
#' 



#' T1.1 No error when a `survfit` object is passed to the function with at least 2 strata
#' T1.2 An error when a `survfit` object is passed to the function with 1 strata

# Requirement T1 ----------------------------------------------------------

context("get_risktable.survfit - T1. The function accepts a `survfit` object")

testthat::test_that("T1.1. No error when a `survfit` object is passed to the function",{

  survfit_object <- survival::survfit(formula = survival::Surv(AVAL, 1 - CNSR) ~ TRTA, data = adtte)
  
  testthat::expect_error(visR::get_risktable(survfit_object), NA)

})

testthat::test_that("TT1.2 An error when a non-`survfit` object is passed to the function",{

  survfit_object <- visR::estimate_KM(adtte, strata = "STUDYID")
  class(survfit_object) <- "blah"
  testthat::expect_error(visR::get_pvalue(survfit_object))

})







testthat::test_that("TT1.3 An error when a non-`survfit` object is passed to the function",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  class(survfit_object) <- "blah"
  testthat::expect_error(visR::get_pvalue(survfit_object))

})


# Requirement T2 ----------------------------------------------------------

context("get_pvalue - T2. The functions tests the null hypothesis of no difference between two or more survival curves using the G-rho family of tests")

testthat::test_that("T2.1 The function supports the Log-Rank test by setting ptype = 'Log-Rank'",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_equal(visR::get_pvalue(survfit_object, ptype = "Log-Rank"), get_pvalue_ref1)

})

testthat::test_that("T2.2 The function supports the Wilcoxon test by setting ptype = 'Wilcoxon'",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_equal(visR::get_pvalue(survfit_object, ptype = 'Wilcoxon'), get_pvalue_ref2)

})

testthat::test_that("T2.3 The function supports the Wilcoxon test by setting ptype = 'Tarone-Ware'",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_equal(visR::get_pvalue(survfit_object, ptype = 'Tarone-Ware'), get_pvalue_ref3)

})

testthat::test_that("T2.4 The function supports the Wilcoxon test by setting ptype = 'All'",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_equal(visR::get_pvalue(survfit_object, ptype = 'All'), get_pvalue_ref[1:3,])

})

testthat::test_that("T2.5 The function supports the use of a custom `rho` in the calculation",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_equal(visR::get_pvalue(survfit_object, ptype = "Custom", rho = 2.4), get_pvalue_ref4)

})

testthat::test_that("T2.6 The function accepts a vector for ptype, containing multiple requests eg c('Tarone-Ware', 'Log-Rank', 'Custom')",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_equal(visR::get_pvalue(survfit_object, ptype = c('Log-Rank', 'Tarone-Ware', 'Custom'), rho = 2.4), get_pvalue_ref134)

})

testthat::test_that("T2.6 The function accepts a vector for ptype, containing multiple requests eg c('Tarone-Ware', 'Log-Rank', 'Custom')",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_equal(visR::get_pvalue(survfit_object, ptype = c('Log-Rank', 'Tarone-Ware', 'Custom'), rho = 2.4), get_pvalue_ref134)

})

testthat::test_that("T2.7 An error when ptype includes 'Custom' but rho is not specified",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_error(visR::get_pvalue(survfit_object, ptype = "Custom"))

})

testthat::test_that("T2.8 An error when a non-supported ptype is requested",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_error(visR::get_pvalue(survfit_object, ptype = "blah"))

})

# Requirement T3 ----------------------------------------------------------

context("get_pvalue - T3. The functions returns the results of the test statistic, degrees of freedom and the p-value of the null hypothesis when requested")

testthat::test_that("T3.1 No error when the test statistic is requested",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_error(visR::get_pvalue(survfit_object, statlist = "test"), NA)

})

testthat::test_that("T3.2 No error when the degrees of freedom is requested",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_error(visR::get_pvalue(survfit_object, statlist = "df"), NA)

})

testthat::test_that("T3.3 No error when the p-value is requested",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_error(visR::get_pvalue(survfit_object, statlist = "pvalue"), NA)

})

testthat::test_that("T3.4 An error when an unsupported argument is used in statlist",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_error(visR::get_pvalue(survfit_object, statlist = "blah"))

})

# Requirement T4 ----------------------------------------------------------

context("get_pvalue - T4. The output object provides the requested information")

testthat::test_that("T4.1 The output object is a data frame",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_identical(class(visR::get_pvalue(survfit_object, statlist = "test")), "data.frame")

})

testthat::test_that("T4.2 The calculated information is available via the columns of the output object",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_identical(colnames(visR::get_pvalue(survfit_object, statlist = c("test", "df"))), c("Equality across strata", "df"))

})

testthat::test_that("T4.3 Each test statistic and associated calculations are available via the rows of the output object",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_equal(nrow(visR::get_pvalue(survfit_object, statlist = c("test", "df"))), 3)

})

testthat::test_that("T4.4 The statistical tests are ordered in line with the order defined in ptype",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_identical(visR::get_pvalue(survfit_object, ptype = c("Wilcoxon", "Log-Rank"))[,1], c("Wilcoxon", "Log-Rank"))

})

testthat::test_that("T4.5 The associated calculations of the statistical tests are ordered in line with the order defined in the statlist",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_identical(colnames(visR::get_pvalue(survfit_object, statlist = c("df", "test"))), c("df", "Equality across strata"))

})
# END OF CODE ----------------------------------------------------------

