#' @title Specifications get_risktable.survfit
#' @section Last updated by:
#' Steven Haesendonckx
#' @section Last update date:
#' 10-APR-2021

# Specifications ----------------------------------------------------------

#' T1. The function accepts a `survfit` object
#' T1.1 No error when a `survfit` object is passed to the function
#' T1.2 An error when a non-`survfit` object is passed to the function
#' 
#' T2. The function relies on the presence of two numeric variables, specified through `AVAL` and `CNSR`, to be present in `data`
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
#' T3.4 When only 1 stratum is specified, the stratum names are added to the `names` attribute
#' T3.5 When more than 1 strata is specified, the stratum names are available in the `names` attribute
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
#' T7.2 The function prefixes the function call with survival



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

