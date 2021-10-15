#' @title Specifications get_pvalue

# Specifications ----------------------------------------------------------

#' T1. The function accepts a `survfit` object with at least 2 strata
#' T1.1 No error when a `survfit` object is passed to the function with at least 2 strata
#' T1.2 An error when a `survfit` object is passed to the function with 1 strata
#' T1.3 An error when a non-`survfit` object is passed to the function
#' T2. The functions tests the null hypothesis of no difference between two or more survival curves using the G-rho family of tests
#' T2.1 The function supports the Log-Rank test by setting ptype = 'Log-Rank'
#' T2.2 The function supports the Wilcoxon test by setting ptype = 'Wilcoxon'
#' T2.3 The function supports the Tarone-Ware test by setting ptype = 'Tarone-Ware'
#' T2.4 The function calculates the default ptype when ptype = 'All'
#' T2.5 The function supports the use of a custom `rho` in the calculation
#' T2.6 The function supports the use of a custom `rho` in the calculation when ptype = `All`
#' T2.7 The function accepts a vector for ptype, containing multiple requests eg c('Log-Rank', Tarone-Ware', 'Custom')
#' T2.8 An error when ptype includes "Custom" but rho is not specified
#' T2.9 An error when a non-supported ptype is requested
#' T3. The functions returns the results of the test statistic, degrees of freedom and the p-value of the null hypothesis when requested
#' T3.1 No error when the test statistic is requested
#' T3.2 No error when the degrees of freedom is requested
#' T3.3 No error when the p-value is requested
#' T3.4 An error when an unsupported argument is used in statlist
#' T4. The output object provides the requested information
#' T4.1 The output object is a data frame
#' T4.2 The summary measures are available via the columns of the output object
#' T4.3 Each test statistic and associated calculations are available via the rows of the output object
#' T4.4 The statistical tests are ordered in line with the order defined in ptype
#' T4.5 The associated calculations of the statistical tests are ordered in line with the order defined in the statlist
#' T4.6 The Chisq statistic has the same precision as the pvalue

# Requirement T1 ----------------------------------------------------------

context("get_pvalue - T1. The function accepts a `survfit` object")

testthat::test_that("T1.1. No error when a `survfit` object is passed to the function with at least 2 strata",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_error(visR::get_pvalue(survfit_object), NA)
})

testthat::test_that("TT1.2 An error when a `survfit` object is passed to the function with 1 strata",{

  survfit_object <- visR::estimate_KM(adtte, strata = "STUDYID")
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

testthat::test_that("T2.3 The function supports the Tarone-Ware test by setting ptype = 'Tarone-Ware'",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_equal(visR::get_pvalue(survfit_object, ptype = 'Tarone-Ware'), get_pvalue_ref3)
})

testthat::test_that("T2.4 The function calculates the default ptype when ptype = 'All'",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_equal(visR::get_pvalue(survfit_object, ptype = 'All'), get_pvalue_ref[1:3,])
})

testthat::test_that("T2.5 The function supports the use of a custom `rho` in the calculation",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_equal(visR::get_pvalue(survfit_object, ptype = "Custom", rho = 2.4), get_pvalue_ref4)
})

testthat::test_that("T2.6 The function supports the use of a custom `rho` in the calculation when ptype = `All`",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_equal(visR::get_pvalue(survfit_object, ptype = "All", rho = 2.4), get_pvalue_ref)
})

testthat::test_that("T2.7 The function accepts a vector for ptype, containing multiple requests eg c('Tarone-Ware', 'Log-Rank', 'Custom')",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_equal(visR::get_pvalue(survfit_object, ptype = c('Log-Rank', 'Tarone-Ware', 'Custom'), rho = 2.4), get_pvalue_ref134)
})

testthat::test_that("T2.8 An error when ptype includes 'Custom' but rho is not specified",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_error(visR::get_pvalue(survfit_object, ptype = "Custom"))
})

testthat::test_that("T2.9 An error when a non-supported ptype is requested",{

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

testthat::test_that("The Chisq statistic has the same precision as the pvalue",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  totest <- visR::get_pvalue(survfit_object, statlist = c("Chisq", "pvalue"))

  testthat::expect_identical( nchar(sapply(strsplit(as.character(totest[["p-value"]]), "\\."), "[[", 2))
                             ,nchar(sapply(strsplit(as.character(totest[["Chisq"]]), "\\."), "[[", 2)))
  
  survfit_object <- visR::estimate_KM(adtte, strata = "SEX")
  totest <- visR::get_pvalue(survfit_object, statlist = c("Chisq", "pvalue"))

  testthat::expect_identical( nchar(sapply(strsplit(as.character(totest[["p-value"]]), "\\."), "[[", 2))
                             ,nchar(sapply(strsplit(as.character(totest[["Chisq"]]), "\\."), "[[", 2)))
})

# END OF CODE ----------------------------------------------------------

