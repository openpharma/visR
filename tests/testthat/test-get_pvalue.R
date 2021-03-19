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
#' T2. The functions tests the null hypothesis of no difference between two or more survival curves using the G-rho family of tests
#' T2.1 The function supports the Log-Rank test by setting ptype = 'Log-Rank'
#' T2.2 The function supports the Wilcoxon test by setting ptype = 'Wilcoxon'
#' T2.3 The function supports the Tarone-Ware test by setting ptype = 'Tarone-Ware'
#' T2.4 The function calculates the default ptype when ptype = 'All'
#' T2.5 The function supports the use of a custom `rho` in the calculation
#' T2.6 The function accepts a vector for ptype, containing multiple requests eg c('Tarone-Ware', 'Log-Rank', 'Custom')
#' T2.7 An error when ptype includes "Custom" but rho is not specified
#' T2.8 An error when a non-supported ptype is requested
#'
#' results are from survdiff
#' T2.3 The output object is sorted according to ptype and statlist
#' p values are formatted
#'
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

  survdiff(formula = survival::Surv(AVAL, 1 - CNSR) ~ TRTA, data = adtte, rho=0)


  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_error(visR::get_pvalue(survfit_object, ptype = "Log-Rank"), NA)

})

testthat::test_that("T2.2 The function supports the Wilcoxon test by setting ptype = 'Wilcoxon'",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_error(visR::get_pvalue(survfit_object, ptype = "Wilcoxon"), NA)

})

testthat::test_that("T2.3 The function supports the Wilcoxon test by setting ptype = 'Tarone-Ware'",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_error(visR::get_pvalue(survfit_object, ptype = "Tarone-Ware"), NA)

})


testthat::test_that("T2.4 The function supports the Wilcoxon test by setting ptype = 'All'",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_error(visR::get_pvalue(survfit_object, ptype = "All"), NA)

})

testthat::test_that("T2.5 The function supports the use of a custom `rho` in the calculation",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_error(visR::get_pvalue(survfit_object, ptype = "Custom", rho = 2.4), NA)

})


testthat::test_that("T2.6 The function accepts a vector for ptype, containing multiple requests eg c('Tarone-Ware', 'Log-Rank', 'Custom')",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_error(visR::get_pvalue(survfit_object, ptype = c('Tarone-Ware', 'Log-Rank', 'Custom'), rho = 2.4), NA)

})



#'
#' T2.7 An error when ptype includes "Custom" but rho is not specified
#' T2.8 An error when a non-supported ptype is requested

  if (is.null(ptype))
    stop("Specify a valid ptype.")
  if (!base::any(c("Log-Rank", "Wilcoxon", "Tarone-Ware", "Custom", "All") %in% ptype))
    stop("Specify a valid type")
  if ("Custom" %in% ptype & is.null(rho))
    stop("ptype = `Custom`. Please, specify rho.")
  if (is.null(statlist) |
      !base::all(statlist %in% c("test", "df", "Chisq", "p")))
    stop("Specify valid `statlist` arguments.")


# END OF CODE ----------------------------------------------------------

