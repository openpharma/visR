#' @title Specifications test-Surv_CDISC.R
#' @section Last updated by: Daniel Sjoberg
#' @section Last update date:
#'
#' @section List of tested specifications
#' T1. The function returns a `Surv` object.
#' T1.1. No error when arguments are used as indicated in documentation.
#' T2. Errors are triggered with bad argument inputs.
#' T2.1. Errors are triggered with bad argument inputs.
#' T3. Results match when not using Surv_CDISC.
#' T3.1. Surv_CDISC() and Surv() results match

# Requirement T1 ----------------------------------------------------------

testthat::context("Surv_CDISC - T1. The function returns a `Surv` object.")

testthat::test_that("T1.1. No error when arguments are used as indicated in documentation.", {
  testthat::expect_error(survival::survfit(visR::Surv_CDISC() ~ 1, data = adtte), NA)
  testthat::expect_error(survival::survfit(visR::Surv_CDISC() ~ SEX, data = adtte), NA)

  testthat::expect_error(adtte %>% visR::estimate_KM(formula = Surv_CDISC() ~ 1), NA)
  testthat::expect_error(adtte %>% visR::estimate_KM(formula = Surv_CDISC() ~ SEX), NA)

  testthat::expect_error(survival::survfit(visR::Surv_CDISC(AVAL, CNSR) ~ 1, data = adtte), NA)
  testthat::expect_error(survival::survfit(visR::Surv_CDISC(AVAL, CNSR) ~ SEX, data = adtte), NA)

  testthat::expect_error(adtte %>% visR::estimate_KM(formula = visR::Surv_CDISC(AVAL, CNSR) ~ 1), NA)
  testthat::expect_error(adtte %>% visR::estimate_KM(formula = visR::Surv_CDISC(AVAL, CNSR) ~ SEX), NA)

  testthat::expect_error(with(adtte, visR::Surv_CDISC(AVAL, CNSR)), NA)
})

# Requirement T2 ----------------------------------------------------------

testthat::context("Surv_CDISC - T2. Errors are triggered with bad argument inputs.")

testthat::test_that("T2.1. Errors are triggered with bad argument inputs.", {
  testthat::expect_error(survival::survfit(visR::Surv_CDISC() ~ 1, data = survival::lung))
  testthat::expect_error(survival::survfit(visR::Surv_CDISC() ~ sex, data = survival::lung))
  testthat::expect_error(survival::survfit(visR::Surv_CDISC() ~ 1, data = survival::lung %>% dplyr::rename(AVAL = time)))
  testthat::expect_error(survival::survfit(visR::Surv_CDISC() ~ sex, data = survival::lung %>% dplyr::rename(AVAL = time)))

  testthat::expect_error(survival::survfit(visR::Surv_CDISC(AVAL = time) ~ 1, data = survival::lung %>% dplyr::mutate(CNSR = as.character(status))))
  testthat::expect_error(survival::survfit(visR::Surv_CDISC(AVAL = time) ~ 1, data = survival::lung %>% dplyr::mutate(CNSR = status)))
  testthat::expect_warning(survival::survfit(visR::Surv_CDISC() ~ 1, data = adtte %>% dplyr::mutate(AVAL = AVAL - 10000)))
})

# Requirement T3 ----------------------------------------------------------

testthat::context("Surv_CDISC - T3. Results match when not using Surv_CDISC.")

testthat::test_that("T3.1. Surv_CDISC() and Surv() results match", {
  testthat::expect_equal(
    with(adtte, visR::Surv_CDISC()),
    with(adtte, survival::Surv(AVAL, 1 - CNSR))
  )

  km1 <- adtte %>% visR::estimate_KM(formula = visR::Surv_CDISC(AVAL, CNSR) ~ 1)
  km2 <- adtte %>% visR::estimate_KM()
  km1$call <- km2$call <- NULL
  testthat::expect_equal(km1, km2)
})
