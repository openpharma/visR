#' @title Specifications test-Surv_CNSR.R
#' @section Last updated by: Steven Haesendonckx
#' @section Last update date: 15JUN2022
#'
#' @section List of tested specifications
#' T1. The function returns a Surv object
#' T1.1 The function returns a Surv object
#' T1.2 The function is compatible with the survival package
#' T1.3 The results of the estimation match between Surv_CNSR and Surv with inverted censoring
#' T2. The function relies on the presence of two numeric variables, specified through AVAL and CNSR, to be present in the envrionment in which they are called
#' T2.1 An error when column name specified through AVAL is not present in the environment
#' T2.2 A warning when the column specified through AVAL has negative values
#' T2.3 A warning when the column specified through AVAL has negative values
#' T2.4 An error when the column name specified through CNSR is not present in the environment
#' T2.5 An error when the column name specified through CNSR in the environment is not numeric

# Requirement T1 ----------------------------------------------------------

testthat::context("Surv_CNSR - T1. The function returns a Surv object")

testthat::test_that("T1.1 The function returns a Surv object", {
  testthat::expect_error(surv1 <- with(adtte, visR::Surv_CNSR()), NA)
  testthat::expect_error(surv2 <- with(adtte, visR::Surv_CNSR()), NA)
  testthat::expect_true(inherits(surv1, "Surv"))
  testthat::expect_true(inherits(surv2, "Surv"))
})

testthat::test_that("T1.2 The function is compatible with the survival package", {
  testthat::expect_error(survival::survfit(visR::Surv_CNSR() ~ 1, data = adtte), NA)
  testthat::expect_error(survival::survfit(visR::Surv_CNSR() ~ SEX, data = adtte), NA)

  testthat::expect_error(adtte %>% visR::estimate_KM(formula = Surv_CNSR() ~ 1), NA)
  testthat::expect_error(adtte %>% visR::estimate_KM(formula = Surv_CNSR() ~ SEX), NA)

  testthat::expect_error(survival::survfit(visR::Surv_CNSR(AVAL, CNSR) ~ 1, data = adtte), NA)
  testthat::expect_error(survival::survfit(visR::Surv_CNSR(AVAL, CNSR) ~ SEX, data = adtte), NA)

  # WHEN THIS TEST FAILS, THAT IS OUR SIGNAL THAT {survival} HAS BEEN UPDATED ON CRAN!!
  # AT THAT POINT WE SHOULD DO THE FOLLOWING
  # 1. UPDATE THIS UNIT TEST TO ASSURE THERE IS _NO_ ERROR WITH coxph()
  # 2. ADD A MIN VERSION REQUIREMENT FOR THE {survival} PACKAGE
  #    THIS CAN BE DONE IN TWO WAYS:
  #      1. in the DESCRIPTION file
  #      2. using rlang::check_installed("survival", version = <add required version number>)
  testthat::expect_error(survival::coxph(visR::Surv_CNSR() ~ SEX, data = adtte))
})


testthat::test_that("T1.3 The results of the estimation match between Surv_CNSR and Surv with inverted censoring", {
  testthat::expect_equal(
    with(adtte, visR::Surv_CNSR()),
    with(adtte, survival::Surv(AVAL, 1 - CNSR))
  )

  km1 <- adtte %>% visR::estimate_KM(formula = visR::Surv_CNSR() ~ 1)
  km2 <- adtte %>% visR::estimate_KM()
  km1$call <- km2$call <- NULL
  testthat::expect_equal(km1, km2)

  km1 <- adtte %>% visR::estimate_KM(formula = visR::Surv_CNSR() ~ SEX)
  km2 <- adtte %>% visR::estimate_KM(strata = "SEX")
  km1$call <- km2$call <- NULL
  testthat::expect_equal(km1, km2)
})

# Requirement T2 ----------------------------------------------------------

testthat::test_that("T2.1 An error when column name specified through AVAL is not present in the environment", {
  testthat::expect_true(! "AVAL" %in% colnames(survival::lung))
  testthat::expect_error(survival::survfit(visR::Surv_CNSR() ~ 1, data = survival::lung))
  testthat::expect_error(survival::survfit(visR::Surv_CNSR() ~ sex, data = survival::lung))

  adtte[["AVAL"]] <- NULL
  testthat::expect_error(survival::survfit(visR::Surv_CNSR() ~ 1, data = adtte))
})

testthat::test_that("T2.2 An error when column name specified through AVAL in the environment is not numeric", {
  adtte[["AVAL"]] <- as.character(adtte[["AVAL"]])
  testthat::expect_error(survival::survfit(visR::Surv_CNSR() ~ 1, data = adtte))

  testthat::expect_error(survival::survfit(visR::Surv_CDISC(AVAL = time) ~ 1, data = survival::lung %>% dplyr::mutate(AVAL = as.character(time))))
})

testthat::test_that("T2.3 A warning when the column specified through AVAL has negative values", {
  testthat::expect_warning(survival::survfit(visR::Surv_CNSR() ~ 1, data = adtte %>% dplyr::mutate(AVAL = AVAL - 10000)))
})

testthat::test_that("T2.4 An error when the column name specified through CNSR is not present in the environment", {
  testthat::expect_true(! "CNSR" %in% colnames(survival::lung))
  testthat::expect_error(survival::survfit(visR::Surv_CNSR() ~ 1, data = survival::lung %>% dplyr::rename(AVAL = time)))
  testthat::expect_error(survival::survfit(visR::Surv_CNSR() ~ sex, data = survival::lung %>% dplyr::rename(AVAL = time)))

  adtte[["CNSR"]] <- NULL
  testthat::expect_error(survival::survfit(visR::Surv_CNSR() ~ 1, data = adtte))
})

testthat::test_that("T2.5 An error when the column name specified through CNSR in the environment is not numeric", {
  adtte[["CNSR"]] <- as.character(adtte[["CNSR"]])
  testthat::expect_error(survival::survfit(visR::Surv_CNSR() ~ 1, data = adtte))

  testthat::expect_error(survival::survfit(visR::Surv_CDISC(AVAL = time) ~ 1, data = survival::lung %>% dplyr::mutate(CNSR = as.character(status))))
})

# END OF CODE -------------------------------------------------------------
