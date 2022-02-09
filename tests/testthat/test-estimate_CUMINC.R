#' @title Specifications test-estimate_CUMINC.R
#' @section Last updated by: Daniel Sjoberg (danield.sjoberg@@gmail.com)
#' @section Last update date: 2022-01-15T10:56:12
#'
#' @section List of tested specifications
#' T1. The function wraps `tidycmprsk::cuminc()` appropriately
#' T1.1. No errors `estimate_cuminc()`
#' T1.2. No errors `estimate_cuminc()` sister functions

# Requirement T1 ----------------------------------------------------------

testthat::context("estimate_cuminc - T1. The function wraps `tidycmprsk::cuminc()` appropriately")

testthat::test_that("T1.1. No errors `estimate_cuminc()`", {
  expect_error(
    estimate_cuminc(tidycmprsk::trial, CNSR = "death_cr", AVAL = "ttdeath") %>%
      visr() %>%
      add_CI() %>%
      add_CNSR() %>%
      add_risktable(),
    NA
  )

  expect_error(
    cuminc1 <-
      estimate_cuminc(tidycmprsk::trial, strata = "trt", CNSR = "death_cr", AVAL = "ttdeath"),
    NA
  )

  expect_equal(
    cuminc1[c("failcode", "cmprsk", "conf.level", "tidy")],
    tidycmprsk::cuminc(tidycmprsk::Surv(ttdeath, death_cr) ~ trt,
                       tidycmprsk::trial)[c("failcode", "cmprsk", "conf.level", "tidy")]
  )
})

testthat::test_that("T1.2. No errors `estimate_cuminc()` sister functions", {
  cuminc1 <-
    estimate_cuminc(tidycmprsk::trial, strata = "trt",
                    CNSR = "death_cr", AVAL = "ttdeath")

  expect_error(cuminc1_visr <- visr(cuminc1), NA)

  expect_error(
    cuminc1_visr %>%
      add_CI() %>%
      add_CNSR() %>%
      add_risktable(),
    NA
  )

  expect_error(
    cuminc1_visr %>%
      add_risktable(statlist = c("n.risk", "n.event")),
    NA
  )

  expect_error(
    cuminc1_visr %>%
      add_risktable(statlist = c("n.risk", "n.event"),
                    group = "statlist"),
    NA
  )

  expect_error(
    cuminc1_visr %>%
      add_risktable(statlist = c("n.risk", "n.event"),
                    label = c("No. at Risk", "No. Events")),
    NA
  )

  expect_error(
    cuminc1_visr %>%
      add_risktable(statlist = c("n.risk", "n.event"),
                    label = c("No. at Risk", "No. Events"),
                    group = "statlist"),
    NA
  )

  expect_error(
    cuminc1_visr %>%
      add_risktable(statlist = c("n.risk", "n.event"),
                    label = c("No. at Risk", "No. Events"),
                    group = "statlist",
                    collapse = TRUE),
    NA
  )

  expect_error(
    cuminc1_visr %>%
      add_risktable(statlist = c("n.risk", "n.event"),
                    label = c("No. at Risk", "No. Events"),
                    collapse = TRUE),
    NA
  )
})

# END OF CODE -------------------------------------------------------------
