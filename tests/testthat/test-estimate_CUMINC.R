#' @title Specifications estimate_cuminc
#' @section Last updated by: Joana Barros
#' @section Last update date: 05-FEB-2022

# Specifications ----------------------------------------------------------

#' @section List of tested specifications
#' T1. The function accepts a `data.frame` `tibble` or `data.table`
#' T1.1 No error when `data` is of class `data.frame`
#' T1.2 No error when `data` is of class `tibble`
#' T1.3 No error when `data` is of class `data.table`
#' T1.4 An error when `data` is of an unexpected class, eg `list`
#' 
#' 

# ------------------------------------------------------------- 
#' T1. The function wraps `tidycmprsk::cuminc()` appropriately
#' T1.1 No errors `estimate_cuminc()`
#' T1.2 No errors `estimate_cuminc()` sister functions

# Requirement T1 ----------------------------------------------------------

testthat::context("estimate_cuminc - T1. The function accepts a `data.frame` `tibble` or `data.table`")

testthat::test_that("T1.1 No error when `data` is of class `data.frame`", {
  
  data <- tidycmprsk::trial
  testthat::expect_error(
    visR::estimate_cuminc(data, CNSR = "death_cr", AVAL = "ttdeath"), NA)
  
})

testthat::test_that("T1.2 No error when `data` is of class `tibble`", {
  
  data <- dplyr::as_tibble(tidycmprsk::trial)
  testthat::expect_error(
    visR::estimate_cuminc(data, CNSR = "death_cr", AVAL = "ttdeath"), NA)
  
})

testthat::test_that("T1.3 No error when `data` is of class `data.table`", {
  
  if (nzchar(find.package("data.table"))){
    data <- data.table::as.data.table(tidycmprsk::trial)
    testthat::expect_error(
      visR::estimate_cuminc(data, CNSR = "death_cr", AVAL = "ttdeath"), NA)
  }
  
})

testthat::test_that("T1.4 An error when `data` is of an unexpected class, eg `list`", {
  
  data <- base::as.list(tidycmprsk::trial)
  testthat::expect_error(
    visR::estimate_cuminc(data, CNSR = "death_cr", AVAL = "ttdeath"))
  
})


# ------------------------------------------------------------- 
context("estimate_cuminc - T1. The function wraps `tidycmprsk::cuminc()` appropriately")

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

# END OF CODE ----------------------------------------------------------

