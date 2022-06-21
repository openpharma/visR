#' @title Specifications test-tableone.R
#' @section Last updated by: Tim Treis (tim.treis@@outlook.de)
#' @section Last update date: 2022-02-09T15:22:32
#'
#' @section List of tested specifications
#' T1. The function accepts a `data.frame` as the main `data` object.
#' T1.1 No error when `data` is of class `data.frame`.
#' T1.2 An error when `data` is not of class `data.frame`.
#' T2. The function accepts additional parameters.
#' T2.1 No error when `title` is not `NULL`.
#' T2.2 No error when `datasource` is not `NULL`.
#' T2.3 No error when `footnote` is not `NULL`.
#' T2.4 No error when `strata` is not `NULL`.
#' T2.5 No error when `overall` is a `logical`.
#' T2.6 An error when `overall` is not a `logical`.

# Requirement T1 ----------------------------------------------------------

testthat::context("tableone - T1. The function accepts a `data.frame` as the main `data` object.")

testthat::test_that("T1.1 No error when `data` is of class `data.frame`.", {
  adtte %>%
    visR::tableone(title = NULL, datasource = NULL) %>%
    testthat::expect_error(NA)
})

testthat::test_that("T1.2 An error when `data` is not of class `data.frame`.", {
  1 %>%
    visR::tableone(title = NULL, datasource = NULL) %>%
    testthat::expect_error()

  "visR" %>%
    visR::tableone(title = NULL, datasource = NULL) %>%
    testthat::expect_error()

  c(1, 2, 3) %>%
    visR::tableone(title = NULL, datasource = NULL) %>%
    testthat::expect_error()

  NULL %>%
    visR::tableone(title = NULL, datasource = NULL) %>%
    testthat::expect_error()
})

# Requirement T2 ---------------------------------------------------------------

testthat::context("tableone - T2. The function accepts additional parameters.")

testthat::test_that("T2.1 No error when `title` is not `NULL`.", {
  adtte %>%
    visR::tableone(title = 1, datasource = NULL) %>%
    testthat::expect_error(NA)

  adtte %>%
    visR::tableone(title = "visR", datasource = NULL) %>%
    testthat::expect_error(NA)

  adtte %>%
    visR::tableone(title = c(1, 2, 3), datasource = NULL) %>%
    testthat::expect_error(NA)
})

testthat::test_that("T2.2 No error when `datasource` is not `NULL`.", {
  adtte %>%
    visR::tableone(title = NULL, datasource = 1) %>%
    testthat::expect_error(NA)

  adtte %>%
    visR::tableone(title = NULL, datasource = "visR") %>%
    testthat::expect_error(NA)

  adtte %>%
    visR::tableone(title = NULL, datasource = c(1, 2, 3)) %>%
    testthat::expect_error(NA)
})

testthat::test_that("T2.3 No error when `footnote` is not `NULL`.", {
  adtte %>%
    visR::tableone(title = NULL, datasource = NULL, footnote = 1) %>%
    testthat::expect_error(NA)

  adtte %>%
    visR::tableone(title = NULL, datasource = NULL, footnote = "visR") %>%
    testthat::expect_error(NA)

  adtte %>%
    visR::tableone(title = NULL, datasource = NULL, footnote = c(1, 2, 3)) %>%
    testthat::expect_error(NA)
})

testthat::test_that("T2.4 No error when `strata` is not `NULL`.", {
  adtte %>%
    visR::tableone(title = NULL, datasource = NULL, strata = "SEX") %>%
    testthat::expect_error(NA)
})

testthat::test_that("T2.5 No error when `overall` is a `logical`.", {
  adtte %>%
    visR::tableone(title = NULL, datasource = NULL, overall = TRUE) %>%
    testthat::expect_error(NA)

  adtte %>%
    visR::tableone(title = NULL, datasource = NULL, overall = FALSE) %>%
    testthat::expect_error(NA)
})

testthat::test_that("T2.6 An error when `overall` is not a `logical`.", {
  adtte %>%
    visR::tableone(title = NULL, datasource = NULL, overall = "visR") %>%
    testthat::expect_error()
})

# END OF CODE -------------------------------------------------------------
