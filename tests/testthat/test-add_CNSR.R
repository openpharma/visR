#' @title Specifications test-add_CNSR.R
#' @section Last updated by: Tim Treis (tim.treis@@outlook.de)
#' @section Last update date: 2022-02-09T15:22:32
#'
#' @section List of tested specifications
#' T1. The output plots after adding confidence intervals don't differ from the reviewed plots
#' T1.1 No error when censoring is plotted for one strata with default parameters
#' T1.2 No error when censoring is plotted for more than one strata with default parameters
#' T1.3 No error when `shape` is set to an empty string.
#' T1.4 No error when `shape` is set to a numerical in [0-25].
#' T1.5 No error when `shape` is set to an atomic string.
#' T1.6 No error when `size` is set to a numerical.
#' T2. Warnings/errors in case of missing data or weird arguments are thrown.
#' T2.1 Error when object is not of class `ggsurvfit`.
#' T2.2 Warning when a character is provided as `size`
#' T2.3 Warning when NULL or NA are provided as `size`.
#' T2.4 Warning when NULL or NA are provided as `shape`.
#' T2.5 Warning when a non-atomic string is provided as `shape`.
#' T2.6 Warning when `shape` is set to a numerical outside of [0-25].
#' T2.7 A ggplot warning when a non-matching vector for `size` is specified
#' T2.8 A ggplot warning when a non-matching vector for `shape` is specified

# Requirement T1 ----------------------------------------------------------

context("add_CNSR - T1. The output plots after adding confidence intervals don't differ from the reviewed plots")

test_that("T1.1 No error when censoring is plotted for one strata with default parameters", {
  p <- adtte %>%
    visR::estimate_KM() %>%
    visR::visr()

  expect_error(p %>% visR::add_CNSR(), NA)

  skip_on_cran()
  p %>%
    visR::add_CNSR() %>%
    vdiffr::expect_doppelganger(title = "add_CNSR_T1_1_one_strata")
})

test_that("T1.2 No error when censoring is plotted for more than one strata with default parameters", {
  p <- adtte %>%
    visR::estimate_KM(strata = "TRTP") %>%
    visR::visr()

  expect_error(p %>% visR::add_CNSR(), NA)

  skip_on_cran()
  p %>%
    visR::add_CNSR() %>%
    vdiffr::expect_doppelganger(title = "add_CNSR_T1_2_TRTP")
})

test_that("T1.3 No error when `shape` is set to an empty string.", {
  p <- adtte %>%
    visR::estimate_KM(strata = "SEX") %>%
    visR::visr()

  expect_error(p %>% visR::add_CNSR(shape = ""), NA)

  skip_on_cran()
  p %>%
    visR::add_CNSR(shape = "") %>%
    vdiffr::expect_doppelganger(title = "add_CNSR_T1_3_shape_empty_string")
})

test_that("T1.4 No error when `shape` is set to a numerical in [0-25].", {
  p <- adtte %>%
    visR::estimate_KM(strata = "SEX") %>%
    visR::visr()

  expect_error(p %>% visR::add_CNSR(shape = 0), NA)
  expect_error(p %>% visR::add_CNSR(shape = 5), NA)
  expect_error(p %>% visR::add_CNSR(shape = 10), NA)
  expect_error(p %>% visR::add_CNSR(shape = 15), NA)
  expect_error(p %>% visR::add_CNSR(shape = 20), NA)
  expect_error(p %>% visR::add_CNSR(shape = 25), NA)

  skip_on_cran()
  p %>%
    visR::add_CNSR(shape = 15) %>%
    vdiffr::expect_doppelganger(title = "add_CNSR_T1_4_shape_valid_numerical")
})

test_that("T1.5 No error when `shape` is set to an atomic string.", {
  p <- adtte %>%
    visR::estimate_KM(strata = "SEX") %>%
    visR::visr()

  expect_error(p %>% visR::add_CNSR(shape = "a"), NA)
  expect_error(p %>% visR::add_CNSR(shape = "B"), NA)
  expect_error(p %>% visR::add_CNSR(shape = "0"), NA)
  expect_error(p %>% visR::add_CNSR(shape = "."), NA)
  expect_error(p %>% visR::add_CNSR(shape = "µ"), NA)

  skip_on_cran()
  p %>%
    visR::add_CNSR(shape = "µ") %>%
    vdiffr::expect_doppelganger(title = "add_CNSR_T1_5_shape_atomic_string")
})

test_that("T1.6 No error when `size` is set to a numerical.", {
  p <- adtte %>%
    visR::estimate_KM(strata = "SEX") %>%
    visR::visr()

  expect_error(p %>% visR::add_CNSR(size = -500), NA)
  expect_error(p %>% visR::add_CNSR(size = -5), NA)
  expect_error(p %>% visR::add_CNSR(size = 0), NA)
  expect_error(p %>% visR::add_CNSR(size = 5), NA)
  expect_error(p %>% visR::add_CNSR(size = 500), NA)

  skip_on_cran()
  p %>%
    visR::add_CNSR(size = -5) %>%
    vdiffr::expect_doppelganger(title = "add_CNSR_T1_7_size_negative_numerical")
  p %>%
    visR::add_CNSR(size = 0) %>%
    vdiffr::expect_doppelganger(title = "add_CNSR_T1_7_size_zero")
  p %>%
    visR::add_CNSR(size = 5) %>%
    vdiffr::expect_doppelganger(title = "add_CNSR_T1_7_size_positive_numerical")
})


# Requirement T2 ---------------------------------------------------------------

context("add_CNSR - T2. Warnings/errors in case of missing data or weird arguments are thrown.")

test_that("T2.1 Error when object is not of class `ggsurvfit`.", {
  p <- adtte %>%
    visR::estimate_KM() %>%
    visR::visr()

  p_without_ggsurvfit <- p
  class(p_without_ggsurvfit) <- class(p)[class(p) != "ggsurvfit"]

  expect_error(p %>% visR::add_CNSR(), NA)
  expect_error(p_without_ggsurvfit %>% visR::add_CNSR())
})

test_that("T2.2 Warning when a character is provided as `size`", {
  p <- adtte %>%
    visR::estimate_KM(strata = "SEX") %>%
    visR::visr()

  expected_warning <- "Invalid `size` specified. Setting it to 2."
  expect_warning(p %>% visR::add_CNSR(size = "a"), expected_warning)
})

test_that("T2.3 Warning when NULL or NA are provided as `size`.", {
  p <- adtte %>%
    visR::estimate_KM(strata = "SEX") %>%
    visR::visr()

  expected_warning <- "Invalid `size` specified. Setting it to 2."
  expect_warning(p %>% visR::add_CNSR(size = NULL), expected_warning)
  expect_warning(p %>% visR::add_CNSR(size = NA), expected_warning)
})

test_that("T2.4 Warning when NULL or NA are provided as `shape`.", {
  p <- adtte %>%
    visR::estimate_KM(strata = "SEX") %>%
    visR::visr()

  expected_warning <- "Invalid `shape` specified. Setting it to 3."
  expect_warning(p %>% visR::add_CNSR(shape = NULL), expected_warning)
  expect_warning(p %>% visR::add_CNSR(shape = NA), expected_warning)
})

test_that("T2.5 Warning when a non-atomic string is provided as `shape`.", {
  p <- adtte %>%
    visR::estimate_KM(strata = "SEX") %>%
    visR::visr()

  expected_warning <- "Invalid `shape` specified. If specifiyng a symbol, it must be a single character. Setting it to 3."
  expect_warning(p %>% visR::add_CNSR(shape = "visR"), expected_warning)
})

test_that("T2.6 Warning when `shape` is set to a numerical outside of [0-25].", {
  p <- adtte %>%
    visR::estimate_KM(strata = "SEX") %>%
    visR::visr()

  expect_warning(p %>% visR::add_CNSR(shape = -1))
  expect_warning(p %>% visR::add_CNSR(shape = 26))
})

test_that("T2.7 A ggplot warning when a non-matching vector for `size` is specified", {
  p <- adtte %>%
    visR::estimate_KM(strata = "SEX") %>%
    visR::visr()

  # Cause warning
  p %>% visR::add_CNSR(size = list("We", "let", "ggplot", "test", "lists"))

  # Catch warning
  abortive_warning <- .check_traceback_stack_for_ggplot_aesthetics_warning()

  ggplot_error <- "Aesthetics must be either length 1 or the same as the data"
  expect_match(abortive_warning, ggplot_error)
})

test_that("T2.8 A ggplot warning when a non-matching vector for `shape` is specified", {
  p <- adtte %>%
    visR::estimate_KM(strata = "SEX") %>%
    visR::visr()

  # Cause warning
  p %>% visR::add_CNSR(shape = list("We", "let", "ggplot", "test", "lists"))

  # Catch warning
  abortive_warning <- .check_traceback_stack_for_ggplot_aesthetics_warning()

  ggplot_error <- "Aesthetics must be either length 1 or the same as the data"
  expect_match(abortive_warning, ggplot_error)
})

# END OF CODE -------------------------------------------------------------
