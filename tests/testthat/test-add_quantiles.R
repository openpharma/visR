#' @title Specifications test-add_quantiles.R
#' @section Last updated by: Tim Treis (tim.treis@@outlook.de)
#' @section Last update date: 2022-02-09T15:22:32
#'
#' @section List of tested specifications
#' T1. The quantile lines are y-axis-transformation dependent.
#' T1.1 No error when the default `surv` option is used.
#' T1.2 No error when a function is passed as a string.
#' T1.3 No error when a `.Primitive` function is used.
#' T1.4 No error when a custom function is used.
#' T2. If any parameter is specified incorrectly, defaults are used instead.
#' T2.1 A warning if `quantiles` is not a numeric or vector of numerics.
#' T2.2 A warning if `linetype` is not a character string.
#' T2.3 A warning if `linecolour` is not a character string.
#' T2.4 A warning if `alpha` is not a character string or outside [0, 1].
#' T2.5 No error when the default parameters are used.
#' T2.6 No error when `quantiles` is a single number.
#' T2.7 No error when `quantiles` is a numeric vector.
#' T2.8 No error when `linetype` is a non-default character string.
#' T2.9 No error when `linetype` is `mixed`.
#' T2.10 No error when `linecolour` is a non-default character string.
#' T2.11 No error when `linecolour` is `strata`.

# Requirement T1 ----------------------------------------------------------

testthat::context("add_quantiles - T1. The quantile lines are y-axis-transformation dependent.")

testthat::test_that("T1.1 No error when the default `surv` option is used.", {
  adtte %>%
    visR::estimate_KM() %>%
    visR::visr() %>% # defaults to fun = "surv"
    visR::add_quantiles() %>%
    testthat::expect_error(NA)
})

testthat::test_that("T1.2 No error when a function is passed as a string.", {
  adtte %>%
    visR::estimate_KM() %>%
    visR::visr(fun = "log") %>%
    visR::add_quantiles() %>%
    testthat::expect_error(NA)

  adtte %>%
    visR::estimate_KM() %>%
    visR::visr(fun = "event") %>%
    visR::add_quantiles() %>%
    testthat::expect_error(NA)

  suppressWarnings(
    adtte %>%
      visR::estimate_KM() %>%
      visR::visr(fun = "cloglog")
  ) %>%
    visR::add_quantiles() %>%
    testthat::expect_error(NA)

  adtte %>%
    visR::estimate_KM() %>%
    visR::visr(fun = "pct") %>%
    visR::add_quantiles() %>%
    testthat::expect_error(NA)

  adtte %>%
    visR::estimate_KM() %>%
    visR::visr(fun = "logpct") %>%
    visR::add_quantiles() %>%
    testthat::expect_error(NA)

  adtte %>%
    visR::estimate_KM() %>%
    visR::visr(fun = "cumhaz") %>%
    visR::add_quantiles() %>%
    testthat::expect_error(NA)
})

testthat::test_that("T1.3 No error when a `.Primitive` function is used.", {
  adtte %>%
    visR::estimate_KM() %>%
    visR::visr(fun = log, y_label = "neded_since_fun_not_string") %>%
    visR::add_quantiles() %>%
    testthat::expect_error(NA)
})

testthat::test_that("T1.4 No error when a custom function is used.", {
  suppressWarnings(
    adtte %>%
      visR::estimate_KM() %>%
      visR::visr(
        fun = function(x) log(-log(x)),
        y_label = "neded_since_fun_not_string"
      )
  ) %>%
    visR::add_quantiles() %>%
    testthat::expect_error(NA)
})

# Requirement T2 ---------------------------------------------------------------

testthat::context("add_quantiles - T2. If any parameter is specified incorrectly, defaults are used instead.")

testthat::test_that("T2.1 A warning if `quantiles` is not a numeric or vector of numerics.", {
  gg <- adtte %>%
    visR::estimate_KM() %>%
    visR::visr()

  expected_warning <- "Invalid argument for `quantiles`, must be numeric. Setting it to 0.5."

  gg %>%
    visR::add_quantiles(quantiles = NULL) %>%
    testthat::expect_warning(expected_warning)

  gg %>%
    visR::add_quantiles(quantiles = "visR") %>%
    testthat::expect_warning(expected_warning)

  gg %>%
    visR::add_quantiles(quantiles = c(0.2, "visR", 0.5)) %>%
    testthat::expect_warning(expected_warning)
})

testthat::test_that("T2.2 A warning if `linetype` is not a character string.", {
  gg <- adtte %>%
    visR::estimate_KM() %>%
    visR::visr()

  expected_warning <- "Invalid argument for `linetype`, must be a character string. Setting it to default 'dashed'."

  gg %>%
    visR::add_quantiles(linetype = NULL) %>%
    testthat::expect_warning(expected_warning)

  gg %>%
    visR::add_quantiles(linetype = 1) %>%
    testthat::expect_warning(expected_warning)
})

testthat::test_that("T2.3 A warning if `linecolour` is not a character string.", {
  gg <- adtte %>%
    visR::estimate_KM() %>%
    visR::visr()

  expected_warning <- "Invalid argument for `linecolour`, must be a character string. Setting it to default 'grey50'."

  gg %>%
    visR::add_quantiles(linecolour = NULL) %>%
    testthat::expect_warning(expected_warning)

  gg %>%
    visR::add_quantiles(linecolour = 1) %>%
    testthat::expect_warning(expected_warning)
})

testthat::test_that("T2.4 A warning if `alpha` is not a character string or outside [0, 1].", {
  gg <- adtte %>%
    visR::estimate_KM() %>%
    visR::visr()

  expected_warning <- "Invalid `alpha` argument, must be between 0 and 1. Setting it to 1.0."

  gg %>%
    visR::add_quantiles(alpha = "visR") %>%
    testthat::expect_warning(expected_warning)

  gg %>%
    visR::add_quantiles(alpha = -1) %>%
    testthat::expect_warning(expected_warning)

  gg %>%
    visR::add_quantiles(alpha = 10) %>%
    testthat::expect_warning(expected_warning)
})


testthat::test_that("T2.5 No error when the default parameters are used.", {
  gg <- adtte %>%
    visR::estimate_KM() %>%
    visR::visr()

  gg %>%
    visR::add_quantiles() %>%
    testthat::expect_error(NA)
})

testthat::test_that("T2.6 No error when `quantiles` is a single number.", {
  gg <- adtte %>%
    visR::estimate_KM() %>%
    visR::visr()

  gg %>%
    visR::add_quantiles(quantiles = 0.5) %>%
    testthat::expect_error(NA)
})

testthat::test_that("T2.7 No error when `quantiles` is a numeric vector.", {
  gg <- adtte %>%
    visR::estimate_KM() %>%
    visR::visr()

  gg %>%
    visR::add_quantiles(quantiles = c(0.25, 0.50)) %>%
    testthat::expect_error(NA)
})

testthat::test_that("T2.8 No error when `linetype` is a non-default character string.", {
  gg <- adtte %>%
    visR::estimate_KM() %>%
    visR::visr()

  gg %>%
    visR::add_quantiles(
      quantiles = c(0.25, 0.50),
      linetype = "solid"
    ) %>%
    testthat::expect_error(NA)
})

testthat::test_that("T2.9 No error when `linetype` is `mixed`.", {
  gg <- adtte %>%
    visR::estimate_KM() %>%
    visR::visr()

  gg %>%
    visR::add_quantiles(
      quantiles = c(0.25, 0.50),
      linetype = "mixed"
    ) %>%
    testthat::expect_error(NA)
})

testthat::test_that("T2.10 No error when `linecolour` is a non-default character string.", {
  gg <- adtte %>%
    visR::estimate_KM() %>%
    visR::visr()

  gg %>%
    visR::add_quantiles(
      quantiles = c(0.25, 0.50),
      linetype = "solid",
      linecolour = "red"
    ) %>%
    testthat::expect_error(NA)
})

testthat::test_that("T2.11 No error when `linecolour` is `strata`.", {
  gg <- adtte %>%
    visR::estimate_KM() %>%
    visR::visr()

  gg %>%
    visR::add_quantiles(
      quantiles = c(0.25, 0.50),
      linetype = "solid",
      linecolour = "strata"
    ) %>%
    testthat::expect_error(NA)
})

# END OF CODE -------------------------------------------------------------
