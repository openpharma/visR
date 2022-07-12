#' @title Specifications test-stat_stepribbon.R
#' @section Last updated by: Daniel Sjoberg (danield.sjoberg@@gmail.com)
#' @section Last update date: 2022-07-12T04:47:32
#'
#' @section List of tested specifications
#' T1. visR::stat_stepribbon() accepts aes() args ymin and ymax.
#' T1.1 visR::stat_stepribbon() inherits aes from ggplot()
#' T1.2 visR::stat_stepribbon() respects aes from visR::stat_stepribbon()
#' T2. visR::stat_stepribbon() works with the color/fill aesthetic.
#' T2.1 The step ribbon is correctly applied when there are multiple lines from the color/fill aesthetic
#' T3. visR::stat_stepribbon(direction=) argument is respected.
#' T3.1 The visR::stat_stepribbon(direction=) argument is able to produce figures based on both argument options.

# Requirement T1 ----------------------------------------------------------

testthat::context("stat_stepribbon - T1. visR::stat_stepribbon() accepts aes() args ymin and ymax.")

test_that("T1.1 visR::stat_stepribbon() inherits aes from ggplot()", {
  survfit_tidy <-
    survival::survfit(survival::Surv(time, status) ~ 1, data = survival::lung) %>%
    survival::survfit0() %>%
    broom::tidy()

  testthat::expect_error(
    gg_step1 <-
      survfit_tidy %>%
      ggplot2::ggplot(ggplot2::aes(x = time, y = estimate, ymin = conf.low, ymax = conf.high)) +
      ggplot2::geom_step() +
      ggplot2::geom_ribbon(stat = "stepribbon", alpha = 0.2),
    NA
  )
  vdiffr::expect_doppelganger("geom_ribbon-stepribbon-ggplot-aes", gg_step1)

  testthat::expect_error(
    gg_step2 <-
      survfit_tidy %>%
      ggplot2::ggplot(ggplot2::aes(x = time, y = estimate, ymin = conf.low, ymax = conf.high)) +
      ggplot2::geom_step() +
      stat_stepribbon(alpha = 0.2),
    NA
  )
  vdiffr::expect_doppelganger("stat_stepribbon-ggplot-aes", gg_step2)
})

test_that("T1.2 visR::stat_stepribbon() respects aes from visR::stat_stepribbon()", {
  survfit_tidy <-
    survival::survfit(survival::Surv(time, status) ~ 1, data = survival::lung) %>%
    survival::survfit0() %>%
    broom::tidy()

  testthat::expect_error(
    gg_step1 <-
      survfit_tidy %>%
      ggplot2::ggplot(ggplot2::aes(x = time, y = estimate)) +
      ggplot2::geom_step() +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = conf.low, ymax = conf.high),
                           stat = "stepribbon", alpha = 0.2),
    NA
  )
  vdiffr::expect_doppelganger("geom_ribbon-stepribbon-geom_ribbon-aes", gg_step1)

  testthat::expect_error(
    gg_step2 <-
      survfit_tidy %>%
      ggplot2::ggplot(ggplot2::aes(x = time, y = estimate)) +
      ggplot2::geom_step() +
      stat_stepribbon(ggplot2::aes(ymin = conf.low, ymax = conf.high), alpha = 0.2),
    NA
  )
  vdiffr::expect_doppelganger("stat_stepribbon-stat_stepribbon-aes", gg_step2)
})


# Requirement T2 ----------------------------------------------------------

testthat::context("stat_stepribbon - visR::stat_stepribbon() works with the color/fill aesthetic.")

test_that("T2.1 The step ribbon is correctly applied when there are multiple lines from the color/fill aesthetic.", {
  survfit_tidy <-
    survival::survfit(survival::Surv(time, status) ~ sex, data = survival::lung) %>%
    survival::survfit0() %>%
    broom::tidy()

  testthat::expect_error(
    gg_step1 <-
      survfit_tidy %>%
      ggplot2::ggplot(ggplot2::aes(x = time, y = estimate,
                                   ymin = conf.low, ymax = conf.high,
                                   color = strata)) +
      ggplot2::geom_step() +
      ggplot2::geom_ribbon(stat = "stepribbon", alpha = 0.2),
    NA
  )
  vdiffr::expect_doppelganger("geom_ribbon-stepribbon-aes-with-color", gg_step1)
})

# Requirement T3 ----------------------------------------------------------

testthat::context("stat_stepribbon - visR::stat_stepribbon(direction=) argument is respected.")

test_that("T3.1 The visR::stat_stepribbon(direction=) argument is able to produce figures based on both argument options.", {
  survfit_tidy <-
    survival::survfit(survival::Surv(time, status) ~ 1, data = survival::lung) %>%
    survival::survfit0() %>%
    broom::tidy()

  testthat::expect_error(
    gg_step1 <-
      survfit_tidy %>%
      ggplot2::ggplot(ggplot2::aes(x = time, y = estimate,
                                   ymin = conf.low, ymax = conf.high)) +
      ggplot2::geom_step() +
      stat_stepribbon(ggplot2::aes(ymin = conf.low, ymax = conf.high),
                      alpha = 0.2, direction = "vh"),
    NA
  )
  vdiffr::expect_doppelganger("geom_ribbon-stepribbon-direction-vh", gg_step1)

  testthat::expect_error(
    gg_step2 <-
      survfit_tidy %>%
      ggplot2::ggplot(ggplot2::aes(x = time, y = estimate,
                                   ymin = conf.low, ymax = conf.high)) +
      ggplot2::geom_step() +
      stat_stepribbon(ggplot2::aes(ymin = conf.low, ymax = conf.high),
                      alpha = 0.2, direction = "hv"),
    NA
  )
  vdiffr::expect_doppelganger("geom_ribbon-stepribbon-direction-hv", gg_step2)
})
