#' @title Specifications test-get_summary.R
#' @section Last updated by: Tim Treis (tim.treis@@outlook.de)
#' @section Last update date: 2022-02-09T15:22:32
#'
#' @section List of tested specifications
#' T1. The function accepts a survival object
#' T1.1 No error when `survfit_object` is a survfit object
#' T1.2 An error when `survfit_object` is a data.frame
#' T1.3 An error when `survfit_object` is a tibble
#' T1.4 An error when `survfit_object` is a data.table
#' T1.5 An error when `survfit_object` is a random object
#' T1.6 An error when `survfit_object` is NULL
#' T2. The function accepts an argument that specifies the summaries to be displayed
#' T2.1 An error when `statlist` is NULL
#' T2.2 An error when the `statlist` contains non-allowed strings e.g. `blah`
#' T2.3 No error when the `statlist` contains arguments `strata`, `records`, `events`, `median`, `LCL`, `UCL`, or `CI`
#' T2.4 The values in the column `strata` are the same as the `strata` in the `survfit` object
#' T2.5. The values in the column `strata` contain 'Overall' when no strata are present in the `survfit` object
#' T2.6 The values in the column `No. of subjects` are the same as the values of `n` in the `survfit` object
#' T2.7 The values in the column `No. of events` are the same as the events in the `survfit` object
#' T2.8 The values in the column `Median(surv.time)` are the same as the median values in the `survfit` object
#' T2.9 The values in the column `0.95LCL` are the same as the lower confidence values in the `survfit` object
#' T2.10 The values in the column `0.95UCL` are the same as the upper confidence values in the `survfit` object
#' T2.11 The values in the column `0.95CI` are the same as the confidence intervals in the `survfit` object
#' T2.12 An error when the confidence intervals are requested and not calculated in the survival object
#' T2.13 Column name for confidence intervals changes for different confidence levels

# Requirement T1 ----------------------------------------------------------

testthat::context("get_summary - T1. The function accepts a survival object")

testthat::test_that("T1.1 No error when `survfit_object` is a survfit object", {
  survfit_object <- visR::estimate_KM(adtte, strata = "TRTP")
  testthat::expect_error(visR::get_summary(survfit_object), NA)
})

testthat::test_that("T1.2 An error when `survfit_object` is a data.frame", {
  survfit_object <- visR::adtte
  testthat::expect_error(visR::get_summary(survfit_object))
})

testthat::test_that("T1.3 An error when `survfit_object` is a tibble", {
  survfit_object <- tibble::as_tibble(visR::adtte)
  testthat::expect_error(visR::get_summary(survfit_object))
})

testthat::test_that("T1.4 An error when `survfit_object` is a data.table", {
  survfit_object <- data.table::as.data.table(visR::adtte)
  testthat::expect_error(visR::get_summary(survfit_object))
})

testthat::test_that("T1.5 An error when `survfit_object` is a random object", {
  survfit_object <- "A"
  testthat::expect_error(visR::get_summary(survfit_object))
})

testthat::test_that("T1.6 An error when `survfit_object` is NULL", {
  survfit_object <- NULL
  testthat::expect_error(visR::get_summary(survfit_object))
})

# Requirement T2 ---------------------------------------------------------------

testthat::context("get_summary - T2. The function accepts an argument that specifies the summaries to be displayed")

testthat::test_that("T2.1 An error when `statlist` is NULL", {
  survfit_object <- visR::estimate_KM(adtte, strata = "TRTP")

  testthat::expect_error(visR::get_summary(survfit_object, statlist = NULL))
})

testthat::test_that("T2.2 An error when the `statlist` contains non-allowed strings e.g. `blah`", {
  survfit_object <- visR::estimate_KM(adtte, strata = "TRTP")
  statlist <- c("strata", "blah")

  testthat::expect_error(visR::get_summary(survfit_object, statlist = statlist))
})

testthat::test_that("T2.3 No error when the `statlist` contains arguments `strata`, `records`, `events`, `median`, `LCL`, `UCL`, or `CI`", {
  survfit_object <- visR::estimate_KM(adtte, strata = "TRTP")
  statlist <- c("strata", "records", "events", "median", "LCL", "UCL", "CI")

  testthat::expect_error(visR::get_summary(survfit_object, statlist = statlist), NA)
})

testthat::test_that("T2.4 The values in the column `strata` are the same as the `strata` in the `survfit` object", {
  survfit_object <- visR::estimate_KM(adtte, strata = "TRTP")
  strata <- unlist(visR::get_summary(survfit_object, statlist = c("strata")))

  testthat::expect_equal(names(survfit_object$strata), unname(strata))
})

testthat::test_that("T2.5. The values in the column `strata` contain 'Overall' when no strata are present in the `survfit` object", {
  survfit_object <- survival::survfit(survival::Surv(AVAL, 1 - CNSR) ~ 1,
    data = adtte
  )
  strata <- unlist(visR::get_summary(survfit_object, statlist = c("strata")))

  testthat::expect_equal("Overall", unname(strata))
})

testthat::test_that("T2.6 The values in the column `No. of subjects` are the same as the values of `n` in the `survfit` object", {
  survfit_object <- visR::estimate_KM(adtte)
  strata <- unlist(visR::get_summary(survfit_object, statlist = "records"))

  testthat::expect_equal(survfit_object$n, unname(strata))
})

testthat::test_that("T2.7 The values in the column `No. of events` are the same as the events in the `survfit` object", {
  survfit_object <- visR::estimate_KM(adtte, strata = "TRTP")
  df <- data.frame(
    `n.events` = survfit_object$n.event,
    strata = rep(names(survfit_object$strata), survfit_object$strata)
  )
  aggregated_events <- stats::aggregate(n.events ~ strata, data = df, sum)$n.events
  strata <- unlist(visR::get_summary(survfit_object, statlist = c("events")))

  testthat::expect_equal(aggregated_events, unname(strata))
})

testthat::test_that("T2.8 The values in the column `Median(surv.time)` are the same as the median values in the `survfit` object", {
  survfit_object <- visR::estimate_KM(adtte, strata = "TRTP")

  df <- data.frame(
    surv = survfit_object$surv,
    time = survfit_object$time,
    strata = rep(names(survfit_object$strata), survfit_object$strata)
  )

  suppressWarnings(
    inds <- stats::aggregate(surv ~ strata, data = df, function(x) {
      min(which(x <= 0.5))
    })
  )

  strata <- unlist(visR::get_summary(survfit_object, statlist = c("median")))

  medians <- apply(inds, 1, function(ind) {
    df[df$strata == as.character(ind[1]), ][as.numeric(ind[2]), "time"]
  })

  testthat::expect_equal(medians, unname(strata))
})

testthat::test_that("T2.9 The values in the column `0.95LCL` are the same as the lower confidence values in the `survfit` object", {
  survfit_object <- visR::estimate_KM(adtte, strata = "TRTP")

  df <- data.frame(
    lower = survfit_object$lower,
    time = survfit_object$time,
    strata = rep(names(survfit_object$strata), survfit_object$strata)
  )

  suppressWarnings(
    inds <- stats::aggregate(lower ~ strata, data = df, function(x) {
      min(which(x <= 0.5))
    })
  )

  lower <- apply(inds, 1, function(ind) {
    df[df$strata == as.character(ind[1]), ][as.numeric(ind[2]), "time"]
  })

  strata <- unlist(visR::get_summary(survfit_object, statlist = c("LCL")))

  testthat::expect_equal(lower, unname(strata))
})

testthat::test_that("T2.10 The values in the column `0.95UCL` are the same as the upper confidence values in the `survfit` object", {
  survfit_object <- visR::estimate_KM(adtte, strata = "TRTP")

  df <- data.frame(
    surv = survfit_object$upper,
    time = survfit_object$time,
    strata = rep(names(survfit_object$strata), survfit_object$strata)
  )

  suppressWarnings(
    inds <- stats::aggregate(surv ~ strata, data = df, function(x) {
      min(which(x <= 0.5))
    })
  )

  upper <- apply(inds, 1, function(ind) {
    df[df$strata == as.character(ind[1]), ][as.numeric(ind[2]), "time"]
  })

  strata <- unlist(visR::get_summary(survfit_object, statlist = c("UCL")))

  testthat::expect_equal(upper, unname(strata))
})

testthat::test_that("T2.11 The values in the column `0.95CI` are the same as the confidence intervals in the `survfit` object", {
  survfit_object <- visR::estimate_KM(adtte, strata = "TRTP")

  df <- data.frame(
    lower = survfit_object$lower,
    upper = survfit_object$upper,
    time = survfit_object$time,
    strata = rep(names(survfit_object$strata), survfit_object$strata)
  )

  suppressWarnings(
    inds <- stats::aggregate(upper ~ strata, data = df, function(x) {
      min(which(x <= 0.5))
    })
  )
  upper <- apply(inds, 1, function(ind) {
    df[df$strata == as.character(ind[1]), ][as.numeric(ind[2]), "time"]
  })

  suppressWarnings(
    inds <- stats::aggregate(lower ~ strata, data = df, function(x) {
      min(which(x <= 0.5))
    })
  )
  lower <- apply(inds, 1, function(ind) {
    df[df$strata == as.character(ind[1]), ][as.numeric(ind[2]), "time"]
  })

  ci <- sapply(1:length(lower), function(i) {
    paste0("(", lower[i], ";", upper[i], ")")
  })

  strata <- unlist(visR::get_summary(survfit_object, statlist = c("CI")))

  testthat::expect_equal(ci, unname(strata))
})

testthat::test_that("T2.12 An error when the confidence intervals are requested and not calculated in the survival object", {
  survfit_object <- visR::estimate_KM(adtte, strata = "TRTP", conf.type = "none")
  suppressWarnings(
    testthat::expect_error(visR::get_summary(survfit_object, statlist = c("CI")))
  )
})

testthat::test_that("T2.13 Column name for confidence intervals changes for different confidence levels", {
  survfit_object <- visR::estimate_KM(adtte, strata = "TRTP", conf.int = 0.80)
  ci_colnames <- colnames(visR::get_summary(survfit_object, statlist = c("CI")))
  testthat::expect_equal("0.8CI", ci_colnames)
})

# END OF CODE -------------------------------------------------------------
