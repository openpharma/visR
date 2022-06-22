#' @title Specifications test-get_quantile.R
#' @section Last updated by: Tim Treis (tim.treis@@outlook.de)
#' @section Last update date: 2022-02-09T15:22:32
#'
#' @section List of tested specifications
#' T1. The function accepts a `survfit` object
#' T1.1 No error when a `survfit` object is passed to the function with at least 2 strata
#' T1.2 An error when a `survfit` object is passed to the function with 1 strata
#' T1.3 An error when a non-`survfit` object is passed to the function
#' T1.4 An error when `survfit_object` does not exist in the global environment
#' T2. The function accepts a tolerance limit
#' T2.1 An error when the tolerance is not numeric
#' T2.2 No error when the tolerance is numeric
#' T3. The function accepts a numeric vector specifying the probabilities
#' T3.1 No error when the probability vector is numeric and contains values below or equal to 1
#' T3.2 An error when the probabilities are not numeric
#' T3.3 An error when the probabilities requested are above 1
#' T4. The function accepts a logical argument to request for the confidence intervals of the quantiles
#' T4.1 No error when the argument to request confidence intervals is logical
#' T4.2 An error when the argument to request confidence intervals is not logical
#' T4.3 An error when the confidence intervals are requested, but not estimated in the `survfit` object
#' T5. The function is a wrapper around quantile method for `survfit` objects
#' T5.1 The get_quantiles provides the same information as get_quantiles
#' T6. The function returns a dataframe with the requested information
#' T6.1 The function returns a dataframe
#' T6.2 The output contains a column with the strata names
#' T6.3 The output contains a column with the quantities
#' T6.4 The output contains columns with the requested quantiles

# Requirement T1 ----------------------------------------------------------

testthat::context("get_pvalue - T1. The function accepts a `survfit` object")

testthat::test_that("T1.1 No error when a `survfit` object is passed to the function with at least 2 strata", {
  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_error(visR::get_quantile(survfit_object), NA)
})

testthat::test_that("T1.2 An error when a `survfit` object is passed to the function with 1 strata", {
  survfit_object <- visR::estimate_KM(adtte, strata = NULL)
  testthat::expect_error(visR::get_quantile(survfit_object), NA)
})

testthat::test_that("T1.3 An error when a non-`survfit` object is passed to the function", {
  testthat::expect_error(visR::get_quantile(adtte))
})

testthat::test_that("T1.4 An error when `survfit_object` does not exist in the global environment", {
  testthat::expect_error(blah %>% visR::get_quantile())
  testthat::expect_error(visR::get_quantile(survfit_object = blah))
})

# Requirement T2 ---------------------------------------------------------------

testthat::context("get_pvalue - T2. The function accepts a tolerance limit")

testthat::test_that("T2.1 An error when the tolerance is not numeric", {
  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_error(visR::get_quantile(survfit_object, tolerance = "blah"))
})

testthat::test_that("T2.2 No error when the tolerance is numeric", {
  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_error(visR::get_quantile(survfit_object,
    tolerance = 0.000000000001
  ), NA)
})

# Requirement T3 ---------------------------------------------------------------

testthat::context("get_pvalue - T3. The function accepts a numeric vector specifying the probabilities")

testthat::test_that("T3.1 No error when the probability vector is numeric and contains values below or equal to 1", {
  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_error(visR::get_quantile(survfit_object,
    probs = c(0.50, 0.10)
  ), NA)
})

testthat::test_that("T3.2 An error when the probabilities are not numeric", {
  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_error(visR::get_quantile(survfit_object, probs = "blah"))
})

testthat::test_that("T3.3 An error when the probabilities requested are above 1", {
  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_error(visR::get_quantile(survfit_object,
    probs = c(0.50, 1.00, 3.00)
  ))
})

# Requirement T4 ---------------------------------------------------------------

testthat::context("get_pvalue - T4. The function accepts a logical argument to request for the confidence intervals of the quantiles")

testthat::test_that("T4.1 No error when the argument to request confidence intervals is logical", {
  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_error(visR::get_quantile(survfit_object, conf.int = TRUE), NA)
})

testthat::test_that("T4.2 An error when the argument to request confidence intervals is not logical", {
  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_error(visR::get_quantile(survfit_object, conf.int = "blah"))
})

testthat::test_that("T4.3 An error when the confidence intervals are requested, but not estimated in the `survfit` object", {
  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA", conf.int = FALSE)
  testthat::expect_error(visR::get_quantile(survfit_object))
})

# Requirement T5 ---------------------------------------------------------------

testthat::context("get_pvalue - T5. The function is a wrapper around quantile method for `survfit` objects")

testthat::test_that("T5.1 The get_quantiles provides the same information as get_quantiles", {
  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  quant_surv <- quantile(survfit_object)

  q <- do.call(rbind.data.frame, quant_surv)

  strata <- as.character(unlist(lapply(quant_surv, rownames)))
  quantity <- unlist(lapply(strsplit(rownames(q), "\\.", fixed = FALSE), `[[`, 1))

  final <- data.frame(
    cbind(strata, quantity, q),
    row.names = NULL,
    check.names = FALSE
  )

  final <- final[order(final[, "strata"], final[, "quantity"]), ]

  testthat::expect_equal(visR::get_quantile(survfit_object), final)
})

# Requirement T6 ---------------------------------------------------------------

testthat::context("get_pvalue - T6. The function returns a dataframe with the requested information")

testthat::test_that("T6.1 The function returns a dataframe", {
  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  quant <- visR::get_quantile(survfit_object, conf.int = TRUE)

  testthat::expect_true(inherits(quant, "data.frame"))
})

testthat::test_that("T6.2 The output contains a column with the strata names", {
  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  quant <- visR::get_quantile(survfit_object, conf.int = TRUE)

  stratNm <- unique(names(survfit_object[["strata"]]))

  testthat::expect_equal(as.character(stratNm), as.character(unique(quant[["strata"]])))
})

testthat::test_that("T6.3 The output contains a column with the quantities", {
  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  quant <- visR::get_quantile(survfit_object, conf.int = TRUE)

  quaNm <- c("lower", "quantile", "upper")

  testthat::expect_equal(as.character(quaNm), as.character(unique(quant[["quantity"]])))
})

testthat::test_that("T6.4 The output contains columns with the requested quantiles", {
  probs <- c(0.50, 0.10)
  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  quant <- visR::get_quantile(survfit_object, probs = probs)

  colNm <- colnames(quant)[-which(colnames(quant) %in% c("strata", "quantity"))]

  testthat::expect_true(base::all(as.character(probs * 100) %in% colNm))
})

# END OF CODE -------------------------------------------------------------
