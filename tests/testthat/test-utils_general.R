#' @title Specifications test-utils_general.R
#' @section Last updated by: Tim Treis (tim.treis@@outlook.de)
#' @section Last update date: 2022-02-09T15:22:32
#'
#' @section List of tested specifications
#' T1. The input number is correctly formatted.
#' T1.1 Numbers smaller than 0.001 return the string '<0.001'.
#' T1.2 Numbers greater than 0.999 return the string '>0.999'.
#' T1.3 A number in \[0.001, 0.999\] is rounded to a precision of 3 with a total width of 6 characters.

# Requirement T1 ----------------------------------------------------------

testthat::context("utils_general - T1. The input number is correctly formatted.")

testthat::test_that("T1.1 Numbers smaller than 0.001 return the string '<0.001'.", {
  input <- 0.0001
  output <- visR:::.pvalformat(input)
  testthat::expect_identical(output, "<0.001")
})

testthat::test_that("T1.2 Numbers greater than 0.999 return the string '>0.999'.", {
  input <- 0.9999
  output <- visR:::.pvalformat(input)
  testthat::expect_identical(output, ">0.999")
})

testthat::test_that("T1.3 A number in \\[0.001, 0.999\\] is rounded to a precision of 3 with a total width of 6 characters.", {
  input <- 0.314159
  output <- visR:::.pvalformat(input)

  # Precision
  testthat::expect_true(base::nchar(base::strsplit(output, "\\.")[[1]][2]) == 3)

  # Total width
  testthat::expect_true(base::nchar(output) == 6)
})

# END OF CODE -------------------------------------------------------------
