#' @title Specifications test-utils_table.R
#' @section Last updated by: Tim Treis (tim.treis@@outlook.de)
#' @section Last update date: 2022-02-09T15:22:32
#'
#' @section List of tested specifications
#' NA
#' T1. Correct values for summarize_long.numeric
#' T2. Correct values for summarize_long.factors
#' T3. Correct values for summarize_long.default
#' T4. Correct values for summarize_short.numeric
#' T5. Correct values for summarize_short.factor and summarize_short.string

# Requirement T1 ----------------------------------------------------------

testthat::context("utils_table - T1. Correct values for summarize_long.numeric")

test_that("T1.1. Correct mean values for numeric values", {
  values <- 1:5
  summary <- visR::summarize_long(values)
  testthat::expect_equal(summary[[1]]$mean, mean(values))
})

test_that("T1.2. Correct min value for numeric values", {
  values <- 1:5
  summary <- visR::summarize_long(values)
  testthat::expect_equal(summary[[1]]$min, min(values))
})

test_that("T1.3. Correct max value for numeric values", {
  values <- 1:5
  summary <- visR::summarize_long(values)
  testthat::expect_equal(summary[[1]]$max, max(values))
})

test_that("T1.4. Correct Q1 value for numeric values", {
  values <- 1:5
  summary <- visR::summarize_long(values)
  testthat::expect_equal(summary[[1]]$Q1, stats::quantile(values)[2])
})

test_that("T1.5. Correct Q3 value for numeric values", {
  values <- 1:5
  summary <- visR::summarize_long(values)
  testthat::expect_equal(summary[[1]]$Q3, stats::quantile(values)[4])
})

test_that("T1.6. Correct SD value for numeric values", {
  values <- 1:5
  summary <- visR::summarize_long(values)
  testthat::expect_equal(summary[[1]]$sd, stats::sd(values))
})

test_that("T1.7. Correct median value for numeric values", {
  values <- 1:5
  summary <- visR::summarize_long(values)
  testthat::expect_equal(summary[[1]]$median, stats::median(values))
})

test_that("T1.8. Integers as correctly dispatched to summarize_long.numeric", {
  values <- 1:5
  testthat::expect_equal(
    visR::summarize_long(values),
    visR::summarize_long(as.integer(values))
  )
})

# Requirement T2 ---------------------------------------------------------------

testthat::context("utils_table - T2. Correct values for summarize_long.factors")

test_that("T2.1. Correct count of factor values", {
  values <- as.factor(c("A", "A", "B"))
  summary <- visR::summarize_long(values)
  testthat::expect_equal(summary[[1]]$`N A`, sum(values == "A"))
})

test_that("T2.2. Correct perentage of factor values", {
  values <- as.factor(c("A", "A", "B"))
  summary <- visR::summarize_long(values)
  testthat::expect_equal(
    round(summary[[1]]$`% A`, 2),
    round((sum(values == "A") / length(values)) * 100, 2)
  )
})


# Requirement T3 ---------------------------------------------------------------

testthat::context("utils_table - T3. Correct values for summarize_long.default")

test_that("T3.1. Correct count of unique values", {
  values <- c("A", "A", "B")
  summary <- visR::summarize_long(values)
  testthat::expect_equal(summary[[1]]$unique_values, length(unique(values)))
})

test_that("T3.1. Correct count of missing values", {
  values <- c("A", "A", "B")
  summary <- visR::summarize_long(values)
  testthat::expect_equal(summary[[1]]$nmiss, 0)
})

# Requirement T3 ---------------------------------------------------------------

testthat::context("utils_table - T4. Correct values for summarize_short.numeric")

test_that("T4.1. Correct mean values for numeric values in summarize_short", {
  values <- 1:5
  summary <- visR::summarize_short(values)
  num <- paste0(
    round(mean(values), 2),
    " (",
    round(stats::sd(values), 2),
    ")"
  )
  testthat::expect_equal(summary[[1]]$`Mean (SD)`, num)
})

test_that("T4.2. Correct median values for numeric values in summarize_short", {
  values <- 1:5
  summary <- visR::summarize_short(values)
  num <- paste0(
    round(stats::median(values), 2),
    " (",
    round(stats::quantile(values, probs = 0.25, na.rm = TRUE), 2),
    "-",
    round(stats::quantile(values, probs = 0.75, na.rm = TRUE), 2),
    ")"
  )
  testthat::expect_equal(summary[[1]]$`Median (IQR)`, num)
})

test_that("T4.3. Correct range values for numeric values in summarize_short", {
  values <- 1:5
  summary <- visR::summarize_short(values)
  num <- paste0(
    round(min(values), 2),
    "-",
    round(max(values), 2)
  )
  testthat::expect_equal(summary[[1]]$`Min-max`, num)
})


test_that("T4.4. Correct missing values for numeric values in summarize_short", {
  values <- 1:5
  summary <- visR::summarize_short(values)
  num <- paste0(
    sum(is.na(values)), " (",
    sum(is.na(values)) / length(values), "%)"
  )
  testthat::expect_equal(summary[[1]]$Missing, num)
})

# Requirement T5 ---------------------------------------------------------------

testthat::context("utils_table - T5. Correct values for summarize_short.factor and summarize_short.string")

test_that("T5.1. Correct value for factors in summarize_short", {
  values <- as.factor(c("A", "A", "B"))
  summary <- visR::summarize_short(values)
  num <- paste0(
    sum(values == "A"), " (",
    round(sum(values == "A") / length(values) * 100, 1), "%)"
  )
  testthat::expect_equal(summary[[1]]$A, num)
})

test_that("T5.2. Correct default value in summarize_short", {
  values <- c("A", "A", "B")
  summary <- visR::summarize_short(values)
  num <- paste0(
    sum(values == "A"), " (",
    round(sum(values == "A") / length(values) * 100, 1), "%)"
  )
  testthat::expect_equal(
    summary[[1]]$`Unique values`,
    as.character(sum(values == "A"))
  )
})

# END OF CODE -------------------------------------------------------------
