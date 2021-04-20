#' @title Specifications get_summary
#' @section Last updated by:
#' Rebecca Albrecht
#' @section Last update date:
#' 20-APR-2021

# Specifications ----------------------------------------------------------

#' T1. utils_table - T1. Correct values for summarize.numeric
#' T1.1. Correct mean values for numerics
#' T1.2. Correct min value for numerics
#' T1.3. Correct max value for numerics
#' T1.4. Correct Q1 value for numerics
#' T1.5. Correct Q3 value for numerics
#' T1.6. Correct SD value for numerics
#' T1.7. Correct median value for numerics
#' T2. utils_table - T2. Correct values for summarize.factors
#' T2.1. Correct count of factor values
#' T2.2. Correct perentage of factor values
#' T3. utils_table - T2. Correct values for summarize.default
#' T3.1. Correct count of unique values
#' T3.2. Correct count of missing values
#' T4. utils_table - T1. Correct values for summarize_tab1.numeric
#' T4.1. Correct mean values for numerics in summarize_tab1
#' T4.2. Correct median values for numerics in summarize_tab1
#' T4.3. Correct range values for numerics in summarize_tab1
#' T4.4. Correct missing values for numerics in summarize_tab1
#' T5. utils_table - T1. Correct values for summarize_tab1.factor and summarize_tab1.string
#' T5.1. Correct value for factors in summarize_tab1
#' T5.2. Correct default value in summarize_tab1


# Requirement T1 ----------------------------------------------------------

context("T1. utils_table - T1. Correct values for summarize.numeric")

test_that("T1.1. Correct mean values for numerics", {
  values <- 1:5
  summary <- visR::summarize(values)
  testthat::expect_equal(summary[[1]]$mean, base::mean(values))
})

test_that("T1.2. Correct min value for numerics", {
  values <- 1:5
  summary <- visR::summarize(values)
  testthat::expect_equal(summary[[1]]$min, base::min(values))
})

test_that("T1.3. Correct max value for numerics", {
  values <- 1:5
  summary <- visR::summarize(values)
  testthat::expect_equal(summary[[1]]$max, base::max(values))
})

test_that("T1.4. Correct Q1 value for numerics", {
  values <- 1:5
  summary <- visR::summarize(values)
  testthat::expect_equal(summary[[1]]$Q1, stats::quantile(values)[2])
})

test_that("T1.5. Correct Q3 value for numerics", {
  values <- 1:5
  summary <- visR::summarize(values)
  testthat::expect_equal(summary[[1]]$Q3, stats::quantile(values)[4])
})

test_that("T1.6. Correct SD value for numerics", {
  values <- 1:5
  summary <- visR::summarize(values)
  testthat::expect_equal(summary[[1]]$sd, stats::sd(values))
})

test_that("T1.7. Correct median value for numerics", {
  values <- 1:5
  summary <- visR::summarize(values)
  testthat::expect_equal(summary[[1]]$median, stats::median(values))
})

test_that("T1.7. Integers as correctly dispatched to summarize.numeric", {
  values <- 1:5
  testthat::expect_equal(visR::summarize(values), visR::summarize(as.integer(values)))
})

# Requirement T2 ----------------------------------------------------------

context("T2. utils_table - T2. Correct values for summarize.factors")

test_that("T2.1. Correct count of factor values", {
  values <- base::as.factor(c("A", "A", "B"))
  summary <- visR::summarize(values)
  testthat::expect_equal(summary[[1]]$`N A`, base::sum(values=="A"))
})

test_that("T2.2. Correct perentage of factor values", {
  values <- base::as.factor(c("A", "A", "B"))
  summary <- visR::summarize(values)
  testthat::expect_equal(base::round(summary[[1]]$`% A`, 2), base::round((base::sum(values=="A")/base::length(values))*100, 2))
})


# Requirement T3 ----------------------------------------------------------

context("T3. utils_table - T2. Correct values for summarize.default")

test_that("T3.1. Correct count of unique values", {
  values <- c("A", "A", "B")
  summary <- visR::summarize(values)
  testthat::expect_equal(summary[[1]]$unique_values, base::length(base::unique(values)))
})

test_that("T3.1. Correct count of missing values", {
  values <- c("A", "A", "B")
  summary <- visR::summarize(values)
  testthat::expect_equal(summary[[1]]$nmiss, 0)
})

# Requirement T4 ----------------------------------------------------------

context("T4. utils_table - T1. Correct values for summarize_tab1.numeric")

test_that("T4.1. Correct mean values for numerics in summarize_tab1", {
  values <- 1:5
  summary <- visR::summarize_tab1(values)
  num <- base::paste0(base::round(base::mean(x),2), 
                " (", 
                base::round(stats::sd(x),2), 
                ")")
  testthat::expect_equal(summary[[1]]$`Mean (SD)`, num)
})

test_that("T4.2. Correct median values for numerics in summarize_tab1", {
  values <- 1:5
  summary <- visR::summarize_tab1(values)
  num <- base::paste0(base::round(stats::median(x),2), 
                " (", 
                base::round(stats::quantile(x, probs=0.25, na.rm = TRUE),2), 
                "-",
                base::round(stats::quantile(x, probs=0.75, na.rm = TRUE),2), 
                ")")
  testthat::expect_equal(summary[[1]]$`Median (IQR)`, num)
})

test_that("T4.3. Correct range values for numerics in summarize_tab1", {
  values <- 1:5
  summary <- visR::summarize_tab1(values)
  num <- base::paste0(base::round(base::min(x),2), 
                "-", 
                base::round(base::max(x),2))
  testthat::expect_equal(summary[[1]]$`Min-max`, num)
})


test_that("T4.4. Correct missing values for numerics in summarize_tab1", {
  values <- 1:5
  summary <- visR::summarize_tab1(values)
  num <- base::paste0(base::sum(is.na(values)), " (",base::sum(is.na(values))/base::length(values) , "%)")
  testthat::expect_equal(summary[[1]]$Missing, num)
})

# Requirement T5 ----------------------------------------------------------

context("T5. utils_table - T1. Correct values for summarize_tab1.factor and summarize_tab1.string")

test_that("T5.1. Correct value for factors in summarize_tab1", {
  values <- base::as.factor(c("A", "A", "B"))
  summary <- visR::summarize_tab1(values)
  num <- base::paste0(base::sum(values=="A"), " (", round(base::sum(values=="A")/base::length(values)*100, 1), "%)")
  testthat::expect_equal(summary[[1]]$A , num)
})

test_that("T5.2. Correct default value in summarize_tab1", {
  values <- c("A", "A", "B")
  summary <- visR::summarize_tab1(values)
  num <- base::paste0(base::sum(values=="A"), " (", round(base::sum(values=="A")/base::length(values)*100, 1), "%)")
  testthat::expect_equal(summary[[1]]$`Unique values`, as.character(base::sum(values=="A")))
})


# END ---------------------------------------------------------------------

