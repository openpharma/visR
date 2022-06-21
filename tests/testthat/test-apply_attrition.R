#' @title Specifications test-apply_attrition.R
#' @section Last updated by: Tim Treis (tim.treis@@outlook.de)
#' @section Last update date: 2022-02-09T15:22:32
#'
#' @section List of tested specifications
#' T1. The function accepts a `data.frame` `tibble` or `data.table`
#' T1.1. No error when `data` is of class `data.frame`
#' T1.2. No error when `data` is of class `tibble`
#' T1.3. No error when `data` is of class `data.table`
#' T1.4. An error when `data` is of class `list`
#' T1.5 An error when `data` is NULL
#' T1.6 An error when `data` is NA
#' T1.7 An error when `data` does not exist in the global environment
#' T2. The function correctly handles arguments
#' T2.1 No error when `criteria_conditions` is a character vector
#' T2.2 An error when `criteria_conditions` is not a character vector
#' T2.3 An error when `criteria_conditions` is NULL
#' T2.4 An error when `data` is missing.
#' T2.5 An error when `criteria_conditions` is missing.
#' T3. The function filters correctly when provided a vector of single filters
#' T3.1 Correct filtering string column
#' T3.2 Correct filtering integer column
#' T3.3 Correct filtering factor column
#' T4. The function filters correctly when provided a vector of combined filters
#' T4.1 Correct filtering using a combined filter containing logical `and` (`&`)
#' T4.2 Correct filtering using a combined filter containing logical `or` (`|`)
#' T5. The returned object is of correct class
#' T5.1 The object is of class `data.frame`

# Requirement T1 ----------------------------------------------------------

testthat::context("apply_attrition - T1. The function accepts a `data.frame` `tibble` or `data.table`")

testthat::test_that("T1.1. No error when `data` is of class `data.frame`", {
  data <- adtte
  testthat::expect_error(
    visR::apply_attrition(data, c("TRTP == 'Placebo'", "AGE >= 75")), NA
  )
})


testthat::test_that("T1.2. No error when `data` is of class `tibble`", {
  data <- dplyr::as_tibble(adtte)
  testthat::expect_error(
    visR::apply_attrition(
      data,
      criteria_conditions = c("TRTP == 'Placebo'", "AGE >= 75")
    ), NA
  )
})

testthat::test_that("T1.3. No error when `data` is of class `data.table`", {
  if (nzchar(find.package("data.table"))) {
    data <- data.table::as.data.table(adtte)
    testthat::expect_error(
      visR::apply_attrition(
        data,
        criteria_conditions = c("TRTP == 'Placebo'", "AGE >= 75")
      ), NA
    )
  }
})

testthat::test_that("T1.4. An error when `data` is of class `list`", {
  data <- base::as.list(adtte)
  testthat::expect_error(
    visR::apply_attrition(
      data,
      criteria_conditions = c("TRTP == 'Placebo'", "AGE >= 75")
    )
  )
})

testthat::test_that("T1.5 An error when `data` is NULL", {
  testthat::expect_error(
    visR::apply_attrition(
      NULL,
      criteria_conditions = c("TRTP == 'Placebo'", "AGE >= 75")
    )
  )
})

testthat::test_that("T1.6 An error when `data` is NA", {
  testthat::expect_error(
    visR::apply_attrition(
      NA,
      criteria_conditions = c("TRTP == 'Placebo'", "AGE >= 75")
    )
  )
})

testthat::test_that("T1.7 An error when `data` does not exist in the global environment", {
  testthat::expect_error(
    visR::apply_attrition(
      blah,
      criteria_conditions = c("TRTP == 'Placebo'", "AGE >= 75")
    )
  )
})

# Requirement T2 ---------------------------------------------------------------

testthat::context("apply_attrition - T2. The function correctly handles arguments")

testthat::test_that("T2.1 No error when `criteria_conditions` is a character vector", {
  testthat::expect_error(
    visR::apply_attrition(
      adtte,
      criteria_conditions = c("TRTP == 'Placebo'", "AGE >= 75")
    ), NA
  )
})

testthat::test_that("T2.2 An error when `criteria_conditions` is not a character vector", {
  testthat::expect_error(
    visR::apply_attrition(
      adtte,
      criteria_conditions = 123
    )
  )
})

testthat::test_that("T2.3 An error when `criteria_conditions` is NULL", {
  testthat::expect_error(
    visR::apply_attrition(
      adtte,
      criteria_conditions = NULL
    )
  )
})

testthat::test_that("T2.4 An error when `data` is missing.", {
  testthat::expect_error(
    visR::apply_attrition(criteria_conditions = NULL)
  )
})

testthat::test_that("T2.5 An error when `criteria_conditions` is missing.", {
  testthat::expect_error(visR::apply_attrition(data = adtte))
})

# Requirement T3 ---------------------------------------------------------------

testthat::context("apply_attrition - T3. The function filters correctly when provided a vector of single filters")

testthat::test_that("T3.1 Correct filtering string column", {
  testthat::expect_equal(
    visR::apply_attrition(
      adtte,
      criteria_conditions = c("TRTP == 'Placebo'")
    ), adtte %>% dplyr::filter(TRTP == "Placebo")
  )
})

testthat::test_that("T3.2 Correct filtering integer column", {
  testthat::expect_equal(
    visR::apply_attrition(
      adtte,
      criteria_conditions = c("AGE >= 75")
    ), adtte %>% dplyr::filter(AGE >= 75)
  )
})

testthat::test_that("T3.3 Correct filtering factor column", {
  data <- adtte %>% dplyr::mutate(AGEGR1 = factor(AGEGR1))
  testthat::expect_equal(
    visR::apply_attrition(
      data,
      criteria_conditions = c("AGEGR1 == '< 65'")
    ), data %>% dplyr::filter(AGEGR1 == "< 65")
  )
})

# Requirement T4 ---------------------------------------------------------------

testthat::context("apply_attrition - T4. The function filters correctly when provided a vector of combined filters")

testthat::test_that("T4.1 Correct filtering using a combined filter containing logical `and` (`&`)", {
  testthat::expect_equal(
    visR::apply_attrition(
      adtte,
      criteria_conditions = c("AGEGR1 == '< 65' & SEX == 'M'")
    ), adtte %>%
      dplyr::filter(AGEGR1 == "< 65") %>%
      dplyr::filter(SEX == "M")
  )

  testthat::expect_equal(
    visR::apply_attrition(
      adtte,
      criteria_conditions = c(
        "AGEGR1 == '< 65' & SEX == 'M'",
        "TRTP == 'Placebo'"
      )
    ), adtte %>%
      dplyr::filter(AGEGR1 == "< 65") %>%
      dplyr::filter(SEX == "M") %>%
      dplyr::filter(TRTP == "Placebo")
  )
})

#
testthat::test_that("T4.2 Correct filtering using a combined filter containing logical `or` (`|`)", {
  testthat::expect_equal(
    visR::apply_attrition(
      adtte,
      criteria_conditions = c("AGEGR1 == '< 65' | SEX == 'M'")
    ), adtte %>%
      dplyr::filter(AGEGR1 == "< 65" | SEX == "M")
  )

  testthat::expect_equal(
    visR::apply_attrition(
      adtte,
      criteria_conditions = c(
        "AGEGR1 == '< 65' | SEX == 'M'",
        "TRTP == 'Placebo'"
      )
    ), adtte %>%
      dplyr::filter(AGEGR1 == "< 65" | SEX == "M") %>%
      dplyr::filter(TRTP == "Placebo")
  )

  testthat::expect_equal(
    visR::apply_attrition(
      adtte,
      criteria_conditions = c(
        "TRTP == 'Placebo'",
        "AGEGR1 == '< 65' | SEX == 'M'",
        "RACE != 'WHITE'"
      )
    ), adtte %>%
      dplyr::filter(AGEGR1 == "< 65" | SEX == "M") %>%
      dplyr::filter(TRTP == "Placebo") %>%
      dplyr::filter(RACE != "WHITE")
  )
})

# Requirement T5 ---------------------------------------------------------------

testthat::context("apply_attrition - T5. The returned object is of correct class")

testthat::test_that("T5.1 The object is of class `data.frame`", {
  outdf <- visR::apply_attrition(
    adtte,
    criteria_conditions = c("TRTP == 'Placebo'", "AGE >= 75")
  )

  testthat::expect_s3_class(outdf, "data.frame")
})

# END OF CODE -------------------------------------------------------------
