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

context("apply_attrition - T1. The function accepts a `data.frame` `tibble` or `data.table`")

test_that("T1.1. No error when `data` is of class `data.frame`", {
  data <- adtte
  expect_error(
    visR::apply_attrition(data, c("TRTP == 'Placebo'", "AGE >= 75")), NA
  )
})


test_that("T1.2. No error when `data` is of class `tibble`", {
  data <- dplyr::as_tibble(adtte)
  expect_error(
    visR::apply_attrition(
      data,
      criteria_conditions = c("TRTP == 'Placebo'", "AGE >= 75")
    ), NA
  )
})

test_that("T1.3. No error when `data` is of class `data.table`", {
  if (nzchar(find.package("data.table"))) {
    data <- data.table::as.data.table(adtte)
    expect_error(
      visR::apply_attrition(
        data,
        criteria_conditions = c("TRTP == 'Placebo'", "AGE >= 75")
      ), NA
    )
  }
})

test_that("T1.4. An error when `data` is of class `list`", {
  data <- base::as.list(adtte)
  expect_error(
    visR::apply_attrition(
      data,
      criteria_conditions = c("TRTP == 'Placebo'", "AGE >= 75")
    )
  )
})

test_that("T1.5 An error when `data` is NULL", {
  expect_error(
    visR::apply_attrition(
      NULL,
      criteria_conditions = c("TRTP == 'Placebo'", "AGE >= 75")
    )
  )
})

test_that("T1.6 An error when `data` is NA", {
  expect_error(
    visR::apply_attrition(
      NA,
      criteria_conditions = c("TRTP == 'Placebo'", "AGE >= 75")
    )
  )
})

test_that("T1.7 An error when `data` does not exist in the global environment", {
  expect_error(
    visR::apply_attrition(
      blah,
      criteria_conditions = c("TRTP == 'Placebo'", "AGE >= 75")
    )
  )
})

# Requirement T2 ---------------------------------------------------------------

context("apply_attrition - T2. The function correctly handles arguments")

test_that("T2.1 No error when `criteria_conditions` is a character vector", {
  expect_error(
    visR::apply_attrition(
      adtte,
      criteria_conditions = c("TRTP == 'Placebo'", "AGE >= 75")
    ), NA
  )
})

test_that("T2.2 An error when `criteria_conditions` is not a character vector", {
  expect_error(
    visR::apply_attrition(
      adtte,
      criteria_conditions = 123
    )
  )
})

test_that("T2.3 An error when `criteria_conditions` is NULL", {
  expect_error(
    visR::apply_attrition(
      adtte,
      criteria_conditions = NULL
    )
  )
})

test_that("T2.4 An error when `data` is missing.", {
  expect_error(
    visR::apply_attrition(criteria_conditions = NULL)
  )
})

test_that("T2.5 An error when `criteria_conditions` is missing.", {
  expect_error(visR::apply_attrition(data = adtte))
})

# Requirement T3 ---------------------------------------------------------------

context("apply_attrition - T3. The function filters correctly when provided a vector of single filters")

test_that("T3.1 Correct filtering string column", {
  expect_equal(
    visR::apply_attrition(
      adtte,
      criteria_conditions = c("TRTP == 'Placebo'")
    ), adtte %>% dplyr::filter(TRTP == "Placebo")
  )
})

test_that("T3.2 Correct filtering integer column", {
  expect_equal(
    visR::apply_attrition(
      adtte,
      criteria_conditions = c("AGE >= 75")
    ), adtte %>% dplyr::filter(AGE >= 75)
  )
})

test_that("T3.3 Correct filtering factor column", {
  data <- adtte %>% dplyr::mutate(AGEGR1 = factor(AGEGR1))
  expect_equal(
    visR::apply_attrition(
      data,
      criteria_conditions = c("AGEGR1 == '< 65'")
    ), data %>% dplyr::filter(AGEGR1 == "< 65")
  )
})

# Requirement T4 ---------------------------------------------------------------

context("apply_attrition - T4. The function filters correctly when provided a vector of combined filters")

test_that("T4.1 Correct filtering using a combined filter containing logical `and` (`&`)", {
  expect_equal(
    visR::apply_attrition(
      adtte,
      criteria_conditions = c("AGEGR1 == '< 65' & SEX == 'M'")
    ), adtte %>%
      dplyr::filter(AGEGR1 == "< 65") %>%
      dplyr::filter(SEX == "M")
  )

  expect_equal(
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
test_that("T4.2 Correct filtering using a combined filter containing logical `or` (`|`)", {
  expect_equal(
    visR::apply_attrition(
      adtte,
      criteria_conditions = c("AGEGR1 == '< 65' | SEX == 'M'")
    ), adtte %>%
      dplyr::filter(AGEGR1 == "< 65" | SEX == "M")
  )

  expect_equal(
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

  expect_equal(
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

context("apply_attrition - T5. The returned object is of correct class")

test_that("T5.1 The object is of class `data.frame`", {
  outdf <- visR::apply_attrition(
    adtte,
    criteria_conditions = c("TRTP == 'Placebo'", "AGE >= 75")
  )

  expect_s3_class(outdf, "data.frame")
})

# END OF CODE -------------------------------------------------------------
