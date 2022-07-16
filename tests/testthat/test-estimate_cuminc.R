#' @title Specifications test-estimate_cuminc.R
#' @section Last updated by: Steven Haesendonckx (haesendonckx.steven@@gmail.com)
#' @section Last update date: 2022-05-01T14:21:53
#'
#' @section List of tested specifications
#' T1. The function accepts a `data.frame` `tibble` or `data.table`
#' T1.1 No error when `data` is of class `data.frame`
#' T1.2 No error when `data` is of class `tibble`
#' T1.3 No error when `data` is of class `data.table`
#' T1.4 An error when `data` is of an unexpected class, eg `list`
#' T1.5 An error when `data` is NULL
#' T2. The function relies on the presence of two numeric variables, specified through `AVAL` and `CNSR`, to be present in `data`
#' T2.1 An error when colname specified through `AVAL` is not present in `data`
#' T2.2 An error when colname specified through `AVAL` is not numeric
#' T2.3 No error when the colname specified through `AVAL` is not the proposed default
#' T2.4 An error when the colname specified through `CNSR` is not present in `data`
#' T2.5 An error when colname specified through `CNSR` is not a factor
#' T2.6 No error when the colname specified through `CNSR` is not the proposed default
#' T3. The user can specify strata
#' T3.1 An error when the columns, specifying the strata are not available in `data`
#' T3.2 No error when strata is NULL
#' T3.3 An error when `strata` is not part of `data`
#' T4. The user can specify conf.int
#' T4.1 An error when the conf.int is not numeric
#' T4.2 An error when the conf.int is numeric but not between 0 and 1
#' T4.3 No error when the conf.int is numeric and between 0 and 1
#' T4.4 A correct value for conf.int results in the resulting
#' T5. The function removes all rows with NA values inside any of the strata, CNSR or AVAL
#' T5.1 The function removes all rows with NA values inside any of the strata, CNSR or AVAL
#' T6. The function does not alter the calculation of tidycmprsk::cuminc
#' T6.1 The function gives the same results as tidycmprsk::cuminc

# Requirement T1 ----------------------------------------------------------

context("estimate_cuminc - T1. The function accepts a `data.frame` `tibble` or `data.table`")

test_that("T1.1 No error when `data` is of class `data.frame`", {
  data <- tidycmprsk::trial
  expect_error(
    visR::estimate_cuminc(data = data, CNSR = "death_cr", AVAL = "ttdeath"), NA
  )
})

test_that("T1.2 No error when `data` is of class `tibble`", {
  data <- dplyr::as_tibble(tidycmprsk::trial)
  expect_error(
    visR::estimate_cuminc(data = data, CNSR = "death_cr", AVAL = "ttdeath"), NA
  )
})

test_that("T1.3 No error when `data` is of class `data.table`", {
  if (nzchar(find.package("data.table"))) {
    data <- data.table::as.data.table(tidycmprsk::trial)
    expect_error(
      visR::estimate_cuminc(data = data, CNSR = "death_cr", AVAL = "ttdeath"), NA
    )
  }
})

test_that("T1.4 An error when `data` is of an unexpected class, eg `list`", {
  data <- base::as.list(tidycmprsk::trial)
  expect_error(
    visR::estimate_cuminc(data = data, CNSR = "death_cr", AVAL = "ttdeath")
  )
})

test_that("T1.5 An error when `data` is NULL", {
  expect_error(
    visR::estimate_cuminc(data = NULL, CNSR = "death_cr", AVAL = "ttdeath")
  )
})

# Requirement T2 ----------------------------------------------------------

context("estimate_cuminc - T2. The function relies on the presence of two numeric variables, specified through `AVAL` and `CNSR`, to be present in `data`")

test_that("T2.1 An error when colname specified through `AVAL` is not present in `data`", {
  data <- tidycmprsk::trial
  expect_error(
    visR::estimate_cuminc(data = data, CNSR = "death_cr", AVAL = "test")
  )
})

test_that("T2.2 An error when colname specified through `AVAL` is not numeric", {
  data <- tidycmprsk::trial
  data[["ttdeath"]] <- as.character(data[["ttdeath"]])

  expect_error(
    visR::estimate_cuminc(data = data, CNSR = "death_cr", AVAL = "ttdeath")
  )
})

test_that("T2.3 No error when the colname specified through `AVAL` is not the proposed default", {
  data <- tidycmprsk::trial
  expect_error(
    visR::estimate_cuminc(data = data, CNSR = "death_cr", AVAL = "ttdeath"), NA
  )
})

test_that("T2.4 An error when colname specified through `CNSR` is not present in `data`", {
  data <- tidycmprsk::trial
  expect_error(
    visR::estimate_cuminc(data = data, CNSR = "test", AVAL = "ttdeath")
  )
})

test_that("T2.5 An error when colname specified through `CNSR` is not a factor", {
  data <- tidycmprsk::trial
  data[["death_cr"]] <- as.numeric(data[["death_cr"]])

  expect_error(
    visR::estimate_cuminc(data = data, CNSR = "death_cr", AVAL = "ttdeath")
  )
})

test_that("T2.6 No error when the colname specified through `CNSR` is not the proposed default", {
  data <- tidycmprsk::trial
  expect_error(
    visR::estimate_cuminc(data = data, CNSR = "death_cr", AVAL = "ttdeath"), NA
  )
})

# Requirement T3 ----------------------------------------------------------

context("estimate_cuminc - T3. The user can specify strata")

test_that("T3.1 An error when the columns, specifying the strata are not available in `data`", {
  data <- tidycmprsk::trial
  data[["trt"]] <- NULL
  expect_error(
    visR::estimate_cuminc(data = data, CNSR = "death_cr", AVAL = "ttdeath", strata = "trt")
  )
})

test_that("T3.2 No error when strata is NULL", {
  data <- tidycmprsk::trial
  expect_error(
    visR::estimate_cuminc(data = data, CNSR = "death_cr", AVAL = "ttdeath", strata = NULL), NA
  )
})

test_that("T3.3 An error when `strata` is not part of `data`", {
  data <- tidycmprsk::trial
  data[["trt"]] <- NULL
  expect_error(
    visR::estimate_cuminc(data = data, CNSR = "death_cr", AVAL = "ttdeath", strata = "trt")
  )
})

# Requirement T4 ----------------------------------------------------------

context("estimate_cuminc - T4. The user can specify conf.int")

test_that("T4.1 An error when the conf.int is not numeric", {
  data <- tidycmprsk::trial
  expect_error(
    visR::estimate_cuminc(data = data, CNSR = "death_cr", AVAL = "ttdeath", strata = "trt", conf.int = "blah")
  )
})

test_that("T4.2 An error when the conf.int is numeric but not between 0 and 1", {
  data <- tidycmprsk::trial
  expect_error(
    visR::estimate_cuminc(data = data, CNSR = "death_cr", AVAL = "ttdeath", strata = "trt", conf.int = 2)
  )
})

test_that("T4.3 No error when the conf.int is numeric and between 0 and 1", {
  data <- tidycmprsk::trial
  conf <- 0.60
  expect_error(visR::estimate_cuminc(data = data, CNSR = "death_cr", AVAL = "ttdeath", strata = "trt", conf.int = conf), NA)
})

test_that("T4.4 A correct value for conf.int results in the resulting", {
  data <- tidycmprsk::trial
  conf <- 0.60
  cuminc <- visR::estimate_cuminc(data = data, CNSR = "death_cr", AVAL = "ttdeath", strata = "trt", conf.int = conf)
  expect_equal(conf, cuminc[["conf.level"]])
})





# Requirement T5 ----------------------------------------------------------

context("estimate_cuminc - T5. The function removes all rows with NA values inside any of the strata, CNSR or AVAL")

test_that("T5.1 The function removes all rows with NA values inside any of the strata, CNSR or AVAL", {
  data <- tidycmprsk::trial
  data[1:10, "trt"] <- NA
  data[11:20, "ttdeath"] <- NA
  data[21:30, "death_cr"] <- NA

  ## Keep NA
  cumobj <- visR::estimate_cuminc(data = data, CNSR = "death_cr", AVAL = "ttdeath", strata = "trt")

  ## Drop NA
  data <- tidyr::drop_na(data, trt, ttdeath, death_cr)
  cumobjNA <- visR::estimate_cuminc(data = data, CNSR = "death_cr", AVAL = "ttdeath", strata = "trt")

  expect_equal(cumobjNA, cumobj)
})

# Requirement T6 ----------------------------------------------------------

context("estimate_cuminc - T6. The function does not alter the calculation of tidycmprsk::cuminc")

test_that("T6.1 The function gives the same results as tidycmprsk::cuminc", {
  data <- tidycmprsk::trial
  cumvisr <- visR::estimate_cuminc(data = data, CNSR = "death_cr", AVAL = "ttdeath", strata = "trt")
  cumori <- tidycmprsk::cuminc(
    formula = stats::as.formula(paste0("survival::Surv(ttdeath, death_cr) ~  trt")),
    data = data
  )

  expect_equal(cumvisr, cumori)
})

# END OF CODE -------------------------------------------------------------
