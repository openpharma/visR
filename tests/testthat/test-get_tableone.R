#' @title Specifications test-get_tableone.R
#' @section Last updated by: Tim Treis (tim.treis@@outlook.de)
#' @section Last update date: 2022-02-09T15:22:32
#'
#' @section List of tested specifications
#' T1. The function accepts a `data.frame` `tibble` or `data.table`
#' T1.1. No error when `data` is of class `data.frame`
#' T1.2. No error when `data` is of class `tibble`
#' T1.3 No error when `data` is of class `data.table`
#' T1.4 An error when `data` is of class `list`
#' T1.5 An error when `data` is NULL
#' T2. The function accepts a list of `colnames` in the `data` as `strata`
#' T2.1 An error when `strata` is a number
#' T2.2 An error when `strata` is a string that is not a colname in `data`
#' T2.3 Additional colnames in the tableone are the `strata` values (for one `strata`)
#' T2.4 Additional colnames in the tableone are the crossproduct of all `strata` values (for more than one `strata`)
#' T3. The tableone includes expected columnnames
#' T3.1 Tableone by default includes columns `variable`, `statistic`, and `Total`
#' T3.2 Tableone still includes the colum `Total` if `overall` is FALSE but no `strata` is given
#' T3.3 Tableone does not include the colum `Total` if `overall` is FALSE and a `strata` is given
#' T4. The function only accepts suitable summary functions
#' T4.1 An error when the `summary_function` is NULL
#' T4.2 An error when the `summary_function` is a string
#' T4.3 An error when the `summary_function` is a function not build for it
#' T4.4 An error when the `summary_function` is `summarize_long`
#' T4.5 No error when the `summary_function` is `summarize_short`
#' T5. The tableone removes strata variables in rows that leads NA values
#' T5.1 An error when the the table includes one strata variable
#' T5.2 An error when the the table includes multiple strata variables

# Requirement T1 ----------------------------------------------------------

testthat::context("get_tableone - T1. The function accepts a `data.frame` `tibble` or `data.table`")

testthat::test_that("T1.1. No error when `data` is of class `data.frame`", {
  testthat::expect_error(visR::get_tableone(data = adtte), NA)
})


testthat::test_that("T1.2. No error when `data` is of class `tibble`", {
  data <- dplyr::as_tibble(adtte)

  testthat::expect_error(visR::get_tableone(data = data), NA)
})

testthat::test_that("T1.3 No error when `data` is of class `data.table`", {
  if (nzchar(find.package("data.table"))) {
    data <- data.table::as.data.table(adtte)
    testthat::expect_error(visR::get_tableone(data = data), NA)
  }
})

testthat::test_that("T1.4 An error when `data` is of class `list`", {
  data <- base::as.list(adtte)
  testthat::expect_error(visR::get_tableone(data = data))
})

testthat::test_that("T1.5 An error when `data` is NULL", {
  testthat::expect_error(visR::get_tableone(data = NULL))
})

# Requirement T2 ---------------------------------------------------------------

testthat::context("get_tableone - T2. The function accepts a list of `colnames` in the `data` as `strata`")

testthat::test_that("T2.1 An error when `strata` is a number", {
  testthat::expect_error(visR::get_tableone(data = adtte, strata = 1))
})

testthat::test_that("T2.2 An error when `strata` is a string that is not a colname in `data`", {
  testthat::expect_error(visR::get_tableone(data = adtte, strata = "blah"))
})

testthat::test_that("T2.3 Additional colnames in the tableone are the `strata` values (for one `strata`)", {
  trtp_colnames <- colnames(visR::get_tableone(
    data = adtte,
    strata = c("TRTP")
  ))[4:6]
  testthat::expect_equal(trtp_colnames, levels(adtte$TRTP))
})

testthat::test_that("T2.4 Additional colnames in the tableone are the crossproduct of all `strata` values (for more than one `strata`)", {
  mapply_colnames <- c(mapply(function(x, y) paste(x, y, sep = "_"),
    levels(adtte$TRTP),
    MoreArgs = list(levels(adtte$SEX))
  ))
  visR_colnames <- colnames(visR::get_tableone(
    data = adtte,
    strata = c("TRTP", "SEX")
  ))[4:9]
  testthat::expect_equal(mapply_colnames, visR_colnames)
})

# Requirement T3 ---------------------------------------------------------------

testthat::context("get_tableone - T3. The tableone includes expected columnnames")

testthat::test_that("T3.1 Tableone by default includes columns `variable`, `statistic`, and `Total`", {
  tableone_colnames <- colnames(visR::get_tableone(data = adtte))

  testthat::expect_equal(tableone_colnames, c("variable", "statistic", "Total"))
})

testthat::test_that("T3.2 Tableone still includes the colum `Total` if `overall` is FALSE but no `strata` is given", {
  tableone_colnames <- colnames(visR::get_tableone(data = adtte, overall = FALSE))

  testthat::expect_equal(tableone_colnames, c("variable", "statistic", "Total"))
})

testthat::test_that("T3.3 Tableone does not include the colum `Total` if `overall` is FALSE and a `strata` is given", {
  tblone_colnames <- colnames(visR::get_tableone(
    data = adtte,
    overall = FALSE,
    strata = c("TRTP")
  ))

  testthat::expect_equal(
    tblone_colnames,
    c("variable", "statistic", levels(adtte$TRTP))
  )
})

# Requirement T4 ---------------------------------------------------------------

testthat::context("get_tableone - T4. The function only accepts suitable summary functions")

testthat::test_that("T4.1 An error when the `summary_function` is NULL", {
  testthat::expect_error(visR::get_tableone(
    data = adtte,
    summary_function = NULL
  ))
})

testthat::test_that("T4.2 An error when the `summary_function` is a string", {
  testthat::expect_error(visR::get_tableone(
    data = adtte,
    summary_function = "A"
  ))
})

testthat::test_that("T4.3 An error when the `summary_function` is a function not build for it", {
  testthat::expect_error(visR::get_tableone(
    data = adtte,
    summary_function = sum
  ))
})

testthat::test_that("T4.4 An error when the `summary_function` is `summarize_long`", {
  testthat::expect_error(visR::get_tableone(
    data = adtte,
    summary_function = summarize_long
  ))
})


testthat::test_that("T4.5 No error when the `summary_function` is `summarize_short`", {
  testthat::expect_error(visR::get_tableone(
    data = adtte,
    summary_function = summarize_short
  ), NA)
})

# Requirement T5 ---------------------------------------------------------------

testthat::context("get_tableone - T5. The tableone removes strata variables in rows that leads NA values")

testthat::test_that("T5.1 An error when the the table includes one strata variable", {
  strata <- c("SEX")

  table <- adtte %>%
    visR::get_tableone(strata = strata)

  testthat::expect_true(sum(strata %in% unique(table$variable)) == 0)
})

testthat::test_that("T5.2 An error when the the table includes multiple strata variables", {
  strata <- c("SEX", "RACE")

  table <- adtte %>%
    visR::get_tableone(strata = strata)

  testthat::expect_true(sum(strata %in% unique(table$variable)) == 0)

  table <- adtte %>%
    visR::get_tableone(strata = strata, overall = FALSE)

  testthat::expect_true(sum(strata %in% unique(table$variable)) == 0)
})

# END OF CODE -------------------------------------------------------------
