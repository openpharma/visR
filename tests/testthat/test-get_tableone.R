#' @title Specifications get_tableone
#' @section Last updated by:
#' Rebecca Albrecht
#' @section Last update date:
#' 20-APR-2021

# Specifications ----------------------------------------------------------

#' T1. The function accepts a `data.frame` `tibble` or `data.table`
#' T1.1 No error when `data` is a data.frame
#' T1.2 No error when `data` is a tibble
#' T1.3 No error when `data` is a data.table
#' T1.4 An error when `data` is a random object
#' T1.5 An error when `data` is NULL
#' T2. The function accepts a list of `colnames` in the `data` as `strata`
#' T2.1. An error when `strata` is a number
#' T2.2. An error when `strata` is a string that is not a colname in `data`
#' T2.3. Additional colnames in the tableone are the `strata` values (for one `strata`)
#' T2.4. Additional colnames in the tableone are the crossproduct of all `strata` values (for more than one `strata`)
#' T3. The tableone includes expected columnnames
#' T3.1. Tableone by default includes columns `variable`,  `statistic`, and `Total`
#' T3.2. Tableone still includes the colum `Total` if `overall` is FALSE but no `strata` is given
#' T3.3. Tableone does not include the colum `Total` if `overall` is FALSE and a `strata` is given
#' T4. The function only accepts suitable summary functions
#' T4.1. An error when the `summary_function` is NULL
#' T4.2. An error when the `summary_function` is a string
#' T4.3. An error when the `summary_function` is a function not build for it
#' T4.4. An error when the `summary_function` is `summarize_long`
#' T4.5. No error when the `summary_function` is `summarize_short`


# Requirement T1 ----------------------------------------------------------

context("get_tableone - T1. The function accepts a `data.frame` `tibble` or `data.table`")

testthat::test_that("T1.1. No error when `data` is of class `data.frame`",{

  data <- adtte
  testthat::expect_error(visR::get_tableone(data = data), NA)

})


testthat::test_that("T1.2. No error when `data` is of class `tibble`",{

  data <- dplyr::as_tibble(adtte)
  testthat::expect_error(visR::get_tableone(data = data), NA)

})

testthat::test_that("T1.3. No error when `data` is of class `data.table`",{

  if ("data.table" %in% rownames(installed.packages())){
    data <- data.table::as.data.table(adtte)
    testthat::expect_error(visR::get_tableone(data = data), NA)
  }
})

testthat::test_that("T1.4. An error when `data` is of class `list`",{

  data <- base::as.list(adtte)
  testthat::expect_error(visR::get_tableone(data = data))

})

testthat::test_that("T1.5 An error when `data` is NULL",{

  testthat::expect_error(visR::get_tableone(data = NULL))

})

# Requirement T2 ----------------------------------------------------------

context("get_tableone - T2. The function accepts a list of `colnames` in the `data` as `strata`")

testthat::test_that("T2.1. An error when `strata` is a number",{

  data <- adtte
  testthat::expect_error(visR::get_tableone(data = data, strata = 1))

})

testthat::test_that("T2.2. An error when `strata` is a string that is not a colname in `data`",{

  data <- adtte
  testthat::expect_error(visR::get_tableone(data = data, strata = "blah"))

})

testthat::test_that("T2.3. Additional colnames in the tableone are the `strata` values (for one `strata`)",{

  data <- adtte
  levels(adtte$TRTP)
  testthat::expect_equal(colnames(visR::get_tableone(data = data, strata=c("TRTP")))[4:6], levels(adtte$TRTP))

})

testthat::test_that("T2.4. Additional colnames in the tableone are the crossproduct of all `strata` values (for more than one `strata`)",{

  data <- adtte
  colnames=c(mapply(function(x, y)paste(x, y, sep="_"), levels(adtte$TRTP), MoreArgs =list(levels(adtte$SEX))))
  testthat::expect_equal(colnames(visR::get_tableone(data = data, strata=c("TRTP", "SEX")))[4:9], colnames)

})

# Requirement T3 ----------------------------------------------------------

context("get_tableone - T3. The tableone includes expected columnnames")

testthat::test_that("T3.1. Tableone by default includes columns `variable`,  `statistic`, and `Total`", {
  data <- adtte
  testthat::expect_equal(colnames(visR::get_tableone(data = data)), c("variable", "statistic", "Total"))
})

testthat::test_that("T3.2. Tableone still includes the colum `Total` if `overall` is FALSE but no `strata` is given", {
  data <- adtte
  testthat::expect_equal(colnames(visR::get_tableone(data = data, overall = FALSE)), c("variable", "statistic", "Total"))
})

testthat::test_that("T3.3. Tableone does not include the colum `Total` if `overall` is FALSE and a `strata` is given", {
  data <- adtte
  testthat::expect_equal(colnames(visR::get_tableone(data = data, overall = FALSE, strata=c("TRTP"))), c("variable", "statistic", levels(data$TRTP)))
})

# Requirement T4 ----------------------------------------------------------

context("get_tableone - T4. The function only accepts suitable summary functions")

testthat::test_that("T4.1. An error when the `summary_function` is NULL",{

  data <- adtte
  testthat::expect_error(visR::get_tableone(data = data, summary_function = NULL))

})

testthat::test_that("T4.2. An error when the `summary_function` is a string",{

  data <- adtte
  testthat::expect_error(visR::get_tableone(data = data, summary_function = "A"))

})

testthat::test_that("T4.3. An error when the `summary_function` is a function not build for it",{

  data <- adtte
  testthat::expect_error(visR::get_tableone(data = data, summary_function = sum))

})

testthat::test_that("T4.4. An error when the `summary_function` is `summarize_long`",{

  data <- adtte
  testthat::expect_error(visR::get_tableone(data = data, summary_function = summarize_long))

})


testthat::test_that("T4.5. No error when the `summary_function` is `summarize_short`",{

  data <- adtte
  testthat::expect_error(visR::get_tableone(data = data, summary_function = summarize_short), NA)

})

# END ---------------------------------------------------------------------
