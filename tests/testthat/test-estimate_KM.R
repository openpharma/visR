#' @title Specifications estimate_KM
#' @section Last updated by:
#' Steven Haesendonckx
#' @section Last update date:
#' 13-DEC-2020

# Specifications ----------------------------------------------------------

library(testthat)
library(visR)
library(dplyr)
library(survival)

#' T1. The function accepts a dataframe or tibble
#' T1.1 No error when `data` is a data.frame
#' T1.2 No error when `data` is a tibble
#' T1.3 No error when `data` is a data.table
#' T1.4 An error when `data` is a random object
#' T1.5 An error when `data` is NULL
#' T2. The function relies on the presence of two numeric variables, AVAL and CNSR, to be present in `data`
#' T2.1 An error when AVAL is not present in `data`
#' T2.2 An error when AVAL is not numeric
#' T2.3 An error when CNSR is not present in `data`
#' T2.4 An error when CNSR is not numeric
#' T3. T3. The user can specify strata
#' T3.1 An error when the columns, specifying the strata are not available in `data`
#' T3.2 No error when strata is NULL
#' T3.3 When no strata are specified, an artificial strata is created 'Overall'
#' 
#' 


# Requirement T1 ----------------------------------------------------------

context("estimate_KM - T1. The function accepts a `data.frame` `tibble` or `data.table`")

testthat::test_that("T1.1. No error when `data` is of class `data.frame`",{

  data <- adtte
  testthat::expect_error(visR::estimate_KM(data = data), NA)

})


testthat::test_that("T1.2. No error when `data` is of class `tibble`",{

  data <- dplyr::as_tibble(adtte)
  testthat::expect_error(visR::estimate_KM(data = data), NA)

})

testthat::test_that("T1.3. No error when `data` is of class `data.table`",{

  if ("data.table" %in% rownames(installed.packages())){
    data <- data.table::as.data.table(adtte)
    testthat::expect_error(visR::estimate_KM(data = data), NA)
  }
})

testthat::test_that("T1.4. An error when `data` is of class `list`",{

  data <- base::as.list(adtte)
  testthat::expect_error(visR::estimate_KM(data = data))
  
})

testthat::test_that("T1.5 An error when `data` is NULL",{

  testthat::expect_error(visR::estimate_KM(data = NULL))
  
})

# Requirement T2 ----------------------------------------------------------

context("estimate_KM - T2. The function relies on the presence of two numeric variables, AVAL and CNSR, to be present in `data`")

testthat::test_that("T2.1 An error when `data` does not contain the variable AVAL",{
  
  data <- adtte[,-which(colnames(adtte) == "AVAL")]

  testthat::expect_error(visR::estimate_KM(data = data))
  
})

testthat::test_that("T2.2 An error when AVAL is not numeric",{
  
  data <- adtte
  data[["AVAL"]] <- as.character(data[["AVAL"]])

  testthat::expect_error(visR::estimate_KM(data = data))
  
})

testthat::test_that("T2.3 An error when `data` does not contain the variable CNSR",{
  
  data <- adtte[,-which(colnames(adtte) == "CNSR")]

  testthat::expect_error(visR::estimate_KM(data = data))
  
})

testthat::test_that("T2.4 An error when CNSR is not numeric",{
  
  data <- adtte
  data[["CNSR"]] <- as.character(data[["CNSR"]])

  testthat::expect_error(visR::estimate_KM(data = data))
  
})

# Requirement T3 ----------------------------------------------------------

context("estimate_KM - T3. The user can specify strata")

testthat::test_that("T3.1 An error when the columns, specifying the strata are not available in `data`",{
  
  data <- adtte
  testthat::expect_error(visR::estimate_KM(data = data, strata = "blah"))
  
})

testthat::test_that("T3.2 No error when strata is NULL",{
  
  data <- adtte
  testthat::expect_error(visR::estimate_KM(data = data, strata = NULL), NA)
  
})

testthat::test_that("T3.3 When no strata are specified, an artificial strata is created 'Overall'",{
  
  data <- adtte
  survobj <- visR::estimate_KM(data = data, strata = NULL)
  
  testthat::expect_equal(names(survobj[["strata"]]), "Overall")
  
})

# Requirement T4 ----------------------------------------------------------




# T4. The function removes all rows with NA values inside any of the strata

# data1 <- adtte
# data2 <- data1
# data2[1:5, "AVAL"] <- NA
# 
# 
# estimate_KM(data = data1)
# estimate_KM(data = data2)

# T5. The function does not alter the calculation of survival::survfit

# T6. The function adds additional information when available
# T6.1 The function adds PARAM
# T6.2 The function adds PARAMCD

# T7. The function updates the call 

# T8. the function is compatible with map and %>% and return data

# T9. The function return a survfit

