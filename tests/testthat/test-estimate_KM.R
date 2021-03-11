#' @title Specifications estimate_KM
#' @section Last updated by:
#' Steven Haesendonckx
#' @section Last update date:
#' 15-FEB-2021

# Specifications ----------------------------------------------------------

library(testthat)
library(visR)
library(survival)

#' T1. The function accepts a dataframe or tibble
#' T1.1 No error when `data` is a data.frame
#' T1.2 No error when `data` is a tibble
#' T1.3 No error when `data` is a data.table
#' T1.4 An error when `data` is a random object
#' T1.5 An error when `data` is NULL
#' T1.6 An error when `data` does not exist in the global environment
#' T2. The function relies on the presence of two numeric variables, AVAL and CNSR, to be present in `data`
#' T2.1 An error when colname specified as AVAL is not present in `data`
#' T2.2 An error when colname specified as AVAL is not numeric
#' T2.3 An error when colname specified as CNSR is not present in `data`
#' T2.4 An error when colname specified as CNSR is not numeric
#' T3. The user can specify strata
#' T3.1 An error when the columns, specifying the strata are not available in `data`
#' T3.2 No error when strata is NULL
#' T3.3 When no strata are specified, an artificial strata is created 'Overall'
#' T4. The function removes all rows with NA values inside any of the strata
#' T5. The function does not alter the calculation of survival::survfit
#' T5.1 The function gives the same results as survival::survfit
#' T5.2 The function adds timepoint = 0
#' T5.3 The function allows additional arguments to be passed, specific for survival::survfit
#' T5.4 The function returns an object of class `survfit`
#' T6. The function adds additional information to the survfit object when available
#' T6.1 The calculation is not affected by the addition of additional parameters
#' T6.2 The function add PARAM/PARAMCD when available
#' T7. The function call supports traceability
#' T7.1 The function updates call$data when magrittr pipe is used

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

testthat::test_that("T1.6 An error when `data` does not exist in the global environment",{

  testthat::expect_error(blah %>% visR::estimate_KM())

})

# Requirement T2 ----------------------------------------------------------

context("estimate_KM - T2. The function relies on the presence of two numeric variables, AVAL and CNSR, to be present in `data`")

testthat::test_that("T2.1 An error when `data` does not contain the variable AVAL",{
  
  AVAL <- "AVAL"
  
  data <- adtte[,-which(colnames(adtte) == AVAL)]

  testthat::expect_error(visR::estimate_KM(data = data))

})

testthat::test_that("T2.2 An error when AVAL is not numeric",{

  AVAL <- "AVAL"
  
  data <- adtte
  data[[AVAL]] <- as.character(data[[AVAL]])

  testthat::expect_error(visR::estimate_KM(data = data))

})

testthat::test_that("T2.3 An error when `data` does not contain the variable CNSR",{

  CNSR <- "CNSR"
  
  data <- adtte[,-which(colnames(adtte) == CNSR)]

  testthat::expect_error(visR::estimate_KM(data = data))

})

testthat::test_that("T2.4 An error when CNSR is not numeric",{

  CNSR <- "CNSR"
  
  data <- adtte
  data[[CNSR]] <- as.character(data[[CNSR]])

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

context("estimate_KM - T4. The function removes all rows with NA values inside any of the strata, CNSR or AVAL")

testthat::test_that("T4.1 The function removes all rows with NA values inside any of the strata, CNSR or AVAL",{

  data <- adtte
  data[1:10,"SEX"] <- NA
  data[11:20, "AVAL"] <- NA
  data[21:30, "CNSR"] <- NA

  ## Keep NA
  survobj <- visR::estimate_KM(data = data, strata = "SEX")

  ## Drop NA
  data <- tidyr::drop_na(data, AVAL, CNSR, SEX)
  survobjNA   <- visR::estimate_KM(data = data, strata = "SEX")

  testthat::expect_equal(survobjNA, survobj)

})

# Requirement T5 ----------------------------------------------------------

# context("estimate_KM - T5. The function does not alter the calculation of survival::survfit")
#
# testthat::test_that("T5.1 The function gives the same results as survival::survfit",{
#
#   ## survival package
#   survobj_survival <- survival::survfit(Surv(AVAL, 1-CNSR) ~ SEX, data = adtte)
#   survobj_survival <- survival::survfit0(survobj_survival, start.time = 0)
#
#   ## visR
#   survobj_visR <- visR::estimate_KM(data = adtte, strata = "SEX")
#
#   ## Compare common elements
#   Common_Nms <- base::intersect(names(survobj_survival), names(survobj_visR))
#
#
#   list_survival <- lapply(survobj_survival, "[")[Common_Nms]
#   list_visR <- lapply(survobj_visR, "[")[Common_Nms]
#
#   testthat::expect_equal(list_survival, list_visR)
# })

# testthat::test_that("T5.2 The function adds timepoint = 0",{
#
#   ## survival package
#   survobj_survival <- survival::survfit(Surv(AVAL, 1-CNSR) ~ SEX, data = adtte)
#   survobj_survival <- survival::survfit0(survobj_survival, start.time = 0)
#
#   ## visR
#   survobj_visR <- visR::estimate_KM(data = adtte, strata = "SEX")
#
#   ## Compare common elements
#   Common_Nms <- base::intersect(names(survobj_survival), names(survobj_visR))
#
#
#   list_survival <- lapply(survobj_survival, "[")[Common_Nms]
#   list_visR <- lapply(survobj_visR, "[")[Common_Nms]
#
#   testthat::expect_equal(list_survival, list_visR)
# })
#
# testthat::test_that("T5.3 The function allows additional arguments to be passed, specific for survival::survfit",{
#
#   ## survival package
#   survobj_survival <- survival::survfit(Surv(AVAL, 1-CNSR) ~ SEX, data = adtte, ctype = 2, conf.type = "plain")
#   survobj_survival <- survival::survfit0(survobj_survival, start.time = 0)
#
#   ## visR
#   survobj_visR <- visR::estimate_KM(data = adtte, strata = "SEX", ctype = 2, conf.type = "plain")
#
#   ## Compare common elements
#   Common_Nms <- base::intersect(names(survobj_survival), names(survobj_visR))
#
#
#   list_survival <- lapply(survobj_survival, "[")[Common_Nms]
#   list_visR <- lapply(survobj_visR, "[")[Common_Nms]
#
#   testthat::expect_equal(list_survival, list_visR)
# })

testthat::test_that("T5.4 The function returns an object of class `survfit`",{

  ## visR
  survobj_visR <- visR::estimate_KM(data = adtte, strata = "SEX", ctype = 2, conf.type = "plain")

  testthat::expect_true(inherits(survobj_visR, "survfit"))
})


# Requirement T6 ----------------------------------------------------------

# context("estimate_KM - T6. The function adds additional information to the survfit object when available")
#
# testthat::test_that("T6.1 The calculation is not affected by the addition of additional parameters",{
#
#   ## survival package
#   survobj_survival <- survival::survfit(Surv(AVAL, 1-CNSR) ~ SEX, data = adtte)
#   survobj_survival <- survival::survfit0(survobj_survival, start.time = 0)
#
#   ## visR
#   survobj_visR <- visR::estimate_KM(data = adtte, strata = "SEX")
#
#   ## Compare common elements
#   Common_Nms <- base::intersect(names(survobj_survival), names(survobj_visR))
#   Unique_Nms_visR <- base::setdiff(names(survobj_visR), names(survobj_survival))
#
#   list_survival <- lapply(survobj_survival, "[")[Common_Nms]
#   list_visR <- lapply(survobj_visR, "[")[Common_Nms]
#
#   ## calculation is not affected by addition of additional parameters. Same test as in requirement T5.
#   testthat::expect_equal(list_survival, list_visR)
# })

testthat::test_that("T6.2 The function add PARAM/PARAMCD when available",{

  ## survival package
  survobj_survival <- survival::survfit(Surv(AVAL, 1-CNSR) ~ SEX, data = adtte)
  survobj_survival <- survival::survfit0(survobj_survival, start.time = 0)

  ## visR
  survobj_visR <- visR::estimate_KM(data = adtte, strata = "SEX")

  ## Compare common elements
  Unique_Nms_visR <- base::setdiff(names(survobj_visR), names(survobj_survival))
  list_visR <- lapply(survobj_visR, "[")[Unique_Nms_visR]

  testthat::expect_equal(list_visR[[2]], "TTDE")
  testthat::expect_equal(list_visR[[1]], "Time to First Dermatologic Event")
})

# Requirement T7 ----------------------------------------------------------
context("estimate_KM - T7. The function call supports traceability")

testthat::test_that("T7.1 The function updates call$data when magrittr pipe is used",{

  ## survival package
  survobj_survival <- adtte %>%
    survival::survfit(Surv(AVAL, 1-CNSR) ~ SEX, data = .) %>%
    survival::survfit0(start.time = 0)
  call_survival <- as.list(survobj_survival[["call"]])


  ## survival package
  survobj_visR <- adtte %>%
    visR::estimate_KM(data = ., strata = "SEX")
  call_visR <- as.list(survobj_visR[["call"]])


  testthat::expect_equal(call_visR[["data"]], as.symbol("adtte"))
})


# END OF CODE ----------------------------------------------------------

