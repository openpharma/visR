#' @title Specifications test-estimate_KM.R
#' @section Last updated by: Tim Treis (tim.treis@@outlook.de)
#' @section Last update date: 2021-12-03 09:32:51
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
#' T2.5 An error when the colname specified through `CNSR` is not numeric
#' T2.6 No error when the colname specified through `CNSR` is not the proposed default
#' T3. The user can specify strata
#' T3.1 An error when the columns, specifying the strata are not available in `data`
#' T3.2 No error when strata is NULL
#' T3.3 When no strata are specified, an artificial strata is created 'Overall'
#' T3.4 When only 1 stratum is specified, the stratum names are added to the `names` attribute'
#' T3.5 When more than 1 strata is specified, the stratum names are available in the `names` attribute
#' T4. The function removes all rows with NA values inside any of the strata, CNSR or AVAL
#' T4.1 The function removes all rows with NA values inside any of the strata, CNSR or AVAL
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
#' T7.2 The function prefixes the function call with survival

# Requirement T1 ---------------------------------------------------------------

testthat::context("estimate_KM - T1. The function accepts a `data.frame` `tibble` or `data.table`")

testthat::test_that("T1.1 No error when `data` is of class `data.frame`", {

  data <- adtte
  testthat::expect_error(visR::estimate_KM(data = data), NA)

})


testthat::test_that("T1.2 No error when `data` is of class `tibble`", {

  data <- dplyr::as_tibble(adtte)
  testthat::expect_error(visR::estimate_KM(data = data), NA)

})

testthat::test_that("T1.3 No error when `data` is of class `data.table`", {

  if (nzchar(find.package("data.table"))){
    data <- data.table::as.data.table(adtte)
    testthat::expect_error(visR::estimate_KM(data = data), NA)
  }
})

testthat::test_that("T1.4 An error when `data` is of an unexpected class, eg `list`", {

  data <- base::as.list(adtte)
  testthat::expect_error(visR::estimate_KM(data = data))

})

testthat::test_that("T1.5 An error when `data` is NULL", {

  testthat::expect_error(visR::estimate_KM(data = NULL))

})

# Requirement T2 ---------------------------------------------------------------

testthat::context("estimate_KM - T2. The function relies on the presence of two numeric variables, specified through `AVAL` and `CNSR`, to be present in `data`")

testthat::test_that("T2.1 An error when colname specified through `AVAL` is not present in `data`", {

  data <- adtte[,-which(colnames(adtte) == "AVAL")]

  testthat::expect_error(visR::estimate_KM(data = data))

})

testthat::test_that("T2.2 An error when colname specified through `AVAL` is not numeric", {

  data <- adtte
  data[["AVAL"]] <- as.character(data[["AVAL"]])

  testthat::expect_error(visR::estimate_KM(data = data))

})

testthat::test_that("T2.3 No error when the colname specified through `AVAL` is not the proposed default", {

  data <- adtte
  data$AVAL2 <- data$AVAL
  data <- data[,-which(colnames(adtte) == "AVAL")]

  testthat::expect_error(visR::estimate_KM(data = data, AVAL="AVAL2"), NA)

})

testthat::test_that("T2.4 An error when the colname specified through `CNSR` is not present in `data`", {


  data <- adtte[,-which(colnames(adtte) == "CNSR")]

  testthat::expect_error(visR::estimate_KM(data = data))

})

testthat::test_that("T2.5 An error when the colname specified through `CNSR` is not numeric", {

  data <- adtte
  data[["CNSR"]] <- as.character(data[["CNSR"]])

  testthat::expect_error(visR::estimate_KM(data = data))

})

testthat::test_that("T2.6 No error when the colname specified through `CNSR` is not the proposed default", {

  data <- adtte
  data$CNSR2 <- data$CNSR
  data <- dplyr::select(data, -CNSR)

  testthat::expect_error(visR::estimate_KM(data = data, CNSR="CNSR2"), NA)

})

# Requirement T3 ---------------------------------------------------------------

testthat::context("estimate_KM - T3. The user can specify strata")

testthat::test_that("T3.1 An error when the columns, specifying the strata are not available in `data`", {

  data <- adtte
  testthat::expect_error(visR::estimate_KM(data = data, strata = "blah"))

})

testthat::test_that("T3.2 No error when strata is NULL",{

  data <- adtte
  testthat::expect_error(visR::estimate_KM(data = data, strata = NULL), NA)

})

testthat::test_that("T3.3 When no strata are specified, an artificial strata is created 'Overall'", {

  data <- adtte
  survobj <- visR::estimate_KM(data = data, strata = NULL)

  testthat::expect_equal(names(survobj[["strata"]]), "Overall")

})

testthat::test_that("T3.4 When only 1 stratum is specified, the stratum names are added to the `names` attribute'", {

  data <- adtte
  survobj <- visR::estimate_KM(data = data, strata = "STUDYID")

  testthat::expect_equal(names(survobj[["strata"]]), "STUDYID=CDISCPILOT01")

})

testthat::test_that("T3.5 When more than 1 strata is specified, the stratum names are available in the `names` attribute", {

  data <- adtte
  survobj <- visR::estimate_KM(data = data, strata = "SEX")

  testthat::expect_equal(names(survobj[["strata"]]), c("SEX=F", "SEX=M"))

})

# Requirement T4 ---------------------------------------------------------------

testthat::context("estimate_KM - T4. The function removes all rows with NA values inside any of the strata, CNSR or AVAL")

testthat::test_that("T4.1 The function removes all rows with NA values inside any of the strata, CNSR or AVAL", {

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

# Requirement T5 ---------------------------------------------------------------

testthat::context("estimate_KM - T5. The function does not alter the calculation of survival::survfit")

testthat::test_that("T5.1 The function gives the same results as survival::survfit", {

  ## survival package
  survobj_survival <- survival::survfit(survival::Surv(AVAL, 1-CNSR) ~ SEX, 
                                        data = adtte)
  survobj_survival <- survival::survfit0(survobj_survival, start.time = 0)

  ## visR
  survobj_visR <- visR::estimate_KM(data = adtte, strata = "SEX")

  ## Compare common elements
  Common_Nms <- base::intersect(names(survobj_survival), names(survobj_visR))
  Common_Nms <- Common_Nms[-which(Common_Nms == "call")]

  list_survival <- lapply(survobj_survival, "[")[Common_Nms]
  list_visR <- lapply(survobj_visR, "[")[Common_Nms]

  testthat::expect_equal(list_survival, list_visR)
})

testthat::test_that("T5.2 The function adds timepoint = 0",{

  ## survival package
  survobj_survival <- survival::survfit(survival::Surv(AVAL, 1-CNSR) ~ SEX, 
                                        data = adtte)
  survobj_survival <- survival::survfit0(survobj_survival, start.time = 0)

  ## visR
  survobj_visR <- visR::estimate_KM(data = adtte, strata = "SEX")

  ## Compare common elements
  Common_Nms <- base::intersect(names(survobj_survival), names(survobj_visR))
  Common_Nms <- Common_Nms[-which(Common_Nms == "call")]


  list_survival <- lapply(survobj_survival, "[")[Common_Nms]
  list_visR <- lapply(survobj_visR, "[")[Common_Nms]

  testthat::expect_equal(list_survival, list_visR)
})

testthat::test_that("T5.3 The function allows additional arguments to be passed, specific for survival::survfit", {

  ## survival package
  survobj_survival <- survival::survfit(survival::Surv(AVAL, 1-CNSR) ~ SEX, 
                                        data = adtte, 
                                        ctype = 2, 
                                        conf.type = "plain")
  survobj_survival <- survival::survfit0(survobj_survival, start.time = 0)

  ## visR
  survobj_visR <- visR::estimate_KM(data = adtte, 
                                    strata = "SEX", 
                                    ctype = 2, 
                                    conf.type = "plain")

  ## Compare common elements
  Common_Nms <- base::intersect(names(survobj_survival), names(survobj_visR))
  Common_Nms <- Common_Nms[-which(Common_Nms == "call")]

  list_survival <- lapply(survobj_survival, "[")[Common_Nms]
  list_visR <- lapply(survobj_visR, "[")[Common_Nms]

  testthat::expect_equal(list_survival, list_visR)
})

testthat::test_that("T5.4 The function returns an object of class `survfit`", {

  ## visR
  survobj_visR <- visR::estimate_KM(data = adtte, 
                                    strata = "SEX", 
                                    ctype = 2, 
                                    conf.type = "plain")

  testthat::expect_true(inherits(survobj_visR, "survfit"))
})


# Requirement T6 ---------------------------------------------------------------

testthat::context("estimate_KM - T6. The function adds additional information to the survfit object when available")

testthat::test_that("T6.1 The calculation is not affected by the addition of additional parameters", {

  ## survival package
  survobj_survival <- survival::survfit(survival::Surv(AVAL, 1-CNSR) ~ SEX, 
                                        data = adtte)
  survobj_survival <- survival::survfit0(survobj_survival, start.time = 0)

  ## visR
  survobj_visR <- visR::estimate_KM(data = adtte, strata = "SEX")

  ## Compare common elements
  Common_Nms <- base::intersect(names(survobj_survival), names(survobj_visR))
  Common_Nms <- Common_Nms[-which(Common_Nms == "call")]

  Unique_Nms_visR <- base::setdiff(names(survobj_visR), names(survobj_survival))

  list_survival <- lapply(survobj_survival, "[")[Common_Nms]
  list_visR <- lapply(survobj_visR, "[")[Common_Nms]

  ## calculation is not affected by addition of parameters. Same as T5.
  testthat::expect_equal(list_survival, list_visR)
})

testthat::test_that("T6.2 The function add PARAM/PARAMCD when available", {

  ## survival package
  survobj_survival <- survival::survfit(survival::Surv(AVAL, 1-CNSR) ~ SEX, 
                                        data = adtte)
  survobj_survival <- survival::survfit0(survobj_survival, start.time = 0)

  ## visR
  survobj_visR <- visR::estimate_KM(data = adtte, strata = "SEX")

  ## Compare common elements
  Unique_Nms_visR <- base::setdiff(names(survobj_visR), names(survobj_survival))
  list_visR <- lapply(survobj_visR, "[")[Unique_Nms_visR]

  testthat::expect_equal(list_visR[[2]], "TTDE")
  testthat::expect_equal(list_visR[[1]], "Time to First Dermatologic Event")
})

# Requirement T7 ---------------------------------------------------------------

testthat::context("estimate_KM - T7. The function call supports traceability")

testthat::test_that("T7.1 The function updates call$data when magrittr pipe is used", {

  ## survival package
  survobj_survival <- adtte %>%
    survival::survfit(survival::Surv(AVAL, 1-CNSR) ~ SEX, data = .) %>%
    survival::survfit0(start.time = 0)
  call_survival <- as.list(survobj_survival[["call"]])

  ## survival package
  survobj_visR <- adtte %>%
    visR::estimate_KM(data = ., strata = "SEX")
  call_visR <- as.list(survobj_visR[["call"]])

  testthat::expect_equal(call_visR[["data"]], as.symbol("adtte"))
})

testthat::test_that("T7.2 The function prefixes the function call with survival", {

  ## survival package
  survobj_survival <- adtte %>%
    survival::survfit(survival::Surv(AVAL, 1-CNSR) ~ SEX, data = .) %>%
    survival::survfit0(start.time = 0)
  call_survival <- as.list(survobj_survival[["call"]])

  ## survival package
  survobj_visR <- adtte %>%
    visR::estimate_KM(data = ., strata = "SEX")
  call_visR <- as.list(survobj_visR[["call"]])

  testthat::expect_equal(call_visR[[1]], quote(survival::survfit))
})