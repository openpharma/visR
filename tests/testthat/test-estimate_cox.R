#' @title Specifications test-estimate_cox.R
#' @section Last updated by: Steven Haesendonckx/Alexandra Papadopoulou
#' @section Last update date: 2022-05-07
#'
#' @section List of tested specifications
#' T1.  The function accepts a `data.frame` `tibble` or `data.table`
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
#' T3.  The user can specify the equation
#' T3.1 An error when the columns, specifying the equation are not available in `data`
#' T3.2 No error when equation is NULL
#' T3.3 When no equation is specified, the 'null model' is displayed
#' T3.4 When the equation has a term with only one factor, expect an error
#' T4. The function allows `conf.int` argument to be passed, specific for survival::summary.coxph
#' T4.1 An error when the `conf.int` is not numeric
#' T4.2 An error when the `conf.int` is numeric but not between 0 and 1
#' T4.3 No error when the `conf.int` is numeric and between 0 and 1
#' T5.  The function removes all rows with NA values inside CNSR or AVAL
#' T5.1 The function removes all rows with NA values inside CNSR or AVAL
#' T6. The function does not alter the calculation of survival::coxph but adds additional components from survival::summary.coxph in the return object
#' T6.1 The function gives the same results as survival::coxph adding the components from survival::summary.coxph
#' T6.2 The function allows additional arguments to be passed, specific for survival::coxph & survival::summary.coxph
#' T6.3 The function returns an object of class `coxph`
#' T7. The function adds additional information to the survfit object when available
#' T7.1 The calculation is not affected by the addition of additional parameters
#' T7.2 The function adds PARAM/PARAMCD when available
#' T8. The function call supports traceability
#' T8.1 The function updates call["data"] when magrittr pipe is used
#' T8.2 The function prefixes the function call with survival

# Requirement T1 ----------------------------------------------------------

testthat::context("estimate_cox - T1. The function accepts a `data.frame` `tibble` or `data.table`")

testthat::test_that("T1.1 No error when `data` is of class `data.frame`", {

  data <- adtte
  testthat::expect_error(visR::estimate_cox(data = data), NA)

})


testthat::test_that("T1.2 No error when `data` is of class `tibble`", {

  data <- dplyr::as_tibble(adtte)
  testthat::expect_error(visR::estimate_cox(data = data), NA)

})

testthat::test_that("T1.3 No error when `data` is of class `data.table`", {

  if (nzchar(find.package("data.table"))){
    data <- data.table::as.data.table(adtte)
    testthat::expect_error(visR::estimate_cox(data = data), NA)
  }
})

testthat::test_that("T1.4 An error when `data` is of an unexpected class, eg `list`", {

  data <- base::as.list(adtte)
  testthat::expect_error(visR::estimate_cox(data = data))

})

testthat::test_that("T1.5 An error when `data` is NULL", {

  testthat::expect_error(visR::estimate_cox(data = NULL))

})

# Requirement T2 ---------------------------------------------------------------

testthat::context("estimate_cox - T2. The function relies on the presence of two numeric variables, specified through `AVAL` and `CNSR`, to be present in `data`")

testthat::test_that("T2.1 An error when colname specified through `AVAL` is not present in `data`", {

  data <- adtte[,-which(colnames(adtte) == "AVAL")]

  testthat::expect_error(visR::estimate_cox(data = data))

})

testthat::test_that("T2.2 An error when colname specified through `AVAL` is not numeric", {

  data <- adtte
  data[["AVAL"]] <- as.character(data[["AVAL"]])

  testthat::expect_error(visR::estimate_cox(data = data))

})

testthat::test_that("T2.3 No error when the colname specified through `AVAL` is not the proposed default", {

  data <- adtte
  data$AVAL2 <- data$AVAL
  data <- data[,-which(colnames(adtte) == "AVAL")]

  testthat::expect_error(visR::estimate_cox(data = data, AVAL="AVAL2"), NA)

})

testthat::test_that("T2.4 An error when the colname specified through `CNSR` is not present in `data`", {


  data <- adtte[,-which(colnames(adtte) == "CNSR")]

  testthat::expect_error(visR::estimate_cox(data = data))

})

testthat::test_that("T2.5 An error when the colname specified through `CNSR` is not numeric", {

  data <- adtte
  data[["CNSR"]] <- as.character(data[["CNSR"]])

  testthat::expect_error(visR::estimate_cox(data = data))

})

testthat::test_that("T2.6 No error when the colname specified through `CNSR` is not the proposed default", {

  data <- adtte
  data$CNSR2 <- data$CNSR
  data <- dplyr::select(data, -CNSR)

  testthat::expect_error(visR::estimate_cox(data = data, CNSR="CNSR2"), NA)

})

# Requirement T3 ---------------------------------------------------------------

testthat::context("estimate_cox - T3. The user can specify the equation")

testthat::test_that("T3.1 An error when the columns, specifying the equation are not available in `data`", {

  data <- adtte
  testthat::expect_error(visR::estimate_cox(data = data, equation = "blah"))

})

testthat::test_that("T3.2 No error when equation is NULL",{

  data <- adtte
  testthat::expect_error(visR::estimate_cox(data = data, equation = NULL), NA)

})

testthat::test_that("T3.3 When no equation is specified, the 'null model' is displayed", {

  data <- adtte
  survobj <- visR::estimate_cox(data = data)

  testthat::expect_equal(names(survobj[["formula"]]), NULL)

})

testthat::test_that("T3.4 When the equation has a term with only one factor, expect an error", {

  data <- adtte
  testthat::expect_error(visR::estimate_cox(data = data, equation = "STUDYID"))

})

# Requirement T4 ----------------------------------------------------------

testthat::context("estimate_cox - T4. The function allows `conf.int` argument to be passed, specific for survival::summary.coxph")

testthat::test_that("T4.1 An error when the `conf.int` is not numeric", {

  data <- adtte
  testthat::expect_error(
    visR::estimate_cox(data = data, equation = "SEX + AGE", conf.int = "blah"))

})

testthat::test_that("T4.2 A warning when the `conf.int` is numeric but not between 0 and 1", {

  data <- adtte
  testthat::expect_warning(
    visR::estimate_cox(data = data, equation = "SEX + AGE", conf.int = 2))

})

testthat::test_that("T4.3 No error when the `conf.int` is numeric and between 0 and 1", {

  data <- adtte
  conf <- 0.60
  testthat::expect_error(visR::estimate_cox(data = data, equation = "SEX + AGE", conf.int = conf), NA)

})


# Requirement T5 ---------------------------------------------------------------

testthat::context("estimate_cox - T5. The function removes all rows with NA values inside CNSR or AVAL")

testthat::test_that("T5.1 The function removes all rows with NA values inside CNSR or AVAL", {

  data <- adtte
  data[11:20, "AVAL"] <- NA
  data[21:30, "CNSR"] <- NA

  ## Keep NA
  survobj <- visR::estimate_cox(data = data, equation  = "SEX + AGE")

  ## Drop NA
  data <- tidyr::drop_na(data, AVAL, CNSR, SEX, AGE)
  survobjNA   <- visR::estimate_cox(data = data, equation = "SEX + AGE")

  testthat::expect_equal(survobjNA, survobj)

})

# Requirement T5 ---------------------------------------------------------------

testthat::context("estimate_cox - T6. The function does not alter the calculation of survival::coxph but adds additional components from survival::summary.coxph in the return object")

testthat::test_that("T6.1 The function gives the same results as survival::coxph", {

  ## survival package
  survobj_survival <- survival::coxph(survival::Surv(AVAL, 1-CNSR) ~ SEX + AGE,
                                        data = adtte)


  ## visR
  survobj_visR <- visR::estimate_cox(data = adtte, equation = "SEX + AGE")

  ## Compare common elements
  Common_Nms <- base::intersect(names(survobj_survival), names(survobj_visR))
  Common_Nms <- Common_Nms[-which(Common_Nms == "call")]

  list_survival <- survobj_survival[Common_Nms]
  list_visR     <- survobj_visR[Common_Nms]

  testthat::expect_equal(list_survival, list_visR)
})

testthat::test_that("T6.2 The function allows additional arguments to be passed, specific for survival::coxph", {

  ## survival package
  survobj_survival <- survival::coxph(survival::Surv(AVAL, 1-CNSR) ~ SEX + AGE,
                                        data = adtte,
                                        ties = "breslow")


  ## visR
  survobj_visR <- visR::estimate_cox(data = adtte,
                                    equation = "SEX + AGE",
                                    ties = "breslow")

  ## Compare common elements
  Common_Nms <- base::intersect(names(survobj_survival), names(survobj_visR))
  Common_Nms <- Common_Nms[-which(Common_Nms == "call")]

  list_survival <- survobj_survival[Common_Nms]
  list_visR     <- survobj_visR[Common_Nms]

  testthat::expect_equal(list_survival, list_visR)
})

testthat::test_that("T6.3 The function returns additional components specific for survival::summary.coxph", {

  ## survival package
  survobj_survival <- survival::coxph(survival::Surv(AVAL, 1-CNSR) ~ SEX + AGE,
                                      data = adtte,
                                      ties = "breslow")

  survobj_survival <- summary(survobj_survival)
  ## visR
  survobj_visR <- visR::estimate_cox(data = adtte,
                                     equation = "SEX + AGE",
                                     ties = "breslow")

  ## Compare common elements
  Common_Nms <- base::intersect(names(survobj_survival), names(survobj_visR))
  Common_Nms <- Common_Nms[-which(Common_Nms %in% c("call", "coefficients", "concordance"))]

  list_survival <- survobj_survival[Common_Nms]
  list_visR     <- survobj_visR[Common_Nms]

  testthat::expect_equal(list_survival, list_visR)
})

testthat::test_that("T6.3 The function returns an object of class `coxph`", {

  ## visR
  survobj_visR <- visR::estimate_cox(data = adtte,
                                    equation = "SEX + AGE",
                                    ties = "breslow",
                                    conf.int = 0.9)

  testthat::expect_true(inherits(survobj_visR, "coxph"))
})


# Requirement T7 ---------------------------------------------------------------

testthat::context("estimate_cox - T7. The function adds additional information to the survfit object when available")

testthat::test_that("T7.1 The calculation is not affected by the addition of additional parameters", {

  ## survival package
  survobj_survival <- survival::coxph(survival::Surv(AVAL, 1-CNSR) ~ SEX + AGE,
                                       ties = "breslow",
                                       data = adtte)
  summary_survobj_survival <- summary(survobj_survival, conf.int = 0.9)
  survobj_survival["conf.int"] <- summary_survobj_survival["conf.int"]

  ## visR
  survobj_visR <- visR::estimate_cox(data = adtte, equation  = "SEX + AGE",
                                     ties = "breslow", conf.int = 0.9)

  ## Compare common elements
  Common_Nms <- base::intersect(names(survobj_survival), names(survobj_visR))
  Common_Nms <- Common_Nms[-which(Common_Nms == "call")]

  Unique_Nms_visR <- base::setdiff(names(survobj_visR), names(survobj_survival))

  list_survival <- survobj_survival[Common_Nms]
  list_visR     <- survobj_visR[Common_Nms]

  ## calculation is not affected by addition of parameters. Same as T5.
  testthat::expect_equal(list_survival, list_visR)
})

testthat::test_that("T6.2 The function adds PARAM/PARAMCD when available", {

  ## survival package
  survobj_survival <- survival::coxph(survival::Surv(AVAL, 1-CNSR) ~ SEX + AGE,
                                        data = adtte)

  ## visR
  survobj_visR <- visR::estimate_cox(data = adtte, equation = "SEX + AGE")
  survobj_visR$strata_lbls <- NULL

  ## Compare common elements
  Unique_Nms_visR <- base::setdiff(names(survobj_visR), names(survobj_survival))
  list_visR <- survobj_visR[Unique_Nms_visR]

  testthat::expect_equal(list_visR[[2]], "TTDE")
  testthat::expect_equal(list_visR[[1]], "Time to First Dermatologic Event")
})


# Requirement T8 ---------------------------------------------------------------

testthat::context("estimate_cox - T8. The function call supports traceability")

testthat::test_that("T8.1 The function updates call[`data`] when magrittr pipe is used", {

  ## survival package
  survobj_survival <- adtte %>%
    survival::coxph(survival::Surv(AVAL, 1-CNSR) ~ SEX +  AGE, data = .)

  call_survival <- as.list(survobj_survival[["call"]])

  ## survival package
  survobj_visR <- adtte %>%
    visR::estimate_cox(data = ., equation = "SEX +  AGE")
  call_visR <- as.list(survobj_visR[["call"]])

  testthat::expect_equal(call_visR[["data"]], as.symbol("adtte"))
})

testthat::test_that("T8.2 The function prefixes the function call with survival", {

  ## survival package
  survobj_survival <- adtte %>%
    survival::coxph(survival::Surv(AVAL, 1-CNSR) ~ SEX + AGE, data = .)

  call_survival <- as.list(survobj_survival[["call"]])

  ## survival package
  survobj_visR <- adtte %>%
    visR::estimate_cox(data = ., equation = "SEX + AGE")
  call_visR <- as.list(survobj_visR[["call"]])

  testthat::expect_equal(call_visR[[1]], quote(survival::coxph))
})

# END OF CODE -------------------------------------------------------------
