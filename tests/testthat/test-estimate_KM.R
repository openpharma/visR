#' @title Specifications test-estimate_KM.R
#' @section Last updated by: Daniel Sjoberg (danield.sjoberg@@gmail.com)
#' @section Last update date: 2022-04-20T04:47:32
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
#' T3.6 When only 1 stratum is specified, the stratum labels are added to the `strata_lbs` list element
#' T3.7 When more than 1 strata is specified, the stratum labels are added to the `strata_lbs` list element
#' T4. The function removes all rows with NA values inside any of the strata, CNSR or AVAL
#' T4.1 The function removes all rows with NA values inside any of the strata, CNSR or AVAL
#' T5. The function does not alter the calculation of survival::survfit
#' T5.1 The function gives the same results as survival::survfit
#' T5.2 The function adds timepoint = 0
#' T5.3 The function allows additional arguments to be passed, specific for survival::survfit
#' T5.4 The function returns an object of class `survfit`
#' T6. The function adds additional information to the survfit object when available
#' T6.1 The calculation is not affected by the addition of additional parameters
#' T6.2 The function adds PARAM/PARAMCD when available
#' T6.3 The function adds strata labels from the data when available
#' T6.4 The function adds strata labels equal to the strata name when strata labels are not available from the data
#' T6.5 The function adds the data set name
#' T6.6 The function adds the environment to the call
#' T7. The function call supports traceability
#' T7.1 The function updates .$data_name when magrittr pipe is used
#' T7.2 The function prefixes the function call with survival
#' T8. Piped datasets still return accurate results
#' T8.1 Piped datasets still return accurate results
#' T9. The user can specify formula argument
#' T9.1 The formula argument returns the same results and the data method.
#' T9.2 The formula argument triggers error messages with incorrect function specification.

# Requirement T1 ----------------------------------------------------------

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

testthat::test_that("T3.6 When only 1 stratum is specified, the stratum labels are added to the `strata_lbs` list element", {

  data <- adtte
  survobj <- visR::estimate_KM(data = data, strata = NULL)

  testthat::expect_true(is.null(survobj$strata_lbls))

})

testthat::test_that("T3.7 When more than 1 strata is specified, the stratum labels are added to the `strata_lbs` list element", {

  data <- adtte
  survobj <- visR::estimate_KM(data = data, strata = "SEX")

  testthat::expect_equal(survobj$strata_lbls, list(SEX = "Sex"))

  survobj <- visR::estimate_KM(data = data, strata = c("RACE", "SEX"))

  testthat::expect_equal(survobj$strata_lbls, list(RACE = "Race", SEX = "Sex"))

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

testthat::test_that("T6.2 The function adds PARAM/PARAMCD when available", {

  ## survival package
  survobj_survival <- survival::survfit(survival::Surv(AVAL, 1-CNSR) ~ SEX,
                                        data = adtte)
  survobj_survival <- survival::survfit0(survobj_survival, start.time = 0)

  ## visR
  survobj_visR <- visR::estimate_KM(data = adtte, strata = "SEX")
  survobj_visR$strata_lbls <- NULL

  ## Compare common elements
  Unique_Nms_visR <- setdiff(names(survobj_visR), c(names(survobj_survival), "data_name"))
  list_visR <- lapply(survobj_visR, "[")[Unique_Nms_visR]

  testthat::expect_equal(list_visR[[2]], "TTDE")
  testthat::expect_equal(list_visR[[1]], "Time to First Dermatologic Event")
})

testthat::test_that("T6.3 The function adds strata labels from the data when available", {

  data <- adtte
  survobj <- visR::estimate_KM(data = data, strata = "SEX")

  testthat::expect_equal(survobj$strata_lbls, list(SEX = "Sex"))

})

testthat::test_that("T6.4 The function adds strata labels equal to the strata name when strata labels are not available from the data", {

  data <- adtte
  attr(data[["SEX"]], "label") <- NULL
  survobj <- visR::estimate_KM(data = data, strata = "SEX")

  testthat::expect_equal(survobj$strata_lbls, list(SEX = "SEX"))

})

testthat::test_that("T6.5 The function adds the data set name", {

  survobj <- visR::estimate_KM(data = adtte, strata = "SEX")
  testthat::expect_equal(survobj$data_name, "adtte")

  survobj <- visR::estimate_KM(data = adtte[adtte$SEX == "F", ], strata = "RACE")
  testthat::expect_equal(survobj$data_name, "adtte")

  survobj <- adtte %>%
    dplyr::filter(SEX == "F")%>%
    visR::estimate_KM(data = ., strata = "RACE")
  testthat::expect_equal(survobj$data_name, "adtte")

  survobj <- adtte %>%
    dplyr::filter(SEX == "F")%>%
    visR::estimate_KM(strata = "RACE")
  testthat::expect_equal(survobj$data_name, "adtte")

  # no data_name error when there is no data_name
  expect_error(rlang::inject(visR::estimate_KM(data = !!adtte, strata = "RACE")), NA)
})

testthat::test_that("T6.6 The function adds the environment to the call", {

  survobj <- visR::estimate_KM(data = adtte, strata = "SEX")

  testthat::expect_true(inherits(attr(survobj$call, ".Environment"), "environment"))

})


# Requirement T7 ---------------------------------------------------------------

testthat::context("estimate_KM - T7. The function call supports traceability")

testthat::test_that("T7.1 The function updates .$data_name when magrittr pipe is used", {
  ## using .
  survobj_visR <-
    adtte %>%
    visR::estimate_KM(data = ., strata = "SEX")
  testthat::expect_equal(survobj_visR[["data_name"]], "adtte")

  # without .
  survobj_visR <-
    adtte %>%
    visR::estimate_KM(strata = "SEX")
  testthat::expect_equal(survobj_visR[["data_name"]], "adtte")
})

testthat::test_that("T7.2 The function prefixes the function call with survival", {
  ## survival package
  survobj_visR <-
    adtte %>%
    visR::estimate_KM(data = ., strata = "SEX")
  call_visR <- as.list(rlang::quo_squash(survobj_visR[["call"]]))

  testthat::expect_equal(call_visR[[1]], quote(survival::survfit))
})

# Requirement T8 ---------------------------------------------------------------

testthat::context("estimate_KM - T8. Piped datasets still return accurate results")

testthat::test_that("T8.1 Piped datasets still return accurate results",{
  estimate_KM <-
    adtte %>%
    dplyr::filter(SEX == "F", AGE < 60) %>%
    visR::estimate_KM(strata = "TRTA")
  survfit <-
    survival::survfit(
      survival::Surv(AVAL, 1 - CNSR) ~ TRTA,
      data =
        adtte %>%
        dplyr::filter(SEX == "F", AGE < 60)
    ) %>%
    survival::survfit0()
  vals_to_check <- names(survfit) %>% setdiff(c("strata", "call"))
  testthat::expect_equal(unclass(survfit)[vals_to_check], unclass(estimate_KM)[vals_to_check])

  estimate_KM <-
    adtte[1:100, ] %>%
    visR::estimate_KM(strata = "TRTA")
  survfit <-
    survival::survfit(
      survival::Surv(AVAL, 1 - CNSR) ~ TRTA,
      data = adtte[1:100, ]
    ) %>%
    survival::survfit0()
  testthat::expect_equal(unclass(survfit)[vals_to_check], unclass(estimate_KM)[vals_to_check])
})

# Requirement T9 ---------------------------------------------------------------

testthat::context("estimate_KM - T9. The user can specify formula argument")

testthat::test_that("T9.1 The formula argument returns the same results and the data AVAL,CNSR,strata version", {
  # with CDISC data
  km1 <- visR::estimate_KM(data = adtte, strata = "SEX")
  km2 <- visR::estimate_KM(formula = Surv(AVAL, 1 - CNSR) ~ SEX, data = adtte)
  km1$call <- km2$call <- NULL
  testthat::expect_equal(km1, km2)

  # without CDISC data
  km1 <-
    survival::veteran %>%
    dplyr::mutate(AVAL = time,
                  CNSR = dplyr::if_else(status == 1, 0, 1)
    ) %>%
    visR::estimate_KM(strata = "trt")
  km2 <- visR::estimate_KM(data = survival::veteran, formula = Surv(time, status) ~ trt)
  km1$call <- km2$call <- NULL
  testthat::expect_equal(km1, km2)

  # CDICS args are ignored when formula arg is used
  km1 <- visR::estimate_KM(data = adtte, strata = "SEX")
  km2 <- visR::estimate_KM(strata = "SEX", AVAL = "AVAL", CNSR = "CNSR", formula = Surv(AVAL, 1 - CNSR) ~ SEX, data = adtte)
  km1$call <- km2$call <- NULL
  testthat::expect_equal(km1, km2)
})

testthat::test_that("T9.2 The formula argument triggers error messages with incorrect function specification.", {
  testthat::expect_error(
    visR::estimate_KM(formula = Surv(AVAL, 1 - CNSR) ~ SEX, data = letters)
  )
  testthat::expect_error(
    visR::estimate_KM(formula = letters, data = suvival::lung)
  )
  testthat::expect_error(
    visR::estimate_KM(formula = Surv(AVAL, 1 - CNSR) ~ SEX)
  )

  testthat::expect_error(
    visR::estimate_KM(formula = Surv(AVAL, 1 - CNSR) ~ 1, data = letters)
  )
  testthat::expect_error(
    visR::estimate_KM(formula = Surv(AVAL, 1 - CNSR) ~ 1)
  )
  testthat::expect_error(
    visR::estimate_KM(formula = Surv(AVAL, 1 - CNSR) ~ NOT_A_VARIABLE, data = adtte)
  )
})

# END OF CODE -------------------------------------------------------------
