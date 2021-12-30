#' @title Specifications test-tidyme.R
#' @section Last updated by: Tim Treis (tim.treis@@outlook.de)
#' @section Last update date: 2021-12-03 09:32:51
#'
#' @section List of tested specifications
#' T1. The function accepts an S3 object
#' T1.1 No error when a `survfit` object is passed to the function
#' T1.2 No error when a `survfit` object with missing values is passed to the function
#' T1.3 No error when a non-`survfit` S3 object is passed to the function
#' T2. The function tidies up an associated object
#' T2.1 The default method throws a message to indicate it relies on broom::tidy
#' T3. The S3 method, associated with a `survfit` object, outputs an extended tidied tibble
#' T3.1 The S3 method, associated with a `survfit` object, returns a tibble
#' T3.2 The S3 method, associated with a `survfit` object, has columns representing all list elements of the S3 object
#' T3.3 The S3 method, associated with a `survfit` object, turns list elements that represent integer numbers into integers

# Requirement T1 ---------------------------------------------------------------

testthat::context("tidyme - T1. The function accepts an S3 object")

testthat::test_that("T1.1 No error when a `survfit` object is passed to the function",{

  survfit_object <- visR::estimate_KM(data = adtte, strata = "TRTA")
  testthat::expect_error(visR::tidyme(survfit_object), NA)

})

testthat::test_that("T1.2 No error when a `survfit` object with missing values is passed to the function",{

  adtte_with_na <- adtte
  is.na(adtte_with_na$SEX[sample(seq_len(nrow(adtte_with_na)), size = 40)]) <- TRUE

  survfit_object <- visR::estimate_KM(data = adtte_with_na, strata = "SEX")
  testthat::expect_error(visR::tidyme(survfit_object), NA)

})

testthat::test_that("T1.3 No error when a non-`survfit` S3 object is passed to the function",{

  lm_object <- stats::lm(data = adtte, TRTDUR ~ AVAL)
  testthat::expect_error(visR::tidyme(lm_object), NA)

})

# Requirement T2 ---------------------------------------------------------------

testthat::context("tidyme - T2. The function tidies up an associated object")

testthat::test_that("T2.1 The default method throws a message to indicate it relies on broom::tidy",{

  lm_object <- stats::lm(data = adtte, TRTDUR ~ AVAL)

  testthat::expect_message(visR::tidyme(lm_object))

})


# Requirement T3 ---------------------------------------------------------------

testthat::context("tidyme - T3. The S3 method, associated with a `survfit` object, outputs an extended tidied tibble")

testthat::test_that("T3.1 The S3 method, associated with a `survfit` object, returns a tibble",{

  survfit_object <- visR::estimate_KM(data = adtte, strata = "TRTA")
  survfit_object_tidy <- tidyme(survfit_object)
  testthat::expect_true(inherits(survfit_object_tidy, c("tbl", "tbl_df")))

})

testthat::test_that("T3.2 The S3 method, associated with a `survfit` object, has columns representing all list elements of the S3 object",{

  survfit_object <- visR::estimate_KM(data = adtte, strata = "TRTA")
  survfit_object_tidy <- tidyme(survfit_object)

  surv_object_df <- base::with(survfit_object, 
                               data.frame(time = as.integer(time),
                                          n.risk = as.integer(n.risk),
                                          n.event = as.integer(n.event),
                                          n.censor = as.integer(n.censor),
                                          surv,
                                          std.err,
                                          cumhaz,
                                          std.chaz,
                                          type,
                                          logse,
                                          conf.int,
                                          conf.type,
                                          lower,
                                          upper,
                                          stringsAsFactors = FALSE))

  surv_object_df <- surv_object_df %>%
    dplyr::mutate(call = rep(list(survfit_object[["call"]]), 
                             sum(survfit_object[["strata"]])))
  surv_object_df["strata"] <- rep(names(survfit_object[["strata"]]), 
                                  survfit_object[["strata"]])
  surv_object_df["n.strata"] <- rep(survfit_object[["n"]], 
                                    survfit_object[["strata"]])
  surv_object_df["PARAM"] <- rep(survfit_object[["PARAM"]], 
                                 sum(survfit_object[["strata"]]))
  surv_object_df["PARAMCD"] <- rep(survfit_object[["PARAMCD"]], 
                                   sum(survfit_object[["strata"]]))

  surv_object_df <- surv_object_df %>% dplyr::as_tibble()

  cn <- colnames(survfit_object_tidy)

  for (i in 1:length(cn)){
    testthat::expect_equal(surv_object_df[,cn[i]], survfit_object_tidy[,cn[i]])
  }

})

testthat::test_that("T3.3 The S3 method, associated with a `survfit` object, turns list elements that represent integer numbers into integers",{

  survfit_object <- visR::estimate_KM(data = adtte, strata = "TRTA")
  survfit_object_tidy <- tidyme(survfit_object)

  testthat::expect_true(inherits(survfit_object_tidy[["n.risk"]], "integer"))
  testthat::expect_true(inherits(survfit_object_tidy[["n.censor"]], "integer"))
  testthat::expect_true(inherits(survfit_object_tidy[["n.event"]], "integer"))
  testthat::expect_true(inherits(survfit_object_tidy[["n.strata"]], "integer"))

})