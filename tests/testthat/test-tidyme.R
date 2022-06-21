#' @title Specifications test-tidyme.R
#' @section Last updated by: Daniel Sjoberg (danield.sjoberg@@gmail.com)
#' @section Last update date: 2022-04-20T04:47:32
#'
#' @section List of tested specifications
#' T1. The function accepts an S3 object
#' T1.1 No error when a `survfit` object is passed to the function
#' T1.2 No error when a `survfit` object with missing values is passed to the function
#' T1.3 No error when a non-`survfit` S3 object is passed to the function
#' T2. The function tidies up an associated object
#' T2.1 The default method throws a message to indicate it relies on broom::tidy
#' T3. The S3 method, associated with a `survfit` object, outputs an extended tidied data.frame
#' T3.1 The S3 method, associated with a `survfit` object, returns a data.frame
#' T3.2 The S3 method, associated with a `survfit` object, has columns representing all list elements of the S3 object
#' T3.3 The S3 method, associated with a `survfit` object, turns list elements that represent integer numbers into integers
#' T3.4 The S3 method, assocated with a `survfit` object, turns the strata into a factor
#' T3.5 The S3 method, associated with a `survfit` object, add the original object as an attribute to the tidied object
#' T4 The S3 method, associated with a `survfit` object ensures compatibility with broom-dependent workflows
#' T4.1 The S3 method, associated with a `survfit` object, copies content to columns with the nomenclature used in broom::tidy

# Requirement T1 ----------------------------------------------------------

testthat::context("tidyme - T1. The function accepts an S3 object")

testthat::test_that("T1.1 No error when a `survfit` object is passed to the function", {
  survfit_object <- visR::estimate_KM(data = adtte, strata = "TRTA")
  testthat::expect_error(visR::tidyme(survfit_object), NA)
})

testthat::test_that("T1.2 No error when a `survfit` object with missing values is passed to the function", {
  adtte_with_na <- adtte
  is.na(adtte_with_na$SEX[sample(seq_len(nrow(adtte_with_na)), size = 40)]) <- TRUE

  survfit_object <- visR::estimate_KM(data = adtte_with_na, strata = "SEX")
  testthat::expect_error(visR::tidyme(survfit_object), NA)
})

testthat::test_that("T1.3 No error when a non-`survfit` S3 object is passed to the function", {
  lm_object <- stats::lm(data = adtte, TRTDUR ~ AVAL)
  testthat::expect_error(visR::tidyme(lm_object), NA)
})

# Requirement T2 ---------------------------------------------------------------

testthat::context("tidyme - T2. The function tidies up an associated object")

testthat::test_that("T2.1 The default method throws a message to indicate it relies on broom::tidy", {
  lm_object <- stats::lm(data = adtte, TRTDUR ~ AVAL)

  testthat::expect_message(visR::tidyme(lm_object))
})


# Requirement T3 ---------------------------------------------------------------

testthat::context("tidyme - T3. The S3 method, associated with a `survfit` object, outputs an extended tidied data.frame")

testthat::test_that("T3.1 The S3 method, associated with a `survfit` object, returns a data.frame", {
  survfit_object <- visR::estimate_KM(data = adtte, strata = "TRTA")
  survfit_object_tidy <- tidyme(survfit_object)
  testthat::expect_true(inherits(survfit_object_tidy, "data.frame"))
})

testthat::test_that("T3.2 The S3 method, associated with a `survfit` object, has columns representing all list elements of the S3 object", {
  survfit_object <- visR::estimate_KM(data = adtte, strata = "TRTA")
  survfit_object_tidy <- tidyme(survfit_object)

  surv_object_df <- base::with(
    survfit_object,
    data.frame(
      time = as.integer(time),
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
      stringsAsFactors = FALSE
    )
  )

  surv_object_df["PARAM"] <- rep(
    survfit_object[["PARAM"]],
    sum(survfit_object[["strata"]])
  )
  surv_object_df["PARAMCD"] <- rep(
    survfit_object[["PARAMCD"]],
    sum(survfit_object[["strata"]])
  )

  surv_object_df <- surv_object_df %>%
    dplyr::mutate(call = rep(
      list(rlang::quo_squash(survfit_object[["call"]])),
      sum(survfit_object[["strata"]])
    ))

  surv_object_df["PARAM"] <- rep(
    survfit_object[["PARAM"]],
    sum(survfit_object[["strata"]])
  )
  surv_object_df["PARAMCD"] <- rep(
    survfit_object[["PARAMCD"]],
    sum(survfit_object[["strata"]])
  )

  surv_object_df[["estimate"]] <- surv_object_df[["surv"]]
  surv_object_df[["std.error"]] <- surv_object_df[["std.err"]]
  surv_object_df[["conf.low"]] <- surv_object_df[["lower"]]
  surv_object_df[["conf.high"]] <- surv_object_df[["upper"]]

  surv_object_df["strata"] <-
    rep(
      names(survfit_object[["strata"]]),
      survfit_object[["strata"]]
    ) %>%
    {
      gsub(pattern = "TRTA=", replacement = "", x = ., fixed = TRUE)
    }

  surv_object_df["strata"] <- factor(surv_object_df[["strata"]], levels = unique(surv_object_df[["strata"]]))

  surv_object_df["n.strata"] <- rep(
    survfit_object[["n"]],
    survfit_object[["strata"]]
  )

  colnames(survfit_object_tidy)
  colnames(surv_object_df)

  # not checking call because it's a quo with an env attached, and matching the env is ¯\_(ツ)_/¯
  testthat::expect_equal(
    surv_object_df %>% dplyr::select(-call),
    survfit_object_tidy %>% dplyr::select(-call),
    check.attributes = FALSE
  )
})

testthat::test_that("T3.3 The S3 method, associated with a `survfit` object, turns list elements that represent integer numbers into integers", {
  survfit_object <- visR::estimate_KM(data = adtte, strata = "TRTA")
  survfit_object_tidy <- tidyme(survfit_object)

  testthat::expect_true(inherits(survfit_object_tidy[["n.risk"]], "integer"))
  testthat::expect_true(inherits(survfit_object_tidy[["n.censor"]], "integer"))
  testthat::expect_true(inherits(survfit_object_tidy[["n.event"]], "integer"))
  testthat::expect_true(inherits(survfit_object_tidy[["n.strata"]], "integer"))
})

testthat::test_that("T3.4 The S3 method, assocated with a `survfit` object, turns the strata into a factor", {
  dt <- adtte
  dt[["TRTA"]] <- factor(dt[["TRTA"]], levels = c("Xanomeline Low Dose", "Xanomeline High Dose", "Placebo"))

  survfit_object <- visR::estimate_KM(data = dt, strata = "TRTA")
  survfit_object_tidy <- tidyme(survfit_object)

  testthat::expect_true(inherits(survfit_object_tidy[["strata"]], "factor"))
  testthat::expect_equal(levels(survfit_object_tidy[["strata"]]), paste0(levels(dt$TRTA)))
})

testthat::test_that("T3.5 The S3 method, associated with a `survfit` object, add the original object as an attribute to the tidied object", {
  survobj <- visR::estimate_KM(adtte, strata = "TRTA")
  visr_tidy <- visR::tidyme(survobj)

  testthat::expect_equal(attributes(visr_tidy)[["survfit_object"]], survobj)
})

# Requirement T4 ---------------------------------------------------------------

testthat::context("tidyme - T4. The S3 method, associated with a `survfit` object ensures compatibility with broom-dependent workflows")

testthat::test_that("T4.1 The S3 method, associated with a `survfit` object, copies content to columns with the nomenclature used in broom::tidy", {
  survobj <- visR::estimate_KM(adtte, strata = "TRTA")
  visr_tidy <- visR::tidyme(survobj)
  broom_tidy <- as.data.frame(broom::tidy(survobj))
  have <- names(visr_tidy)
  want <- names(broom_tidy)

  testthat::expect_true(any(want %in% have))

  # construct df for testthat check
  test_df <- data.frame(
    A = c(visr_tidy[["std.err"]], visr_tidy[["surv"]], visr_tidy[["lower"]], visr_tidy[["upper"]]),
    B = c(visr_tidy[["std.error"]], visr_tidy[["estimate"]], visr_tidy[["conf.low"]], visr_tidy[["conf.high"]]),
    C = c(broom_tidy[["std.error"]], broom_tidy[["estimate"]], broom_tidy[["conf.low"]], broom_tidy[["conf.high"]])
  ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(rowwise_sd = sd(dplyr::c_across(A:C)))

  testthat::expect_true(all(test_df$rowwise_sd == 0))
})

# END OF CODE -------------------------------------------------------------
