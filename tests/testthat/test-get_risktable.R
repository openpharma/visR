#' @title Specifications get_risktable.survfit
#' @section Last updated by:
#' Steven Haesendonckx
#' @section Last update date:
#' 10-APR-2021

# Specifications ----------------------------------------------------------

#' T1. The function accepts a `survfit` object
#' T1.1 No error when a `survfit` object is passed to the function
#' T1.2 An error when a non-`survfit` object is passed to the function
#' T2. The function accepts an argument that specifies the time at which the risk set is calculated
#' T2.1 An error when the times specified are negative
#' T2.2 The function orders the times argument internally to avoid errors
#' T2.3 The function proposes 11 times which are equally spaced when no times are specified 
#' T3. The function accepts a `statlist` to be displayed for which labels can be specified
#' T3.1 No error when the `statlist` contains allowed strings
#' T3.2 An error when the `statlist` contains non-allowed strings eg "blah"
#' T3.3 Duplicate entries are removed from `statlist`
#' T3.4 An error when the `label` is not a character vector or a factor
#' T3.5 No error when the `label` is a character vector
#' T3.6 No error when the `label` is a factor
#' T4. The function matches the length of the `label` vector with that of the `statlist` vector
#' T4.1 The function supplies defaults to increase the length of the `label` vector to same length as the `statlist` vector 
#' T4.2 The supplied defaults for the `label` vector match the arguments specified in the `statlist`
#' T4.3 The function limits the length of the `label` vector to the length of the `statlist` vector
#' T5. The function groups the calculation by strata, by statlist or overall
#' T5.1 An error when the `group` argument is not equal to `strata` or `statlist`
#' T5.2 An error when the `group` argument is not of length 1
#' T5.3 No error when the `group` argument is `strata`
#' T5.4 No error when the `group` arguments is `statlist`
#' T5.5 The calculations are grouped by strata when group = `strata`
#' T5.6 The calculations are grouped by statlist when group = `statlist`
#' T5.7 The calculations are in agreement with expectations
#' T6. The function allows the calculations to be grouped overall 
#' T6.1 An error when the argument collapse is not boolean
#' T6.2 No error when the argument collapse is boolean
#' T6.3 The calculations are grouped overall when collapse = TRUE
#' T6.4 The calculations are in agreement with expectations when grouped overall
#' T6.5 No error when there is only one strata available and collapse = TRUE
#' T7. The output dataset is a data.frame with attributes for downstream processing
#' T7.1 The output dataset is a data.frame
#' T7.2 The output dataset has the attribute `time_ticks` that specifies the times
#' T7.3 The output dataset has the attribute `title` that specifies the labels used in downstream functions
#' T7.4 The output dataset has the attribute `statlist` that reflects the ´group´ used

# Requirement T1 ----------------------------------------------------------

context("get_risktable.survfit - T1. The function accepts a `survfit` object")

testthat::test_that("T1.1. No error when a `survfit` object is passed to the function",{

  survfit_object <- survival::survfit(formula = survival::Surv(AVAL, 1 - CNSR) ~ TRTA, data = adtte)
  
  testthat::expect_error(visR::get_risktable(survfit_object), NA)

})

testthat::test_that("TT1.2 An error when a non-`survfit` object is passed to the function",{

  survfit_object <- visR::estimate_KM(adtte, strata = "STUDYID")
  class(survfit_object) <- "blah"
  testthat::expect_error(visR::get_risktable(survfit_object))

})

# Requirement T2 ----------------------------------------------------------

context("get_risktable.survfit - T2. The function accepts an argument that specifies the time at which the risk set is calculated")

testthat::test_that("T2.1 An error when the times specified are negative",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_error(visR::get_risktable(survfit_object, times = c(0, 5, -20, 9)))

})

testthat::test_that("T2.2 The function orders the times argument internally to avoid errors",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  times <- c(0, 9, 5, 10, 8, 3)
  risktable <- visR::get_risktable(survfit_object, times = times)
  testthat::expect_equal(unique(risktable[["time"]]), times[order(times)])
})

testthat::test_that("T2.3 The function proposes 11 times which are equally spaces when no times are specified",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  risktable <- visR::get_risktable(survfit_object)
  
  testthat::expect_equal(length(risktable[["time"]]), 11)
  testthat::expect_equal(risktable[["time"]], seq(0, 200, 20))
})


# Requirement T3 ----------------------------------------------------------

context("get_risktable.survfit - T3. The function accepts a `statlist` to be displayed for which labels can be specified")

testthat::test_that("T3.1 No error when the `statlist` contains allowed strings",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_error(visR::get_risktable(survfit_object, statlist = "n.risk"), NA)
})

testthat::test_that("T3.2 An error when the `statlist` contains non-allowed strings eg 'blah'",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_error(visR::get_risktable(survfit_object, statlist = c("blah", "n.risk", -3)))
})

testthat::test_that("T3.3 Duplicate entries are removed from `statlist`",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  risktable <- visR::get_risktable(survfit_object, statlist = c("n.risk", "n.risk"))
  testthat::expect_equal(length(risktable[which(risktable[["time"]] == 0), "y_values"]), 1)
})

testthat::test_that("T3.4 An error when the `label` is not a character vector or a factor",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_error(visR::get_risktable(survfit_object, label = 10))
})

testthat::test_that("T3.5 No error when the `label` is a character vector",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  label = c("blah", "moreblah")
  testthat::expect_error(visR::get_risktable(survfit_object, label = label), NA)
})

testthat::test_that("T3.6 No error when the `label` is a factor",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  label = as.factor(c("blah", "moreblah"))
  testthat::expect_error(visR::get_risktable(survfit_object, label = label), NA)
})

# Requirement T4 ----------------------------------------------------------

context("get_risktable.survfit - T4. The function matches the length of the `label` vector with that of the `statlist` vector")

testthat::test_that("T4.1 The function supplies defaults to increase the length of the `label` vector to same length as the `statlist` vector",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  risktable <- visR::get_risktable(survfit_object, label = c("label 1"), statlist = c("n.risk", "n.censor"))
  testthat::expect_equal(nrow(unique(risktable["y_values"])), 2)
})

testthat::test_that("T4.2 The supplied defaults for the `label` vector match the arguments specified in the `statlist`",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  risktable <- visR::get_risktable(survfit_object, statlist = c("n.risk", "n.censor"))
  testthat::expect_equal(as.character(unique(unlist(risktable["y_values"]))), c("At risk", "Censored"))
})

testthat::test_that("T4.3 The function limits the length of the `label` vector to the length of the `statlist` vector",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  risktable <- visR::get_risktable(survfit_object, label = c("label 1", "label 2", "label 3", "label 4"), statlist = c("n.risk", "n.censor"))
  testthat::expect_equal(nrow(unique(risktable["y_values"])), 2)
  testthat::expect_equal(as.character(unique(unlist(risktable["y_values"]))), c("label 1", "label 2"))
})

# Requirement T5 ----------------------------------------------------------

context("get_risktable.survfit - T5. The function groups the calculation by strata, by statlist or overall")

testthat::test_that("T5.1 An error when the `group` argument is not equal to `strata` or `statlist`",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_error(visR::get_risktable(survfit_object, group = "blah"))
})

testthat::test_that("T5.2 An error when the `group` argument is not of length 1",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_error(visR::get_risktable(survfit_object, group = c("statlist", "strata")))
})

testthat::test_that("T5.3 No error when the `group` argument is `strata`",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_error(visR::get_risktable(survfit_object, group = "strata"), NA)
})

testthat::test_that("T5.4 No error when the `group` arguments is `statlist`",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_error(visR::get_risktable(survfit_object, group = "statlist"), NA)
})

testthat::test_that("T5.5 The calculations are grouped by strata when group = `strata`",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  risktable <- visR::get_risktable(survfit_object, group = "strata")
  testthat::expect_equal(colnames(risktable[3:length(colnames(risktable))]) , gsub("^.*=", "", names(survfit_object$strata)))
})

testthat::test_that("T5.6 The calculations are grouped by statlist when group = `statlist`",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  risktable <- visR::get_risktable(survfit_object, group = "statlist")
  testthat::expect_equal(levels(risktable[["y_values"]]) , gsub("^.*=", "", names(survfit_object$strata)))
  testthat::expect_equal(colnames(risktable[3:length(colnames(risktable))]), c("n.risk", "n.event", "n.censor"))
})

testthat::test_that("T5.7 The calculations are in agreement with what is expected",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  risktable_visR <- visR::get_risktable(survfit_object, group = "strata")
  attr(risktable_visR, "time_ticks") <- NULL
  attr(risktable_visR, "title") <- NULL
  attr(risktable_visR, "statlist") <- NULL
  
  risktable_ref <-  data.frame(
  `time` = seq(0, 200, 20),
  `y_values` = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), .Label = "At risk", class = "factor"),
  `Placebo` = c(86, 75, 65, 59, 50, 47, 45, 42, 40, 35, 0),
  `Xanomeline High Dose` = c(84, 48, 31, 14, 7, 4, 4, 4, 4, 3, 0),
  `Xanomeline Low Dose` = c(84, 58, 31, 20, 14, 12, 8, 6, 6, 5, 0),
  stringsAsFactors = FALSE,
  check.names = FALSE)
  
  testthat::expect_equal(risktable_visR, risktable_ref)
})

# Requirement T6 ----------------------------------------------------------

context("get_risktable.survfit - T6. The function allows the calculations to be grouped overall ")

testthat::test_that("T6.1 An error when the argument collapse is not boolean",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_error(visR::get_risktable(survfit_object, collapse = "blah"))
})

testthat::test_that("T6.2 No error when the argument collapse is boolean",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_error(visR::get_risktable(survfit_object, collapse = TRUE), NA)
})

testthat::test_that("T6.3 The calculations are grouped overall when collapse = TRUE",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  risktable_ungroup <- visR::get_risktable(survfit_object, collapse = FALSE)
  risktable_group <- visR::get_risktable(survfit_object, collapse = TRUE)
  attr(risktable_group, "time_ticks") <- NULL
  attr(risktable_group, "title") <- NULL
  attr(risktable_group, "statlist") <- NULL

  risktable_test <- risktable_ungroup
  risktable_test[["Overall"]] <- base::rowSums(risktable_test[,3:length(colnames(risktable_test))])
  risktable_test <- risktable_test[,c("time", "y_values", "Overall")]
    
  testthat::expect_equal(risktable_test, risktable_group)
})

testthat::test_that("T6.4 The calculations are in agreement with expectations when grouped overall",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  risktable_visR <- visR::get_risktable(survfit_object, group = "strata", collapse = TRUE)
  attr(risktable_visR, "time_ticks") <- NULL
  attr(risktable_visR, "title") <- NULL
  attr(risktable_visR, "statlist") <- NULL
  
  risktable_ref <-  data.frame(
  `time` = seq(0, 200, 20),
  `y_values` = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), .Label = "At risk", class = "factor"),
  `Overall` = c(254, 181, 127, 93, 71, 63, 57, 52, 50, 43, 0),
  stringsAsFactors = FALSE,
  check.names = FALSE)
  
  testthat::expect_equal(risktable_visR, risktable_ref)
})

testthat::test_that("T6.5 No error when there is only one strata available and collapse = TRUE",{

  survfit_object <- visR::estimate_KM(adtte)
  risktable_visR <- visR::get_risktable(survfit_object, collapse = TRUE)
  testthat::expect_error(visR::get_risktable(survfit_object, collapse = TRUE), NA)
})

# Requirement T7 ----------------------------------------------------------

context("get_risktable.survfit - T7. The output dataset is a data.frame with attributes for downstream processing")

testthat::test_that("T7.1 The output dataset is a data.frame",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_true(inherits(visR::get_risktable(survfit_object), "data.frame"))
})

testthat::test_that("T7.2 The output dataset has the attribute `time_ticks` that specifies the times",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  times <- c(20, 40, 80)
  risktable <- visR::get_risktable(survfit_object, times = times)
  testthat::expect_equal(attr(risktable, "time_ticks"), times)
})

testthat::test_that("T7.3 The output dataset has the attribute `title` that specifies the labels used in downstream functions",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")

  risktable_bystat <- visR::get_risktable(survfit_object, group = "statlist")
  risktable_bystrat <- visR::get_risktable(survfit_object, group = "strata")
  
  testthat::expect_equal(attr(risktable_bystat, "title"), "At risk")
  testthat::expect_equal(attr(risktable_bystrat, "title"), base::sub('.*=', '', names(survfit_object$strata)))
})

testthat::test_that("T7.4 The output dataset has the attribute `statlist` that reflects the ´group´ used",{

  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")

  risktable_bystat <- visR::get_risktable(survfit_object, group = "statlist")
  risktable_bystrat <- visR::get_risktable(survfit_object, group = "strata")
  
  testthat::expect_equal(attr(risktable_bystat, "statlist"), "n.risk")
  testthat::expect_equal(attr(risktable_bystrat, "statlist"), base::sub('.*=', '', names(survfit_object$strata)))
})

# END OF CODE ----------------------------------------------------------

