#' @title Specifications test-get_risktable.R
#' @section Last updated by: Daniel Sjoberg (danield.sjoberg@@gmail.com)
#' @section Last update date: 2022-02-16T07:57:46
#'
#' @section List of tested specifications
#' T1. The function accepts a `survfit` object
#' T1.1 No error when a `survfit` object is passed to the function
#' T1.2 An error when a non-`survfit` object is passed to the function
#' T1.3 No error when no strata is present in the `survfit` object
#' T1.4 No error when one strata is present in the `survfit` object
#' T1.5 No error when multiple strata are present in the `survfit` object
#' T1.6 The strata combinations are used, not the individual strata
#' T1.7 When no strata were specified, an artificial strata is created 'Overall'
#' T2. The function accepts an argument that specifies the time at which the risk set is calculated
#' T2.1 An error when the times specified are negative
#' T2.2 The function orders the times argument internally to avoid errors
#' T2.3 The function proposes 11 times which are equally spaces when no times are specified
#' T2.4 The risktable is correctly calculated when only 1 timepoint is used
#' T3. The function accepts a `statlist` to be displayed for which labels can be specified
#' T3.1 No error when the `statlist` contains allowed strings
#' T3.2 An error when the `statlist` contains non-allowed strings eg 'blah'
#' T3.3 Duplicate entries are removed from `statlist`
#' T3.4 An error when the `label` is not a character vector or a factor
#' T3.5 No error when the `label` is a character vector
#' T3.6 No error when the `label` is a factor
#' T4. The function matches the length of the `label` vector with that of the `statlist` vector
#' T4.1 The function supplies defaults to increase the length of the `label` vector to same length as the `statlist` vector
#' T4.2 The supplied defaults for the `label` vector match the arguments specified in the `statlist`
#' T4.3 The function limits the length of the `label` vector to the length of the `statlist` vector
#' T5. The functions calculates requested summary across time
#' T5.1 The function is able to calculate the number of events from a `survfit` object
#' T5.2 The function is able to calculate the number of censored events from a `survfit` object
#' T5.3 The function is able to calculate the number of at risk from a `survfit` object
#' T5.4 The function is able to calculate the cumulative number of censored events from a `survfit` object
#' T5.5 The function is able to calculate the cumulative number of events from a `survfit` object
#' T6. The function groups the calculation by strata, by statlist or overall
#' T6.1 An error when the `group` argument is not equal to `strata` or `statlist`
#' T6.2 An error when the `group` argument is not of length 1
#' T6.3 No error when the `group` argument is `strata`
#' T6.4 No error when the `group` arguments is `statlist`
#' T6.5 The calculations are grouped by strata when group = `strata`
#' T6.6 The calculations are grouped by statlist when group = `statlist`
#' T6.7 The calculations are in agreement with what is expected
#' T7. The function allows the calculations to be grouped overall
#' T7.1 An error when the argument collapse is not boolean
#' T7.2 No error when the argument collapse is boolean
#' T7.3 The calculations are grouped overall when collapse = TRUE
#' T7.4 The calculations are in agreement with expectations when grouped overall
#' T7.5 No error when there is only one strata available and collapse = TRUE
#' T8. The output dataset is a data.frame with attributes for downstream processing
#' T8.1 The output dataset is a data.frame
#' T8.2 The output dataset has the attribute `time_ticks` that specifies the times
#' T8.3 The output dataset has the attribute `title` that specifies the labels used in downstream functions
#' T8.4 The output dataset has the attribute `statlist` that reflects the ´group´ used
#' T9. Tests for `get_risktable.tidycmprsk()`
#' T9.1 Results are accurate without error

# Requirement T1 ----------------------------------------------------------

testthat::context("get_risktable.survfit - T1. The function accepts a `survfit` object")

testthat::test_that("T1.1 No error when a `survfit` object is passed to the function", {
  survfit_object <- survival::survfit(
    formula = survival::Surv(AVAL, 1 - CNSR) ~ TRTA,
    data = adtte
  )

  testthat::expect_error(visR::get_risktable(survfit_object), NA)
})

testthat::test_that("T1.2 An error when a non-`survfit` object is passed to the function", {
  survfit_object <- visR::estimate_KM(adtte, strata = "STUDYID")
  class(survfit_object) <- "blah"
  testthat::expect_error(visR::get_risktable(survfit_object))
})

testthat::test_that("T1.3 No error when no strata is present in the `survfit` object", {
  survfit_object <- visR::estimate_KM(adtte)
  testthat::expect_error(visR::get_risktable(survfit_object), NA)
})

testthat::test_that("T1.4 No error when one strata is present in the `survfit` object", {
  survfit_object <- visR::estimate_KM(adtte, strata = c("SEX"))
  testthat::expect_error(visR::get_risktable(survfit_object), NA)
})

testthat::test_that("T1.5 No error when multiple strata are present in the `survfit` object", {
  survfit_object <- visR::estimate_KM(adtte, strata = c("SEX", "TRTP"))
  testthat::expect_error(visR::get_risktable(survfit_object), NA)
})

testthat::test_that("T1.6 The strata combinations are used, not the individual strata", {
  survfit_object <- visR::estimate_KM(adtte, strata = c("SEX", "TRTP"))
  risktable <- visR::get_risktable(survfit_object, group = "statlist")
  testthat::expect_equal(
    as.character(unique(risktable[["y_values"]])),
    visR:::.get_strata(names(survfit_object$strata))
  )
})

testthat::test_that("T1.7 When no strata were specified, an artificial strata is created 'Overall'", {
  survfit_object <- visR::estimate_KM(adtte)
  risktable <- visR::get_risktable(survfit_object, group = "statlist")
  unique_yvals <- as.character(unique(risktable[["y_values"]]))
  testthat::expect_equal(unique_yvals, "Overall")
})

# Requirement T2 ----------------------------------------------------------

testthat::context("get_risktable.survfit - T2. The function accepts an argument that specifies the time at which the risk set is calculated")

testthat::test_that("T2.1 An error when the times specified are negative", {
  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_error(visR::get_risktable(survfit_object,
    times = c(0, 5, -20, 9)
  ))
})

testthat::test_that("T2.2 The function orders the times argument internally to avoid errors", {
  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  times <- c(0, 9, 5, 10, 8, 3)
  risktable <- visR::get_risktable(survfit_object, times = times)
  testthat::expect_equal(unique(risktable[["time"]]), times[order(times)])
})

testthat::test_that("T2.3 The function proposes 11 times which are equally spaces when no times are specified", {
  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  risktable <- visR::get_risktable(survfit_object)

  testthat::expect_equal(length(risktable[["time"]]), 11)
  testthat::expect_equal(risktable[["time"]], seq(0, 200, 20))
})

testthat::test_that("T2.4 The risktable is correctly calculated when only 1 timepoint is used", {
  survfit_object <- visR::estimate_KM(adtte)

  risktable <- visR::get_risktable(survfit_object, times = 20, statlist = c("n.risk", "n.event", "n.censor", "cum.event", "cum.censor"))

  expect <- c(summary(survfit_object, times = 20)[["n.risk"]], summary(survfit_object, times = 20)[["n.event"]], summary(survfit_object, times = 20)[["n.censor"]], summary(survfit_object, times = 20)[["n.event"]], summary(survfit_object, times = 20)[["n.censor"]])

  testthat::expect_equal(risktable[["Overall"]], expect)
})

# Requirement T3 ----------------------------------------------------------

testthat::context("get_risktable.survfit - T3. The function accepts a `statlist` to be displayed for which labels can be specified")

testthat::test_that("T3.1 No error when the `statlist` contains allowed strings", {
  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_error(visR::get_risktable(survfit_object, statlist = "n.risk"), NA)
})

testthat::test_that("T3.2 An error when the `statlist` contains non-allowed strings eg 'blah'", {
  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_error(visR::get_risktable(survfit_object,
    statlist = c("blah", "n.risk", -3)
  ))
})

testthat::test_that("T3.3 Duplicate entries are removed from `statlist`", {
  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  risktable <- visR::get_risktable(survfit_object,
    statlist = c("n.risk", "n.risk")
  )
  n_levels <- length(risktable[which(risktable[["time"]] == 0), "y_values"])

  testthat::expect_equal(n_levels, 1)
})

testthat::test_that("T3.4 An error when the `label` is not a character vector or a factor", {
  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_error(visR::get_risktable(survfit_object, label = 10))
})

testthat::test_that("T3.5 No error when the `label` is a character vector", {
  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  label <- c("blah", "moreblah")
  testthat::expect_error(visR::get_risktable(survfit_object, label = label), NA)
})

testthat::test_that("T3.6 No error when the `label` is a factor", {
  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  label <- as.factor(c("blah", "moreblah"))
  testthat::expect_error(visR::get_risktable(survfit_object, label = label), NA)
})

# Requirement T4 ----------------------------------------------------------

testthat::context("get_risktable.survfit - T4. The function matches the length of the `label` vector with that of the `statlist` vector")

testthat::test_that("T4.1 The function supplies defaults to increase the length of the `label` vector to same length as the `statlist` vector", {
  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  risktable <- visR::get_risktable(survfit_object,
    label = c("label 1"),
    statlist = c("n.risk", "n.censor")
  )
  testthat::expect_equal(nrow(unique(risktable["y_values"])), 2)
})

testthat::test_that("T4.2 The supplied defaults for the `label` vector match the arguments specified in the `statlist`", {
  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  risktable <- visR::get_risktable(survfit_object,
    statlist = c("n.risk", "n.censor")
  )
  testthat::expect_equal(
    as.character(unique(unlist(risktable["y_values"]))),
    c("At risk", "Censored")
  )
})

testthat::test_that("T4.3 The function limits the length of the `label` vector to the length of the `statlist` vector", {
  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  risktable <- visR::get_risktable(survfit_object,
    label = paste("label", seq(1, 4)),
    statlist = c("n.risk", "n.censor")
  )
  testthat::expect_equal(nrow(unique(risktable["y_values"])), 2)
  testthat::expect_equal(
    as.character(unique(unlist(risktable["y_values"]))),
    c("label 1", "label 2")
  )
})

# Requirement T5 ----------------------------------------------------------

testthat::context("get_risktable.survfit - T5. T5. The functions calculates requested summary across time")

testthat::test_that("T5.1 The function is able to calculate the number of events from a `survfit` object", {
  survobj <- survival::survfit.formula(data = adtte, formula = survival::Surv(AVAL, 1 - CNSR) ~ 1)
  atrisk <- summary(survobj, times = c(0, 20, 40, 60, 80, 100, 120, 140, 160, 180, 200), extend = TRUE)[["n.risk"]]

  atrisk_visr <- visR::estimate_KM(adtte) %>%
    visR::get_risktable(group = "statlist") %>%
    dplyr::pull(n.risk)

  testthat::expect_equal(atrisk, atrisk_visr)
})

testthat::test_that("T5.2 The function is able to calculate the number of censored events from a `survfit` object", {
  survobj <- survival::survfit.formula(data = adtte, formula = survival::Surv(AVAL, CNSR) ~ 1)
  censored <- summary(survobj, times = c(0, 20, 40, 60, 80, 100, 120, 140, 160, 180, 200), extend = TRUE)[["n.event"]]

  censor_visr <- visR::estimate_KM(adtte) %>%
    visR::get_risktable(group = "statlist", statlist = "n.censor") %>%
    dplyr::pull(n.censor)

  testthat::expect_equal(censored, censor_visr)
})

testthat::test_that("T5.3 The function is able to calculate the number of at risk from a `survfit` object", {
  survobj <- survival::survfit.formula(data = adtte, formula = survival::Surv(AVAL, 1 - CNSR) ~ 1)
  events <- summary(survobj, times = c(0, 20, 40, 60, 80, 100, 120, 140, 160, 180, 200), extend = TRUE)[["n.event"]]

  events_visr <- visR::estimate_KM(adtte) %>%
    visR::get_risktable(group = "statlist", statlist = "n.event") %>%
    dplyr::pull(n.event)

  testthat::expect_equal(events, events_visr)
})

testthat::test_that("T5.4 The function is able to calculate the cumulative number of censored events from a `survfit` object", {
  survobj <- survival::survfit.formula(data = adtte, formula = survival::Surv(AVAL, CNSR) ~ 1)
  censored <- summary(survobj, times = c(0, 20, 40, 60, 80, 100, 120, 140, 160, 180, 200), extend = TRUE)[["n.event"]]
  cum.censor <- cumsum(censored)

  cumcensor_visr <- visR::estimate_KM(adtte) %>%
    visR::get_risktable(group = "statlist", statlist = "cum.censor") %>%
    dplyr::pull(cum.censor)

  testthat::expect_equal(cum.censor, cumcensor_visr)
})

testthat::test_that("T5.5 The function is able to calculate the cumulative number of events from a `survfit` object", {
  survobj <- survival::survfit.formula(data = adtte, formula = survival::Surv(AVAL, 1 - CNSR) ~ 1)
  events <- summary(survobj, times = c(0, 20, 40, 60, 80, 100, 120, 140, 160, 180, 200), extend = TRUE)[["n.event"]]
  cum.events <- cumsum(events)

  cumevents_visr <- visR::estimate_KM(adtte) %>%
    visR::get_risktable(group = "statlist", statlist = "cum.event") %>%
    dplyr::pull(cum.event)

  testthat::expect_equal(cum.events, cumevents_visr)
})

# Requirement T6 ---------------------------------------------------------------

testthat::context("get_risktable.survfit - T6. The function groups the calculation by strata, by statlist or overall")

testthat::test_that("T6.1 An error when the `group` argument is not equal to `strata` or `statlist`", {
  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_error(visR::get_risktable(survfit_object, group = "blah"))
})

testthat::test_that("T6.2 An error when the `group` argument is not of length 1", {
  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_error(visR::get_risktable(survfit_object,
    group = c("statlist", "strata")
  ))
})

testthat::test_that("T6.3 No error when the `group` argument is `strata`", {
  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_error(visR::get_risktable(survfit_object, group = "strata"), NA)
})

testthat::test_that("T6.4 No error when the `group` arguments is `statlist`", {
  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_error(visR::get_risktable(survfit_object, group = "statlist"), NA)
})

testthat::test_that("T6.5 The calculations are grouped by strata when group = `strata`", {
  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  risktable <- visR::get_risktable(

    survfit_object,
    group = "strata",
    statlist = c("n.risk", "n.censor", "n.event")
  )
  testthat::expect_equal(
    object = colnames(risktable[3:length(colnames(risktable))]),
    expected = gsub("^.*=", "", names(survfit_object$strata))
  )
})

testthat::test_that("T6.6 The calculations are grouped by statlist when group = `statlist`", {
  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")

  risktable <- visR::get_risktable(
    survfit_object,
    group = "statlist",
    statlist = c("n.risk", "n.censor", "n.event", "cum.censor", "cum.event")
  )

  testthat::expect_equal(
    object = levels(risktable[["y_values"]]),
    expected = gsub("^.*=", "", names(survfit_object$strata))
  )
  testthat::expect_equal(
    object = colnames(risktable[3:length(colnames(risktable))]),
    expected = c("n.risk", "n.censor", "n.event", "cum.censor", "cum.event")
  )
})

testthat::test_that("T6.7 The calculations are in agreement with what is expected", {

  ## test for strata
  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  risktable_visR <- visR::get_risktable(survfit_object, group = "strata")
  attr(risktable_visR, "time_ticks") <- NULL
  attr(risktable_visR, "title") <- NULL
  attr(risktable_visR, "statlist") <- NULL

  risktable_ref <- data.frame(
    `time` = seq(0, 200, 20),
    `y_values` = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L),
      .Label = "At risk",
      class = "factor"
    ),
    `Placebo` = c(86, 75, 65, 59, 50, 47, 45, 42, 40, 35, 0),
    `Xanomeline High Dose` = c(84, 48, 31, 14, 7, 4, 4, 4, 4, 3, 0),
    `Xanomeline Low Dose` = c(84, 58, 31, 20, 14, 12, 8, 6, 6, 5, 0),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  class(risktable_ref) <- c("risktable", class(risktable_ref))

  testthat::expect_equal(risktable_visR, risktable_ref)

  ## test for statlist
  survfit_object <- visR::estimate_KM(adtte)
  risktable_visR <- visR::get_risktable(survfit_object,
    times = c(0, 20),
    statlist = c("n.censor", "n.risk", "n.event", "cum.censor", "cum.event")
  )
  attr(risktable_visR, "time_ticks") <- NULL
  attr(risktable_visR, "title") <- NULL
  attr(risktable_visR, "statlist") <- NULL

  risktable_ref <- structure(
    list(
      time = c(0, 20, 0, 20, 0, 20, 0, 20, 0, 20),
      y_values = structure(c(1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L, 5L, 5L), .Label = c("Censored", "At risk", "Events", "Cum. Censored", "Cum. Events"), class = "factor"),
      Overall = c(0, 19, 254, 181, 0, 57, 0, 19, 0, 57)
    ),
    row.names = c(2L, 7L, 1L, 6L, 3L, 8L, 4L, 9L, 5L, 10L),
    class = c("risktable", "data.frame")
  )

  testthat::expect_equal(risktable_visR, risktable_ref)

  attributes(risktable_ref)
})

# Requirement T7 ---------------------------------------------------------------

testthat::context("get_risktable.survfit - T7. The function allows the calculations to be grouped overall ")

testthat::test_that("T7.1 An error when the argument collapse is not boolean", {
  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_error(visR::get_risktable(survfit_object, collapse = "blah"))
})

testthat::test_that("T7.2 No error when the argument collapse is boolean", {
  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_error(visR::get_risktable(survfit_object, collapse = TRUE), NA)
})

testthat::test_that("T7.3 The calculations are grouped overall when collapse = TRUE", {
  survfit_object_trt <- visR::estimate_KM(adtte, strata = "TRTA")
  survfit_object_all <- visR::estimate_KM(adtte)

  risktable_visR_trt <- visR::get_risktable(survfit_object_trt, group = "strata", collapse = TRUE)
  risktable_visR_all <- visR::get_risktable(survfit_object_all, group = "strata")

  testthat::expect_equal(risktable_visR_trt, risktable_visR_all)
})

testthat::test_that("T7.4 The calculations are in agreement with expectations when grouped overall", {
  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  risktable_ungroup <- visR::get_risktable(survfit_object, collapse = FALSE)
  risktable_group <- visR::get_risktable(survfit_object, collapse = TRUE)

  risktable_test <- risktable_ungroup
  risktable_test[["Overall"]] <- rowSums(risktable_test[, 3:length(colnames(risktable_test))])
  risktable_test <- risktable_test[, c("time", "y_values", "Overall")]

  attributes(risktable_group) <- NULL
  attributes(risktable_test) <- NULL

  testthat::expect_equal(risktable_test, risktable_group)
})

testthat::test_that("T7.5 No error when there is only one strata available and collapse = TRUE", {
  survfit_object <- visR::estimate_KM(adtte)
  risktable_visR <- visR::get_risktable(survfit_object, collapse = TRUE)
  testthat::expect_error(visR::get_risktable(survfit_object, collapse = TRUE), NA)
})

# Requirement T8 ----------------------------------------------------------

testthat::context("get_risktable.survfit - T8. The output dataset is a data.frame with attributes for downstream processing")

testthat::test_that("T8.1 The output dataset is a data.frame", {
  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  testthat::expect_true(inherits(visR::get_risktable(survfit_object), "data.frame"))
})

testthat::test_that("T8.2 The output dataset has the attribute `time_ticks` that specifies the times", {
  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")
  times <- c(20, 40, 80)
  risktable <- visR::get_risktable(survfit_object, times = times)
  testthat::expect_equal(attr(risktable, "time_ticks"), times)
})

testthat::test_that("T8.3 The output dataset has the attribute `title` that specifies the labels used in downstream functions", {
  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")

  risktable_bystat <- visR::get_risktable(survfit_object, group = "statlist")
  risktable_bystrat <- visR::get_risktable(survfit_object, group = "strata")

  testthat::expect_equal(attr(risktable_bystat, "title"), "At risk")
  testthat::expect_equal(
    attr(risktable_bystrat, "title"),
    sub(".*=", "", names(survfit_object$strata))
  )
})

testthat::test_that("T8.4 The output dataset has the attribute `statlist` that reflects the ´group´ used", {
  survfit_object <- visR::estimate_KM(adtte, strata = "TRTA")

  risktable_bystat <- visR::get_risktable(survfit_object, group = "statlist")
  risktable_bystrat <- visR::get_risktable(survfit_object, group = "strata")

  testthat::expect_equal(attr(risktable_bystat, "statlist"), "n.risk")
  testthat::expect_equal(
    attr(risktable_bystrat, "statlist"),
    sub(".*=", "", names(survfit_object$strata))
  )
})

# Requirement T9 ----------------------------------------------------------

testthat::context("get_risktable.survfit - T9. Tests for `get_risktable.tidycmprsk()`")

testthat::test_that("T9.1 Results are accurate without error", {
  cuminc <-
    visR::estimate_cuminc(
      tidycmprsk::trial,
      AVAL = "ttdeath",
      CNSR = "death_cr"
    )

  testthat::expect_equal(
    cuminc %>%
      get_risktable(times = 12, statlist = c("n.event", "n.risk", "n.censor")),
    cuminc %>%
      tidycmprsk::tidy(times = 12) %>%
      dplyr::filter(outcome %in% "death from cancer") %>%
      dplyr::select(time, n.risk, n.event, n.censor) %>%
      tidyr::pivot_longer(cols = c(n.risk, n.event, n.censor)) %>%
      dplyr::mutate(
        name = factor(name,
          levels = c("n.event", "n.risk", "n.censor"),
          labels = c("Events", "At Risk", "Censored")
        )
      ) %>%
      dplyr::arrange(.data[["name"]]) %>%
      rlang::set_names(c("time", "y_values", "Overall")) %>%
      as.data.frame(),
    ignore_attr = TRUE,
    check.attributes = FALSE
  )

  cuminc2 <-
    visR::estimate_cuminc(
      tidycmprsk::trial,
      AVAL = "ttdeath",
      CNSR = "death_cr",
      strata = "trt"
    )

  testthat::expect_equal(
    cuminc2 %>%
      get_risktable(
        times = 12,
        statlist = c("n.event", "n.risk", "n.censor"),
        collapse = TRUE
      ),
    cuminc %>%
      tidycmprsk::tidy(times = 12) %>%
      dplyr::filter(outcome %in% "death from cancer") %>%
      dplyr::select(time, n.risk, n.event, n.censor) %>%
      tidyr::pivot_longer(cols = c(n.risk, n.event, n.censor)) %>%
      dplyr::mutate(
        name = factor(name,
          levels = c("n.event", "n.risk", "n.censor"),
          labels = c("Events", "At Risk", "Censored")
        )
      ) %>%
      dplyr::arrange(.data[["name"]]) %>%
      rlang::set_names(c("time", "y_values", "Overall")) %>%
      as.data.frame(),
    ignore_attr = TRUE,
    check.attributes = FALSE
  )

  testthat::expect_equal(
    cuminc2 %>%
      get_risktable(
        times = 12,
        statlist = c("n.event", "n.risk", "n.censor"),
        collapse = FALSE
      ),
    cuminc2 %>%
      tidycmprsk::tidy(times = 12) %>%
      dplyr::filter(outcome %in% "death from cancer") %>%
      dplyr::select(time, strata, n.risk, n.event, n.censor) %>%
      tidyr::pivot_longer(cols = c(n.risk, n.event, n.censor)) %>%
      tidyr::pivot_wider(
        id_cols = c(time, name), names_from = strata, values_from = value
      ) %>%
      dplyr::mutate(
        name = factor(name,
          levels = c("n.event", "n.risk", "n.censor"),
          labels = c("Events", "At Risk", "Censored")
        )
      ) %>%
      dplyr::arrange(.data[["name"]]) %>%
      dplyr::rename(y_values = name) %>%
      as.data.frame(),
    ignore_attr = TRUE,
    check.attributes = FALSE
  )

  testthat::expect_equal(
    cuminc2 %>%
      get_risktable(
        times = 12,
        statlist = c("n.event", "n.risk", "n.censor"),
        group = "statlist"
      ),
    cuminc2 %>%
      tidycmprsk::tidy(times = 12) %>%
      dplyr::filter(outcome %in% "death from cancer") %>%
      dplyr::select(time, strata, n.risk, n.event, cum.event, n.censor, cum.censor) %>%
      dplyr::rename(y_values = strata) %>%
      as.data.frame(),
    ignore_attr = TRUE,
    check.attributes = FALSE
  )

  expect_equal(
    cuminc2 %>%
      get_risktable(
        times = 12,
        statlist = c("n.event", "n.risk", "n.censor"),
        label = c("No. Events", "No. Risk", "No. Censored")
      ) %>%
      dplyr::pull(y_values) %>%
      levels(),
    c("No. Events", "No. Risk", "No. Censored")
  )
})

# END OF CODE -------------------------------------------------------------
