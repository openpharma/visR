#' @title Specifications test-visr_survfit.R
#' @section Last updated by: Daniel Sjoberg (danield.sjoberg@@gmail.com)
#' @section Last update date: 2022-04-20T04:47:32
#'
#' @section List of tested specifications
#' T1. visR::visr() only accepts `survfit` or `attrition`.
#' T1.1 No error when applied to a `survfit` object.
#' T1.2 No error when applied to a `attrition` object.
#' T1.3 An error when applied to an object that is not `survfit`.
#' T2. Invalid parameters are captured when applying `visR::visr()` to a `survfit` object and respective warnings/errors are thrown.
#' T2.1 No error when `x_label` is `NULL`, a `character` string or an `expression`.
#' T2.2 No error when `y_label` is `NULL`, a `character` string or an `expression`.
#' T2.3 No error when `x_units` is `NULL` or a `character` string.
#' T2.4 No error when `x_ticks` is `NULL` or a `numeric` value.
#' T2.5 No error when `y_ticks` is `NULL` or a `numeric` value.
#' T2.6 No error when a valid option is passed to `legend_position`.
#' T2.7 An error when `x_label` is not `NULL`, not a `character` string nor an `expression`.
#' T2.8 No warning when `x_label` is `NULL` and the `survfit` object has a `PARAM` and a `PARAMCD` column.
#' T2.9 No warning when `x_label` is `NULL` and the `survfit` object has a `PARAM` but no `PARAMCD` column.
#' T2.10 No warning when `x_label` is `NULL` and the `survfit` object has no `PARAM` but a `PARAMCD` column.
#' T2.11 No warning when `x_label` is `NULL` and the `survfit` object has no `PARAM` and no `PARAMCD` column.
#' T2.12 When `x_label` is `NULL` and the `survfit` object does have a `PARAM` column, the `x_label` is set to `PARAM`.
#' T2.13 When `x_label` is `NULL` and the `survfit` object does not have a `PARAM` but a `PARAMCD` column, the `x_label` is set to `PARAMCD`.
#' T2.14 When `x_label` is `NULL` and the `survfit` object does have a `PARAM` but no `PARAMCD` column, the `x_label` is set to `PARAM`.
#' T2.15 When `x_label` is `NULL` and the `survfit` object does not have a `PARAM` or `PARAMCD` column, the `x_label` is `NULL`.
#' T2.16 A warning when `x_label` is `NULL` and the `PARAM` column has more than one unique entry.
#' T2.17 A warning when `x_label` is `NULL` and the `PARAMCD` column has more than one unique entry.
#' T2.18 When `x_label` and `x_unit` are both defined, they are concatenated into the final `x_label`.
#' T2.19 An error when `y_label` is not `NULL`, a `character` string or an `expression`.
#' T2.20 An error when `x_units` is not `NULL` or a `character` string.
#' T2.21 An error when `x_ticks` is not `NULL` or a `numeric`.
#' T2.22 An error when `y_ticks` is not `NULL` or a `numeric`.
#' T2.23 No error when a valid option is passed to `legend_position`.
#' T2.24 An error when the string is not amongst the valid options for `legend_position`.
#' T2.25 An error when an undefined option is passed to `legend_position`.
#' T3. The y-axis properties are correctly deducted from the provided `fun` when applying `visR::visr()` to a `survfit` object.
#' T3.1 No error when `y_label` is `NULL` and `fun` is one of the valid string options.
#' T3.2 An error when `y_label` is `NULL`, `fun` is a string but not one of the valid options.
#' T3.3 No error when `y_label` is a string and `fun` is a function.
#' T3.4 An error when `y_label` is `NULL` and `fun` is a function.
#' T3.5 A warning when the provided function causes undefined values, f.e. log(-log(2)).
#' T3.6 An error when `fun` is neither a `character` string nor a function.
#' T3.7 The `fun` argument is stored in the final object as attribute `fun`.
#' T4. The legend follows the model strata label and levels.
#' T4.1 The legend follows the model strata levels.
#' T4.2 The color legend title represents the strata label.
#' T5. The final object is a ggplot of class `ggsurvfit`.
#' T5.1 The final object is a ggplot of class `ggplot`.
#' T5.2 The final object is a ggplot of class `ggsurvfit`.
#' T6. The final object does not exclude parts of KM estimate.
#' T6.1 The final object zooms and does not exclude trialing pieces of lines.

# Requirement T1 ----------------------------------------------------------

testthat::context("visr_plot - T1. visR::visr() only accepts `survfit` or `attrition`.")

testthat::test_that("T1.1 No error when applied to a `survfit` object.", {
  survfit_object <- adtte %>%
    visR::estimate_KM("SEX")

  testthat::expect_error(survfit_object %>% visR::visr(), NA)
})

testthat::test_that("T1.2 No error when applied to a `attrition` object.", {
  attrition_object <- adtte %>%
    visR::get_attrition(
      criteria_descriptions = c(
        "1. Placebo Group",
        "2. Be 75 years of age or older.",
        "3. White",
        "4. Site 709"
      ),
      criteria_conditions = c(
        "TRTP=='Placebo'",
        "AGE>=75",
        "RACE=='WHITE'",
        "SITEID==709"
      ),
      subject_column_name = "USUBJID"
    )

  testthat::expect_error(attrition_object %>% visR::visr("Criteria", "Remaining N"), NA)
})

testthat::test_that("T1.3 An error when applied to an object that is not `survfit`.", {
  testthat::expect_error("visR" %>% visR::visr.survfit())
  testthat::expect_error(1 %>% visR::visr.survfit())
  testthat::expect_error(NA %>% visR::visr.survfit())
  testthat::expect_error(TRUE %>% visR::visr.survfit())
  testthat::expect_error(list() %>% visR::visr.survfit())
})

# Requirement T2 ---------------------------------------------------------------

testthat::context("visr_plot - T2. Invalid parameters are captured when applying `visR::visr()` to a `survfit` object and respective warnings/errors are thrown.")

testthat::test_that("T2.1 No error when `x_label` is `NULL`, a `character` string or an `expression`.", {
  survfit_object <- adtte %>%
    visR::estimate_KM("SEX")

  testthat::expect_error(survfit_object %>% visR::visr(), NA) # Equal to x_label = NULL
  testthat::expect_error(survfit_object %>% visR::visr(x_label = NULL), NA)
  testthat::expect_error(survfit_object %>% visR::visr(x_label = "visR"), NA)
  testthat::expect_error(survfit_object %>% visR::visr(x_label = expression(sum(x, a, b))), NA)
})

testthat::test_that("T2.2 No error when `y_label` is `NULL`, a `character` string or an `expression`.", {
  survfit_object <- adtte %>%
    visR::estimate_KM("SEX")

  testthat::expect_error(survfit_object %>% visR::visr(), NA) # Equal to y_label = NULL
  testthat::expect_error(survfit_object %>% visR::visr(y_label = NULL), NA)
  testthat::expect_error(survfit_object %>% visR::visr(y_label = "visR"), NA)
  testthat::expect_error(survfit_object %>% visR::visr(y_label = expression(sum(x, a, b))), NA)
})

testthat::test_that("T2.3 No error when `x_units` is `NULL` or a `character` string.", {
  survfit_object <- adtte %>%
    visR::estimate_KM("SEX")

  testthat::expect_error(survfit_object %>% visR::visr(), NA) # Equal to y_label = NULL
  testthat::expect_error(survfit_object %>% visR::visr(x_units = NULL), NA)
  testthat::expect_error(survfit_object %>% visR::visr(x_units = "visR"), NA)
})

testthat::test_that("T2.4 No error when `x_ticks` is `NULL` or a `numeric` value.", {
  survfit_object <- adtte %>%
    visR::estimate_KM("SEX")

  testthat::expect_error(survfit_object %>% visR::visr(), NA) # Equal to y_label = NULL
  testthat::expect_error(survfit_object %>% visR::visr(x_ticks = NULL), NA)
  testthat::expect_error(survfit_object %>% visR::visr(x_ticks = 1), NA)
  testthat::expect_error(survfit_object %>% visR::visr(x_ticks = c(0, 100)), NA)
  testthat::expect_error(survfit_object %>% visR::visr(x_ticks = seq(0, 100, 20)), NA)
})

testthat::test_that("T2.5 No error when `y_ticks` is `NULL` or a `numeric` value.", {
  survfit_object <- adtte %>%
    visR::estimate_KM("SEX")

  testthat::expect_error(survfit_object %>% visR::visr(), NA) # Equal to y_label = NULL
  testthat::expect_error(survfit_object %>% visR::visr(y_ticks = NULL), NA)
  testthat::expect_error(survfit_object %>% visR::visr(y_ticks = 1), NA)
  testthat::expect_error(survfit_object %>% visR::visr(y_ticks = c(0, 100)), NA)
  testthat::expect_error(survfit_object %>% visR::visr(y_ticks = seq(0, 100, 20)), NA)
})

testthat::test_that("T2.6 No error when a valid option is passed to `legend_position`.", {
  survfit_object <- adtte %>%
    visR::estimate_KM("SEX")

  testthat::expect_error(survfit_object %>% visR::visr(legend_position = "top"), NA)
  testthat::expect_error(survfit_object %>% visR::visr(legend_position = "bottom"), NA)
  testthat::expect_error(survfit_object %>% visR::visr(legend_position = "right"), NA)
  testthat::expect_error(survfit_object %>% visR::visr(legend_position = "left"), NA)
  testthat::expect_error(survfit_object %>% visR::visr(legend_position = "none"), NA)
  testthat::expect_error(survfit_object %>% visR::visr(legend_position = c(0.5, 0.5)), NA)
})

testthat::test_that("T2.7 An error when `x_label` is not `NULL`, not a `character` string nor an `expression`.", {
  survfit_object <- adtte %>%
    visR::estimate_KM("SEX")

  testthat::expect_error(survfit_object %>% visR::visr(x_label = 1))
  testthat::expect_error(survfit_object %>% visR::visr(x_label = NA))
  testthat::expect_error(survfit_object %>% visR::visr(x_label = TRUE))
  testthat::expect_error(survfit_object %>% visR::visr(x_label = list()))
  testthat::expect_error(survfit_object %>% visR::visr(x_label = stats::lm(AGE ~ TRTDUR, adtte)))
})

testthat::test_that("T2.8 No warning when `x_label` is `NULL` and the `survfit` object has a `PARAM` and a `PARAMCD` column.", {
  survfit_object <- adtte %>%
    visR::estimate_KM("SEX")

  testthat::expect_warning(survfit_object %>% visR::visr(x_label = NULL), NA)
})

testthat::test_that("T2.9 No warning when `x_label` is `NULL` and the `survfit` object has a `PARAM` but no `PARAMCD` column.", {
  survfit_object <- adtte %>%
    dplyr::select(-PARAMCD) %>%
    visR::estimate_KM("SEX")

  testthat::expect_warning(survfit_object %>% visR::visr(x_label = NULL), NA)
})

testthat::test_that("T2.10 No warning when `x_label` is `NULL` and the `survfit` object has no `PARAM` but a `PARAMCD` column.", {
  survfit_object <- adtte %>%
    dplyr::select(-PARAM) %>%
    visR::estimate_KM("SEX")

  testthat::expect_warning(survfit_object %>% visR::visr(x_label = NULL), NA)
})

testthat::test_that("T2.11 No warning when `x_label` is `NULL` and the `survfit` object has no `PARAM` and no `PARAMCD` column.", {
  survfit_object <- adtte %>%
    dplyr::select(-c(PARAM, PARAMCD)) %>%
    visR::estimate_KM("SEX")

  testthat::expect_warning(survfit_object %>% visR::visr(x_label = NULL), NA)
})

testthat::test_that("T2.12 When `x_label` is `NULL` and the `survfit` object does have a `PARAM` column, the `x_label` is set to `PARAM`.", {
  survfit_object <- adtte %>%
    visR::estimate_KM("SEX")

  gg <- survfit_object %>% visR::visr(x_label = NULL)

  testthat::expect_true("Time to First Dermatologic Event" %in% gg$labels$x)
})

testthat::test_that("T2.13 When `x_label` is `NULL` and the `survfit` object does not have a `PARAM` but a `PARAMCD` column, the `x_label` is set to `PARAMCD`.", {
  survfit_object <- adtte %>%
    dplyr::select(-PARAM) %>%
    visR::estimate_KM("SEX")

  gg <- survfit_object %>% visR::visr(x_label = NULL)

  testthat::expect_true("TTDE" %in% gg$labels$x)
})

testthat::test_that("T2.14 When `x_label` is `NULL` and the `survfit` object does have a `PARAM` but no `PARAMCD` column, the `x_label` is set to `PARAM`.", {
  survfit_object <- adtte %>%
    dplyr::select(-PARAMCD) %>%
    visR::estimate_KM("SEX")

  gg <- survfit_object %>% visR::visr(x_label = NULL)

  testthat::expect_true("Time to First Dermatologic Event" %in% gg$labels$x)
})

testthat::test_that("T2.15 When `x_label` is `NULL` and the `survfit` object does not have a `PARAM` or `PARAMCD` column, the `x_label` is `NULL`.", {
  survfit_object <- adtte %>%
    dplyr::select(-c(PARAM, PARAMCD)) %>%
    visR::estimate_KM("SEX")

  suppressWarnings(gg <- survfit_object %>% visR::visr(x_label = NULL))

  testthat::expect_true("Time" %in% gg$labels$x)
})

testthat::test_that("T2.16 A warning when `x_label` is `NULL` and the `PARAM` column has more than one unique entry.", {
  survfit_object <- adtte %>%
    visR::estimate_KM("SEX")

  survfit_object$PARAM <- rep(survfit_object$PARAM, length(survfit_object$surv) - 1)
  survfit_object$PARAM <- c(survfit_object$PARAM, "visR")

  expected_warning <- "More than one unique entry in 'PARAM'."
  survfit_object %>%
    visR::visr(x_label = NULL) %>%
    testthat::expect_warning(expected_warning)
})

testthat::test_that("T2.17 A warning when `x_label` is `NULL` and the `PARAMCD` column has more than one unique entry.", {
  survfit_object <- adtte %>%
    dplyr::select(-PARAM) %>%
    visR::estimate_KM("SEX")

  survfit_object$PARAMCD <- rep(survfit_object$PARAMCD, length(survfit_object$surv) - 1)
  survfit_object$PARAMCD <- c(survfit_object$PARAMCD, "visR")

  expected_warning <- "More than one unique entry in 'PARAMCD'."
  survfit_object %>%
    visR::visr(x_label = NULL) %>%
    testthat::expect_warning(expected_warning)
})

testthat::test_that("T2.18 When `x_label` and `x_unit` are both defined, they are concatenated into the final `x_label`.", {
  survfit_object <- adtte %>%
    visR::estimate_KM("SEX")

  gg <- survfit_object %>% visR::visr(x_label = "visR", x_unit = "Rsiv")

  testthat::expect_equal(gg$labels$x, "visR (Rsiv)")
})

testthat::test_that("T2.19 An error when `y_label` is not `NULL`, a `character` string or an `expression`.", {
  survfit_object <- adtte %>%
    visR::estimate_KM("SEX")

  testthat::expect_error(survfit_object %>% visR::visr(y_label = 1))
  testthat::expect_error(survfit_object %>% visR::visr(y_label = NA))
  testthat::expect_error(survfit_object %>% visR::visr(y_label = TRUE))
  testthat::expect_error(survfit_object %>% visR::visr(y_label = list()))
  testthat::expect_error(survfit_object %>% visR::visr(y_label = stats::lm(AGE ~ TRTDUR, adtte)))
})

testthat::test_that("T2.20 An error when `x_units` is not `NULL` or a `character` string.", {
  survfit_object <- adtte %>%
    visR::estimate_KM("SEX")

  testthat::expect_error(survfit_object %>% visR::visr(x_units = 1))
  testthat::expect_error(survfit_object %>% visR::visr(x_units = NA))
  testthat::expect_error(survfit_object %>% visR::visr(x_units = TRUE))
  testthat::expect_error(survfit_object %>% visR::visr(x_units = list()))
  testthat::expect_error(survfit_object %>% visR::visr(x_units = stats::lm(AGE ~ TRTDUR, adtte)))
})

testthat::test_that("T2.21 An error when `x_ticks` is not `NULL` or a `numeric`.", {
  survfit_object <- adtte %>%
    visR::estimate_KM("SEX")

  testthat::expect_error(survfit_object %>% visR::visr(x_ticks = "visR"))
  testthat::expect_error(survfit_object %>% visR::visr(x_ticks = NA))
  testthat::expect_error(survfit_object %>% visR::visr(x_ticks = TRUE))
  testthat::expect_error(survfit_object %>% visR::visr(x_ticks = list()))
  testthat::expect_error(survfit_object %>% visR::visr(x_ticks = stats::lm(AGE ~ TRTDUR, adtte)))
})

testthat::test_that("T2.22 An error when `y_ticks` is not `NULL` or a `numeric`.", {
  survfit_object <- adtte %>%
    visR::estimate_KM("SEX")

  testthat::expect_error(survfit_object %>% visR::visr(y_ticks = "visR"))
  testthat::expect_error(survfit_object %>% visR::visr(y_ticks = NA))
  testthat::expect_error(survfit_object %>% visR::visr(y_ticks = TRUE))
  testthat::expect_error(survfit_object %>% visR::visr(y_ticks = list()))
  testthat::expect_error(survfit_object %>% visR::visr(y_ticks = stats::lm(AGE ~ TRTDUR, adtte)))
})

testthat::test_that("T2.23 No error when a valid option is passed to `legend_position`.", {
  survfit_object <- adtte %>%
    visR::estimate_KM("SEX")

  testthat::expect_error(survfit_object %>% visR::visr(legend_position = "top"), NA)
  testthat::expect_error(survfit_object %>% visR::visr(legend_position = "bottom"), NA)
  testthat::expect_error(survfit_object %>% visR::visr(legend_position = "right"), NA)
  testthat::expect_error(survfit_object %>% visR::visr(legend_position = "left"), NA)
  testthat::expect_error(survfit_object %>% visR::visr(legend_position = "none"), NA)
  testthat::expect_error(survfit_object %>% visR::visr(legend_position = c(0.5, 0.5)), NA)
})

testthat::test_that("T2.24 An error when the string is not amongst the valid options for `legend_position`.", {
  survfit_object <- adtte %>%
    visR::estimate_KM("SEX")

  testthat::expect_error(survfit_object %>% visR::visr(legend_position = "visR"))
})

testthat::test_that("T2.25 An error when an undefined option is passed to `legend_position`.", {
  survfit_object <- adtte %>%
    visR::estimate_KM("SEX")

  testthat::expect_error(survfit_object %>% visR::visr(legend_position = "visR"))
  testthat::expect_error(survfit_object %>% visR::visr(legend_position = c(0)))
  testthat::expect_error(survfit_object %>% visR::visr(legend_position = c(0.5, 0.5, 0.5)))

  could_set_to_NA <- tryCatch(
    {
      tmp <- print(survfit_object %>% visR::visr(legend_position = NA))
      TRUE
    },
    error = function(cond) {
      FALSE
    }
  )

  testthat::expect_false(could_set_to_NA)

  could_set_to_bool <- tryCatch(
    {
      tmp <- print(survfit_object %>% visR::visr(legend_position = TRUE))
      TRUE
    },
    error = function(cond) {
      FALSE
    }
  )

  testthat::expect_false(could_set_to_bool)

  could_set_to_list <- tryCatch(
    {
      tmp <- print(survfit_object %>% visR::visr(legend_position = list()))
      TRUE
    },
    error = function(cond) {
      FALSE
    }
  )

  testthat::expect_false(could_set_to_list)
})

# Requirement T3 ---------------------------------------------------------------

testthat::context("visr_plot - T3. The y-axis properties are correctly deducted from the provided `fun` when applying `visR::visr()` to a `survfit` object.")

testthat::test_that("T3.1 No error when `y_label` is `NULL` and `fun` is one of the valid string options.", {
  survfit_object <- adtte %>%
    visR::estimate_KM("SEX")

  testthat::expect_error(survfit_object %>% visR::visr(y_label = NULL, fun = "surv"), NA)
  testthat::expect_error(survfit_object %>% visR::visr(y_label = NULL, fun = "log"), NA)
  testthat::expect_error(survfit_object %>% visR::visr(y_label = NULL, fun = "event"), NA)
  testthat::expect_warning(survfit_object %>% visR::visr(y_label = NULL, fun = "cloglog")) # No error, but data causes introduction of NAs.
  testthat::expect_error(survfit_object %>% visR::visr(y_label = NULL, fun = "pct"), NA)
  testthat::expect_error(survfit_object %>% visR::visr(y_label = NULL, fun = "logpct"), NA)
  testthat::expect_error(survfit_object %>% visR::visr(y_label = NULL, fun = "cumhaz"), NA)
})

testthat::test_that("T3.2 An error when `y_label` is `NULL`, `fun` is a string but not one of the valid options.", {
  survfit_object <- adtte %>%
    visR::estimate_KM("SEX")

  testthat::expect_error(survfit_object %>% visR::visr(y_label = NULL, fun = "visR"))
})

testthat::test_that("T3.3 No error when `y_label` is a string and `fun` is a function.", {
  survfit_object <- adtte %>%
    visR::estimate_KM("SEX")

  testthat::expect_error(survfit_object %>% visR::visr(y_label = "visR", fun = log), NA)
})

testthat::test_that("T3.4 An error when `y_label` is `NULL` and `fun` is a function.", {
  survfit_object <- adtte %>%
    visR::estimate_KM("SEX")

  testthat::expect_error(survfit_object %>% visR::visr(y_label = NULL, fun = log))
})

testthat::test_that("T3.5 A warning when the provided function causes undefined values, f.e. log(-log(2)).", {
  survfit_object <- adtte %>%
    visR::estimate_KM("SEX")

  testthat::expect_warning(survfit_object %>% visR::visr(y_label = NULL, fun = "cloglog"))
})

testthat::test_that("T3.6 An error when `fun` is neither a `character` string nor a function.", {
  survfit_object <- adtte %>%
    visR::estimate_KM("SEX")

  testthat::expect_error(survfit_object %>% visR::visr(fun = 1))
  testthat::expect_error(survfit_object %>% visR::visr(fun = NULL))
  testthat::expect_error(survfit_object %>% visR::visr(fun = NA))
  testthat::expect_error(survfit_object %>% visR::visr(fun = TRUE))
  testthat::expect_error(survfit_object %>% visR::visr(fun = list()))
})

testthat::test_that("T3.7 The `fun` argument is stored in the final object as attribute `fun`.", {
  survfit_plot <- adtte %>%
    visR::estimate_KM("SEX") %>%
    visR::visr(fun = "log")

  testthat::expect_true(inherits(attr(survfit_plot, "fun"), "function"))
})

# Requirement T4 ---------------------------------------------------------------

testthat::context("visr_plot - T4. The legend follows the model strata label and levels.")

testthat::test_that("T4.1 The legend follows the model strata levels.", {
  dt <- adtte
  dt[["TRTA"]] <- factor(dt[["TRTA"]], levels = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"))

  survfit_plot <- visR::estimate_KM(dt, strata = "TRTA") %>%
    visR::visr(fun = "log")

  leg <- ggplot2::ggplot_build(survfit_plot)
  labs <- leg$plot$scales$scales[[4]]$get_labels()

  testthat::expect_equal(paste0(levels(dt$TRTA)), labs)
})

testthat::test_that("T4.2 The color legend title represents the strata label.", {
  survfit_plot <- adtte %>%
    visR::estimate_KM(strata = "TRTA") %>%
    visR::visr()

  testthat::expect_equal(
    survfit_plot$labels$colour,
    "Actual Treatment"
  )

  survfit_plot <- adtte %>%
    visR::estimate_KM(strata = c("TRTA", "SEX")) %>%
    visR::visr()

  testthat::expect_equal(
    survfit_plot$labels$colour,
    "Actual Treatment, Sex"
  )

  survfit_plot <- adtte %>%
    visR::estimate_KM() %>%
    visR::visr()

  testthat::expect_equal(
    survfit_plot$labels$colour,
    ""
  )
})

# Requirement T5 ---------------------------------------------------------------

testthat::context("visr_plot - T5. The final object is a ggplot of class `ggsurvfit`.")

testthat::test_that("T5.1 The final object is a ggplot of class `ggplot`.", {
  survfit_plot <- adtte %>%
    visR::estimate_KM("SEX") %>%
    visR::visr(fun = "log")

  testthat::expect_true(inherits(survfit_plot, "ggplot"))
})

testthat::test_that("T5.2 The final object is a ggplot of class `ggsurvfit`.", {
  survfit_plot <- adtte %>%
    visR::estimate_KM("SEX") %>%
    visR::visr(fun = "log")

  testthat::expect_true(inherits(survfit_plot, "ggsurvfit"))
})

# Requirement T6 ---------------------------------------------------------------

testthat::context("visr_plot - T6. The final object does not exclude parts of KM estimate.")

testthat::test_that("T6.1 The final object zooms and does not exclude trialing pieces of lines.", {
  plot.zoom <-
    visR::estimate_KM(
      data = survival::lung %>% dplyr::mutate(time = ifelse(time > 1000, 2001, time)),
      formula = survival::Surv(time, status) ~ 1
    ) %>%
    visR::visr(x_ticks = seq(0, 2000, by = 200)) %>%
    visR::add_risktable()

  vdiffr::expect_doppelganger("plot-zoom", plot.zoom)
})

# END OF CODE -------------------------------------------------------------
