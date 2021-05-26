#' @title Specifications visr_plot
#' @section Last updated by:
#' Tim Treis
#' @section Last update date:
#' 25-MAY-2021

# Specifications ----------------------------------------------------------

#' T1. visR::visr() only accepts `survfit` or `attrition`.
#' T1.1 No error when applied to a `survfit` object.
#' T1.2 No error when applied to a `attrition` object.
#' T1.3 An error when applied to an object that is not `survfit` or `attrition`.
#' T2. No invalid parameters are used when applying `visR::visr()` to a `survfit` object.
#' T2.1 No error when `x_label` is `NULL`, a `character` string or an `expression`.
#' T2.2 No error when `y_label` is `NULL`, a `character` string or an `expression`.
#' T2.3 No error when `x_units` is `NULL` or a `character` string.
#' T2.4 No error when `x_ticks` is `NULL` or a `numeric` value.
#' T2.5 No error when `y_ticks` is `NULL` or a `numeric` value.
#' T2.6 No error when a valid option is passed to `legend_position`.
#' T2.7 An error when `x_label` is not `NULL`, a `character` string or an `expression`.
#' T2.8 An error when `y_label` is not `NULL`, a `character` string or an `expression`.
#' T2.9 An error when `x_units` is not `NULL` or a `character` string.
#' T2.10 An error when `x_ticks` is not `NULL` or a `numeric`.
#' T2.11 An error when `y_ticks` is not `NULL` or a `numeric`.
#' T2.12 No error when a valid option is passed to `legend_position`.
#' T2.13 An error when the string is not amongst the valid options for `legend_position`.
#' T2.14 An error when an invalid option is passed to `legend_position`.
#' T3. The y-axis properties are correctly deducted from the provided `fun` when applying `visR::visr()` to a `survfit` object.
#' T3.1 No error when `y_label` is `NULL` and `fun` is one of the valid string options.
#' T3.2 An error when `y_label` is `NULL`, `fun` is a string but not one of the valid options.
#' T3.3 No error when `y_label` is a string and `fun` is a function.
#' T3.4 An error when `y_label` is `NULL` and `fun` is a function.
#' T3.5 A warning when the provided function causes undefined values, f.e. log(-log(2)).
#' T4. No invalid parameters are used when applying `visR::visr()` to a `attrition` object.
#' T4.1 No error when `description_column_name` is a `character` string that is found in the colnames of the `attrition` object.
#' T4.2 No error when `value_column_name` is a `character` string that is found in the colnames of the `attrition` object.
#' T4.3 No error when `complement_column_name` is a `character` string that is found in the colnames of the `attrition` object.
#' T4.4 No error when `box_width` is a `numeric` value.
#' T4.5 No error when `font_size` is a `numeric` value.
#' T4.6 No error when `fill` is a `character` string that is a valid colour.
#' T4.7 No error when `border` is a `character` string that is a valid colour.
#' T4.8 An error when `description_column_name` is a `character` string but is not found in the colnames of the `attrition` object.
#' T4.9 An error when `description_column_name` is not `character` string.
#' T4.10 An error when `value_column_name` is a `character` string but is not found in the colnames of the `attrition` object.
#' T4.11 An error when `value_column_name` is not `character` string.
#' T4.12 An error when `complement_column_name` is a `character` string but is not found in the colnames of the `attrition` object.
#' T4.13 An error when `complement_column_name` is not `character` string.
#' T4.14 A warning when `box_width` is not a `numeric` value.
#' T4.15 A warning when `font_size` is not a `numeric` value.
#' T4.16 An error when `fill` is a `character` string but not a valid colour.
#' T4.17 An error when `fill` is not a `character` string.
#' T4.18 An error when `border` is a `character` string but not a valid colour.
#' T4.19 An error when `border` is not a `character` string.

# Requirement T1 ----------------------------------------------------------

testthat::context("visr_plot - T1. visR::visr() only accepts `survfit` or `attrition`.")

testthat::test_that("T1.1 No error when applied to a `survfit` object.", {
  
  survfit_object <- adtte %>%
    visR::estimate_KM("SEX") 
  
  testthat::expect_error(survfit_object %>% visR::visr(), NA)
  
})

testthat::test_that("T1.2 No error when applied to a `attrition` object.", {
  
  attrition_object <- adtte %>%
    visR::get_attrition(criteria_descriptions = c("1. Placebo Group", 
                                                  "2. Be 75 years of age or older.",
                                                  "3. White", 
                                                  "4. Site 709"),
                        criteria_conditions   = c("TRTP=='Placebo'",
                                                  "AGE>=75",
                                                  "RACE=='WHITE'", 
                                                  "SITEID==709"),
                        subject_column_name   = 'USUBJID') 
  
  testthat::expect_error(attrition_object %>% visR::visr("Criteria", "Remaining N"), NA)
  
})

testthat::test_that("T1.3 An error when applied to an object that is not `survfit` or `attrition`.", {
  
  testthat::expect_error("visR" %>% visR::visr())
  testthat::expect_error(1 %>% visR::visr())
  testthat::expect_error(NA %>% visR::visr())
  testthat::expect_error(TRUE %>% visR::visr())
  testthat::expect_error(list() %>% visR::visr())
  testthat::expect_error(stats::lm(AGE ~ TRTDUR, adtte) %>% visR::visr())
  
})

# Requirement T2 ----------------------------------------------------------

testthat::context("visr_plot - T2. No invalid parameters are used when applying `visR::visr()` to a `survfit` object.")

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

testthat::test_that("T2.7 An error when `x_label` is not `NULL`, a `character` string or an `expression`.", {
  
  survfit_object <- adtte %>%
    visR::estimate_KM("SEX") 
  
  testthat::expect_error(survfit_object %>% visR::visr(x_label = 1))
  testthat::expect_error(survfit_object %>% visR::visr(x_label = NA))
  testthat::expect_error(survfit_object %>% visR::visr(x_label = TRUE))
  testthat::expect_error(survfit_object %>% visR::visr(x_label = list()))
  testthat::expect_error(survfit_object %>% visR::visr(x_label = stats::lm(AGE ~ TRTDUR, adtte)))
  
})

testthat::test_that("T2.8 An error when `y_label` is not `NULL`, a `character` string or an `expression`.", {
  
  survfit_object <- adtte %>%
    visR::estimate_KM("SEX") 
  
  testthat::expect_error(survfit_object %>% visR::visr(y_label = 1))
  testthat::expect_error(survfit_object %>% visR::visr(y_label = NA))
  testthat::expect_error(survfit_object %>% visR::visr(y_label = TRUE))
  testthat::expect_error(survfit_object %>% visR::visr(y_label = list()))
  testthat::expect_error(survfit_object %>% visR::visr(y_label = stats::lm(AGE ~ TRTDUR, adtte)))
  
})

testthat::test_that("T2.9 An error when `x_units` is not `NULL` or a `character` string.", {
  
  survfit_object <- adtte %>%
    visR::estimate_KM("SEX") 
  
  testthat::expect_error(survfit_object %>% visR::visr(x_units = 1))
  testthat::expect_error(survfit_object %>% visR::visr(x_units = NA))
  testthat::expect_error(survfit_object %>% visR::visr(x_units = TRUE))
  testthat::expect_error(survfit_object %>% visR::visr(x_units = list()))
  testthat::expect_error(survfit_object %>% visR::visr(x_units = stats::lm(AGE ~ TRTDUR, adtte)))
  
})

testthat::test_that("T2.10 An error when `x_ticks` is not `NULL` or a `numeric`.", {
  
  survfit_object <- adtte %>%
    visR::estimate_KM("SEX") 
  
  testthat::expect_error(survfit_object %>% visR::visr(x_ticks = "visR"))
  testthat::expect_error(survfit_object %>% visR::visr(x_ticks = NA))
  testthat::expect_error(survfit_object %>% visR::visr(x_ticks = TRUE))
  testthat::expect_error(survfit_object %>% visR::visr(x_ticks = list()))
  testthat::expect_error(survfit_object %>% visR::visr(x_ticks = stats::lm(AGE ~ TRTDUR, adtte)))
  
})

testthat::test_that("T2.11 An error when `y_ticks` is not `NULL` or a `numeric`.", {
  
  survfit_object <- adtte %>%
    visR::estimate_KM("SEX") 
  
  testthat::expect_error(survfit_object %>% visR::visr(y_ticks = "visR"))
  testthat::expect_error(survfit_object %>% visR::visr(y_ticks = NA))
  testthat::expect_error(survfit_object %>% visR::visr(y_ticks = TRUE))
  testthat::expect_error(survfit_object %>% visR::visr(y_ticks = list()))
  testthat::expect_error(survfit_object %>% visR::visr(y_ticks = stats::lm(AGE ~ TRTDUR, adtte)))
  
})

testthat::test_that("T2.12 No error when a valid option is passed to `legend_position`.", {
  
  survfit_object <- adtte %>%
    visR::estimate_KM("SEX") 
  
  testthat::expect_error(survfit_object %>% visR::visr(legend_position = "top"), NA)
  testthat::expect_error(survfit_object %>% visR::visr(legend_position = "bottom"), NA)
  testthat::expect_error(survfit_object %>% visR::visr(legend_position = "right"), NA)
  testthat::expect_error(survfit_object %>% visR::visr(legend_position = "left"), NA)
  testthat::expect_error(survfit_object %>% visR::visr(legend_position = "none"), NA)
  testthat::expect_error(survfit_object %>% visR::visr(legend_position = c(0.5, 0.5)), NA)
  
})

testthat::test_that("T2.13 An error when the string is not amongst the valid options for `legend_position`.", {
  
  survfit_object <- adtte %>%
    visR::estimate_KM("SEX") 
  
  testthat::expect_error(survfit_object %>% visR::visr(legend_position = "visR"))
  
})

testthat::test_that("T2.14 An error when an invalid option is passed to `legend_position`.", {
  
  survfit_object <- adtte %>%
    visR::estimate_KM("SEX") 
  
  testthat::expect_error(survfit_object %>% visR::visr(legend_position = "visR"))
  testthat::expect_error(survfit_object %>% visR::visr(legend_position = c(0)))
  testthat::expect_error(survfit_object %>% visR::visr(legend_position = c(0.5, 0.5, 0.5)))
  
  could_set_to_NA <- tryCatch ({
    
    tmp <- print(survfit_object %>% visR::visr(legend_position = NA))
    TRUE
    
  }, error=function(cond) {
    
    FALSE
    
  })   
  
  testthat::expect_false(could_set_to_NA)
  
  could_set_to_bool <- tryCatch ({
    
    tmp <- print(survfit_object %>% visR::visr(legend_position = TRUE))
    TRUE
    
  }, error=function(cond) {
    
    FALSE
    
  })   
  
  testthat::expect_false(could_set_to_bool)
  
  could_set_to_list <- tryCatch ({
    
    tmp <- print(survfit_object %>% visR::visr(legend_position = list()))
    TRUE
    
  }, error=function(cond) {
    
    FALSE
    
  })   
  
  testthat::expect_false(could_set_to_list)
  
})

# Requirement T3 ----------------------------------------------------------

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

# Requirement T4 ----------------------------------------------------------

testthat::context("visr_plot - T4. No invalid parameters are used when applying `visR::visr()` to a `attrition` object.")

testthat::test_that("T4.1 No error when `description_column_name` is a `character` string that is found in the colnames of the `attrition` object.", {
  
  attrition_object <- adtte %>%
    visR::get_attrition(criteria_descriptions = c("1. Placebo Group", 
                                                  "2. Be 75 years of age or older.",
                                                  "3. White", 
                                                  "4. Site 709"),
                        criteria_conditions   = c("TRTP=='Placebo'",
                                                  "AGE>=75",
                                                  "RACE=='WHITE'", 
                                                  "SITEID==709"),
                        subject_column_name   = 'USUBJID') 
  
  testthat::expect_error(attrition_object %>% visR::visr(description_column_name = "Criteria"), NA)
  
})

testthat::test_that("T4.2 No error when `value_column_name` is a `character` string that is found in the colnames of the `attrition` object.", {
  
  attrition_object <- adtte %>%
    visR::get_attrition(criteria_descriptions = c("1. Placebo Group", 
                                                  "2. Be 75 years of age or older.",
                                                  "3. White", 
                                                  "4. Site 709"),
                        criteria_conditions   = c("TRTP=='Placebo'",
                                                  "AGE>=75",
                                                  "RACE=='WHITE'", 
                                                  "SITEID==709"),
                        subject_column_name   = 'USUBJID') 
  
  testthat::expect_error(attrition_object %>% visR::visr(value_column_name = "Remaining N"), NA)
  
})

testthat::test_that("T4.3 No error when `complement_column_name` is a `character` string that is found in the colnames of the `attrition` object.", {
  
  attrition_object <- adtte %>%
    visR::get_attrition(criteria_descriptions = c("1. Placebo Group", 
                                                  "2. Be 75 years of age or older.",
                                                  "3. White", 
                                                  "4. Site 709"),
                        criteria_conditions   = c("TRTP=='Placebo'",
                                                  "AGE>=75",
                                                  "RACE=='WHITE'", 
                                                  "SITEID==709"),
                        subject_column_name   = 'USUBJID') 
  
  attrition_object <- attrition_object %>%
    dplyr::bind_cols(Complement = c("", 
                                    "Treatment Group", 
                                    "Younger than 75 years of age", 
                                    "Non-White", 
                                    "Not Site 709"))
  
  testthat::expect_error(attrition_object %>% visR::visr(complement_column_name = "Complement"), NA)
  
})

testthat::test_that("T4.4 No error when `box_width` is a `numeric` value.", {
  
  attrition_object <- adtte %>%
    visR::get_attrition(criteria_descriptions = c("1. Placebo Group", 
                                                  "2. Be 75 years of age or older.",
                                                  "3. White", 
                                                  "4. Site 709"),
                        criteria_conditions   = c("TRTP=='Placebo'",
                                                  "AGE>=75",
                                                  "RACE=='WHITE'", 
                                                  "SITEID==709"),
                        subject_column_name   = 'USUBJID') 
  
  testthat::expect_error(attrition_object %>% visR::visr(box_width = 500), NA)
  
})

testthat::test_that("T4.5 No error when `font_size` is a `numeric` value.", {
  
  attrition_object <- adtte %>%
    visR::get_attrition(criteria_descriptions = c("1. Placebo Group", 
                                                  "2. Be 75 years of age or older.",
                                                  "3. White", 
                                                  "4. Site 709"),
                        criteria_conditions   = c("TRTP=='Placebo'",
                                                  "AGE>=75",
                                                  "RACE=='WHITE'", 
                                                  "SITEID==709"),
                        subject_column_name   = 'USUBJID') 
  
  testthat::expect_error(attrition_object %>% visR::visr(font_size = 13), NA)
  
})

testthat::test_that("T4.6 No error when `fill` is a `character` string that is a valid colour.", {
  
  attrition_object <- adtte %>%
    visR::get_attrition(criteria_descriptions = c("1. Placebo Group", 
                                                  "2. Be 75 years of age or older.",
                                                  "3. White", 
                                                  "4. Site 709"),
                        criteria_conditions   = c("TRTP=='Placebo'",
                                                  "AGE>=75",
                                                  "RACE=='WHITE'", 
                                                  "SITEID==709"),
                        subject_column_name   = 'USUBJID') 
  
  testthat::expect_error(attrition_object %>% visR::visr(fill = "grey"), NA)
  
})

testthat::test_that("T4.7 No error when `border` is a `character` string that is a valid colour.", {
  
  attrition_object <- adtte %>%
    visR::get_attrition(criteria_descriptions = c("1. Placebo Group", 
                                                  "2. Be 75 years of age or older.",
                                                  "3. White", 
                                                  "4. Site 709"),
                        criteria_conditions   = c("TRTP=='Placebo'",
                                                  "AGE>=75",
                                                  "RACE=='WHITE'", 
                                                  "SITEID==709"),
                        subject_column_name   = 'USUBJID') 
  
  testthat::expect_error(attrition_object %>% visR::visr(border = "grey"), NA)
  
})

testthat::test_that("T4.8 An error when `description_column_name` is a `character` string but is not found in the colnames of the `attrition` object.", {
  
  attrition_object <- adtte %>%
    visR::get_attrition(criteria_descriptions = c("1. Placebo Group", 
                                                  "2. Be 75 years of age or older.",
                                                  "3. White", 
                                                  "4. Site 709"),
                        criteria_conditions   = c("TRTP=='Placebo'",
                                                  "AGE>=75",
                                                  "RACE=='WHITE'", 
                                                  "SITEID==709"),
                        subject_column_name   = 'USUBJID') 
  
  testthat::expect_error(attrition_object %>% visR::visr(description_column_name = "visR"))

})

testthat::test_that("T4.9 An error when `description_column_name` is not `character` string.", {
  
  attrition_object <- adtte %>%
    visR::get_attrition(criteria_descriptions = c("1. Placebo Group", 
                                                  "2. Be 75 years of age or older.",
                                                  "3. White", 
                                                  "4. Site 709"),
                        criteria_conditions   = c("TRTP=='Placebo'",
                                                  "AGE>=75",
                                                  "RACE=='WHITE'", 
                                                  "SITEID==709"),
                        subject_column_name   = 'USUBJID') 
  
  
  
  testthat::expect_error(attrition_object %>% visR::visr(description_column_name = 1))
  testthat::expect_error(attrition_object %>% visR::visr(description_column_name = NA))
  testthat::expect_error(attrition_object %>% visR::visr(description_column_name = NULL))
  testthat::expect_error(attrition_object %>% visR::visr(description_column_name = TRUE))
  testthat::expect_error(attrition_object %>% visR::visr(description_column_name = list()))
  testthat::expect_error(attrition_object %>% visR::visr(description_column_name = stats::lm(AGE ~ TRTDUR, adtte) %>% visR::visr()))
  
  
})

testthat::test_that("T4.10 An error when `value_column_name` is a `character` string but is not found in the colnames of the `attrition` object.", {
  
  attrition_object <- adtte %>%
    visR::get_attrition(criteria_descriptions = c("1. Placebo Group", 
                                                  "2. Be 75 years of age or older.",
                                                  "3. White", 
                                                  "4. Site 709"),
                        criteria_conditions   = c("TRTP=='Placebo'",
                                                  "AGE>=75",
                                                  "RACE=='WHITE'", 
                                                  "SITEID==709"),
                        subject_column_name   = 'USUBJID') 
  
  testthat::expect_error(attrition_object %>% visR::visr(value_column_name = "visR"))
  
})

testthat::test_that("T4.11 An error when `value_column_name` is not `character` string.", {
  
  attrition_object <- adtte %>%
    visR::get_attrition(criteria_descriptions = c("1. Placebo Group", 
                                                  "2. Be 75 years of age or older.",
                                                  "3. White", 
                                                  "4. Site 709"),
                        criteria_conditions   = c("TRTP=='Placebo'",
                                                  "AGE>=75",
                                                  "RACE=='WHITE'", 
                                                  "SITEID==709"),
                        subject_column_name   = 'USUBJID') 
  
  testthat::expect_error(attrition_object %>% visR::visr(value_column_name = 1))
  testthat::expect_error(attrition_object %>% visR::visr(value_column_name = NA))
  testthat::expect_error(attrition_object %>% visR::visr(value_column_name = NULL))
  testthat::expect_error(attrition_object %>% visR::visr(value_column_name = TRUE))
  testthat::expect_error(attrition_object %>% visR::visr(value_column_name = list()))
  testthat::expect_error(attrition_object %>% visR::visr(value_column_name = stats::lm(AGE ~ TRTDUR, adtte) %>% visR::visr()))
  
})

testthat::test_that("T4.12 An error when `complement_column_name` is a `character` string but is not found in the colnames of the `attrition` object.", {
  
  attrition_object <- adtte %>%
    visR::get_attrition(criteria_descriptions = c("1. Placebo Group", 
                                                  "2. Be 75 years of age or older.",
                                                  "3. White", 
                                                  "4. Site 709"),
                        criteria_conditions   = c("TRTP=='Placebo'",
                                                  "AGE>=75",
                                                  "RACE=='WHITE'", 
                                                  "SITEID==709"),
                        subject_column_name   = 'USUBJID') 
  
  testthat::expect_error(attrition_object %>% visR::visr(complement_column_name = "visR"))
  
})

testthat::test_that("T4.13 An error when `complement_column_name` is not `character` string.", {
  
  attrition_object <- adtte %>%
    visR::get_attrition(criteria_descriptions = c("1. Placebo Group", 
                                                  "2. Be 75 years of age or older.",
                                                  "3. White", 
                                                  "4. Site 709"),
                        criteria_conditions   = c("TRTP=='Placebo'",
                                                  "AGE>=75",
                                                  "RACE=='WHITE'", 
                                                  "SITEID==709"),
                        subject_column_name   = 'USUBJID') 
  
  testthat::expect_error(attrition_object %>% visR::visr(complement_column_name = 1))
  testthat::expect_error(attrition_object %>% visR::visr(complement_column_name = NA))
  testthat::expect_error(attrition_object %>% visR::visr(complement_column_name = NULL))
  testthat::expect_error(attrition_object %>% visR::visr(complement_column_name = TRUE))
  testthat::expect_error(attrition_object %>% visR::visr(complement_column_name = list()))
  testthat::expect_error(attrition_object %>% visR::visr(complement_column_name = stats::lm(AGE ~ TRTDUR, adtte) %>% visR::visr()))
  
})

testthat::test_that("T4.14 A warning when `box_width` is not a `numeric` value.", {
  
  attrition_object <- adtte %>%
    visR::get_attrition(criteria_descriptions = c("1. Placebo Group", 
                                                  "2. Be 75 years of age or older.",
                                                  "3. White", 
                                                  "4. Site 709"),
                        criteria_conditions   = c("TRTP=='Placebo'",
                                                  "AGE>=75",
                                                  "RACE=='WHITE'", 
                                                  "SITEID==709"),
                        subject_column_name   = 'USUBJID') 
  
  testthat::expect_warning(attrition_object %>% visR::visr(box_width = "visR"))
  testthat::expect_warning(attrition_object %>% visR::visr(box_width = NA))
  testthat::expect_warning(attrition_object %>% visR::visr(box_width = NULL))
  testthat::expect_warning(attrition_object %>% visR::visr(box_width = TRUE))
  testthat::expect_warning(attrition_object %>% visR::visr(box_width = list()))
  
})

testthat::test_that("T4.15 A warning when `font_size` is not a `numeric` value.", {
  
  attrition_object <- adtte %>%
    visR::get_attrition(criteria_descriptions = c("1. Placebo Group", 
                                                  "2. Be 75 years of age or older.",
                                                  "3. White", 
                                                  "4. Site 709"),
                        criteria_conditions   = c("TRTP=='Placebo'",
                                                  "AGE>=75",
                                                  "RACE=='WHITE'", 
                                                  "SITEID==709"),
                        subject_column_name   = 'USUBJID') 
  
  testthat::expect_warning(attrition_object %>% visR::visr(font_size = "visR"))
  testthat::expect_warning(attrition_object %>% visR::visr(font_size = NA))
  testthat::expect_warning(attrition_object %>% visR::visr(font_size = NULL))
  testthat::expect_warning(attrition_object %>% visR::visr(font_size = TRUE))
  testthat::expect_warning(attrition_object %>% visR::visr(font_size = list()))
  
})

testthat::test_that("T4.16 An error when `fill` is a `character` string but not a valid colour.", {
  
  attrition_object <- adtte %>%
    visR::get_attrition(criteria_descriptions = c("1. Placebo Group", 
                                                  "2. Be 75 years of age or older.",
                                                  "3. White", 
                                                  "4. Site 709"),
                        criteria_conditions   = c("TRTP=='Placebo'",
                                                  "AGE>=75",
                                                  "RACE=='WHITE'", 
                                                  "SITEID==709"),
                        subject_column_name   = 'USUBJID') 
  
  could_generate_plot <- tryCatch ({
    
    tmp <- print(attrition_object %>% visR::visr(fill = "visR"))
    TRUE
    
  }, error=function(cond) {
    
    FALSE
    
  })   
  
  testthat::expect_false(could_generate_plot)
  
})

testthat::test_that("T4.17 An error when `fill` is not a `character` string.", {
  
  attrition_object <- adtte %>%
    visR::get_attrition(criteria_descriptions = c("1. Placebo Group", 
                                                  "2. Be 75 years of age or older.",
                                                  "3. White", 
                                                  "4. Site 709"),
                        criteria_conditions   = c("TRTP=='Placebo'",
                                                  "AGE>=75",
                                                  "RACE=='WHITE'", 
                                                  "SITEID==709"),
                        subject_column_name   = 'USUBJID') 
  
  testthat::expect_warning(attrition_object %>% visR::visr(fill = 1))
  testthat::expect_warning(attrition_object %>% visR::visr(fill = NA))
  testthat::expect_warning(attrition_object %>% visR::visr(fill = NULL))
  testthat::expect_warning(attrition_object %>% visR::visr(fill = TRUE))
  testthat::expect_warning(attrition_object %>% visR::visr(fill = list()))
  
})

testthat::test_that("T4.18 An error when `border` is a `character` string but not a valid colour.", {
  
  attrition_object <- adtte %>%
    visR::get_attrition(criteria_descriptions = c("1. Placebo Group", 
                                                  "2. Be 75 years of age or older.",
                                                  "3. White", 
                                                  "4. Site 709"),
                        criteria_conditions   = c("TRTP=='Placebo'",
                                                  "AGE>=75",
                                                  "RACE=='WHITE'", 
                                                  "SITEID==709"),
                        subject_column_name   = 'USUBJID') 
  
  could_generate_plot <- tryCatch ({
    
    tmp <- print(attrition_object %>% visR::visr(border = "border"))
    TRUE
    
  }, error=function(cond) {
    
    FALSE
    
  })   
  
  testthat::expect_false(could_generate_plot)
  
})

testthat::test_that("T4.19 An error when `border` is not a `character` string.", {
  
  attrition_object <- adtte %>%
    visR::get_attrition(criteria_descriptions = c("1. Placebo Group", 
                                                  "2. Be 75 years of age or older.",
                                                  "3. White", 
                                                  "4. Site 709"),
                        criteria_conditions   = c("TRTP=='Placebo'",
                                                  "AGE>=75",
                                                  "RACE=='WHITE'", 
                                                  "SITEID==709"),
                        subject_column_name   = 'USUBJID') 
  
  testthat::expect_warning(attrition_object %>% visR::visr(border = 1))
  testthat::expect_warning(attrition_object %>% visR::visr(border = NA))
  testthat::expect_warning(attrition_object %>% visR::visr(border = NULL))
  testthat::expect_warning(attrition_object %>% visR::visr(border = TRUE))
  testthat::expect_warning(attrition_object %>% visR::visr(border = list()))
  
})


# END ---------------------------------------------------------------------