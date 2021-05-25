#' @title Specifications visr_plot
#' @section Last updated by:
#' Tim Treis
#' @section Last update date:
#' 25-MAY-2021

# Specifications ----------------------------------------------------------

#' T1. Correct values for summarize.numeric



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
  
})


# END ---------------------------------------------------------------------

adtte %>%
  visR::get_attrition(criteria_descriptions = c("1. Placebo Group", 
                                                "2. Be 75 years of age or older.",
                                                "3. White", 
                                                "4. Site 709"),
                      criteria_conditions   = c("TRTP=='Placebo'",
                                                "AGE>=75",
                                                "RACE=='WHITE'", 
                                                "SITEID==709"),
                      subject_column_name   = 'USUBJID')
