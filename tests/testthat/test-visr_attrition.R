#' @title Specifications test-visr_attrition.R
#' @section Last updated by: Tim Treis (tim.treis@@outlook.de)
#' @section Last update date: 2022-02-09T15:22:32
#'
#' @section List of tested specifications
#' T1. Invalid parameters are captured when applying `visR::visr()` to an `attrition` object and respective warnings/errors are thrown.
#' T1.1 No error when `description_column_name` is a `character` string that is found in the colnames of the `attrition` object.
#' T1.2 No error when `value_column_name` is a `character` string that is found in the colnames of the `attrition` object.
#' T1.3 No error when `complement_column_name` is a `character` string that is found in the colnames of the `attrition` object.
#' T1.4 No error when `box_width` is a `numeric` value.
#' T1.5 No error when `font_size` is a `numeric` value.
#' T1.6 No error when `fill` is a `character` string that is a valid colour.
#' T1.7 No error when `border` is a `character` string that is a valid colour.
#' T1.8 An error when `description_column_name` is a `character` string but is not found in the colnames of the `attrition` object.
#' T1.9 An error when `description_column_name` is not `character` string.
#' T1.10 An error when `value_column_name` is a `character` string but is not found in the colnames of the `attrition` object.
#' T1.11 An error when `value_column_name` is not `character` string.
#' T1.12 An error when `complement_column_name` is a `character` string but is not found in the colnames of the `attrition` object.
#' T1.13 An error when `complement_column_name` is not `character` string.
#' T1.14 A warning when `box_width` is not a `numeric` value.
#' T1.15 A warning when `font_size` is not a `numeric` value.
#' T1.16 An error when `fill` is a `character` string but not a valid colour.
#' T1.17 An error when `fill` is not a `character` string.
#' T1.18 An error when `border` is a `character` string but not a valid colour.
#' T1.19 An error when `border` is not a `character` string.

# Requirement T1 ----------------------------------------------------------

testthat::context("visr_plot - T1. Invalid parameters are captured when applying `visR::visr()` to an `attrition` object and respective warnings/errors are thrown.")

testthat::test_that("T1.1 No error when `description_column_name` is a `character` string that is found in the colnames of the `attrition` object.", {
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

  testthat::expect_error(attrition_object %>% visR::visr(description_column_name = "Criteria"), NA)
})

testthat::test_that("T1.2 No error when `value_column_name` is a `character` string that is found in the colnames of the `attrition` object.", {
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

  testthat::expect_error(attrition_object %>% visR::visr(value_column_name = "Remaining N"), NA)
})

testthat::test_that("T1.3 No error when `complement_column_name` is a `character` string that is found in the colnames of the `attrition` object.", {
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

  attrition_object <- attrition_object %>%
    dplyr::bind_cols(Complement = c(
      "",
      "Treatment Group",
      "Younger than 75 years of age",
      "Non-White",
      "Not Site 709"
    ))

  testthat::expect_error(attrition_object %>% visR::visr(complement_column_name = "Complement"), NA)
})

testthat::test_that("T1.4 No error when `box_width` is a `numeric` value.", {
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

  testthat::expect_error(attrition_object %>% visR::visr(box_width = 500), NA)
})

testthat::test_that("T1.5 No error when `font_size` is a `numeric` value.", {
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

  testthat::expect_error(attrition_object %>% visR::visr(font_size = 13), NA)
})

testthat::test_that("T1.6 No error when `fill` is a `character` string that is a valid colour.", {
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

  testthat::expect_error(attrition_object %>% visR::visr(fill = "grey"), NA)
})

testthat::test_that("T1.7 No error when `border` is a `character` string that is a valid colour.", {
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

  testthat::expect_error(attrition_object %>% visR::visr(border = "grey"), NA)
})

testthat::test_that("T1.8 An error when `description_column_name` is a `character` string but is not found in the colnames of the `attrition` object.", {
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

  testthat::expect_error(attrition_object %>% visR::visr(description_column_name = "visR"))
})

testthat::test_that("T1.9 An error when `description_column_name` is not `character` string.", {
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



  testthat::expect_error(attrition_object %>% visR::visr(description_column_name = 1))
  testthat::expect_error(attrition_object %>% visR::visr(description_column_name = NA))
  testthat::expect_error(attrition_object %>% visR::visr(description_column_name = NULL))
  testthat::expect_error(attrition_object %>% visR::visr(description_column_name = TRUE))
  testthat::expect_error(attrition_object %>% visR::visr(description_column_name = list()))
  testthat::expect_error(attrition_object %>% visR::visr(description_column_name = stats::lm(AGE ~ TRTDUR, adtte) %>% visR::visr()))
})

testthat::test_that("T1.10 An error when `value_column_name` is a `character` string but is not found in the colnames of the `attrition` object.", {
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

  testthat::expect_error(attrition_object %>% visR::visr(value_column_name = "visR"))
})

testthat::test_that("T1.11 An error when `value_column_name` is not `character` string.", {
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

  testthat::expect_error(attrition_object %>% visR::visr(value_column_name = 1))
  testthat::expect_error(attrition_object %>% visR::visr(value_column_name = NA))
  testthat::expect_error(attrition_object %>% visR::visr(value_column_name = NULL))
  testthat::expect_error(attrition_object %>% visR::visr(value_column_name = TRUE))
  testthat::expect_error(attrition_object %>% visR::visr(value_column_name = list()))
  testthat::expect_error(attrition_object %>% visR::visr(value_column_name = stats::lm(AGE ~ TRTDUR, adtte) %>% visR::visr()))
})

testthat::test_that("T1.12 An error when `complement_column_name` is a `character` string but is not found in the colnames of the `attrition` object.", {
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

  testthat::expect_error(attrition_object %>% visR::visr(complement_column_name = "visR"))
})

testthat::test_that("T1.13 An error when `complement_column_name` is not `character` string.", {
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

  testthat::expect_error(attrition_object %>% visR::visr(complement_column_name = 1))
  testthat::expect_error(attrition_object %>% visR::visr(complement_column_name = NA))
  testthat::expect_error(attrition_object %>% visR::visr(complement_column_name = NULL))
  testthat::expect_error(attrition_object %>% visR::visr(complement_column_name = TRUE))
  testthat::expect_error(attrition_object %>% visR::visr(complement_column_name = list()))
  testthat::expect_error(attrition_object %>% visR::visr(complement_column_name = stats::lm(AGE ~ TRTDUR, adtte) %>% visR::visr()))
})

testthat::test_that("T1.14 A warning when `box_width` is not a `numeric` value.", {
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

  testthat::expect_warning(attrition_object %>% visR::visr(box_width = "visR"))
  testthat::expect_warning(attrition_object %>% visR::visr(box_width = NA))
  testthat::expect_warning(attrition_object %>% visR::visr(box_width = NULL))
  testthat::expect_warning(attrition_object %>% visR::visr(box_width = TRUE))
  testthat::expect_warning(attrition_object %>% visR::visr(box_width = list()))
})

testthat::test_that("T1.15 A warning when `font_size` is not a `numeric` value.", {
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

  testthat::expect_warning(attrition_object %>% visR::visr(font_size = "visR"))
  testthat::expect_warning(attrition_object %>% visR::visr(font_size = NA))
  testthat::expect_warning(attrition_object %>% visR::visr(font_size = NULL))
  testthat::expect_warning(attrition_object %>% visR::visr(font_size = TRUE))
  testthat::expect_warning(attrition_object %>% visR::visr(font_size = list()))
})

testthat::test_that("T1.16 An error when `fill` is a `character` string but not a valid colour.", {
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

  could_generate_plot <- tryCatch(
    {
      tmp <- print(attrition_object %>% visR::visr(fill = "visR"))
      TRUE
    },
    error = function(cond) {
      FALSE
    }
  )

  testthat::expect_false(could_generate_plot)
})

testthat::test_that("T1.17 An error when `fill` is not a `character` string.", {
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

  testthat::expect_warning(attrition_object %>% visR::visr(fill = 1))
  testthat::expect_warning(attrition_object %>% visR::visr(fill = NA))
  testthat::expect_warning(attrition_object %>% visR::visr(fill = NULL))
  testthat::expect_warning(attrition_object %>% visR::visr(fill = TRUE))
  testthat::expect_warning(attrition_object %>% visR::visr(fill = list()))
})

testthat::test_that("T1.18 An error when `border` is a `character` string but not a valid colour.", {
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

  could_generate_plot <- tryCatch(
    {
      tmp <- print(attrition_object %>% visR::visr(border = "border"))
      TRUE
    },
    error = function(cond) {
      FALSE
    }
  )

  testthat::expect_false(could_generate_plot)
})

testthat::test_that("T1.19 An error when `border` is not a `character` string.", {
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

  testthat::expect_warning(attrition_object %>% visR::visr(border = 1))
  testthat::expect_warning(attrition_object %>% visR::visr(border = NA))
  testthat::expect_warning(attrition_object %>% visR::visr(border = NULL))
  testthat::expect_warning(attrition_object %>% visR::visr(border = TRUE))
  testthat::expect_warning(attrition_object %>% visR::visr(border = list()))
})

# END OF CODE -------------------------------------------------------------
