#' @title Specifications test-get_attrition.R
#' @section Last updated by: Tim Treis (tim.treis@@outlook.de)
#' @section Last update date: 2022-02-09T15:22:32
#'
#' @section List of tested specifications
#' T1. The function accepts a `data.frame` `tibble` or `data.table`
#' T1.1 No error when `data` is of class `data.frame`
#' T1.2 No error when `data` is of class `tibble`
#' T1.3 No error when `data` is of class `data.table`
#' T1.4 An error when `data` is of an unexpected class, eg `list`
#' T1.5 An error when `data` is NULL
#' T1.6 An error when `data` does not exist in the global environment
#' T2. The function correctly handles arguments
#' T2.1 No error when `criteria_descriptions` is a character vector
#' T2.2 An error when `criteria_descriptions` is not a character vector
#' T2.3 An error when `criteria_descriptions` is NULL
#' T2.4 No error when `criteria_conditions` is a character vector
#' T2.5 An error when `criteria_conditions` is not a character vector
#' T2.6 An error when `criteria_conditions` is NULL
#' T2.7 No error when `subject_column_name` is a string
#' T2.8 An error when `subject_column_name` is not a character vector
#' T2.9 An error when `subject_column_name` is NULL
#' T2.10 An error when `subject_column_name` is missing as a column in `data`
#' T2.11 An error when `criteria_descriptions` and `criteria_descriptions` do not have the same length
#' T3. The returned object is of correct shape
#' T3.1 Correct number of rows in the data.frame with `criteria_conditions`+1 rows
#' T3.2 Correct number of columns in the data.frame
#' T4. The function filters correctly when provided a vector of single filters
#' T4.1 Correct filtering string column
#' T4.2 Correct filtering integer column
#' T4.3 Correct filtering factor column
#' T5. The function filters correctly when provided a vector of single filters
#' T5.1 Correct filtering using a combined filter containing logical `and` (`&`)
#' T5.2 Correct filtering using a combined filter containing logical `or` (`|`)
#' T6. The returned object is of correct class
#' T6.1 The object is of class `data.frame`
#' T6.2 The object is of class `attrition`

# Requirement T1 ----------------------------------------------------------

testthat::context("get_attrition - T1. The function accepts a `data.frame` `tibble` or `data.table`")

testthat::test_that("T1.1 No error when `data` is of class `data.frame`", {
  data <- adtte
  testthat::expect_error(
    visR::get_attrition(
      data = data,
      criteria_descriptions = c(
        "1. Placebo Group",
        "2. Be 75 years of age or older."
      ),
      criteria_conditions = c("TRTP=='Placebo'", "AGE>=75"),
      subject_column_name = "USUBJID"
    ), NA
  )
})

testthat::test_that("T1.2 No error when `data` is of class `tibble`", {
  data <- dplyr::as_tibble(adtte)
  testthat::expect_error(
    visR::get_attrition(
      data = data,
      criteria_descriptions = c(
        "1. Placebo Group",
        "2. Be 75 years of age or older."
      ),
      criteria_conditions = c("TRTP=='Placebo'", "AGE>=75"),
      subject_column_name = "USUBJID"
    ), NA
  )
})

testthat::test_that("T1.3 No error when `data` is of class `data.table`", {
  if (nzchar(find.package("data.table"))) {
    data <- data.table::as.data.table(adtte)
    testthat::expect_error(
      visR::get_attrition(
        data = data,
        criteria_descriptions = c(
          "1. Placebo Group",
          "2. Be 75 years of age or older."
        ),
        criteria_conditions = c("TRTP=='Placebo'", "AGE>=75"),
        subject_column_name = "USUBJID"
      ), NA
    )
  }
})

testthat::test_that("T1.4 An error when `data` is of an unexpected class, eg `list`", {
  data <- base::as.list(adtte)
  testthat::expect_error(
    visR::get_attrition(
      data = data,
      criteria_descriptions = c(
        "1. Placebo Group",
        "2. Be 75 years of age or older."
      ),
      criteria_conditions = c("TRTP=='Placebo'", "AGE>=75"),
      subject_column_name = "USUBJID"
    )
  )
})

testthat::test_that("T1.5 An error when `data` is NULL", {
  testthat::expect_error(
    visR::get_attrition(
      data = NULL,
      criteria_descriptions = c(
        "1. Placebo Group",
        "2. Be 75 years of age or older."
      ),
      criteria_conditions = c("TRTP=='Placebo'", "AGE>=75"),
      subject_column_name = "USUBJID"
    )
  )
})

testthat::test_that("T1.6 An error when `data` does not exist in the global environment", {
  testthat::expect_error(
    visR::get_attrition(
      data = blah,
      criteria_descriptions = c(
        "1. Placebo Group",
        "2. Be 75 years of age or older."
      ),
      criteria_conditions = c("TRTP=='Placebo'", "AGE>=75"),
      subject_column_name = "USUBJID"
    )
  )
})

# Requirement T2 ---------------------------------------------------------------

testthat::context("get_attrition - T2. The function correctly handles arguments")

testthat::test_that("T2.1 No error when `criteria_descriptions` is a character vector", {
  testthat::expect_error(
    visR::get_attrition(
      data = adtte,
      criteria_descriptions = c(
        "1. Placebo Group",
        "2. Be 75 years of age or older."
      ),
      criteria_conditions = c("TRTP=='Placebo'", "AGE>=75"),
      subject_column_name = "USUBJID"
    ), NA
  )
})

testthat::test_that("T2.2 An error when `criteria_descriptions` is not a character vector", {
  testthat::expect_error(
    visR::get_attrition(
      data = adtte,
      criteria_descriptions = "BLAH",
      criteria_conditions = c("TRTP=='Placebo'", "AGE>=75"),
      subject_column_name = "USUBJID"
    )
  )
})

testthat::test_that("T2.3 An error when `criteria_descriptions` is NULL", {
  testthat::expect_error(
    visR::get_attrition(
      data = adtte,
      criteria_descriptions = NULL,
      criteria_conditions = c("TRTP=='Placebo'", "AGE>=75"),
      subject_column_name = "USUBJID"
    )
  )
})

testthat::test_that("T2.4 No error when `criteria_conditions` is a character vector", {
  testthat::expect_error(
    visR::get_attrition(
      data = adtte,
      criteria_descriptions = c(
        "1. Placebo Group",
        "2. Be 75 years of age or older."
      ),
      criteria_conditions = c("TRTP=='Placebo'", "AGE>=75"),
      subject_column_name = "USUBJID"
    ), NA
  )
})

testthat::test_that("T2.5 An error when `criteria_conditions` is not a character vector", {
  testthat::expect_error(
    visR::get_attrition(
      data = adtte,
      criteria_descriptions = c(
        "1. Placebo Group",
        "2. Be 75 years of age or older."
      ),
      criteria_conditions = 1,
      subject_column_name = "USUBJID"
    )
  )
})

testthat::test_that("T2.6 An error when `criteria_conditions` is NULL", {
  testthat::expect_error(
    visR::get_attrition(
      data = adtte,
      criteria_descriptions = c(
        "1. Placebo Group",
        "2. Be 75 years of age or older."
      ),
      criteria_conditions = NULL,
      subject_column_name = "USUBJID"
    )
  )
})


testthat::test_that("T2.7 No error when `subject_column_name` is a string", {
  testthat::expect_error(
    visR::get_attrition(
      data = adtte,
      criteria_descriptions = c(
        "1. Placebo Group",
        "2. Be 75 years of age or older."
      ),
      criteria_conditions = c("TRTP=='Placebo'", "AGE>=75"),
      subject_column_name = "USUBJID"
    ), NA
  )
})

testthat::test_that("T2.8 An error when `subject_column_name` is not a character vector", {
  testthat::expect_error(
    visR::get_attrition(
      data = adtte,
      criteria_descriptions = c(
        "1. Placebo Group",
        "2. Be 75 years of age or older."
      ),
      criteria_conditions = c("TRTP=='Placebo'", "AGE>=75"),
      subject_column_name = c("USUBJID", "TRTP")
    )
  )
})

testthat::test_that("T2.9 An error when `subject_column_name` is NULL", {
  testthat::expect_error(
    visR::get_attrition(
      data = adtte,
      criteria_descriptions = c(
        "1. Placebo Group",
        "2. Be 75 years of age or older."
      ),
      criteria_conditions = c("TRTP=='Placebo'", "AGE>=75"),
      subject_column_name = NULL
    )
  )
})

testthat::test_that("T2.10 An error when `subject_column_name` is missing as a column in `data`", {
  testthat::expect_error(
    visR::get_attrition(
      data = adtte,
      criteria_descriptions = c(
        "1. Placebo Group",
        "2. Be 75 years of age or older."
      ),
      criteria_conditions = c("TRTP=='Placebo'", "AGE>=75"),
      subject_column_name = "BLAH"
    )
  )
})

testthat::test_that("T2.11 An error when `criteria_descriptions` and `criteria_descriptions` do not have the same length", {
  testthat::expect_error(
    visR::get_attrition(
      data = adtte,
      criteria_descriptions = "BLAH",
      criteria_conditions = c("TRTP=='Placebo'", "AGE>=75"),
      subject_column_name = "USUBJID"
    )
  )
})

# Requirement T3 ---------------------------------------------------------------

testthat::context("get_attrition - T3. The returned object is of correct shape")

testthat::test_that("T3.1 Correct number of rows in the data.frame with `criteria_conditions`+1 rows", {
  cdesc <- c("1. Placebo Group", "2. Be 75 years of age or older.")
  testthat::expect_equal(
    nrow(visR::get_attrition(
      data = adtte,
      criteria_descriptions = cdesc,
      criteria_conditions = c("TRTP=='Placebo'", "AGE>=75"),
      subject_column_name = "USUBJID"
    )), length(cdesc) + 1
  )
})

testthat::test_that("T3.2 Correct number of columns in the data.frame", {
  testthat::expect_equal(
    ncol(visR::get_attrition(
      data = adtte,
      criteria_descriptions = c(
        "1. Placebo Group",
        "2. Be 75 years of age or older."
      ),
      criteria_conditions = c("TRTP=='Placebo'", "AGE>=75"),
      subject_column_name = "USUBJID"
    )), 6
  )
})

# Requirement T4 ---------------------------------------------------------------

testthat::context("get_attrition - T4. The function filters correctly when provided a vector of single filters")

testthat::test_that("T4.1 Correct filtering string column", {
  ninit <- length(unique(adtte$USUBJID))
  filtered_data <- adtte %>% dplyr::filter(TRTP == "Placebo")

  outdf <- visR::get_attrition(adtte,
    criteria_descriptions = c("Placebo Tx"),
    criteria_conditions = c("TRTP=='Placebo'"),
    subject_column_name = "USUBJID"
  )

  testthat::expect_equal(
    outdf$`Remaining N`[length(outdf$`Remaining N`)],
    length(unique(filtered_data$USUBJID))
  )

  testthat::expect_equal(
    outdf$`Excluded N`[length(outdf$`Excluded N`)],
    ninit - length(unique(filtered_data$USUBJID))
  )
})

testthat::test_that("T4.2 Correct filtering integer column", {
  ninit <- length(unique(adtte$USUBJID))
  filtered_data <- adtte %>% dplyr::filter(AGE >= 75)

  outdf <- visR::get_attrition(adtte,
    criteria_descriptions = c("75+ years"),
    criteria_conditions = c("AGE >= 75"),
    subject_column_name = "USUBJID"
  )

  testthat::expect_equal(
    outdf$`Remaining N`[length(outdf$`Remaining N`)],
    length(unique(filtered_data$USUBJID))
  )

  testthat::expect_equal(
    outdf$`Excluded N`[length(outdf$`Excluded N`)],
    ninit - length(unique(filtered_data$USUBJID))
  )
})

testthat::test_that("T4.3 Correct filtering factor column", {
  data <- adtte %>% dplyr::mutate(AGEGR1 = factor(AGEGR1))
  ninit <- length(unique(data$USUBJID))
  filtered_data <- adtte %>% dplyr::filter(AGEGR1 == "< 65")

  outdf <- visR::get_attrition(data,
    criteria_descriptions = c("Age group < 65"),
    criteria_conditions = c("AGEGR1 == '< 65'"),
    subject_column_name = "USUBJID"
  )

  # test remaining n at end of dataframe
  testthat::expect_equal(
    outdf$`Remaining N`[length(outdf$`Remaining N`)],
    length(unique(filtered_data$USUBJID))
  )

  # test excluded n at end of dataframe
  testthat::expect_equal(
    outdf$`Excluded N`[length(outdf$`Excluded N`)],
    ninit - length(unique(filtered_data$USUBJID))
  )
})

# Requirement T5 ---------------------------------------------------------------

testthat::context("get_attrition - T5. The function filters correctly when provided a vector of single filters")

testthat::test_that("T5.1 Correct filtering using a combined filter containing logical `and` (`&`)", {
  ninit <- length(unique(adtte$USUBJID))
  outdf1 <- visR::get_attrition(
    adtte,
    criteria_descriptions = c("Male under 65"),
    criteria_conditions = c("AGEGR1 == '< 65' & SEX == 'M'"),
    subject_column_name = "USUBJID"
  )
  filtered_data <- adtte %>%
    dplyr::filter(AGEGR1 == "< 65") %>%
    dplyr::filter(SEX == "M")

  # test remaining n at end of data.frame
  testthat::expect_equal(
    outdf1$`Remaining N`[length(outdf1$`Remaining N`)],
    length(unique(filtered_data$USUBJID))
  )

  # test excluded n at end of data.frame
  testthat::expect_equal(
    outdf1$`Excluded N`[length(outdf1$`Excluded N`)],
    ninit - length(unique(filtered_data$USUBJID))
  )

  outdf2 <- visR::get_attrition(
    adtte,
    criteria_descriptions = c("Male under 65", "Placebo Group"),
    criteria_conditions = c(
      "AGEGR1 == '< 65' & SEX == 'M'",
      "TRTP=='Placebo'"
    ),
    subject_column_name = "USUBJID"
  )

  filtered_data <- filtered_data %>%
    dplyr::filter(TRTP == "Placebo")

  # test remaining n at end of data.frame
  testthat::expect_equal(
    outdf2$`Remaining N`[length(outdf2$`Remaining N`)],
    length(unique(filtered_data$USUBJID))
  )
})

testthat::test_that("T5.2 Correct filtering using a combined filter containing logical `or` (`|`)", {
  ninit <- length(unique(adtte$USUBJID))
  outdf1 <- visR::get_attrition(
    adtte,
    criteria_descriptions = c("Male or under 65"),
    criteria_conditions = c("AGEGR1 == '< 65' | SEX == 'M'"),
    subject_column_name = "USUBJID"
  )

  filtered_data <- adtte %>%
    dplyr::filter(AGEGR1 == "< 65" | SEX == "M")

  # test remaining n at end of data.frame

  testthat::expect_equal(
    outdf1$`Remaining N`[length(outdf1$`Remaining N`)],
    length(unique(filtered_data$USUBJID))
  )

  # test excluded n at end of data.frame
  testthat::expect_equal(
    outdf1$`Excluded N`[length(outdf1$`Excluded N`)],
    ninit - length(unique(filtered_data$USUBJID))
  )

  outdf2 <- visR::get_attrition(
    adtte,
    criteria_descriptions = c("Male or under 65", "Placebo Group", "White"),
    criteria_conditions = c(
      "AGEGR1 == '< 65' | SEX == 'M'",
      "TRTP == 'Placebo'",
      "RACE == 'WHITE'"
    ),
    subject_column_name = "USUBJID"
  )

  filtered_data <- adtte %>%
    dplyr::filter(AGEGR1 == "< 65" | SEX == "M") %>%
    dplyr::filter(TRTP == "Placebo") %>%
    dplyr::filter(RACE == "WHITE")

  # test remaining n at end of data.frame
  testthat::expect_equal(
    outdf2$`Remaining N`[length(outdf2$`Remaining N`)],
    length(unique(filtered_data$USUBJID))
  )
})

# Requirement T6 ---------------------------------------------------------------

testthat::context("get_attrition - T6. The returned object is of correct class")

testthat::test_that("T6.1 The object is of class `data.frame`", {
  outdf <- visR::get_attrition(
    adtte,
    criteria_descriptions = c(
      "1. Placebo Group",
      "2. Be 75 years of age or older."
    ),
    criteria_conditions = c("TRTP == 'Placebo'", "AGE >= 75"),
    subject_column_name = "USUBJID"
  )

  testthat::expect_s3_class(outdf, "data.frame")
})

testthat::test_that("T6.2 The object is of class `attrition`", {
  outdf <- visR::get_attrition(
    adtte,
    criteria_descriptions = c(
      "1. Placebo Group",
      "2. Be 75 years of age or older."
    ),
    criteria_conditions = c("TRTP == 'Placebo'", "AGE >= 75"),
    subject_column_name = "USUBJID"
  )

  testthat::expect_s3_class(outdf, "attrition")
})

# END OF CODE -------------------------------------------------------------
