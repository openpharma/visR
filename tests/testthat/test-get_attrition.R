#' T1. The function accepts a data.frame or related table object
#' T1.1 No error when `data` is a data.frame
#' T1.2 No error when `data` is a tibble
#' T1.3 No error when `data` is a data.table
#' T1.4 An error when `survfit_object` is a random object
#' T1.5 An error when `survfit_object` is NULL
#' 
#' T2. The function correctly handles arguments
#' T2.1 No error when `criteria_descriptions` is a character vector
#' T2.2 An error when `criteria_descriptions` is not a character vector
#' T2.3 An error when `criteria_descriptions` is NULL
#' T2.4 No error when `criteria_conditions` is a character vector
#' T2.5 An error when `criteria_conditions` is not a character vector
#' T2.6 An error when `criteria_conditions` is NULL
#' T2.7 No error when `subject_column_name` is a string
#' T2.8 An error when `subject_column_name` is not a string
#' T2.9 An error when `subject_column_name` is NULL
#' T2.10 An error when `subject_column_name` is missing as a column in `data`
#' 
#' T3. The returned object is shape
#' T3.1 Correct number of rows in the data.frame with `criteria_conditions`+1 rows
#' T3.2 Correct number of columns in the data.frame
#' 
#' T4. The function filters correctly when provided aa vector of single filters
#' T4.1 Correct filtering string column
#' T4.2 Correct filtering integer column
#' T4.3 Correct filtering factor column
#' 
#' T5. The function filters correctly when provided a vector of combined filters
#' T5.1 Correct filtering using a combined filter containing logical `and` (`&`)
#' T5.2 Correct filtering using a combined filter containing logical `or` (`|`)
#'
#' T6. The returned object is of correct class
#' T3.1 The object is of class `data.frame`
#' T3.2 The object is of class `attritiontable`

context("get_attrition - T1. The function accepts a `data.frame` `tibble` or `data.table`")

testthat::test_that("T1.1. No error when `data` is of class `data.frame`",{
    
    data <- adtte
    testthat::expect_error(
        visR::get_attrition(data = data,
                        criteria_descriptions = c("1. Placebo Group", "2. Be 75 years of age or older."),
                        criteria_conditions   = c("TRTP=='Placebo'","AGE>=75"),
                        subject_column_name   = 'USUBJID'
                        ), NA
    )
    
})


testthat::test_that("T1.2. No error when `data` is of class `tibble`",{
    
    data <- dplyr::as_tibble(adtte)
    testthat::expect_error(
        visR::get_attrition(data = data,
                            criteria_descriptions = c("1. Placebo Group", "2. Be 75 years of age or older."),
                            criteria_conditions   = c("TRTP=='Placebo'","AGE>=75"),
                            subject_column_name   = 'USUBJID'
        ), NA
    )
})

testthat::test_that("T1.3. No error when `data` is of class `data.table`",{
    
    if ("data.table" %in% rownames(installed.packages())){
        data <- data.table::as.data.table(adtte)
        testthat::expect_error(
            visR::get_attrition(data = data,
                                criteria_descriptions = c("1. Placebo Group", "2. Be 75 years of age or older."),
                                criteria_conditions   = c("TRTP=='Placebo'","AGE>=75"),
                                subject_column_name   = 'USUBJID'
            ), NA
        )
    }
})

testthat::test_that("T1.4. An error when `data` is of class `list`",{
    
    data <- base::as.list(adtte)
    testthat::expect_error(
        visR::get_attrition(data = data,
                            criteria_descriptions = c("1. Placebo Group", "2. Be 75 years of age or older."),
                            criteria_conditions   = c("TRTP=='Placebo'","AGE>=75"),
                            subject_column_name   = 'USUBJID'
        )
    )
    
})

testthat::test_that("T1.5 An error when `data` is NULL",{
    
    testthat::expect_error(
        visR::get_attrition(data = NA,
                            criteria_descriptions = c("1. Placebo Group", "2. Be 75 years of age or older."),
                            criteria_conditions   = c("TRTP=='Placebo'","AGE>=75"),
                            subject_column_name   = 'USUBJID'
        )
    )
    
})

testthat::test_that("T1.6 An error when `data` does not exist in the global environment",{
    testthat::expect_error(
        visR::get_attrition(data = blah,
                            criteria_descriptions = c("1. Placebo Group", "2. Be 75 years of age or older."),
                            criteria_conditions   = c("TRTP=='Placebo'","AGE>=75"),
                            subject_column_name   = 'USUBJID'
        )
    )
    
})

context("get_attrition - T6. The returned object is of correct class")

testthat::test_that("T6.1 The object is of class `data.frame`",{
    outdf <- visR::get_attrition(adtte,
                                 criteria_descriptions = c("1. Placebo Group", "2. Be 75 years of age or older."),
                                 criteria_conditions   = c("TRTP=='Placebo'","AGE>=75"),
                                 subject_column_name   = 'USUBJID')
    
    testthat::expect_s3_class(outdf, "data.frame")
    
})

testthat::test_that("T6.2 The object is of class `attritiontable`",{
    outdf <- visR::get_attrition(adtte,
                                 criteria_descriptions = c("1. Placebo Group", "2. Be 75 years of age or older."),
                                 criteria_conditions   = c("TRTP=='Placebo'","AGE>=75"),
                                 subject_column_name   = 'USUBJID')
    
    testthat::expect_s3_class(outdf, "attritiontable")
    
})

