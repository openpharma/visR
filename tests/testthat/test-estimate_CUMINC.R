#' @title Specifications test-estimate_CUMINC.R
#' @section Last updated by: JoanaBarros
#' @section Last update date: 2022-02-11
#'
#' @section List of tested specifications
#' T1. The function accepts a `data.frame` `tibble` or `data.table`
#' T1.1 No error when `data` is of class `data.frame`
#' T1.2 No error when `data` is of class `tibble`
#' T1.3 No error when `data` is of class `data.table`
#' T1.4 An error when `data` is of an unexpected class, eg `list`
#' T2. The function relies on the presence of two numeric variables, specified through `AVAL` and `CNSR`, to be present in `data`
#' T2.1 An error when colname specified through `AVAL` is not present in `data`
#' T2.2 An error when colname specified through `AVAL` is not numeric
#' T2.3 No error when the colname specified through `AVAL` is not the proposed default
#' T2.4 No error when the `AVAL` default colname is present in the `data`
#' T2.5 An error when colname specified through `CNSR` is not present in `data`
#' T2.6 An error when colname specified through `CNSR` is not a factor
#' T2.7 No error when the colname specified through `CNSR` is not the proposed default
#' T2.8 No error when the `CNSR` default colname is present in `data`
#' T3. The function creates an object with attributes
#' T3.1 The `failcode` attribute is present in the object
#' T3.2 The `cmprsk` attribute is present in the object
#' T3.3 The `conf.level` attribute is present in the object
#' T3.4 The `tidy` attribute is present in the object
#' T3.5 The `blueprint` attribute is present in the object
#' T3.6 The `formula` attribute is present in the object
#' T4. The user can specify `strata`
#' T4.1 An error when `strata` is not part of `data`
#' T4.2 No error when `strata` is NULL
#' T4.3 No error when `strata` is NULL
#' T4.4 When no strata is specified, the indicators default to `traditional` in the `blueprint` attribute
#' T4.5 When the strata is specified, the number of predictors equals 1 in the `blueprint` attribute
#' T4.6 When the strata is specified, the number of outcomes in the `blueprint` attribute equals the outcome of interest plus the number of competing events
#' T5. The function works with the sister functions
#' T5.1 No error when used in visr() 
#' T5.2 No error when followed by visr() 
#' T5.3 No error when followed by visr() and add_CI() 
#' T5.4 No error when followed by visr() and add_CNSR() 
#' T5.5 No error when followed by visr() and add_risktable() 
#' T5.6 No error when specifying add_risktable() `statlist` arguments
#' T5.7 No error when specifying add_risktable() `group` arguments
#' T5.8 No error when specifying add_risktable() `label` arguments
#' T5.9 No error when specifying add_risktable() `collapse` arguments

# Requirement T1 ----------------------------------------------------------

testthat::context("estimate_cuminc - T1. The function accepts a `data.frame` `tibble` or `data.table`")

testthat::test_that("T1.1 No error when `data` is of class `data.frame`", {
  
  data <- tidycmprsk::trial
  testthat::expect_error(
    visR::estimate_cuminc(data, CNSR = "death_cr", AVAL = "ttdeath"), NA)
  
})

testthat::test_that("T1.2 No error when `data` is of class `tibble`", {
  
  data <- dplyr::as_tibble(tidycmprsk::trial)
  testthat::expect_error(
    visR::estimate_cuminc(data, CNSR = "death_cr", AVAL = "ttdeath"), NA)
  
})

testthat::test_that("T1.3 No error when `data` is of class `data.table`", {
  
  if (nzchar(find.package("data.table"))){
    data <- data.table::as.data.table(tidycmprsk::trial)
    testthat::expect_error(
      visR::estimate_cuminc(data, CNSR = "death_cr", AVAL = "ttdeath"), NA)
  }
  
})

testthat::test_that("T1.4 An error when `data` is of an unexpected class, eg `list`", {
  
  data <- base::as.list(tidycmprsk::trial)
  testthat::expect_error(
    visR::estimate_cuminc(data, CNSR = "death_cr", AVAL = "ttdeath"))
  
})

# Requirement T2 ----------------------------------------------------------
testthat::context("estimate_cuminc - T2. The function relies on the presence of two numeric variables, specified through `AVAL` and `CNSR`, to be present in `data`")

testthat::test_that("T2.1 An error when colname specified through `AVAL` is not present in `data`", {
  
  data <- tidycmprsk::trial
  testthat::expect_error(
    visR::estimate_cuminc(data, CNSR = "death_cr", AVAL = "test"))
  
})

testthat::test_that("T2.2 An error when colname specified through `AVAL` is not numeric", {
  
  data <- tidycmprsk::trial
  data[["ttdeath"]] <- as.character(data[["ttdeath"]])
  
  testthat::expect_error(
    visR::estimate_cuminc(data, CNSR = "death_cr", AVAL = "ttdeath"))

  
})

testthat::test_that("T2.3 No error when the colname specified through `AVAL` is not the proposed default", {
  
  data <- tidycmprsk::trial
  testthat::expect_error(
    visR::estimate_cuminc(data, CNSR = "death_cr", AVAL = "ttdeath"), NA)
  
})

testthat::test_that("T2.4 No error when the `AVAL` default colname is present in the `data`", {
  
  data <- tidycmprsk::trial
  data$AVAL <- data$ttdeath
  data <- data[,-which(colnames(data) == "ttdeath")]
  
  testthat::expect_error(
    visR::estimate_cuminc(data, CNSR = "death_cr"), NA)
  
})

testthat::test_that("T2.5 An error when colname specified through `CNSR` is not present in `data`", {
  
  data <- tidycmprsk::trial
  testthat::expect_error(
    visR::estimate_cuminc(data, CNSR = "test", AVAL = "ttdeath"))
  
})

testthat::test_that("T2.6 An error when colname specified through `CNSR` is not a factor", {
  
  data <- tidycmprsk::trial
  data[["death_cr"]] <- as.numeric(data[["death_cr"]])
  
  testthat::expect_error(
    visR::estimate_cuminc(data, CNSR = "death_cr", AVAL = "ttdeath"))
  
})

testthat::test_that("T2.7 No error when the colname specified through `CNSR` is not the proposed default", {
  
  data <- tidycmprsk::trial
  testthat::expect_error(
    visR::estimate_cuminc(data, CNSR = "death_cr", AVAL = "ttdeath"), NA)
  
})

testthat::test_that("T2.8 No error when the `CNSR` default colname is present in `data`", {
  
  data <- tidycmprsk::trial
  data$CNSR <- data$death_cr
  data <- data[,-which(colnames(data) == "death_cr")]
  
  testthat::expect_error(
    visR::estimate_cuminc(data, AVAL = "ttdeath"), NA)
  
})

# Requirement T3 ----------------------------------------------------------
testthat::context("estimate_cuminc - T3. The function creates an object with attributes")

testthat::test_that("T3.1 The `failcode` attribute is present in the object", {
  
  data <- tidycmprsk::trial
  cumincobj <- visR::estimate_cuminc(data, CNSR = "death_cr", AVAL = "ttdeath")
    
  testthat::expect_true("failcode" %in% names(cumincobj))

})

testthat::test_that("T3.2 The `cmprsk` attribute is present in the object", {
  
  data <- tidycmprsk::trial
  cumincobj <- visR::estimate_cuminc(data, CNSR = "death_cr", AVAL = "ttdeath")

  testthat::expect_true("cmprsk" %in% names(cumincobj))
  
})


testthat::test_that("T3.3 The `conf.level` attribute is present in the object", {
  
  data <- tidycmprsk::trial
  cumincobj <- visR::estimate_cuminc(data, CNSR = "death_cr", AVAL = "ttdeath")

  testthat::expect_true("conf.level" %in% names(cumincobj))
  
})

testthat::test_that("T3.4 The `tidy` attribute is present in the object", {
  
  data <- tidycmprsk::trial
  cumincobj <- visR::estimate_cuminc(data, CNSR = "death_cr", AVAL = "ttdeath")

  testthat::expect_true("tidy" %in% names(cumincobj))
  
})

testthat::test_that("T3.5 The `blueprint` attribute is present in the object", {
  
  data <- tidycmprsk::trial
  cumincobj <- visR::estimate_cuminc(data, CNSR = "death_cr", AVAL = "ttdeath")

  testthat::expect_true("blueprint" %in% names(cumincobj))
  
})

testthat::test_that("T3.6 The `formula` attribute is present in the object", {
  
  data <- tidycmprsk::trial
  cumincobj <- visR::estimate_cuminc(data, CNSR = "death_cr", AVAL = "ttdeath")

  testthat::expect_true("formula" %in% names(cumincobj))
  
})

# Requirement T4 ----------------------------------------------------------
testthat::context("estimate_cuminc - T4. The user can specify `strata`")

testthat::test_that("T4.1 An error when `strata` is not part of `data`", {
  
  data <- tidycmprsk::trial
  testthat::expect_error(
    visR::estimate_cuminc(data, CNSR = "test", AVAL = "ttdeath", strata = "test"))
})

testthat::test_that("T4.2 No error when `strata` is NULL", {
  
  data <- tidycmprsk::trial
  testthat::expect_error(
    visR::estimate_cuminc(data, CNSR = "test", AVAL = "ttdeath", strata = NULL))
  
})

testthat::test_that("T4.3 No error when `strata` is NULL", {
  
  data <- tidycmprsk::trial
  testthat::expect_error(
    visR::estimate_cuminc(data, CNSR = "test", AVAL = "ttdeath", strata = NULL))
  
})

testthat::test_that("T4.4 When no strata is specified, the indicators default to `traditional` in the `blueprint` attribute", {
  
  data <- tidycmprsk::trial
  cumincobj <- visR::estimate_cuminc(data, CNSR = "death_cr", AVAL = "ttdeath")
  
  testthat::expect_equal(cumincobj$blueprint$indicators, "traditional")
  
})

testthat::test_that("T4.5 When the strata is specified, the number of predictors equals 1 in the `blueprint` attribute", {
  
  data <- tidycmprsk::trial
  cumincobj <- visR::estimate_cuminc(data, CNSR = "death_cr", AVAL = "ttdeath", strata = "trt")
  npredictors <- cumincobj$blueprint$ptypes$predictors
  
  testthat::expect_equal(length(names(npredictors)), 1)
  
})

testthat::test_that("T4.6 When the strata is specified, the number of outcomes in the `blueprint` attribute equals the outcome of interest plus the number of competing events", {
  
  data <- tidycmprsk::trial
  cumincobj <- visR::estimate_cuminc(data, CNSR = "death_cr", AVAL = "ttdeath", strata = "trt")
  noutcomes <- names(cumincobj$blueprint$ptypes$outcomes)
  censoring_status <- unique(data$death_cr) 
  
  testthat::expect_equal(length(noutcomes), length(censoring_status) -1)
  
})

# Requirement T5 ----------------------------------------------------------
testthat::context("estimate_cuminc - T5. The function works with the sister functions")

testthat::test_that("T5.1 No error when used in visr() ", {
  
  data <- tidycmprsk::trial
  testthat::expect_error(
    visR::visr(visR::estimate_cuminc(data, CNSR = "death_cr", AVAL = "ttdeath", 
                               strata = "trt")), NA) 
  
})


testthat::test_that("T5.2 No error when followed by visr() ", {
                      
  data <- tidycmprsk::trial
  testthat::expect_error(
    visR::estimate_cuminc(data, CNSR = "death_cr", AVAL = "ttdeath", strata = "trt") %>%
      visR::visr(), NA) 
                      
})

testthat::test_that("T5.3 No error when followed by visr() and add_CI() ", {
  
  data <- tidycmprsk::trial
  testthat::expect_error(
    visR::estimate_cuminc(data, CNSR = "death_cr", AVAL = "ttdeath", strata = "trt") %>%
      visR::visr() %>% 
      visR::add_CI(), NA) 
  
})

testthat::test_that("T5.4 No error when followed by visr() and add_CNSR() ", {
  
  data <- tidycmprsk::trial
  testthat::expect_error(
    visR::estimate_cuminc(data, CNSR = "death_cr", AVAL = "ttdeath", strata = "trt") %>%
      visR::visr() %>% 
      visR::add_CNSR(), NA) 
  
})

testthat::test_that("T5.5 No error when followed by visr() and add_risktable() ", {
  
  data <- tidycmprsk::trial
  testthat::expect_error(
    visR::estimate_cuminc(data, CNSR = "death_cr", AVAL = "ttdeath", strata = "trt") %>%
      visR::visr() %>% 
      visR::add_risktable(), NA) 
  
})

testthat::test_that("T5.6 No error when specifying add_risktable() `statlist` arguments", {
  
  data <- tidycmprsk::trial
  testthat::expect_error(
    visR::estimate_cuminc(data, CNSR = "death_cr", AVAL = "ttdeath", strata = "trt") %>%
      visR::visr() %>% 
      visR::add_risktable(statlist = c("n.risk", "n.event")), NA) 
  
})

testthat::test_that("T5.7 No error when specifying add_risktable() `group` arguments", {
  
  data <- tidycmprsk::trial
  testthat::expect_error(
    visR::estimate_cuminc(data, CNSR = "death_cr", AVAL = "ttdeath", strata = "trt") %>%
      visR::visr() %>% 
      visR::add_risktable(statlist = c("n.risk", "n.event"),
                          group = "statlist"), NA) 
  
})

testthat::test_that("T5.8 No error when specifying add_risktable() `label` arguments", {
  
  data <- tidycmprsk::trial
  testthat::expect_error(
    visR::estimate_cuminc(data, CNSR = "death_cr", AVAL = "ttdeath", strata = "trt") %>%
      visR::visr() %>% 
      visR::add_risktable(statlist = c("n.risk", "n.event"),
                          label = c("No. at Risk", "No. Events")), NA) 
  
})

testthat::test_that("T5.9 No error when specifying add_risktable() `collapse` arguments", {
  
  data <- tidycmprsk::trial
  cumincobj <- visR::estimate_cuminc(data, CNSR = "death_cr", AVAL = "ttdeath", strata = "trt") %>%
    visR::visr() 
  
  testthat::expect_error(cumincobj %>% 
                           visR::add_risktable(statlist = c("n.risk", "n.event"),
                                               collapse = TRUE), NA) 
  
  testthat::expect_error(cumincobj %>% 
                           visR::add_risktable(statlist = c("n.risk", "n.event"),
                                               group = "statlist",
                                               collapse = TRUE), NA) 
  
})

# END OF CODE -------------------------------------------------------------
