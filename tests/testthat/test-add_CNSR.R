#' @title Specifications plot
#' @section Last updated by:
#' Tim Treis
#' @section Last update date:
#' 18-MAR-2021

# Specifications ---------------------------------------------------------------

#' T1.  The output plots after adding confidence intervals don't differ from the reviewed plots 
#' T1.1 No error when censoring is plotted for one strata with default parameters
#' T1.2 No error when censoring is plotted for more than one strata with default parameters
#' T1.3 No error when a valid non-default `shape` is specified 
#' T1.4 No error when a valid non-default `size` is specified
#' T2.  Warnings/errors in case of missing data or weird arguments are thrown. 
#' T2.1 Error when object is not ggsurvfit
#' T2.2 Warning when a character is provided as `size`
#' T2.3 Warning when NULL is provided as `size`
#' T2.4 Warning when NULL is provided as `shape`
#' T2.5 A ggplot warning when a non-matching vector for `size` is specified
#' T2.6 A ggplot warning when a non-matching vector for `shape` is specified

# Requirement T1 ---------------------------------------------------------------

context("add_CNSR - T1. The output plots after adding confidence intervals don't differ from the reviewed plots")

testthat::test_that("T1.1 No error when censoring is plotted for one strata with default parameters",{
  
  p <- adtte %>% 
    visR::estimate_KM() %>%
    plot() %>%
    visR::add_CNSR()
  vdiffr::expect_doppelganger("add_CNSR_T1_1", p)
  
})

testthat::test_that("T1.2 No error when censoring is plotted for more than one strata with default parameters",{
  
  p_SEX <- adtte %>% 
    visR::estimate_KM(strata = "SEX") %>%
    plot() %>%
    visR::add_CNSR()
  vdiffr::expect_doppelganger("add_CNSR_T1_2_SEX", p_SEX)
  
  p_TRTP <- adtte %>% 
    visR::estimate_KM(strata = "TRTP") %>%
    plot() %>%
    visR::add_CNSR()
  vdiffr::expect_doppelganger("add_CNSR_T1_2_TRTP", p_TRTP)
  
})

testthat::test_that("T1.3 No error when a valid non-default `shape` is specified",{
  
  p_shape_empty <- adtte %>% 
    visR::estimate_KM(strata = "SEX") %>%
    plot() %>%
    visR::add_CNSR(shape = "")
  vdiffr::expect_doppelganger("add_CNSR_T1_3_shape_empty", p_shape_empty)  
  
  p_shape1 <- adtte %>% 
    visR::estimate_KM(strata = "SEX") %>%
    plot() %>%
    visR::add_CNSR(shape = 1)
  vdiffr::expect_doppelganger("add_CNSR_T1_3_shape1", p_shape1)
  
  p_shape11 <- adtte %>% 
    visR::estimate_KM(strata = "SEX") %>%
    plot() %>%
    visR::add_CNSR(shape = 11)
  vdiffr::expect_doppelganger("add_CNSR_T1_3_shape11", p_shape11)
  
  p_shape16 <- adtte %>% 
    visR::estimate_KM(strata = "SEX") %>%
    plot() %>%
    visR::add_CNSR(shape = 16)
  vdiffr::expect_doppelganger("add_CNSR_T1_3_shape16", p_shape16)
  
  p_shape_custom <- adtte %>% 
    visR::estimate_KM(strata = "SEX") %>%
    plot() %>%
    visR::add_CNSR(shape = "?")
  vdiffr::expect_doppelganger("add_CNSR_T1_3_shape_custom", p_shape_custom)
  
})

testthat::test_that("T1.4 No error when a valid non-default `size` is specified",{
  
  p_size0 <- adtte %>% 
    visR::estimate_KM(strata = "SEX") %>%
    plot() %>%
    visR::add_CNSR(size = 0)
  vdiffr::expect_doppelganger("add_CNSR_T1_3_size0", p_size0)
  
  p_size1 <- adtte %>% 
    visR::estimate_KM(strata = "SEX") %>%
    plot() %>%
    visR::add_CNSR(size = 1)
  vdiffr::expect_doppelganger("add_CNSR_T1_3_size1", p_size1)
  
  p_size11 <- adtte %>% 
    visR::estimate_KM(strata = "SEX") %>%
    plot() %>%
    visR::add_CNSR(size = 11)
  vdiffr::expect_doppelganger("add_CNSR_T1_3_size11", p_size11)
  
  p_size21 <- adtte %>% 
    visR::estimate_KM(strata = "SEX") %>%
    plot() %>%
    visR::add_CNSR(size = 21)
  vdiffr::expect_doppelganger("add_CNSR_T1_3_size21", p_size21)
  
  p_size10000 <- adtte %>% 
    visR::estimate_KM(strata = "SEX") %>%
    plot() %>%
    visR::add_CNSR(size = 10000)
  vdiffr::expect_doppelganger("add_CNSR_T1_3_size10000", p_size10000)
  
})


# Requirement T2 ---------------------------------------------------------------

context("add_CNSR - T2. Warnings/errors in case of missing data or weird arguments are thrown.")

testthat::test_that("T2.1 Error when object is not ggsurvfit",{
  
  p <- adtte %>% 
    visR::estimate_KM() %>%
    plot()
  
  p_without_ggsurvfit <- p
  class(p_without_ggsurvfit) <- class(p)[class(p) != "ggsurvfit"]
  
  testthat::expect_error(p %>% visR::add_CNSR(), NA)
  testthat::expect_error(p_without_ggsurvfit %>% visR::add_CNSR())
  
})

testthat::test_that("T2.2 Warning when a character is provided as `size`",{
  
  p <- adtte %>% 
    visR::estimate_KM(strata = "SEX") %>%
    plot()
  
  expected_warning = "Invalid `size` specified. Setting it to 2."
  testthat::expect_warning(p %>% visR::add_CNSR(size = "a"), expected_warning)
  
})

testthat::test_that("T2.3 Warning when NULL is provided as `size`",{
  
  p <- adtte %>% 
    visR::estimate_KM(strata = "SEX") %>%
    plot()
  
  expected_warning = "Invalid `size` specified. Setting it to 2."
  testthat::expect_warning(p %>% visR::add_CNSR(size = NULL), expected_warning)
  
})

testthat::test_that("T2.4 Warning when NULL is provided as `shape`",{
  
  p <- adtte %>% 
    visR::estimate_KM(strata = "SEX") %>%
    plot()
  
  expected_warning = "Invalid `shape` specified. Setting it to 3."
  testthat::expect_warning(p %>% visR::add_CNSR(shape = NULL), expected_warning)
  
})

testthat::test_that("T2.5 A ggplot warning when a non-matching vector for `size` is specified",{
  
  p <- adtte %>% 
    visR::estimate_KM(strata = "SEX") %>%
    plot()
  
  # Cause warning
  p %>% visR::add_CNSR(size = c("We", "let", "ggplot", "test", "lists"))
  
  # Catch warning
  abortive_warning <- visR:::check_traceback_stack_for_ggplot_aesthetics_warning()
  
  ggplot_error <- "Aesthetics must be either length 1 or the same as the data"
  testthat::expect_match(abortive_warning, ggplot_error)
  
})

testthat::test_that("T2.6 A ggplot warning when a non-matching vector for `shape` is specified",{
  
  p <- adtte %>% 
    visR::estimate_KM(strata = "SEX") %>%
    plot()
  
  # Cause warning
  p %>% visR::add_CNSR(shape = c("We", "let", "ggplot", "test", "lists"))
  
  # Catch warning
  abortive_warning <- visR:::check_traceback_stack_for_ggplot_aesthetics_warning()
  
  ggplot_error <- "Aesthetics must be either length 1 or the same as the data"
  testthat::expect_match(abortive_warning, ggplot_error)
  
})

# END OF CODE ------------------------------------------------------------------