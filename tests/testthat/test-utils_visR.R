#' @title Specifications utils_visr
#' @section Last updated by: Tim Treis
#' @section Last update date: 25-JULY-2021

# Specifications ---------------------------------------------------------------

#' T1. Correct values for summarize_long.numeric
#' T1.1 Correct mean values for numeric values

# Requirement T1 ---------------------------------------------------------------

context("utils_visr - T1. `align_plots()` accepts a list of `ggplot` objects.")

testthat::test_that("T1.1 No error when a list of `ggplot` objects is passed.", {
  
  gg_sex <- adtte %>%
    visR::estimate_KM("SEX") %>%
    visR::visr() 
  
  gg_trtp <- adtte %>%
    visR::estimate_KM("TRTP") %>%
    visR::visr()
  
  gg_list = list(gg_sex, gg_trtp)
  
  testthat::expect_error(visR::align_plots(gg_list), NA)
  
})

testthat::test_that("T1.2 An error when nothing or `NULL` is passed.", {
  
  testthat::expect_error(visR::align_plots())
  testthat::expect_error(visR::align_plots(pltlist = NULL))
  
})

testthat::test_that("T1.3 An error when a list containing non-`ggplot` objects is passed.", {
  
  gg_sex <- adtte %>%
    visR::estimate_KM("SEX") %>%
    visR::visr() 
  
  gg_trtp <- adtte %>%
    visR::estimate_KM("TRTP") %>%
    visR::visr()
  
  gg_list = list(gg_sex, gg_trtp, 3)
  
  testthat::expect_error(visR::align_plots(gg_list))
  
})

# Requirement T2 ---------------------------------------------------------------

context("utils_visr - T2. `align_plots()` aligns multiple `ggplot` objects, taking the legend into account.")

testthat::test_that("T2.1 No error when a list of ggplots is passed.", {
  
  gg_sex <- adtte %>%
    visR::estimate_KM("SEX") %>%
    visR::visr() 
  
  gg_trtp <- adtte %>%
    visR::estimate_KM("TRTP") %>%
    visR::visr()
  
  # Legend widths are inequal
  testthat::expect_true(check_grob_width_equal(gg_sex, gg_trtp) != 0)
  
  gg_list <- visR::align_plots(list(gg_sex, gg_trtp))
  
  testthat::skip_on_cran()
  cowplot::plot_grid(plotlist = gg_list, align = "none", nrow = 2) %>%
    vdiffr::expect_doppelganger(title = "utils_visr_T2_1_aligns_legends_of_plots")  
  
})

# END --------------------------------------------------------------------------