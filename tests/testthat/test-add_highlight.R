#' @title Specifications add_highlight
#' @section Last updated by: Tim Treis
#' @section Last update date: 06-Auust-2021

# Specifications ---------------------------------------------------------------

#' T1. No errors when confidence intervals are added to the plots. 

# Requirement T1 ---------------------------------------------------------------

context("add_highlight - T1. No errors when one or more strata are highlighted.")

testthat::test_that("T1.1 No error when `strata` is a character string found in the plot strata.",{
  
  gg <- adtte %>%
    visR::estimate_KM(strata = "TRTP") %>%
    visR::visr()
  
  gg %>%
    visR::add_highlight(strata = "TRTP=Placebo") %>%
    testthat::expect_error(NA)
  
  testthat::skip_on_cran()
  gg %>% 
    visR::add_highlight(strata = "TRTP=Placebo") %>%
    vdiffr::expect_doppelganger(title = "add_highlight_T1_1_no_error_when_strata_is_a_character_string_found_in_the_plot_strata")  
  
})

testthat::test_that("T1.2 No error when `strata` is a `list` or `vector` of character strings found in the plot strata.",{
  
  strata_list <- list("TRTP=Placebo", "TRTP=Xanomeline Low Dose")
  strata_vector <- c("TRTP=Placebo", "TRTP=Xanomeline Low Dose")
  
  gg <- adtte %>%
    visR::estimate_KM(strata = "TRTP") %>%
    visR::visr()
  
  gg %>%
    visR::add_highlight(strata = strata_list) %>%
    testthat::expect_error(NA)
  
  gg %>%
    visR::add_highlight(strata = strata_vector) %>%
    testthat::expect_error(NA)
  
  testthat::skip_on_cran()
  
  gg %>% 
    visR::add_highlight(strata = strata_list) %>%
    vdiffr::expect_doppelganger(title = "add_highlight_T1_2_no_error_when_strata_is_a_list_of_character_strings_found_in_the_plot_strata") 
  
  gg %>% 
    visR::add_highlight(strata = strata_vector) %>%
    vdiffr::expect_doppelganger(title = "add_highlight_T1_2_no_error_when_strata_is_a_vector_of_character_strings_found_in_the_plot_strata")  
  
})

# END OF CODE ------------------------------------------------------------------
