#' @title Specifications utils_visr
#' @section Last updated by: Tim Treis
#' @section Last update date: 30-JULY-2021

# Specifications ---------------------------------------------------------------

#' T1. `align_plots()` accepts a list of `ggplot` objects.
#' T1.1 No error when a list of `ggplot` objects is passed.
#' T1.2 An error when nothing or `NULL` is passed.
#' T1.3 An error when a list containing non-`ggplot` objects is passed.
#' T2. `align_plots()` aligns multiple `ggplot` objects, taking the legend into account.
#' T2.1 No error when a list of ggplots is passed.
#' T3. The function `legendopts()` translates the input to a `ggplot2`-compatible list.
#' T3.1 No error when no arguments are specified.
#' T3.2 A list is returned when no arguments are specified.
#' T3.3 No error when `legend_position` is 'bottom', 'right', 'top', 'left' or 'none'.
#' T3.4 When `legend_position` is 'bottom' and `legend_orientation` is `NULL`, the parameters in the returned object anchor it bottom-centered.
#' T3.5 When `legend_position` is 'right' and `legend_orientation` is `NULL`, the parameters in the returned object anchor it right-centered.
#' T3.6 When `legend_position` is 'top' and `legend_orientation` is `NULL`, the parameters in the returned object anchor it top-centered.
#' T3.7 When `legend_position` is 'left' and `legend_orientation` is `NULL`, the parameters in the returned object anchor it left-centered.
#' T3.8 When `legend_position` is 'none', the parameters in the returned object disable the legend.
#' T3.9 When `legend_orientation` is not `NULL`, it is used as `$leg_opts$orientation` in the returned object.
#' T3.10 When `legend_position` is not a character, but a vector with at least length 2, the first two elements are used as x/y positions for the legend.
#' T3.11 A warning when `legend_position` is a vector with a length greater than 2.
#' T3.12 An error when `legend_position` is not a `character` or a vector with a length of at least 2.


# Requirement T1 ---------------------------------------------------------------

testthat::context("utils_visr - T1. `align_plots()` accepts a list of `ggplot` objects.")

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

testthat::context("utils_visr - T2. `align_plots()` aligns multiple `ggplot` objects, taking the legend into account.")

testthat::test_that("T2.1 No error when a list of ggplots is passed.", {
  
  gg_sex <- adtte %>%
    visR::estimate_KM("SEX") %>%
    visR::visr() 
  
  gg_trtp <- adtte %>%
    visR::estimate_KM("TRTP") %>%
    visR::visr()
  
  # Legend widths are unequal
  testthat::expect_true(check_grob_width_equal(gg_sex, gg_trtp) != 0)
  
  gg_list <- visR::align_plots(list(gg_sex, gg_trtp))
  
  testthat::skip_on_cran()
  cowplot::plot_grid(plotlist = gg_list, align = "none", nrow = 2) %>%
    vdiffr::expect_doppelganger(title = "utils_visr_T2_1_aligns_legends_of_plots")  
  
})

# Requirement T2 ---------------------------------------------------------------

testthat::context("utils_visr - T3. The function `legendopts()` translates the input to a `ggplot2`-compatible list.")

testthat::test_that("T3.1 No error when no arguments are specified.", {
  
  testthat::expect_error(visR:::legendopts(), NA)
  
})

testthat::test_that("T3.2 A list is returned when no arguments are specified.", {
  
  opts <- visR:::legendopts()
  
  testthat::expect_true("list" %in% class(opts))
  
})

testthat::test_that("T3.3 No error when `legend_position` is 'bottom', 'right', 'top', 'left' or 'none'.", {
  
  for (pos in c("bottom", "right", "top", "left", "none")) {
    
    testthat::expect_error(visR:::legendopts(legend_position = pos), NA)
    
  }
  
})

testthat::test_that("T3.4 When `legend_position` is 'bottom' and `legend_orientation` is `NULL`, the parameters in the returned object anchor it bottom-centered.", {
  
  legend <- visR:::legendopts(legend_position = "bottom", legend_orientation = NULL)
  
  testthat::expect_true(legend$leg_opts$xanchor == "center")
  testthat::expect_true(legend$leg_opts$x == 0.5)
  testthat::expect_true(legend$leg_opts$y == -0.2)
  testthat::expect_true(legend$leg_opts$orientation == "h")
  testthat::expect_true(legend$showlegend == TRUE)
  
})

testthat::test_that("T3.5 When `legend_position` is 'right' and `legend_orientation` is `NULL`, the parameters in the returned object anchor it right-centered.", {
  
  legend <- visR:::legendopts(legend_position = "right", legend_orientation = NULL)
  
  testthat::expect_true(legend$leg_opts$yanchor == "center")
  testthat::expect_true(legend$leg_opts$x == 1.2)
  testthat::expect_true(legend$leg_opts$y == 0.5)
  testthat::expect_true(legend$leg_opts$orientation == "v")
  testthat::expect_true(legend$showlegend == TRUE)
  
})

testthat::test_that("T3.6 When `legend_position` is 'top' and `legend_orientation` is `NULL`, the parameters in the returned object anchor it top-centered.", {
  
  legend <- visR:::legendopts(legend_position = "top", legend_orientation = NULL)
  
  testthat::expect_true(legend$leg_opts$xanchor == "center")
  testthat::expect_true(legend$leg_opts$x == 0.5)
  testthat::expect_true(legend$leg_opts$y == 1.2)
  testthat::expect_true(legend$leg_opts$orientation == "h")
  testthat::expect_true(legend$showlegend == TRUE)
  
})

testthat::test_that("T3.7 When `legend_position` is 'left' and `legend_orientation` is `NULL`, the parameters in the returned object anchor it left-centered.", {
  
  legend <- visR:::legendopts(legend_position = "left", legend_orientation = NULL)
  
  testthat::expect_true(legend$leg_opts$yanchor == "center")
  testthat::expect_true(legend$leg_opts$x == -1.0)
  testthat::expect_true(legend$leg_opts$y == 0.5)
  testthat::expect_true(legend$leg_opts$orientation == "v")
  testthat::expect_true(legend$showlegend == TRUE)
  
})

testthat::test_that("T3.8 When `legend_position` is 'none', the parameters in the returned object disable the legend.", {
  
  legend <- visR:::legendopts(legend_position = "none", legend_orientation = NULL)
  
  testthat::expect_true(is.null(legend$leg_opts))
  testthat::expect_false(legend$showlegend)
  
})

testthat::test_that("T3.9 When `legend_orientation` is not `NULL`, it is used as `$leg_opts$orientation` in the returned object.", {
  
  for (orientation in c(1, "visR", c(1, 2, 3))) {
    
    tmp <- visR:::legendopts(legend_orientation = orientation)
    
    testthat::expect_equal(orientation, tmp$leg_opts$orientation)
    
  }
  
})

testthat::test_that("T3.10 When `legend_position` is not a character, but a vector with at least length 2, the first two elements are used as x/y positions for the legend.", {
  
  suppressWarnings(legend <- visR:::legendopts(legend_position = c(1, 2, 3)))
  
  testthat::expect_true(legend$leg_opts$x == 1)
  testthat::expect_true(legend$leg_opts$y == 2)
  
})

testthat::test_that("T3.11 A warning when `legend_position` is a vector with a length greater than 2.", {
  
  expected_warning <- "The provided vector for the legend position contains more than 2 elements, only using the first two."
  visR:::legendopts(legend_position = c(1, 2, 3)) %>% testthat::expect_warning(expected_warning)
  
})

testthat::test_that("T3.12 An error when `legend_position` is not a `character` or a vector with a length of at least 2.", {
  
  visR:::legendopts(legend_position = 1) %>% testthat::expect_error()
  visR:::legendopts(legend_position = c(1)) %>% testthat::expect_error()
  visR:::legendopts(legend_position = c()) %>% testthat::expect_error()
  
})


# END --------------------------------------------------------------------------