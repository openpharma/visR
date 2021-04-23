#' @title Specifications add_risktable.survfit
#' @section Last updated by:
#' Steven Haesendonckx
#' @section Last update date:
#' 23-APR-2021

# Specifications ----------------------------------------------------------

#' T1. The function accepts a `ggsurvfit` object
#' T1.1 No error when a `ggsurvfit` object is passed to the function
#' T1.2 An error when a non-`ggsurvfit` object is passed to the function
#' T2. The risktables are `ggplot` representations of the actual risktables
#' T2.1 The calculated values in the risktable are not affected by the transformation to a `ggplot`
#' T2.2 The risktables are placed below the visR plot, in alignment with the x-axis of a visR plot without legend
#' T2.3 The risktables are placed below the visR plot, in alignment with the x-axis of a visR plot with a legend
#' T3. The output object is ggplot with additional class `ggsurvfit`
#' T3.1 The output object has an additional attribute `components`
#' T3.1 The output object's attribute `components` contains the original visR plot and the risktables
 

# Requirement T1 ----------------------------------------------------------

context("add_risktable.survfit - T1. The function accepts a `ggsurvfit` object")

testthat::test_that("T1.1 No error when a `ggsurvfit` object is passed to the function",{

  visR_plot <- visR::estimate_KM(data = adtte, strata = "TRTA") %>% visR::plot()
  
  testthat::expect_error(visR::add_risktable(visR_plot), NA)

})

testthat::test_that("T1.2 An error when a non-`ggsurvfit` object is passed to the function",{

  survfit_object <- visR::estimate_KM(data = adtte, strata = "TRTA")
  
  testthat::expect_error(visR::add_risktable(survfit_object))

})

# Requirement T2 ----------------------------------------------------------

context("add_risktable.survfit - T2. The risktables are `ggplot` representations of the actual risktables")

testthat::test_that("T2.1 The calculated values in the risktable are not affected by the transformation to a `ggplot`",{

  visR_risk <- visR::estimate_KM(data = adtte, strata = "TRTA") %>% visR::get_risktable()
  visR_plot <- visR::estimate_KM(data = adtte, strata = "TRTA") %>% visR::plot() %>% visR::add_risktable()
  
  plot_risk <- visR_plot$components$Placebo$data

  testthat::expect_equal(visR_risk, plot_risk, check.attributes = FALSE)
  
})

#' testthat::test_that("T2.2 The risktables are placed below the visR plot, in alignment with the x-axis of a visR plot without legend",{
#' 
#'   visR_plot <- visR::estimate_KM(data = adtte, strata = "TRTA") %>%
#'     visR::plot(legend_position = "none") %>%
#'     visR::add_risktable()
#' manage_cases()
#' 
#'   vdiffr::expect_doppelganger("T2_2", visR_plot)  
#' })
#' 
#' 
#' 
#' #' 
#' #' T2.3 The risktables are placed below the visR plot, in alignment with the x-axis of a visR plot with a legend
#' #' T3. The output object is ggplot with additional class `ggsurvfit`
#' #' T3.1 The output object has an additional attribute `components`
#' #' T3.1 The output object's attribute `components` contains the original visR plot and the risktables
#'  
#' 
#' # END OF CODE ----------------------------------------------------------
#' 
