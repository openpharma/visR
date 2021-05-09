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
#' T3. The output object is ggplot with additional class `ggsurvfit` and an attribute `components`
#' T3.1 The output object has an additional attribute `components`
#' T3.2 The attribute components[['visR_plot']] contains the plot used as input
#' T3.3 The attribute components contains the risktables, identified through the risktable titles
#' T3.4 The output has class `ggsurvfit`


# Requirement T1 ----------------------------------------------------------

context("add_risktable.survfit - T1. The function accepts a `ggsurvfit` object")

testthat::test_that("T1.1 No error when a `ggsurvfit` object is passed to the function",{

  visR_plot <- visR::estimate_KM(data = adtte, strata = "TRTA") %>% plot()
  
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
  visR_plot <- visR::estimate_KM(data = adtte, strata = "TRTA") %>% plot() %>% visR::add_risktable()
  
  plot_risk <- visR_plot$components$Placebo$data

  testthat::expect_equal(visR_risk, plot_risk, check.attributes = FALSE)
  
})

# testthat::test_that("T2.2 The risktables are placed below the visR plot, in alignment with the x-axis of a visR plot without legend",{
# 
#   visR_plot <- visR::estimate_KM(data = adtte, strata = "TRTA") %>%
#     plot(legend_position = "none") %>%
#     visR::add_risktable()
#  
#   vdiffr::expect_doppelganger(title = "add_risktable_T2_1", visR_plot) 
#   vdiffr::manage_cases()
# })
# 
# testthat::test_that("T2.3 The risktables are placed below the visR plot, in alignment with the x-axis of a visR plot with legend",{
# 
#   visR_plot <- visR::estimate_KM(data = adtte, strata = "TRTA") %>%
#     plot(legend_position = "right") %>%
#     visR::add_risktable()
#  
#   vdiffr::expect_doppelganger(title = "add_risktable_T2_2", visR_plot) 
#   vdiffr::manage_cases()
#   
# })

# Requirement T3 ----------------------------------------------------------

context("add_risktable.survfit - The output object is ggplot with additional class `ggsurvfit` and attribute `components`")

testthat::test_that("T3.1 The output object has an additional attribute `components`",{

  visR_plot <- visR::estimate_KM(data = adtte, strata = "TRTA") %>% 
    plot() %>%
    visR::add_risktable()
  
  testthat::expect_true("components" %in% names(visR_plot))
  
})

testthat::test_that("T3.2 The attribute components[['visR_plot']] contains the plot used as input",{

  visR_plot_base <- visR::estimate_KM(data = adtte, strata = "TRTA") %>% 
    plot()
    
  visR_plot <- visR_plot_base %>%
    add_risktable()
  
  testthat::expect_equal(visR_plot_base, visR_plot$components$visR_plot)
  
})

testthat::test_that("T3.3 The attribute components contains the risktables, identified through the risktable titles",{

  visR_plot_base <- visR::estimate_KM(data = adtte, strata = "TRTA") %>% 
    plot()
    
  visR_plot <- visR_plot_base %>%
    visR::add_risktable()
  
  risktable1 <- get_risktable( visR::estimate_KM(data = adtte, strata = "TRTA"))
  
  testthat::expect_equal(visR_plot$components$Placebo$data, risktable1, check.attributes = FALSE)
  
  risknames <- names(visR_plot$components)[2:4]
  testthat::expect_equal(risknames, c("Placebo", "Xanomeline High Dose", "Xanomeline Low Dose"))
  
})

testthat::test_that("T3.4 The output has class `ggsurvfit`",{

  visR_plot_base <- visR::estimate_KM(data = adtte, strata = "TRTA") %>% 
    plot()
    
  visR_plot <- visR_plot_base %>%
    visR::add_risktable()
  
  testthat::expect_true(inherits(visR_plot, "ggsurvfit"))
  
})

# END OF CODE ----------------------------------------------------------

