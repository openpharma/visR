#' @title Specifications test-add_risktable.R
#' @section Last updated by: ardeeshany (ardeeshany@@gmail.com)
#' @section Last update date: 2022-05-01T15:52:08
#'
#' @section List of tested specifications
#' T1. The function accepts a `ggsurvfit` and `ggtidycuminc` objects.
#' T1.1 No error when a `ggsurvfit` and `ggtidycuminc` objects is passed to the function.
#' T1.2 An error when a non-`ggsurvfit` object is passed to the function.
#' T2. The risktables are `ggplot` representations of the actual risktables.
#' T2.1 When no strata were specified, an artificial strata is displayed 'Overall'.
#' T2.2 The calculated values in the risktable are not affected by the transformation to a `ggplot`.
#' T2.3 The risktables are placed below the visR plot, in alignment with the x-axis of a visR plot without legend.
#' T2.4 The risktables are placed below the visR plot, in alignment with the x-axis of a visR plot without legend.
#' T3. The output object is ggplot with additional class `ggsurvfit` and attribute `components`.
#' T3.1 The output object has an additional attribute `components`.
#' T3.2 The attribute components[['visR_plot']] contains the plot used as input.
#' T3.3 The attribute components contains the risktables, identified through the risktable titles.
#' T3.4 The output has class `ggsurvfit`.
#' T4. The function accepts a numeric rowgutter value
#' T4.1 An error when the rowgutter is not numeric
#' T4.2 An error when the rowgutter is negative
#' T4.3 An error when the rowgutter is larger than 1
#' T4.4 No error when the rowgutter is numeric between 0 and 1
#' T4.5 No error when the default rowgutter is used
#' T4.6 Changing rowgutter affects on the heights of the table and plot

# Requirement T1 ----------------------------------------------------------

testthat::context("add_risktable.survfit - T1. The function accepts a `ggsurvfit` object.")

testthat::test_that("T1.1 No error when a `ggsurvfit` object is passed to the function.", {
  visR_plot <- visR::estimate_KM(data = adtte, strata = "TRTA") %>% visR::visr()
  testthat::expect_error(visR::add_risktable(visR_plot), NA)

  visR_plot <-
    visR::estimate_cuminc(tidycmprsk::trial, AVAL = "ttdeath", CNSR = "death_cr") %>%
    visR::visr(x_units = "Months")
  testthat::expect_error(visR::add_risktable(visR_plot), NA)
})

testthat::test_that("T1.2 An error when a non-`ggsurvfit` object is passed to the function.", {
  survfit_object <- visR::estimate_KM(data = adtte, strata = "TRTA")

  testthat::expect_error(visR::add_risktable(survfit_object))
})

# Requirement T2 ---------------------------------------------------------------

testthat::context("add_risktable.survfit - T2. The risktables are `ggplot` representations of the actual risktables.")

testthat::test_that("T2.1 When no strata were specified, an artificial strata is displayed 'Overall'.", {
  visR_plot <- adtte %>%
    visR::estimate_KM() %>%
    visR::visr() %>%
    visR::add_risktable()
  strata <- base::intersect("Overall", names(visR_plot$components))

  testthat::expect_error(strata == "Overall", NA)
})

testthat::test_that("T2.2 The calculated values in the risktable are not affected by the transformation to a `ggplot`.", {
  visR_risk <- adtte %>%
    visR::estimate_KM(strata = "TRTA") %>%
    visR::get_risktable()
  visR_plot <- adtte %>%
    visR::estimate_KM(strata = "TRTA") %>%
    visR::visr() %>%
    visR::add_risktable()

  plot_risk <- visR_plot$components$Placebo$data

  testthat::expect_equal(visR_risk, plot_risk, check.attributes = FALSE)
})

testthat::test_that("T2.3 The risktables are placed below the visR plot, in alignment with the x-axis of a visR plot without legend.", {
  visR_plot <- adtte %>%
    visR::estimate_KM(strata = "TRTA") %>%
    visR::visr() +
    ggplot2::theme(legend.position = "none")

  testthat::skip_on_cran()
  visR_plot %>%
    visR::add_risktable() %>%
    vdiffr::expect_doppelganger(title = "add_risktable_T2_3_aligned_when_no_legend")
})

testthat::test_that("T2.4 The risktables are placed below the visR plot, in alignment with the x-axis of a visR plot without legend.", {
  visR_plot <- adtte %>%
    visR::estimate_KM(strata = "TRTA") %>%
    visR::visr() %>%
    visR::add_risktable()

  testthat::skip_on_cran()
  visR_plot %>%
    vdiffr::expect_doppelganger(title = "add_risktable_T2_4_aligned_when_legend")
})

# Requirement T3 ---------------------------------------------------------------

testthat::context("add_risktable.survfit - T3. The output object is ggplot with additional class `ggsurvfit` and attribute `components`.")

testthat::test_that("T3.1 The output object has an additional attribute `components`.", {
  visR_plot <- adtte %>%
    visR::estimate_KM(strata = "TRTA") %>%
    visR::visr() %>%
    visR::add_risktable()

  testthat::expect_true("components" %in% names(visR_plot))
})

testthat::test_that("T3.2 The attribute components[['visR_plot']] contains the plot used as input.", {
  visR_plot_base <- adtte %>%
    visR::estimate_KM(strata = "TRTA") %>%
    visR::visr()

  visR_plot <- visR_plot_base %>%
    visR::add_risktable()

  testthat::expect_equal(visR_plot_base, visR_plot$components$visR_plot)
})

testthat::test_that("T3.3 The attribute components contains the risktables, identified through the risktable titles.", {
  visR_plot_base <- adtte %>%
    visR::estimate_KM(strata = "TRTA") %>%
    visR::visr()

  visR_plot <- visR_plot_base %>%
    visR::add_risktable()

  risktable1 <- adtte %>%
    visR::estimate_KM(strata = "TRTA") %>%
    visR::get_risktable()

  testthat::expect_equal(visR_plot$components$Placebo$data,
    risktable1,
    check.attributes = FALSE
  )

  risknames <- names(visR_plot$components)[2:4]
  testthat::expect_equal(risknames, c(
    "Placebo",
    "Xanomeline High Dose",
    "Xanomeline Low Dose"
  ))
})

testthat::test_that("T3.4 The output has class `ggsurvfit`.", {
  visR_plot_base <- adtte %>%
    visR::estimate_KM(strata = "TRTA") %>%
    visR::visr()

  visR_plot <- visR_plot_base %>%
    visR::add_risktable()

  testthat::expect_true(inherits(visR_plot, "ggsurvfit"))
})


# Requirement T4 ---------------------------------------------------------------

testthat::context("add_risktable.survfit - T4. The function accepts a numeric rowgutter value")

testthat::test_that("T4.1 An error when the rowgutter is not numeric", {
  visR_plot <- adtte %>%
    visR::estimate_KM(strata = "TRTA") %>%
    visr()

  testthat::expect_error(visR::add_risktable(visR_plot, rowgutter = "blah"))
})

testthat::test_that("T4.2 An error when the rowgutter is negative", {
  visR_plot <- adtte %>%
    visR::estimate_KM(strata = "TRTA") %>%
    visr()

  testthat::expect_error(visR::add_risktable(visR_plot, rowgutter = -0.001))
})

testthat::test_that("T4.3 An error when the rowgutter is larger than 1", {
  visR_plot <- adtte %>%
    visR::estimate_KM(strata = "TRTA") %>%
    visr()

  testthat::expect_error(visR::add_risktable(visR_plot, rowgutter = 1.001))
})

testthat::test_that("T4.4 No error when the rowgutter is numeric between 0 and 1", {
  visR_plot <- adtte %>%
    visR::estimate_KM(strata = "TRTA") %>%
    visr()

  testthat::expect_error(visR::add_risktable(visR_plot, rowgutter = 0.2), NA)
})



testthat::test_that("T4.5 No error when the default rowgutter is used", {
  visR_plot <- adtte %>%
    visR::estimate_KM(strata = "TRTA") %>%
    visr()

  testthat::expect_error(visR::add_risktable(visR_plot), NA)

  testthat::skip_on_cran()
  visR::add_risktable(visR_plot) %>%
    vdiffr::expect_doppelganger(title = "T4.No_error_when_the_default_rowgutter_is_used")
})

testthat::test_that("T4.6 Changing rowgutter affects on the heights of the table and plot", {
  visR_plot <- adtte %>%
    visR::estimate_KM(strata = "TRTA") %>%
    visr()

  testthat::skip_on_cran()
  visR::add_risktable(visR_plot, rowgutter = 0.3) %>%
    vdiffr::expect_doppelganger(title = "T4.6_Changing_rowgutter_affects_on_the_heights_of_the_table_and_plot")
})

# END OF CODE -------------------------------------------------------------
