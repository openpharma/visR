#' @title Specifications plot
#' @section Last updated by:
#' Tim Treis
#' @section Last update date:
#' 04-MAR-2021

# Specifications ---------------------------------------------------------------

library(testthat)
library(visR)
library(vdiffr)
library(survival)

map_numbers_to_new_range <- function(numbers, lower, upper) {
  
  # Used to artificially generated a set amount of strata for testing
  # https://stackoverflow.com/a/18303620/10169453
  
  # Shifting the vector so that min(x) == 0
  numbers <- numbers - min(numbers)
  # Scaling to the range of [0, 1]
  numbers <- numbers / max(numbers)
  # Scaling to the needed amplitude
  numbers <- numbers * (upper - lower)
  # Shifting to the needed level
  return(as.factor(round(numbers + lower, 0)))
  
}

#' T1.  The output plots after adding confidence intervals don't differ from the reviewed plots 
#' T1.1 No error when the default parameters are used
#' T1.2 No error when `alpha` is specified
#' T1.3 No error when `style` is specified
#' T1.4 No error when `linetype` is specified
#' T2.  The output plots with different amount of strata don't differ from the reviewed plots 
#' T2.1 No error when only 1 strata is present
#' T2.2 No error when 2 or more strata are present


# Requirement T1 ---------------------------------------------------------------

context("plot - T1. The output plots after adding confidence intervals doesn't differ from the reviewed plots")

testthat::test_that("T1.1 No error when the default parameters are used",{
  
  survfit_object <- adtte %>% visR::estimate_KM()
  p <- visR::plot(survfit_object = survfit_object) %>%
    visR::add_CI()
  vdiffr::expect_doppelganger("add_CI_T1_1", p)
  
})

testthat::test_that("T1.2 No error when `alpha` is specified",{
  
  survfit_object <- adtte %>% visR::estimate_KM(strata = "SEX")
  p <- visR::plot(survfit_object = survfit_object) %>%
    visR::add_CI(alpha=0.5)
  vdiffr::expect_doppelganger("add_CI_T1_2", p)
  
})

testthat::test_that("T1.3 No error when `style` is specified",{
  
  survfit_object <- adtte %>% visR::estimate_KM(strata = "SEX")
  
  p_ribbon <- visR::plot(survfit_object = survfit_object) %>%
    visR::add_CI(style="ribbon")
  vdiffr::expect_doppelganger("add_CI_T1_3_ribbon", p_ribbon)
  
  p_step <- visR::plot(survfit_object = survfit_object) %>%
    visR::add_CI(style="step")
  vdiffr::expect_doppelganger("add_CI_T1_3_step", p_step)
  
})

testthat::test_that("T1.3 No error when `linetype` is specified",{
  
  survfit_object <- adtte %>% visR::estimate_KM(strata = "SEX")
  
  p_blank <- visR::plot(survfit_object = survfit_object) %>%
    visR::add_CI(style="step", linetype="blank")
  vdiffr::expect_doppelganger("add_CI_T1_4_blank", p_blank)
  
  p_solid <- visR::plot(survfit_object = survfit_object) %>%
    visR::add_CI(style="step", linetype="solid")
  vdiffr::expect_doppelganger("add_CI_T1_4_solid", p_solid)
  
  p_dashed <- visR::plot(survfit_object = survfit_object) %>%
    visR::add_CI(style="step", linetype="dashed")
  vdiffr::expect_doppelganger("add_CI_T1_4_dashed", p_dashed)
  
  p_dotted <- visR::plot(survfit_object = survfit_object) %>%
    visR::add_CI(style="step", linetype="dotted")
  vdiffr::expect_doppelganger("add_CI_T1_4_dotted", p_dotted)
  
  p_dotdash <- visR::plot(survfit_object = survfit_object) %>%
    visR::add_CI(style="step", linetype="dotdash")
  vdiffr::expect_doppelganger("add_CI_T1_4_dotdash", p_dotdash)
  
  p_longdash <- visR::plot(survfit_object = survfit_object) %>%
    visR::add_CI(style="step", linetype="longdash")
  vdiffr::expect_doppelganger("add_CI_T1_4_longdash", p_longdash)
  
  p_twodash <- visR::plot(survfit_object = survfit_object) %>%
    visR::add_CI(style="step", linetype="twodash")
  vdiffr::expect_doppelganger("add_CI_T1_4_twodash", p_twodash)
  
})


# Requirement T2 ---------------------------------------------------------------

context("plot - T2. The output plots with different amount of strata don't differ from the reviewed plots")

testthat::test_that("T2.1 No error when only 1 strata is present",{
  
  survfit_object <- adtte %>% visR::estimate_KM()
  p <- visR::plot(survfit_object = survfit_object) %>%
    visR::add_CI(alpha = 0.5)
  vdiffr::expect_doppelganger("add_CI_T2_1", p)
  
})

testthat::test_that("T2.2 No error when 2 or more strata are present",{
  
  # The used color palette is NEJM from ggsci. While its generator has a the
  # option to supply parameters, there are only 8 colours (2021-03-04)
  # https://nanx.me/ggsci/reference/pal_nejm.html
  # https://github.com/openpharma/visR/blob/2087cafbd355382402f1ce28977bfaf1e1c4e254/R/plot.R#L187
  distinct_colors_in_palette <- length(ggsci::pal_nejm()(8))
  
  # Dynamically test all n_strata our color palette allows
  for (n_strata in 2:distinct_colors_in_palette) {
    
    adtte %>% 
      dplyr::mutate(TRTDUR = map_numbers_to_new_range(adtte$TRTDUR, 1, n_strata)) %>% 
      visR::estimate_KM(strata = "TRTDUR") %>%
      visR::plot() %>%
      visR::add_CI() %>%
      vdiffr::expect_doppelganger(title = paste0("add_CI_T2_2_", n_strata, "strata"))
    
  }
  
})

# END OF CODE ------------------------------------------------------------------