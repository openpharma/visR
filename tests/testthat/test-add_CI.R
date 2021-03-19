#' @title Specifications plot
#' @section Last updated by:
#' Tim Treis
#' @section Last update date:
#' 18-MAR-2021

# Specifications ---------------------------------------------------------------

#' T1.  The output plots after adding confidence intervals don't differ from the reviewed plots 
#' T1.1 No error when the default parameters are used
#' T1.2 No error when `alpha` is a numerical value between [0, 1]
#' T1.3 No error when `style` is `ribbon` or `step`
#' T1.4 No error when `linetype` is one of the valid ggplot choices
#' T2.  The output plots with different amount of strata don't differ from the reviewed plots 
#' T2.1 No error when only 1 strata is present
#' T2.2 No error when 2 or more strata are present
#' T3.  Warnings in case of missing data or arguments are thrown. 
#' T3.1 Error when `est.lower` and `est.upper` are not present
#' T3.2 Warning when no valid style was provided
#' T3.3 Warning when `alpha` is not in [0, 1]


# Requirement T1 ---------------------------------------------------------------

context("add_CI - T1. The output plots after adding confidence intervals doesn't differ from the reviewed plots")

testthat::test_that("T1.1 No error when the default parameters are used",{
  
  survfit_object <- adtte %>% visR::estimate_KM()
  p <- visR::plot(survfit_object = survfit_object) %>%
    visR::add_CI()
  vdiffr::expect_doppelganger("add_CI_T1_1", p)
  
})

testthat::test_that("T1.2 No error when `alpha` is a numerical value between [0, 1]",{
  
  p <- adtte %>% 
    visR::estimate_KM(strata = "SEX") %>% 
    visR::plot() 
  
  p %>%    
    visR::add_CI(alpha=0) %>%
    vdiffr::expect_doppelganger(title = "add_CI_T1_alpha_0")
  p %>%    
    visR::add_CI(alpha=0.5) %>%
    vdiffr::expect_doppelganger(title = "add_CI_T1_alpha_05")
  p %>%    
    visR::add_CI(alpha=1) %>%
    vdiffr::expect_doppelganger(title = "add_CI_T1_alpha_1")
  
})

testthat::test_that("T1.3 No error when `style` is `ribbon` or `step`",{
  
  survfit_object <- adtte %>% visR::estimate_KM(strata = "SEX")
  
  p_ribbon <- visR::plot(survfit_object = survfit_object) %>%
    visR::add_CI(style="ribbon")
  vdiffr::expect_doppelganger("add_CI_T1_3_ribbon", p_ribbon)
  
  p_step <- visR::plot(survfit_object = survfit_object) %>%
    visR::add_CI(style="step")
  vdiffr::expect_doppelganger("add_CI_T1_3_step", p_step)
  
})

testthat::test_that("T1.3 No error when `linetype` is one of the valid ggplot choices",{
  
  linetypes <- c("blank",   "solid",    "dashed", "dotted", 
                 "dotdash", "longdash", "twodash")
  
  p <- adtte %>% 
    visR::estimate_KM(strata = "SEX") %>%
    visR::plot()
    
  for (i in 1:length(linetypes)) {
    
    p %>%
      visR::add_CI(style="step", linetype=linetypes[i]) %>%
      vdiffr::expect_doppelganger(title = paste0("add_CI_T1_4_", linetypes[i]))
    p %>%
      visR::add_CI(style="step", linetype=(i-1)) %>%
      vdiffr::expect_doppelganger(title = paste0("add_CI_T1_4_", linetypes[i]))
    
  }
  
})


# Requirement T2 ---------------------------------------------------------------

context("add_CI - T2. The output plots with different amount of strata don't differ from the reviewed plots")

testthat::test_that("T2.1 No error when only 1 strata is present",{
  
  survfit_object <- adtte %>% visR::estimate_KM()
  p <- visR::plot(survfit_object = survfit_object) %>%
    visR::add_CI(alpha = 0.5)
  vdiffr::expect_doppelganger("add_CI_T2_1", p)
  
})

testthat::test_that("T2.2 No error when 2 or more strata are present",{
  
  for (n_strata in c(5, 10, 20)) {
    
   adtte %>%       
      dplyr::mutate(TRTDUR = visR:::map_numbers_to_new_range(adtte$TRTDUR, 1, n_strata)) %>%       
      visR::estimate_KM(strata = "TRTDUR") %>%      
      visR::plot() %>%
      visR::add_CI() %>%
      vdiffr::expect_doppelganger(title = paste0("add_CI_T2_2_", n_strata, "strata"))
  }
  
})

# Requirement T3 ---------------------------------------------------------------

context("add_CI - T3.  Warnings in case of missing data or weird arguments are thrown.")

testthat::test_that("T3.1 Error when `est.lower` and `est.upper` are not present",{
  
  survfit_object <- adtte %>% visR::estimate_KM(strata = "SEX")
  p <- survfit_object %>% visR::plot()
  
  are_present_before <- base::all(c("est.lower", "est.upper") %in% colnames(p$data))
  testthat::expect_equal(are_present_before, TRUE)
  testthat::expect_error(p %>% visR::add_CI(), NA)
  
  p$data = p$data %>% dplyr::select(-c(est.lower, est.upper))
  are_present_after <- base::all(c("est.lower", "est.upper") %in% colnames(p$data))
  testthat::expect_equal(are_present_after, FALSE)
  testthat::expect_error(p %>% visR::add_CI())
  
})

testthat::test_that("T3.2 Warning when no valid style was provided",{
  
  survfit_object <- adtte %>% visR::estimate_KM(strata = "SEX")
  p <- survfit_object %>% visR::plot()
  
  warning_message <- "Invalid `style` argument. Setting `style` to `ribbon`."
  testthat::expect_warning(p %>% visR::add_CI(style = "visR"), warning_message)
  
})

testthat::test_that("T3.3 Warning when `alpha` is not in [0, 1]",{
  
  p <- adtte %>% 
    visR::estimate_KM(strata = "SEX") %>% 
    visR::plot()
  
  warning_message <- "Invalid `alpha` argument, must be between 0 and 1. Setting it to 0.1."
  testthat::expect_warning(p %>% visR::add_CI(alpha = 5), warning_message)
  testthat::expect_warning(p %>% visR::add_CI(alpha = -5), warning_message)
  
})

# END OF CODE ------------------------------------------------------------------