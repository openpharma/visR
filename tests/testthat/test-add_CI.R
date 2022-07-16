#' @title Specifications test-add_CI.R
#' @section Last updated by: Tim Treis (tim.treis@@outlook.de)
#' @section Last update date: 2022-02-09T15:22:32
#'
#' @section List of tested specifications
#' T1. No errors when confidence intervals are added to the plots.
#' T1.1 No error when the default parameters are used
#' T1.2 No error when `alpha` is a numerical value between [0, 1].
#' T1.3 No error when `style` is `ribbon` or `step`.
#' T1.4 No error when `linetype` is one of the valid ggplot choices.
#' T2. No errors when different amount of strata are used.
#' T2.1 No error when only 1 strata is present.
#' T2.2 No error when 2 or more strata are present
#' T3.  Warnings in case of missing data or unexpected arguments are thrown.
#' T3.1 Error when `est.lower` and `est.upper` are not present.
#' T3.2 Warning when no valid style was provided.
#' T3.3 Warning when `alpha` is not in [0, 1].
#' T3.4 Warning when `style` is `ribbon` but a `linetype` was specified.

# Requirement T1 ----------------------------------------------------------

context("add_CI - T1. No errors when confidence intervals are added to the plots.")

test_that("T1.1 No error when the default parameters are used", {
  survfit_object <- adtte %>% visR::estimate_KM()
  p <- visR::visr(survfit_object)

  expect_error(p %>% visR::add_CI(), NA)

  skip_on_cran()
  p %>%
    visR::add_CI() %>%
    vdiffr::expect_doppelganger(title = "add_CI_T1_1_no_error_when_default_parameters_are_used")
})

test_that("T1.2 No error when `alpha` is a numerical value between [0, 1].", {
  p <- adtte %>%
    visR::estimate_KM(strata = "SEX") %>%
    visR::visr()

  expect_error(p %>% visR::add_CI(alpha = 0), NA)
  expect_error(p %>% visR::add_CI(alpha = 0.5), NA)
  expect_error(p %>% visR::add_CI(alpha = 1), NA)

  skip_on_cran()
  p %>%
    visR::add_CI(alpha = 0) %>%
    vdiffr::expect_doppelganger(title = "add_CI_T1_alpha_0")
  p %>%
    visR::add_CI(alpha = 0.5) %>%
    vdiffr::expect_doppelganger(title = "add_CI_T1_alpha_05")
  p %>%
    visR::add_CI(alpha = 1) %>%
    vdiffr::expect_doppelganger(title = "add_CI_T1_alpha_1")
})

test_that("T1.3 No error when `style` is `ribbon` or `step`.", {
  p <- adtte %>%
    visR::estimate_KM(strata = "SEX") %>%
    visR::visr()

  expect_error(p %>% visR::add_CI(style = "ribbon"), NA)
  expect_error(p %>% visR::add_CI(style = "step"), NA)

  skip_on_cran()
  p %>%
    visR::add_CI(style = "ribbon") %>%
    vdiffr::expect_doppelganger(title = "add_CI_T1_3_style_ribbon")
  p %>%
    visR::add_CI(style = "step") %>%
    vdiffr::expect_doppelganger(title = "add_CI_T1_3_style_step")
})

test_that("T1.4 No error when `linetype` is one of the valid ggplot choices.", {
  linetypes <- c(
    "blank", "solid", "dashed", "dotted",
    "dotdash", "longdash", "twodash"
  )

  p <- adtte %>%
    visR::estimate_KM(strata = "SEX") %>%
    visR::visr()

  for (i in 1:length(linetypes)) {
    expect_error(p %>% visR::add_CI(
      style = "step",
      linetype = linetypes[i]
    ), NA)
    expect_error(p %>% visR::add_CI(
      style = "step",
      linetype = (i - 1)
    ), NA)
  }

  skip_on_cran()
  for (i in 1:length(linetypes)) {
    p %>%
      visR::add_CI(style = "step", linetype = linetypes[i]) %>%
      vdiffr::expect_doppelganger(title = paste0(
        "add_CI_T1_4_linetype_",
        linetypes[i]
      ))
    p %>%
      visR::add_CI(style = "step", linetype = (i - 1)) %>%
      vdiffr::expect_doppelganger(title = paste0(
        "add_CI_T1_4_linetype_",
        (i - 1)
      ))
  }
})


# Requirement T2 ---------------------------------------------------------------

context("add_CI - T2. No errors when different amount of strata are used.")

test_that("T2.1 No error when only 1 strata is present.", {
  p <- adtte %>%
    visR::estimate_KM() %>%
    visR::visr()

  expect_error(p %>% visR::add_CI(alpha = 0.5), NA)

  skip_on_cran()
  p %>%
    visR::add_CI(alpha = 0.5) %>%
    vdiffr::expect_doppelganger(title = "add_CI_T2_1_one_strata")
})

test_that("T2.2 No error when 2 or more strata are present", {
  for (n_strata in c(5, 10, 20)) {
    p <- adtte %>%
      dplyr::mutate(TRTDUR = .map_numbers_to_new_range(
        number = adtte$TRTDUR,
        lower = 1,
        upper = n_strata
      )) %>%
      visR::estimate_KM(strata = "TRTDUR") %>%
      visR::visr()

    expect_error(p %>% visR::add_CI(), NA)
  }

  skip_on_cran()
  for (n_strata in c(5, 10, 20)) {
    p <- adtte %>%
      dplyr::mutate(TRTDUR = .map_numbers_to_new_range(
        number = adtte$TRTDUR,
        lower = 1,
        upper = n_strata
      )) %>%
      visR::estimate_KM(strata = "TRTDUR") %>%
      visR::visr()

    p %>%
      visR::add_CI() %>%
      vdiffr::expect_doppelganger(title = paste0(
        "add_CI_T2_2_",
        n_strata,
        "strata"
      ))
  }
})

# Requirement T3 ---------------------------------------------------------------

context("add_CI - T3.  Warnings in case of missing data or unexpected arguments are thrown.")

test_that("T3.1 Error when `est.lower` and `est.upper` are not present.", {
  survfit_object <- adtte %>% visR::estimate_KM(strata = "SEX")
  p <- survfit_object %>% visR::visr()

  are_present_before <- all(c("est.lower", "est.upper") %in% colnames(p$data))
  expect_equal(are_present_before, TRUE)
  expect_error(p %>% visR::add_CI(), NA)

  p$data <- p$data %>% dplyr::select(-c(est.lower, est.upper))
  are_present_after <- all(c("est.lower", "est.upper") %in% colnames(p$data))
  expect_equal(are_present_after, FALSE)
  expect_error(p %>% visR::add_CI())
})

test_that("T3.2 Warning when no valid style was provided.", {
  survfit_object <- adtte %>% visR::estimate_KM(strata = "SEX")
  p <- survfit_object %>% visR::visr()

  warning_message <- "Invalid `style` argument. Setting `style` to `ribbon`."
  expect_warning(p %>% visR::add_CI(style = "visR"), warning_message)
})

test_that("T3.3 Warning when `alpha` is not in [0, 1].", {
  p <- adtte %>%
    visR::estimate_KM(strata = "SEX") %>%
    visR::visr()

  warning_message <- "Invalid `alpha` argument, must be between 0 and 1. Setting it to 0.1."
  expect_warning(p %>% visR::add_CI(alpha = 5), warning_message)
  expect_warning(p %>% visR::add_CI(alpha = -5), warning_message)
})

test_that("T3.4 Warning when `style` is `ribbon` but a `linetype` was specified.", {
  p <- adtte %>%
    visR::estimate_KM(strata = "SEX") %>%
    visR::visr()

  warning_message <- "Argument `linetype` not used for style ribbon."
  expect_warning(
    p %>% visR::add_CI(
      style = "ribbon",
      linetype = 2
    ),
    warning_message
  )
})

# END OF CODE -------------------------------------------------------------
