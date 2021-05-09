#' @title Tests the theming functions
#' @section Last updated by:
#' Tim Treis
#' @section Last update date:
#' 07-MAY-2021

# Specifications ---------------------------------------------------------------

#' T1. The `define_theme` function returns an object that has a valid shape for usage in the `add_theme` function.
#' T1.1 No error when no parameters are specified.
#' T1.2 Not specifying any parameters returns a list.
#' T1.3 No error when `strata` is `NULL`.
#' T1.4 A warning when `strata` is an empty `list`.
#' T1.5 A warning when `strata` is an unnamed `list`.
#' T1.6 No warning when `strata` is a named `list`.
#' T1.7 No error when `fontsizes` is `NULL`.
#' T1.8 A warning when `fontsizes` is an empty `list`.
#' T1.9 A warning when `fontsizes` is an unnamed `list`.
#' T1.10 No warning when `fontsizes` is a named `list`.
#' T1.11 A message when `fontsizes` is a numerical value.
#' T1.12 No error when `fontfamily` is a string.
#' T1.13 A warning when `fontfamily` is anything but a string.
#' T1.14 No error when `grid` is a boolean.
#' T1.15 A warning when `grid` is anything but a boolean
#' T2. The `add_theme` function applies the specified changes to a `ggplot` object.
#' T2.1 No error when a `ggplot` plot is provided, but no theme.
#' T2.2 No error when a `ggplot` plot and a `visR::define_theme` object are provided.
#' T2.3 A message when a theme not generated through `visR::define_theme` is provided.

# Requirement T1 ---------------------------------------------------------------

context("add_theme - T1. The `define_theme` function returns an object that has a valid shape for usage in the `add_theme` function.")

testthat::test_that("T1.1 No error when no parameters are specified.",{
  
  testthat::expect_error(visR::define_theme(), NA)
  
})

testthat::test_that("T1.2 Not specifying any parameters returns a list.",{
  
  theme <- visR::define_theme()
  
  testthat::expect_true(is.list(theme))
  
})

testthat::test_that("T1.3 No error when `strata` is `NULL`.",{
  
  testthat::expect_error(visR::define_theme(strata = NULL), NA)
  
})

testthat::test_that("T1.4 A warning when `strata` is an empty `list`.",{
  
  testthat::expect_warning(visR::define_theme(strata = list()))
  
})

testthat::test_that("T1.5 A warning when `strata` is an unnamed `list`.",{
  
  testthat::expect_warning(visR::define_theme(strata = list("v", "i", "s", "R")))
  
})

testthat::test_that("T1.6 No warning when `strata` is a named `list`.",{
  
  testthat::expect_warning(visR::define_theme(strata = list("visR" = "visR")), NA)
  
})

testthat::test_that("T1.7 No error when `fontsizes` is `NULL`.",{
  
  testthat::expect_error(visR::define_theme(fontsizes = NULL), NA)
  
})

testthat::test_that("T1.8 A warning when `fontsizes` is an empty `list`.",{
  
  testthat::expect_warning(visR::define_theme(fontsizes = list()))
  
})

testthat::test_that("T1.9 A warning when `fontsizes` is an unnamed `list`.",{
  
  testthat::expect_warning(visR::define_theme(fontsizes = list("v", "i", "s", "R")))
  
})

testthat::test_that("T1.10 No warning when `fontsizes` is a named `list`.",{
  
  testthat::expect_warning(visR::define_theme(fontsizes = list("visR" = "visR")), NA)
  
})

testthat::test_that("T1.11 A message when `fontsizes` is a numerical value.",{
  
  testthat::expect_message(visR::define_theme(fontsizes = 12))
  
})

testthat::test_that("T1.12 No error when `fontfamily` is a string.",{
  
  testthat::expect_error(visR::define_theme(fontfamily = "Times"), NA)
  
})

testthat::test_that("T1.13 A warning when `fontfamily` is anything but a string.",{
  
  testthat::expect_warning(visR::define_theme(fontfamily = NULL))
  testthat::expect_warning(visR::define_theme(fontfamily = 12))
  testthat::expect_warning(visR::define_theme(fontfamily = TRUE))
  testthat::expect_warning(visR::define_theme(fontfamily = c()))
  
})

testthat::test_that("T1.14 No error when `bg` is a boolean",{
  
  testthat::expect_error(visR::define_theme(grid = T), NA)
  testthat::expect_error(visR::define_theme(grid = F), NA)
  testthat::expect_error(visR::define_theme(grid = TRUE), NA)
  testthat::expect_error(visR::define_theme(grid = FALSE), NA)
  
})

testthat::test_that("T1.15 A warning when `bg` is anything but a boolean",{
  
  testthat::expect_warning(visR::define_theme(grid = NULL))
  testthat::expect_warning(visR::define_theme(grid = 12))
  testthat::expect_warning(visR::define_theme(grid = "visR"))
  testthat::expect_warning(visR::define_theme(grid = c()))
  
})

# Requirement T2 ---------------------------------------------------------------

context("add_theme - T2. The `add_theme` function applies the specified changes to a `ggplot` object.")

testthat::test_that("T2.1 No error when a `ggplot` plot is provided, but no theme.",{
  
  gg <- adtte %>%
    visR::estimate_KM("SEX") %>%
    plot()
  
  testthat::expect_error(visR::add_theme(gg), NA)
  
})

testthat::test_that("T2.2 No error when a `ggplot` plot and a `visR::define_theme` object are provided.",{
  
  gg <- adtte %>%
    visR::estimate_KM("SEX") %>%
    plot()
  
  theme <- visR::define_theme()
  
  testthat::expect_error(visR::add_theme(gg, theme), NA)
  testthat::expect_error(gg %>% visR::add_theme(theme), NA)
  
})

testthat::test_that("T2.3 A message when a theme not generated through `visR::define_theme` is provided.",{
  
  gg <- adtte %>%
    visR::estimate_KM("SEX") %>%
    plot()
  
  theme <- list("fontfamily" = "Palatino")
  
  testthat::expect_error(visR::add_theme(gg, theme), NA)
  testthat::expect_error(gg %>% visR::add_theme(theme), NA)
  
  testthat::expect_message(visR::add_theme(gg, theme))
  testthat::expect_message(gg %>% visR::add_theme(theme))
  
})


# END OF CODE ------------------------------------------------------------------