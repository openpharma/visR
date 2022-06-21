#' @title Specifications test-define_theme.R
#' @section Last updated by: Steven Haesendonckx (shaesen2@@its.jnj.com)
#' @section Last update date: 2022-06-20
#'
#' @section List of tested specifications
#' T1. The function returns a `visR_theme` object.
#' T1.1 The function returns a list of class `visR_theme`.
#' T1.2 No error when no parameters are specified.
#' T2. The function accepts `strata` as argument.
#' T2.1 No error when `strata` is `NULL`.
#' T2.2 A warning when `strata` is an empty `list`.
#' T2.3 A warning when `strata` is an unnamed `list`.
#' T2.4 No warning when `strata` is a named `list`.
#' T3. The function accepts `fontsizes` as argument.
#' T3.1 No error when `fontsizes` is undefined.
#' T3.2 A warning when `fontsizes` is an empty `list`.
#' T3.3 A warning when `fontsizes` is `character`.
#' T3.4 A message when `fontsizes` is a numerical value.
#' T3.5 A warning when `fontsizes` is an unnamed `list`.
#' T3.6 No warning when `fontsizes` is a named `list`.
#' T4. The function accepts `fontfamily` as argument.
#' T4.1 No error when `fontfamily` is a string.
#' T4.2 A warning when `fontfamily` is an empty string.
#' T4.3 A warning when `fontfamily` is a vector of strings.
#' T4.4 A warning when `fontfamily` is anything but a string.
#' T5. The function accepts `grid` as argument.
#' T5.1 No error when `grid` is a boolean.
#' T5.2 No error when `grid` is a named list with elements `major` `minor`.
#' T5.3 A warning when `grid` is a unnmaed list.
#' T5.4 A warning when `grid` is a named list without elements `major` `minor`.
#' T5.5 A warning when `grid` is anything but a boolean or a list.
#' T6. The function accepts `bg` as argument.
#' T6.1 No error when `bg` is a character.
#' T6.2 A warning when `bg` is anything but a character.
#' T7. The function accepts `legend_position` as argument.
#' T7.1 `legend_position` accept 4 different strings `top` `right` `bottom` `left`.
#' T7.2 `legend_position` accepts NULL.
#' T7.3 A warning when `legend_position` is not NULL nor a string equal to `top` `right` `bottom` `left`.


# Requirement T1 ----------------------------------------------------------

testthat::context("define_theme - T1. `define_theme()` returns a `visR_theme` object.")

testthat::test_that("T1.1 `define_theme()` returns a list of class `visR_theme`.", {
  testthat::expect_true(inherits(visR::define_theme(), "visR_theme"))
  testthat::expect_true(inherits(visR::define_theme(), "list"))
})

testthat::test_that("T1.2 No error when no parameters are specified.", {
  testthat::expect_error(visR::define_theme(), NA)
})

# Requirement T2 ----------------------------------------------------------

testthat::context("define_theme - T2. The function accepts `strata` as argument.")

testthat::test_that("T2.1 No error when `strata` is `NULL`.", {
  testthat::expect_error(visR::define_theme(strata = NULL), NA)
})

testthat::test_that("T2.2 A warning when `strata` is an empty `list`.", {
  testthat::expect_warning(visR::define_theme(strata = list()))
})

testthat::test_that("T2.3 A warning when `strata` is an unnamed `list`.", {
  testthat::expect_warning(visR::define_theme(strata = list("v", "i", "s", "R")))
})

testthat::test_that("T2.4 No warning when `strata` is a named `list`.", {
  testthat::expect_warning(visR::define_theme(strata = list("visR" = "visR")), NA)
})

# Requirement T3 ----------------------------------------------------------

testthat::context("define_theme - T3. The function accepts `fontsizes` as argument.")

testthat::test_that("T3.1 No error when `fontsizes` is undefined.", {
  testthat::expect_error(visR::define_theme(), NA)
})

testthat::test_that("T3.2 A warning when `fontsizes` is an empty `list`.", {
  testthat::expect_warning(visR::define_theme(fontsizes = list()))
})

testthat::test_that("T3.3 A warning when `fontsizes` is `character`.", {
  testthat::expect_warning(visR::define_theme(fontsizes = "NULL"))
})

testthat::test_that("T3.4 A message when `fontsizes` is a numerical value.", {
  testthat::expect_message(visR::define_theme(fontsizes = 12))
})

testthat::test_that("T3.5 A warning when `fontsizes` is an unnamed `list`.", {
  testthat::expect_warning(visR::define_theme(fontsizes = list("s", "R")))
})

testthat::test_that("T3.6 No warning when `fontsizes` is a named `list`.", {
  testthat::expect_warning(visR::define_theme(fontsizes = list("a" = "a")), NA)
})

# Requirement T4 ----------------------------------------------------------

testthat::context("define_theme - T4. The function accepts `fontfamily` as argument.")

testthat::test_that("T4.1 No error when `fontfamily` is a string.", {
  testthat::expect_error(visR::define_theme(fontfamily = "Times"), NA)
})

testthat::test_that("T4.2 A warning when `fontfamily` is an empty string.", {
  testthat::expect_warning(visR::define_theme(fontfamily = ""))
  testthat::expect_warning(visR::define_theme(fontfamily = c("")))
})

testthat::test_that("T4.3 A warning when `fontfamily` is a vector of strings.", {
  testthat::expect_warning(visR::define_theme(fontfamily = c("a", "a")))
})

testthat::test_that("T4.4 A warning when `fontfamily` is anything but a string.", {
  testthat::expect_warning(visR::define_theme(fontfamily = NULL))
  testthat::expect_warning(visR::define_theme(fontfamily = 12))
  testthat::expect_warning(visR::define_theme(fontfamily = TRUE))
  testthat::expect_warning(visR::define_theme(fontfamily = list()))
})

# Requirement T5 ----------------------------------------------------------

testthat::context("define_theme - T5. The function accepts `grid` as argument.")

testthat::test_that("T5.1 No error when `grid` is a boolean.", {
  testthat::expect_error(visR::define_theme(grid = TRUE), NA)
  testthat::expect_error(visR::define_theme(grid = FALSE), NA)
})

testthat::test_that("T5.2 No error when `grid` is a named list with elements `major` `minor`.", {
  testthat::expect_error(visR::define_theme(grid = list("major" = TRUE, "minor" = FALSE)), NA)
  testthat::expect_error(visR::define_theme(grid = list("major" = TRUE)), NA)
  testthat::expect_error(visR::define_theme(grid = list("minor" = TRUE)), NA)
})

testthat::test_that("T5.3 A warning when `grid` is a unnmaed list.", {
  testthat::expect_warning(visR::define_theme(grid = list(TRUE, TRUE)))
})

testthat::test_that("T5.4 A warning when `grid` is a named list without elements `major` `minor`.", {
  testthat::expect_warning(visR::define_theme(grid = list("visR" = TRUE)))
})

testthat::test_that("T5.5 A warning when `grid` is anything but a boolean or a list.", {
  testthat::expect_warning(visR::define_theme(grid = NULL))
  testthat::expect_warning(visR::define_theme(grid = 12))
  testthat::expect_warning(visR::define_theme(grid = "visR"))
  testthat::expect_warning(visR::define_theme(grid = c()))
})

# Requirement T6 ----------------------------------------------------------

testthat::context("define_theme - T6. The function accepts `bg` as argument.")

testthat::test_that("T6.1 No error when `bg` is a character.", {
  testthat::expect_error(visR::define_theme(bg = "blue"), NA)
})

testthat::test_that("T6.2 A warning when `bg` is anything but a character.", {
  testthat::expect_warning(visR::define_theme(bg = NULL))
  testthat::expect_warning(visR::define_theme(bg = 12))
  testthat::expect_warning(visR::define_theme(bg = list()))
})

# Requirement T7 ----------------------------------------------------------

testthat::context("define_theme - T7. The function accepts `legend_position` as argument.")

testthat::test_that("T7.1 `legend_position` accept 4 different strings `top` `right` `bottom` `left`.", {
  testthat::expect_error(visR::define_theme(legend_position = "top"), NA)
  testthat::expect_error(visR::define_theme(legend_position = "bottom"), NA)
  testthat::expect_error(visR::define_theme(legend_position = "left"), NA)
  testthat::expect_error(visR::define_theme(legend_position = "right"), NA)
  testthat::expect_warning(visR::define_theme(legend_position = "top"), NA)
  testthat::expect_warning(visR::define_theme(legend_position = "bottom"), NA)
  testthat::expect_warning(visR::define_theme(legend_position = "left"), NA)
  testthat::expect_warning(visR::define_theme(legend_position = "right"), NA)
})

testthat::test_that("T7.2 `legend_position` accepts NULL.", {
  testthat::expect_error(visR::define_theme(legend_position = NULL), NA)
  testthat::expect_warning(visR::define_theme(legend_position = NULL), NA)
})

testthat::test_that("T7.3 A warning when `legend_position` is not NULL nor a string equal to `top` `right` `bottom` `left`.", {
  testthat::expect_warning(visR::define_theme(legend_position = "visR"))
  testthat::expect_warning(visR::define_theme(legend_position = 12))
  testthat::expect_warning(visR::define_theme(legend_position = list()))
})

# END OF CODE -------------------------------------------------------------
