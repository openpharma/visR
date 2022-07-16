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

context("define_theme - T1. `define_theme()` returns a `visR_theme` object.")

test_that("T1.1 `define_theme()` returns a list of class `visR_theme`.", {
  expect_true(inherits(visR::define_theme(), "visR_theme"))
  expect_true(inherits(visR::define_theme(), "list"))
})

test_that("T1.2 No error when no parameters are specified.", {
  expect_error(visR::define_theme(), NA)
})

# Requirement T2 ----------------------------------------------------------

context("define_theme - T2. The function accepts `strata` as argument.")

test_that("T2.1 No error when `strata` is `NULL`.", {
  expect_error(visR::define_theme(strata = NULL), NA)
})

test_that("T2.2 A warning when `strata` is an empty `list`.", {
  expect_warning(visR::define_theme(strata = list()))
})

test_that("T2.3 A warning when `strata` is an unnamed `list`.", {
  expect_warning(visR::define_theme(strata = list("v", "i", "s", "R")))
})

test_that("T2.4 No warning when `strata` is a named `list`.", {
  expect_warning(visR::define_theme(strata = list("visR" = "visR")), NA)
})

# Requirement T3 ----------------------------------------------------------

context("define_theme - T3. The function accepts `fontsizes` as argument.")

test_that("T3.1 No error when `fontsizes` is undefined.", {
  expect_error(visR::define_theme(), NA)
})

test_that("T3.2 A warning when `fontsizes` is an empty `list`.", {
  expect_warning(visR::define_theme(fontsizes = list()))
})

test_that("T3.3 A warning when `fontsizes` is `character`.", {
  expect_warning(visR::define_theme(fontsizes = "NULL"))
})

test_that("T3.4 A message when `fontsizes` is a numerical value.", {
  expect_message(visR::define_theme(fontsizes = 12))
})

test_that("T3.5 A warning when `fontsizes` is an unnamed `list`.", {
  expect_warning(visR::define_theme(fontsizes = list("s", "R")))
})

test_that("T3.6 No warning when `fontsizes` is a named `list`.", {
  expect_warning(visR::define_theme(fontsizes = list("a" = "a")), NA)
})

# Requirement T4 ----------------------------------------------------------

context("define_theme - T4. The function accepts `fontfamily` as argument.")

test_that("T4.1 No error when `fontfamily` is a string.", {
  expect_error(visR::define_theme(fontfamily = "Times"), NA)
})

test_that("T4.2 A warning when `fontfamily` is an empty string.", {
  expect_warning(visR::define_theme(fontfamily = ""))
  expect_warning(visR::define_theme(fontfamily = c("")))
})

test_that("T4.3 A warning when `fontfamily` is a vector of strings.", {
  expect_warning(visR::define_theme(fontfamily = c("a", "a")))
})

test_that("T4.4 A warning when `fontfamily` is anything but a string.", {
  expect_warning(visR::define_theme(fontfamily = NULL))
  expect_warning(visR::define_theme(fontfamily = 12))
  expect_warning(visR::define_theme(fontfamily = TRUE))
  expect_warning(visR::define_theme(fontfamily = list()))
})

# Requirement T5 ----------------------------------------------------------

context("define_theme - T5. The function accepts `grid` as argument.")

test_that("T5.1 No error when `grid` is a boolean.", {
  expect_error(visR::define_theme(grid = TRUE), NA)
  expect_error(visR::define_theme(grid = FALSE), NA)
})

test_that("T5.2 No error when `grid` is a named list with elements `major` `minor`.", {
  expect_error(visR::define_theme(grid = list("major" = TRUE, "minor" = FALSE)), NA)
  expect_error(visR::define_theme(grid = list("major" = TRUE)), NA)
  expect_error(visR::define_theme(grid = list("minor" = TRUE)), NA)
})

test_that("T5.3 A warning when `grid` is a unnmaed list.", {
  expect_warning(visR::define_theme(grid = list(TRUE, TRUE)))
})

test_that("T5.4 A warning when `grid` is a named list without elements `major` `minor`.", {
  expect_warning(visR::define_theme(grid = list("visR" = TRUE)))
})

test_that("T5.5 A warning when `grid` is anything but a boolean or a list.", {
  expect_warning(visR::define_theme(grid = NULL))
  expect_warning(visR::define_theme(grid = 12))
  expect_warning(visR::define_theme(grid = "visR"))
  expect_warning(visR::define_theme(grid = c()))
})

# Requirement T6 ----------------------------------------------------------

context("define_theme - T6. The function accepts `bg` as argument.")

test_that("T6.1 No error when `bg` is a character.", {
  expect_error(visR::define_theme(bg = "blue"), NA)
})

test_that("T6.2 A warning when `bg` is anything but a character.", {
  expect_warning(visR::define_theme(bg = NULL))
  expect_warning(visR::define_theme(bg = 12))
  expect_warning(visR::define_theme(bg = list()))
})

# Requirement T7 ----------------------------------------------------------

context("define_theme - T7. The function accepts `legend_position` as argument.")

test_that("T7.1 `legend_position` accept 4 different strings `top` `right` `bottom` `left`.", {
  expect_error(visR::define_theme(legend_position = "top"), NA)
  expect_error(visR::define_theme(legend_position = "bottom"), NA)
  expect_error(visR::define_theme(legend_position = "left"), NA)
  expect_error(visR::define_theme(legend_position = "right"), NA)
  expect_warning(visR::define_theme(legend_position = "top"), NA)
  expect_warning(visR::define_theme(legend_position = "bottom"), NA)
  expect_warning(visR::define_theme(legend_position = "left"), NA)
  expect_warning(visR::define_theme(legend_position = "right"), NA)
})

test_that("T7.2 `legend_position` accepts NULL.", {
  expect_error(visR::define_theme(legend_position = NULL), NA)
  expect_warning(visR::define_theme(legend_position = NULL), NA)
})

test_that("T7.3 A warning when `legend_position` is not NULL nor a string equal to `top` `right` `bottom` `left`.", {
  expect_warning(visR::define_theme(legend_position = "visR"))
  expect_warning(visR::define_theme(legend_position = 12))
  expect_warning(visR::define_theme(legend_position = list()))
})

# END OF CODE -------------------------------------------------------------
