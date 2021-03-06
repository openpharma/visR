#' @title Specifications apply_theme
#' @section Last updated by:
#' Tim Treis
#' @section Last update date:
#' 29-MAY-2021

# Specifications ---------------------------------------------------------------

#' T1. The `define_theme()` function returns a `visR_theme` object can contain valid input parameters for `apply_theme()`.
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
#' T1.12 A warning when `fontsizes` is neither `NULL`, a `list` or a `numeric`.
#' T1.13 No error when `fontfamily` is a string.
#' T1.14 A warning when `fontfamily` is an empty string.
#' T1.15 A warning when `fontfamily` is a vector of strings.
#' T1.16 A warning when `fontfamily` is anything but a string.
#' T1.17 No error when `grid` is a boolean.
#' T1.18 A warning when `grid` is anything but a boolean.
#' T1.19 No error when `bg` is a string.
#' T1.20 A warning when `grid` is anything but a boolean.
#' T1.21 The returned theme object is of class `visR_theme`.
#' T2. The `apply_theme` function applies the specified changes to a `ggplot` object.
#' T2.1 No error when a `ggplot` plot is provided, but no theme.
#' T2.2 No error when a `ggplot` plot and a minimal `visR::define_theme` object are provided.
#' T2.3 No error when a `ggplot` plot and a complex `visR::define_theme` object are provided.
#' T2.4 A message when a theme not generated through `visR::define_theme` is provided.
#' T2.5 Colours applied through `visR::apply_theme()` are used in the resulting `ggplot` object.
#' T2.6 Fontsizes applied through `visR::apply_theme()` are used in the resulting `ggplot` object.
#' T2.7 The fontfamily applied through `visR::apply_theme()` is used in the resulting `ggplot` object.
#' T2.8 The grid applied through `visR::apply_theme()` is used in the resulting `ggplot` object.
#' T2.9 The background applied through `visR::apply_theme()` is used in the resulting `ggplot` object.
#' T2.10 The legend_position applied through `visR::apply_theme()` is used in the resulting `ggplot` object.
#' T2.11 The legend_position defined in `visR::visr()` is correctly passed through to the resulting `ggplot` object.

# Requirement T1 ---------------------------------------------------------------

testthat::context("apply_theme - T1. The `define_theme()` function returns a `visR_theme` object can contain valid input parameters for `apply_theme()`.")

testthat::test_that("T1.1 No error when no parameters are specified.", {
  
  testthat::expect_error(visR::define_theme(), NA)
  
})

testthat::test_that("T1.2 Not specifying any parameters returns a list.", {
  
  theme <- visR::define_theme()
  
  testthat::expect_true(is.list(theme))
  
})

testthat::test_that("T1.3 No error when `strata` is `NULL`.", {
  
  testthat::expect_error(visR::define_theme(strata = NULL), NA)
  
})

testthat::test_that("T1.4 A warning when `strata` is an empty `list`.", {
  
  testthat::expect_warning(visR::define_theme(strata = list()))
  
})

testthat::test_that("T1.5 A warning when `strata` is an unnamed `list`.", {
  
  testthat::expect_warning(visR::define_theme(strata = list("v", "i", "s", "R")))
  
})

testthat::test_that("T1.6 No warning when `strata` is a named `list`.", {
  
  testthat::expect_warning(visR::define_theme(strata = list("visR" = "visR")), NA)
  
})

testthat::test_that("T1.7 No error when `fontsizes` is `NULL`.", {
  
  testthat::expect_error(visR::define_theme(fontsizes = NULL), NA)
  
})

testthat::test_that("T1.8 A warning when `fontsizes` is an empty `list`.", {
  
  testthat::expect_warning(visR::define_theme(fontsizes = list()))
  
})

testthat::test_that("T1.9 A warning when `fontsizes` is an unnamed `list`.", {
  
  testthat::expect_warning(visR::define_theme(fontsizes = list("v", "i", "s", "R")))
  
})

testthat::test_that("T1.10 No warning when `fontsizes` is a named `list`.", {
  
  testthat::expect_warning(visR::define_theme(fontsizes = list("visR" = "visR")), NA)
  
})

testthat::test_that("T1.11 A message when `fontsizes` is a numerical value.", {
  
  testthat::expect_message(visR::define_theme(fontsizes = 12))
  
})

testthat::test_that("T1.12 A warning when `fontsizes` is neither `NULL`, a `list` or a `numeric`.", {
  
  testthat::expect_warning(visR::define_theme(fontsizes = "visR"))
  
})

testthat::test_that("T1.13 No error when `fontfamily` is a string.", {
  
  testthat::expect_error(visR::define_theme(fontfamily = "Times"), NA)
  
})

testthat::test_that("T1.14 A warning when `fontfamily` is an empty string.", {
  
  testthat::expect_warning(visR::define_theme(fontfamily = ""))
  testthat::expect_warning(visR::define_theme(fontfamily = c("")))
  
})

testthat::test_that("T1.15 A warning when `fontfamily` is a vector of strings.", {
  
  testthat::expect_warning(visR::define_theme(fontfamily = c("Times", "Helvetica")))
  
})

testthat::test_that("T1.16 A warning when `fontfamily` is anything but a string.", {
  
  testthat::expect_warning(visR::define_theme(fontfamily = NULL))
  testthat::expect_warning(visR::define_theme(fontfamily = 12))
  testthat::expect_warning(visR::define_theme(fontfamily = TRUE))
  testthat::expect_warning(visR::define_theme(fontfamily = list()))
  
})

testthat::test_that("T1.17 No error when `grid` is a boolean", {
  
  testthat::expect_error(visR::define_theme(grid = TRUE), NA)
  testthat::expect_error(visR::define_theme(grid = FALSE), NA)
  
})

testthat::test_that("T1.18 A warning when `grid` is anything but a boolean", {
  
  testthat::expect_warning(visR::define_theme(grid = NULL))
  testthat::expect_warning(visR::define_theme(grid = 12))
  testthat::expect_warning(visR::define_theme(grid = "visR"))
  testthat::expect_warning(visR::define_theme(grid = c()))
  
})

testthat::test_that("T1.19 No error when `bg` is a character.", {
  
  testthat::expect_error(visR::define_theme(bg = "blue"), NA)
  
})

testthat::test_that("T1.20 A warning when `bg` is anything but a character.", {
  
  testthat::expect_warning(visR::define_theme(bg = NULL))
  testthat::expect_warning(visR::define_theme(bg = 12))
  testthat::expect_warning(visR::define_theme(bg = list()))
  
})

testthat::test_that("T1.21 The returned theme object is of class `visR_theme`.", {
  
  testthat::expect_true("visR_theme" %in% class(visR::define_theme()))
  
})

# Requirement T2 ---------------------------------------------------------------

testthat::context("apply_theme - T2. The `apply_theme` function applies the specified changes to a `ggplot` object.")

testthat::test_that("T2.1 No error when a `ggplot` plot is provided, but no theme.", {
  
  gg <- adtte %>%
    visR::estimate_KM("SEX") %>%
    visR::visr()
  
  testthat::expect_error(visR::apply_theme(gg), NA)
  
})

testthat::test_that("T2.2 No error when a `ggplot` plot and a minimal `visR::define_theme` object are provided.", {
  
  gg <- adtte %>%
    visR::estimate_KM("SEX") %>%
    visR::visr()
  
  theme <- visR::define_theme()
  
  testthat::expect_error(visR::apply_theme(gg, theme), NA)
  testthat::expect_error(gg %>% visR::apply_theme(theme), NA)
  
})

testthat::test_that("T2.3 No error when a `ggplot` plot and a complex `visR::define_theme` object are provided.", {
  
  gg <- adtte %>%
    visR::estimate_KM("SEX") %>%
    visR::visr()
  
  theme <- visR::define_theme(strata = list("SEX" = list("F" = "red",
                                                         "M" = "blue"),
                                            "TRTA" = list("Placebo" = "cyan",
                                                          "Xanomeline High Dose" = "purple",
                                                          "Xanomeline Low Dose" = "brown")),
                              fontsizes = list("axis" = 12,
                                               "ticks" = 10),
                              fontfamily = "Helvetica",
                              grid = FALSE,
                              bg = "transparent")
  
  testthat::expect_error(visR::apply_theme(gg, theme), NA)
  testthat::expect_error(gg %>% visR::apply_theme(theme), NA)
  
})

testthat::test_that("T2.4 A message when a theme not generated through `visR::define_theme` is provided.", {
  
  gg <- adtte %>%
    visR::estimate_KM("SEX") %>%
    visR::visr()
  
  theme <- list("fontfamily" = "Palatino")
  
  testthat::expect_error(visR::apply_theme(gg, theme), NA)
  testthat::expect_error(gg %>% visR::apply_theme(theme), NA)
  
  testthat::expect_message(visR::apply_theme(gg, theme))
  testthat::expect_message(gg %>% visR::apply_theme(theme))
  
})

testthat::test_that("T2.5 Colours applied through `visR::apply_theme()` are used in the resulting `ggplot` object.", {
  
  gg <- adtte %>%
    visR::estimate_KM("SEX") %>%
    visR::visr()
  
  theme <- visR::define_theme(strata = list("SEX" = list("F" = "red",
                                                         "M" = "blue"),
                                            "TRTA" = list("Placebo" = "cyan",
                                                          "Xanomeline High Dose" = "purple",
                                                          "Xanomeline Low Dose" = "brown")))
  
  gg <- gg %>% visR::apply_theme(theme)
  ggb <- ggplot2::ggplot_build(gg)
  
  testthat::expect_equal(unique(unlist(theme$strata$SEX)),
                         unique(ggb$data[[1]]$colour))
  
})

testthat::test_that("T2.6 Fontsizes applied through `visR::apply_theme()` are used in the resulting `ggplot` object.", {
  
  gg <- adtte %>%
    visR::estimate_KM("SEX") %>%
    visR::visr()
  
  theme <- visR::define_theme(fontsizes = list("axis" = 12,
                                               "ticks" = 10,
                                               "legend_title" = 10,
                                               "legend_text" = 8))
  
  gg <- gg %>% visR::apply_theme(theme)
  ggb <- ggplot2::ggplot_build(gg)
  
  testthat::expect_equal(theme$fontsizes$axis, ggb$plot$theme$axis.title.x$size)
  testthat::expect_equal(theme$fontsizes$axis, ggb$plot$theme$axis.title.y$size)
  testthat::expect_equal(theme$fontsizes$ticks, ggb$plot$theme$axis.text$size)
  testthat::expect_equal(theme$fontsizes$legend_title, ggb$plot$theme$legend.title$size)
  testthat::expect_equal(theme$fontsizes$legend_text, ggb$plot$theme$legend.text$size)
  
})

testthat::test_that("T2.7 The fontfamily applied through `visR::apply_theme()` is used in the resulting `ggplot` object.", {
  
  gg <- adtte %>%
    visR::estimate_KM("SEX") %>%
    visR::visr()
  
  theme <- visR::define_theme(fontfamily = "Helvetica")
  
  gg <- gg %>% visR::apply_theme(theme)
  ggb <- ggplot2::ggplot_build(gg)
  
  testthat::expect_equal(theme$fontfamily, ggb$plot$theme$text$family)
  
})

testthat::test_that("T2.8 The grid applied through `visR::apply_theme()` is used in the resulting `ggplot` object.", {
  
  gg <- adtte %>%
    visR::estimate_KM("SEX") %>%
    visR::visr()
  
  theme_grid_true <- visR::define_theme(grid = TRUE) # Equal to major = TRUE and minor = FALSE
  theme_grid_false <- visR::define_theme(grid = FALSE) # Equal to major = minor = FALSE
  theme_grid_only_minor <- visR::define_theme(grid = list("major" = FALSE,
                                                          "minor" = TRUE))
  theme_grid_minor_and_major <- visR::define_theme(grid = list("major" = TRUE,
                                                               "minor" = TRUE))
  
  gg_grid_true            <- gg %>% visR::apply_theme(theme_grid_true)
  gg_grid_false           <- gg %>% visR::apply_theme(theme_grid_false)
  gg_grid_only_minor      <- gg %>% visR::apply_theme(theme_grid_only_minor)
  gg_grid_minor_and_major <- gg %>% visR::apply_theme(theme_grid_minor_and_major)
  
  ggb_grid_true            <- ggplot2::ggplot_build(gg_grid_true)
  ggb_grid_false           <- ggplot2::ggplot_build(gg_grid_false)
  ggb_grid_only_minor      <- ggplot2::ggplot_build(gg_grid_only_minor)
  ggb_grid_minor_and_major <- ggplot2::ggplot_build(gg_grid_minor_and_major)
  
  testthat::expect_true(("element_line" %in% class(ggb_grid_true$plot$theme$panel.grid.major)) &
                        ("element_blank" %in% class(ggb_grid_true$plot$theme$panel.grid.minor)))
  
  testthat::expect_true(("element_blank" %in% class(ggb_grid_false$plot$theme$panel.grid.major)) &
                        ("element_blank" %in% class(ggb_grid_false$plot$theme$panel.grid.minor)))
  
  testthat::expect_true(("element_blank" %in% class(ggb_grid_only_minor$plot$theme$panel.grid.major)) &
                        ("element_line" %in% class(ggb_grid_only_minor$plot$theme$panel.grid.minor)))
  
  testthat::expect_true(("element_line" %in% class(ggb_grid_minor_and_major$plot$theme$panel.grid.major)) &
                        ("element_line" %in% class(ggb_grid_minor_and_major$plot$theme$panel.grid.minor)))
  
})

testthat::test_that("T2.9 The background applied through `visR::apply_theme()` is used in the resulting `ggplot` object.", {
  
  gg <- adtte %>%
    visR::estimate_KM("SEX") %>%
    visR::visr()
  
  theme <- visR::define_theme(bg = "transparent")
  
  gg <- gg %>% visR::apply_theme(theme)
  ggb <- ggplot2::ggplot_build(gg)
  
  testthat::expect_equal(theme$bg, ggb$plot$theme$panel.background$fill)
  
})

testthat::test_that("T2.10 The legend_position applied through `visR::apply_theme()` is used in the resulting `ggplot` object.", {
  
  gg <- adtte %>%
    visR::estimate_KM("SEX") %>%
    visR::visr()
  
  theme_top    <- visR::define_theme(legend_position = "top")
  theme_right  <- visR::define_theme(legend_position = "right")
  theme_bottom <- visR::define_theme(legend_position = "bottom")
  theme_left   <- visR::define_theme(legend_position = "left")
  
  gg_top    <- gg %>% visR::apply_theme(theme_top)
  gg_right  <- gg %>% visR::apply_theme(theme_right)
  gg_bottom <- gg %>% visR::apply_theme(theme_bottom)
  gg_left   <- gg %>% visR::apply_theme(theme_left)
  
  ggb_top    <- ggplot2::ggplot_build(gg_top)
  ggb_right  <- ggplot2::ggplot_build(gg_right)
  ggb_bottom <- ggplot2::ggplot_build(gg_bottom)
  ggb_left   <- ggplot2::ggplot_build(gg_left)
  
  testthat::expect_equal(theme_top$legend_position,    ggb_top$plot$theme$legend.position)
  testthat::expect_equal(theme_right$legend_position,  ggb_right$plot$theme$legend.position)
  testthat::expect_equal(theme_bottom$legend_position, ggb_bottom$plot$theme$legend.position)
  testthat::expect_equal(theme_left$legend_position,   ggb_left$plot$theme$legend.position)
  
})

testthat::test_that("T2.11 The legend_position defined in `visR::visr()` is correctly passed through to the resulting `ggplot` object.", {
  
  gg_top <- adtte %>%
    visR::estimate_KM("SEX") %>%
    visR::visr(legend_position = "top")
  gg_right <- adtte %>%
    visR::estimate_KM("SEX") %>%
    visR::visr(legend_position = "right")
  gg_bottom <- adtte %>%
    visR::estimate_KM("SEX") %>%
    visR::visr(legend_position = "bottom")
  gg_left <- adtte %>%
    visR::estimate_KM("SEX") %>%
    visR::visr(legend_position = "left")
  
  gg_top    <- gg_top %>% visR::apply_theme()
  gg_right  <- gg_right %>% visR::apply_theme()
  gg_bottom <- gg_bottom %>% visR::apply_theme()
  gg_left   <- gg_left %>% visR::apply_theme()

  ggb_top    <- ggplot2::ggplot_build(gg_top)
  ggb_right  <- ggplot2::ggplot_build(gg_right)
  ggb_bottom <- ggplot2::ggplot_build(gg_bottom)
  ggb_left   <- ggplot2::ggplot_build(gg_left)
  
  testthat::expect_true("top"    %in% ggb_top$plot$theme$legend.position)
  testthat::expect_true("right"  %in% ggb_right$plot$theme$legend.position)
  testthat::expect_true("bottom" %in% ggb_bottom$plot$theme$legend.position)
  testthat::expect_true("left"   %in% ggb_left$plot$theme$legend.position)
  
})

# END OF CODE ------------------------------------------------------------------