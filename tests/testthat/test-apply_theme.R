#' @title Specifications test-apply_theme.R
#' @section Last updated by: Steven Haesendonckx (shaesen2@@its.jnj.com)
#' @section Last update date: 2022-06-20
#'
#' @section List of tested specifications
#' T1. The function applies the specified changes to a `ggplot` object.
#' T1.1 No error when a `ggplot` plot is provided, but no theme.
#' T1.2 No error when a `ggplot` plot and a minimal `visR::define_theme` object are provided.
#' T1.3 No error when a `ggplot` plot and a complex `visR::define_theme` object are provided.
#' T1.4 A message when a theme not generated through `visR::define_theme` is provided.
#' T1.5 If `fontsizes` is a `numeric`, the other font occurrences are derived from it.
#' T1.6 If `fontsizes` is a `list`, the individual fonts are extracted and used.
#' T1.7 The fontfamily applied through `visR::apply_theme()` is used in the resulting `ggplot` object.
#' T1.8 If `grid` is a single `logical`, it is used for both major and minor grid.
#' T1.9 If `grid` is a named list containing 'major' and/or 'minor' as single `logical`s, these are used for their respective options.
#' T1.10 A warning when `grid` is a named list containing 'major' and/or 'minor' that are not single `logical`s.
#' T1.11 A warning when `grid` is a named list that does not contain 'major' and/or 'minor'.
#' T1.12 The background applied through `visR::apply_theme()` is used in the resulting `ggplot` object.
#' T1.13 T1.14 The legend_position defined in `visR::visr()` is used when not defined through `visR::apply_theme()`..
#' T1.14 The legend_position defined in `visR::visr()` is correctly passed through to the resulting `ggplot` object.
#' T1.15 If a stratum has no colour assigned, the default colour (grey50) is used.
#' T1.16 When the theme dict contains no colour information for the strata of the ggplot object, the default visR colours are used.
#' T1.17 When the stratum requires more colours than the visR palette holds, the default ggplot2 ones are chosen.
#' T1.18 If no strata colors can be mapped to the graph, a warning about the presence of more than 15 strata levels.
#' T1.19 The named list is used in the legend title.


# Requirement T1 ----------------------------------------------------------

testthat::context("apply_theme - T1. The function applies the specified changes to a `ggplot` object.")

testthat::test_that("T1.1 No error when a `ggplot` plot is provided, but no theme.", {
  gg <- adtte %>%
    visR::estimate_KM("SEX") %>%
    visR::visr()

  testthat::expect_error(visR::apply_theme(gg), NA)
})

testthat::test_that("T1.2 No error when a `ggplot` plot and a minimal `visR::define_theme` object are provided.", {
  gg <- adtte %>%
    visR::estimate_KM("SEX") %>%
    visR::visr()

  theme <- visR::define_theme()

  testthat::expect_error(visR::apply_theme(gg, theme), NA)
  testthat::expect_error(gg %>% visR::apply_theme(theme), NA)
})

testthat::test_that("T1.3 No error when a `ggplot` plot and a complex `visR::define_theme` object are provided.", {
  gg <- adtte %>%
    visR::estimate_KM("SEX") %>%
    visR::visr()

  theme <- visR::define_theme(
    strata = list(
      "SEX" = list(
        "F" = "red",
        "M" = "blue"
      ),
      "TRTA" = list(
        "Placebo" = "cyan",
        "Xanomeline High Dose" = "purple",
        "Xanomeline Low Dose" = "brown"
      )
    ),
    fontsizes = list(
      "axis" = 12,
      "ticks" = 10
    ),
    fontfamily = "Helvetica",
    grid = FALSE,
    bg = "transparent"
  )

  testthat::expect_error(visR::apply_theme(gg, theme), NA)
  testthat::expect_error(gg %>% visR::apply_theme(theme), NA)
})

testthat::test_that("T1.4 A message when a theme not generated through `visR::define_theme` is provided.", {
  gg <- adtte %>%
    visR::estimate_KM("SEX") %>%
    visR::visr()

  theme <- list("fontfamily" = "Palatino")

  testthat::expect_error(visR::apply_theme(gg, theme), NA)
  testthat::expect_error(gg %>% visR::apply_theme(theme), NA)

  testthat::expect_message(visR::apply_theme(gg, theme))
  testthat::expect_message(gg %>% visR::apply_theme(theme))
})

testthat::test_that("T1.5 If `fontsizes` is a `numeric`, the other font occurrences are derived from it.", {
  gg <- adtte %>%
    visR::estimate_KM("SEX") %>%
    visR::visr()

  theme <- visR::define_theme(fontsizes = 12)

  gg <- gg %>% visR::apply_theme(theme)
  ggb <- ggplot2::ggplot_build(gg)

  testthat::expect_equal(theme$fontsizes, ggb$plot$theme$axis.title.x$size)
  testthat::expect_equal(theme$fontsizes, ggb$plot$theme$axis.title.y$size)
  testthat::expect_equal(theme$fontsizes, ggb$plot$theme$axis.text$size)
  testthat::expect_equal(theme$fontsizes, ggb$plot$theme$legend.title$size)
  testthat::expect_equal(theme$fontsizes, ggb$plot$theme$legend.text$size)
})

testthat::test_that("T1.6 If `fontsizes` is a `list`, the individual fonts are extracted and used.", {
  gg <- adtte %>%
    visR::estimate_KM("SEX") %>%
    visR::visr()

  theme <- visR::define_theme(fontsizes = list(
    "axis" = 12,
    "ticks" = 10,
    "legend_title" = 10,
    "legend_text" = 8
  ))

  gg <- gg %>% visR::apply_theme(theme)
  ggb <- ggplot2::ggplot_build(gg)

  testthat::expect_equal(theme$fontsizes$axis, ggb$plot$theme$axis.title.x$size)
  testthat::expect_equal(theme$fontsizes$axis, ggb$plot$theme$axis.title.y$size)
  testthat::expect_equal(theme$fontsizes$ticks, ggb$plot$theme$axis.text$size)
  testthat::expect_equal(theme$fontsizes$legend_title, ggb$plot$theme$legend.title$size)
  testthat::expect_equal(theme$fontsizes$legend_text, ggb$plot$theme$legend.text$size)
})

testthat::test_that("T1.7 The fontfamily applied through `visR::apply_theme()` is used in the resulting `ggplot` object.", {
  gg <- adtte %>%
    visR::estimate_KM("SEX") %>%
    visR::visr()

  theme <- visR::define_theme(fontfamily = "Helvetica")

  gg <- gg %>% visR::apply_theme(theme)
  ggb <- ggplot2::ggplot_build(gg)

  testthat::expect_equal(theme$fontfamily, ggb$plot$theme$text$family)
})

testthat::test_that("T1.8 If `grid` is a single `logical`, it is used for both major and minor grid.", {
  gg <- adtte %>%
    visR::estimate_KM("SEX") %>%
    visR::visr()

  theme_grid_false <- visR::define_theme(grid = FALSE) # Equal to major = minor = FALSE

  # Construct "grid = TRUE" case manually since visR::define_theme(grid = TRUE)
  # would result in "major = TRUE; minor = FALSE)" due to our opinionated position
  theme_grid_true <- theme_grid_false
  theme_grid_true$grid <- TRUE

  gg_grid_true <- gg %>% visR::apply_theme(theme_grid_true)
  gg_grid_false <- gg %>% visR::apply_theme(theme_grid_false)

  ggb_grid_true <- ggplot2::ggplot_build(gg_grid_true)
  ggb_grid_false <- ggplot2::ggplot_build(gg_grid_false)

  testthat::expect_true((inherits(ggb_grid_true$plot$theme$panel.grid.major, "element_line")) &
    (inherits(ggb_grid_true$plot$theme$panel.grid.minor, "element_line")))

  testthat::expect_true((inherits(ggb_grid_false$plot$theme$panel.grid.major, "element_blank")) &
    (inherits(ggb_grid_false$plot$theme$panel.grid.minor, "element_blank")))
})

testthat::test_that("T1.9 If `grid` is a named list containing 'major' and/or 'minor' as single `logical`s, these are used for their respective options.", {
  gg <- adtte %>%
    visR::estimate_KM("SEX") %>%
    visR::visr()

  theme_grid_none <- visR::define_theme(grid = list(
    "major" = FALSE,
    "minor" = FALSE
  ))
  theme_grid_only_minor <- visR::define_theme(grid = list(
    "major" = FALSE,
    "minor" = TRUE
  ))
  theme_grid_minor_and_major <- visR::define_theme(grid = list(
    "major" = TRUE,
    "minor" = TRUE
  ))

  gg_grid_none <- gg %>% visR::apply_theme(theme_grid_none)
  gg_grid_only_minor <- gg %>% visR::apply_theme(theme_grid_only_minor)
  gg_grid_minor_and_major <- gg %>% visR::apply_theme(theme_grid_minor_and_major)

  ggb_grid_none <- ggplot2::ggplot_build(gg_grid_none)
  ggb_grid_only_minor <- ggplot2::ggplot_build(gg_grid_only_minor)
  ggb_grid_minor_and_major <- ggplot2::ggplot_build(gg_grid_minor_and_major)

  testthat::expect_true((inherits(ggb_grid_none$plot$theme$panel.grid.major, "element_blank")) &
    (inherits(ggb_grid_none$plot$theme$panel.grid.minor, "element_blank")))

  testthat::expect_true((inherits(ggb_grid_only_minor$plot$theme$panel.grid.major, "element_blank")) &
    (inherits(ggb_grid_only_minor$plot$theme$panel.grid.minor, "element_line")))

  testthat::expect_true((inherits(ggb_grid_minor_and_major$plot$theme$panel.grid.major, "element_line")) &
    (inherits(ggb_grid_minor_and_major$plot$theme$panel.grid.minor, "element_line")))
})

testthat::test_that("T1.10 A warning when `grid` is a named list containing 'major' and/or 'minor' that are not single `logical`s.", {
  gg <- adtte %>%
    visR::estimate_KM("SEX") %>%
    visR::visr()

  theme_major_correct <- visR::define_theme(grid = list(
    "major" = TRUE,
    "minor" = "visR"
  ))
  theme_minor_correct <- visR::define_theme(grid = list(
    "major" = "visR",
    "minor" = TRUE
  ))

  testthat::expect_warning(gg %>% visR::apply_theme(theme_major_correct))
  testthat::expect_warning(gg %>% visR::apply_theme(theme_minor_correct))
})

testthat::test_that("T1.11 A warning when `grid` is a named list that does not contain 'major' and/or 'minor'.", {
  gg <- adtte %>%
    visR::estimate_KM("SEX") %>%
    visR::visr()

  theme <- visR::define_theme(grid = list(
    "major" = "visR",
    "minor" = "Rsiv"
  ))

  names(theme$grid) <- c("visR", "Rsiv")

  testthat::expect_warning(gg %>% visR::apply_theme(theme))
})

testthat::test_that("T1.12 The background applied through `visR::apply_theme()` is used in the resulting `ggplot` object.", {
  gg <- adtte %>%
    visR::estimate_KM("SEX") %>%
    visR::visr()

  theme <- visR::define_theme(bg = "transparent")

  gg <- gg %>% visR::apply_theme(theme)
  ggb <- ggplot2::ggplot_build(gg)

  testthat::expect_equal(theme$bg, ggb$plot$theme$panel.background$fill)
})

testthat::test_that("T1.13 The legend_position applied through `visR::apply_theme()` is used in the resulting `ggplot` object.", {
  gg <- adtte %>%
    visR::estimate_KM("SEX") %>%
    visR::visr()

  theme_top <- visR::define_theme(legend_position = "top")
  theme_right <- visR::define_theme(legend_position = "right")
  theme_bottom <- visR::define_theme(legend_position = "bottom")
  theme_left <- visR::define_theme(legend_position = "left")

  gg_top <- gg %>% visR::apply_theme(theme_top)
  gg_right <- gg %>% visR::apply_theme(theme_right)
  gg_bottom <- gg %>% visR::apply_theme(theme_bottom)
  gg_left <- gg %>% visR::apply_theme(theme_left)

  ggb_top <- ggplot2::ggplot_build(gg_top)
  ggb_right <- ggplot2::ggplot_build(gg_right)
  ggb_bottom <- ggplot2::ggplot_build(gg_bottom)
  ggb_left <- ggplot2::ggplot_build(gg_left)

  testthat::expect_equal(theme_top$legend_position, ggb_top$plot$theme$legend.position)
  testthat::expect_equal(theme_right$legend_position, ggb_right$plot$theme$legend.position)
  testthat::expect_equal(theme_bottom$legend_position, ggb_bottom$plot$theme$legend.position)
  testthat::expect_equal(theme_left$legend_position, ggb_left$plot$theme$legend.position)
})

testthat::test_that("T1.14 The legend_position defined in `visR::visr()` is used when not defined through `visR::apply_theme()`.", {
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

  gg_top <- gg_top %>% visR::apply_theme()
  gg_right <- gg_right %>% visR::apply_theme()
  gg_bottom <- gg_bottom %>% visR::apply_theme()
  gg_left <- gg_left %>% visR::apply_theme()

  ggb_top <- ggplot2::ggplot_build(gg_top)
  ggb_right <- ggplot2::ggplot_build(gg_right)
  ggb_bottom <- ggplot2::ggplot_build(gg_bottom)
  ggb_left <- ggplot2::ggplot_build(gg_left)

  testthat::expect_true("top" %in% ggb_top$plot$theme$legend.position)
  testthat::expect_true("right" %in% ggb_right$plot$theme$legend.position)
  testthat::expect_true("bottom" %in% ggb_bottom$plot$theme$legend.position)
  testthat::expect_true("left" %in% ggb_left$plot$theme$legend.position)
})

testthat::test_that("T1.15 If a stratum has no colour assigned, the default colour (grey50) is used.", {
  theme <- visR::define_theme(
    strata = list(
      "SEX" = list(
        "F" = NULL,
        "M" = "blue"
      ),
      "TRTA" = list(
        "Placebo" = "cyan",
        "Xanomeline High Dose" = "purple",
        "Xanomeline Low Dose" = "brown"
      )
    ),
    fontsizes = list(
      "axis" = 12,
      "ticks" = 10,
      "legend_title" = 10,
      "legend_text" = 8
    ),
    fontfamily = "Helvetica",
    grid = FALSE,
    bg = "transparent",
    legend_position = "top"
  )

  gg <- adtte %>%
    visR::estimate_KM(strata = "SEX") %>%
    visR::visr() %>%
    visR::apply_theme(theme)

  ggb <- ggplot2::ggplot_build(gg)

  testthat::expect_true("grey50" %in% unlist(unique(ggb$data[[1]]["fill"])))
  testthat::expect_true("blue" %in% unlist(unique(ggb$data[[1]]["fill"])))

  ## example 2
  theme <- visR::define_theme(
    strata = list("Sex, ph.ecog" = list(
      "Female, 0" = "red",
      "Male, 0" = "blue"
    ))
  )

  survobj <- survival::lung %>%
    dplyr::mutate(sex = as.factor(ifelse(sex == 1, "Male", "Female"))) %>%
    dplyr::mutate(status = status - 1) %>%
    dplyr::rename(Age = "age", Sex = "sex", Status = "status", Days = "time") %>%
    visR::estimate_KM(strata = c("Sex", "ph.ecog"), CNSR = "Status", AVAL = "Days")

  gg <- survobj %>%
    visR::visr() %>%
    visR::apply_theme(theme)

  ggb <- ggplot2::ggplot_build(gg)

  cols <- unlist(unique(ggb$data[[1]]["fill"]))
  testthat::expect_true("grey50" %in% cols)
  testthat::expect_true("red" %in% cols)
  testthat::expect_true("blue" %in% cols)
})

testthat::test_that("T1.16 If no strata colors can be mapped to the graph, the default visR palette is used as long as there are less than 15 strata levels.", {

  ## example 1
  theme <- visR::define_theme(
    strata = list(
      "Sex" = list(
        "Female" = "blue",
        "Male" = "red"
      ),
      "ph.ecog" = list(
        "0" = "cyan",
        "1" = "purple",
        "2" = "brown"
      )
    )
  )


  survobj <- survival::lung %>%
    dplyr::mutate(sex = as.factor(ifelse(sex == 1, "Male", "Female"))) %>%
    dplyr::mutate(status = status - 1) %>%
    dplyr::rename(Age = "age", Sex = "sex", Status = "status", Days = "time") %>%
    visR::estimate_KM(strata = c("ph.ecog", "Sex"), CNSR = "Status", AVAL = "Days")

  gg <- survobj %>%
    visR::visr() %>%
    visR::apply_theme(theme)

  ggb <- ggplot2::ggplot_build(gg)

  cols <- unlist(unique(ggb$data[[1]]["fill"]))
  names(cols) <- NULL

  testthat::expect_true(length(setdiff(c("#000000", "#490092", "#920000", "#009292", "#B66DFF", "#DBD100", "#FFB677"), cols)) == 0)

  ## example 2
  theme <- visR::define_theme()

  survobj <- survival::lung %>%
    dplyr::mutate(sex = as.factor(ifelse(sex == 1, "Male", "Female"))) %>%
    dplyr::mutate(status = status - 1) %>%
    dplyr::rename(Age = "age", Sex = "sex", Status = "status", Days = "time") %>%
    visR::estimate_KM(strata = NULL, CNSR = "Status", AVAL = "Days")

  gg <- survobj %>%
    visR::visr() %>%
    visR::apply_theme(theme)

  ggb <- ggplot2::ggplot_build(gg)

  cols <- unlist(unique(ggb$data[[1]]["fill"]))
  names(cols) <- NULL

  testthat::expect_true(length(setdiff(c("#000000"), cols)) == 0)
})

testthat::test_that("T1.17 If no strata colors can be mapped to the graph, the original colors are retained if there are more than 15 strata levels.", {

  ## not a relevant strata list, but is required to test the requirement easily
  theme <- visR::define_theme(
    strata = list(
      "Sex" = list(
        "Female" = "blue",
        "Male" = "red"
      ),
      "ph.ecog" = list(
        "0" = "cyan",
        "1" = "purple",
        "2" = "brown"
      )
    )
  )

  survobj <- survival::lung %>%
    dplyr::mutate(sex = as.factor(ifelse(sex == 1, "Male", "Female"))) %>%
    dplyr::mutate(status = status - 1) %>%
    dplyr::rename(Age = "age", Sex = "sex", Status = "status", Days = "time") %>%
    visR::estimate_KM(strata = c("Age", "pat.karno"), CNSR = "Status", AVAL = "Days")

  gg <- suppressWarnings(survobj %>%
    visR::visr() %>%
    visR::apply_theme(theme))

  ggb <- ggplot2::ggplot_build(gg)

  # Get expected colours
  # https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
  gg_colour_hue <- function(n) {
    hues <- seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }

  cols_expected <- gg_colour_hue(n = length(unique(names(survobj$strata))))

  cols <- unlist(unique(ggb$data[[1]]["fill"]))
  names(cols) <- NULL

  testthat::expect_true(length(cols_expected %in% cols) > 1)
})

testthat::test_that("T1.18 If no strata colors can be mapped to the graph, a warning about the presence of more than 15 strata levels.", {

  ## not a relevant strata list, but is required to test the requirement easily
  theme <- visR::define_theme(
    strata = list("Age*pat.karno" = list(
      "39, 90 " = "red",
      "40, 80 " = "blue",
      "41, 80 " = "blue",
      "42, 80 " = "blue",
      "43, 90 " = "blue",
      "44, 80 " = "blue",
      "44, 90 " = "blue",
      "44, 100" = "blue",
      "45, 100" = "blue",
      "46, 100" = "blue",
      "47, 90 " = "blue",
      "48, 60 " = "blue",
      "48, 80 " = "blue",
      "48, 90 " = "blue",
      "49, 60 " = "blue",
      "49, 70 " = "blue",
      "50, 60 " = "blue",
      "50, 80 " = "blue",
      "50, 100" = "blue"
    ))
  )


  survobj <- survival::lung %>%
    dplyr::mutate(sex = as.factor(ifelse(sex == 1, "Male", "Female"))) %>%
    dplyr::mutate(status = status - 1) %>%
    dplyr::rename(Age = "age", Sex = "sex", Status = "status", Days = "time") %>%
    visR::estimate_KM(strata = c("Age", "pat.karno"), CNSR = "Status", AVAL = "Days")

  testthat::expect_warning(survobj %>% visR::visr() %>% apply_theme(theme))
})

testthat::test_that("T1.19 The named list is used in the legend title.", {
  theme <- visR::define_theme(
    strata = list(
      "SEX" = list(
        "F" = NULL,
        "M" = "blue"
      ),
      "TRTA" = list(
        "Placebo" = "cyan",
        "Xanomeline High Dose" = "purple",
        "Xanomeline Low Dose" = "brown"
      )
    ),
    fontsizes = list(
      "axis" = 12,
      "ticks" = 10,
      "legend_title" = 10,
      "legend_text" = 8
    ),
    fontfamily = "Helvetica",
    grid = FALSE,
    bg = "transparent",
    legend_position = "top"
  )

  gg <- adtte %>%
    visR::estimate_KM(strata = "SEX") %>%
    visR::visr() %>%
    visR::apply_theme(theme)


  testthat::expect_equal(get_legend_title(gg), "SEX")

  ## example 2
  theme <- visR::define_theme(
    strata = list("Sex, ph.ecog" = list(
      "Female" = "red",
      "Male" = "blue"
    ))
  )

  survobj <- survival::lung %>%
    dplyr::mutate(sex = as.factor(ifelse(sex == 1, "Male", "Female"))) %>%
    dplyr::mutate(status = status - 1) %>%
    dplyr::rename(Age = "age", Sex = "sex", Status = "status", Days = "time") %>%
    visR::estimate_KM(strata = c("Sex"), CNSR = "Status", AVAL = "Days")

  gg <- survobj %>%
    visR::visr() %>%
    visR::apply_theme(theme)

  testthat::expect_equal(get_legend_title(gg), "Sex, ph.ecog")
})

# testthat::test_that("T1.18 When the strata requires more colours than the visR palette holds, the default ggplot2 ones are chosen.", {
#
#   theme <- visR::define_theme(strata = list("TRTDUR" = list("F" = "red",
#                                                          "M" = "blue")),
#                               fontsizes = list("axis" = 12,
#                                                "ticks" = 10,
#                                                "legend_title" = 10,
#                                                "legend_text" = 8),
#                               fontfamily = "Helvetica",
#                               grid = FALSE,
#                               bg = "transparent",
#                               legend_position = "top")
#
#   adtte2 <- adtte
#   adtte2$TRTDUR <- round(adtte$TRTDUR/10)
#   gg <- adtte2 %>%
#     visR::estimate_KM(strata = "TRTDUR") %>%
#     visR::visr() %>%
#     visR::apply_theme(theme)
#
#   cols_expected <- gg_colour_hue(length(unique(adtte2$TRTDUR)))
#
#   # Get used colours and strip off the alpha part
#   ggb <- ggplot2::ggplot_build(gg)
#   cols_observed <- unlist(unique(ggb$data[[1]]["fill"]))
#   cols_observed <- gsub(".{2}$", "", cols_observed)
#   names(cols_observed) <- NULL
#
#   testthat::expect_equal(cols_expected, cols_observed)
#
# })

# END OF CODE -------------------------------------------------------------
