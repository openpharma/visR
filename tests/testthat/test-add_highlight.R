#' @title Specifications test-add_highlight.R
#' @section Last updated by: Daniel Sjoberg (danield.sjoberg@@gmail.com)
#' @section Last update date: 2022-04-20T04:47:32
#'
#' @section List of tested specifications
#' T1. The function modifies a `ggsurvfit` object and returns it.
#' T1.1 No error when `add_highlight` is called on a `ggsurvfit` object.
#' T1.2 An error when `add_highlight` is called without a plot.
#' T1.3 An error when `add_highlight` is called on a non-`ggplot` object
#' T1.4 An error when `add_highlight` is called on a `ggplot` but non-`ggsurvfit` object.
#' T1.5 The function returns a modified object of type `ggsurvfit`.
#' T2. No errors when one or more strata are highlighted with default parameters.
#' T2.1 No error when `strata` is a character string found in the plot strata.
#' T2.2 An error when `strata` is a character string not found in the plot strata.
#' T2.3 No error when `strata` is a `list` with a single non-character element.
#' T2.4 No error when `strata` is a `list` or `vector` of character strings found in the plot strata.
#' T2.5 An error when `strata` is a `list` or `vector` that holds non-character-string elements.
#' T2.6 An error when `strata` is not a character string or a list.
#' T2.7 An error when `strata` is `NULL` or missing.
#' T2.8 An error when `strata` is not a character string or a list.
#' T3. The opacity of the background strata can be changed through `bg_alpha`.
#' T3.1 No error when `bg_alpha` is a `numberic`.
#' T3.2 An error when `bg_alpha` is a not a `numberic`.
#' T3.3 An error when `bg_alpha` is outside of [0, 1].
#' T3.4 The alpha of the background strata changes with `bg_alpha`.
#' T4. The function modifies the underlying data structure than is interpreted during plotting.
#' T4.1 The function adds the alpha channel to the hex-encoded colour.
#' T4.2 The function also reduces the alpha value of the confidence intervals introduced by `add_CI`.

# Requirement T1 ----------------------------------------------------------

testthat::context("add_highlight - T1. The function modifies a `ggsurvfit` object and returns it.")

testthat::test_that("T1.1 No error when `add_highlight` is called on a `ggsurvfit` object.", {
  gg <- adtte %>%
    visR::estimate_KM(strata = "TRTP") %>%
    visR::visr()

  testthat::expect_true(inherits(gg, "ggsurvfit"))

  gg %>%
    visR::add_highlight(strata = "Placebo") %>%
    testthat::expect_error(NA)
})

testthat::test_that("T1.2 An error when `add_highlight` is called without a plot.", {
  visR::add_highlight() %>% testthat::expect_error()
})

testthat::test_that("T1.3 An error when `add_highlight` is called on a non-`ggplot` object", {
  visR::add_highlight(NULL) %>%
    testthat::expect_error()

  visR::add_highlight("visR") %>%
    testthat::expect_error()

  visR::add_highlight(1) %>%
    testthat::expect_error()

  model <- stats::lm(data = adtte, "AGE ~ TRTDUR")
  class(model) <- c(class(model), "ggsurvfit")

  visR::add_highlight(model) %>%
    testthat::expect_error()
})

testthat::test_that("T1.4 An error when `add_highlight` is called on a `ggplot` but non-`ggsurvfit` object.", {
  gg <- adtte %>%
    ggplot2::ggplot(ggplot2::aes(x = AGE, y = TRTDUR)) +
    ggplot2::geom_point()

  testthat::expect_true(inherits(gg, "ggplot"))
  testthat::expect_false(inherits(gg, "ggsurvfit"))

  gg %>%
    add_highlight() %>%
    testthat::expect_error()
})

testthat::test_that("T1.5 The function returns a modified object of type `ggsurvfit`.", {
  gg <- adtte %>%
    visR::estimate_KM(strata = "TRTP") %>%
    visR::visr()

  testthat::expect_true(inherits(gg, "ggsurvfit"))

  gg_with_highlight <- gg %>% add_highlight("Placebo")

  testthat::expect_true(inherits(gg_with_highlight, "ggsurvfit"))

  testthat::expect_false(base::identical(gg, gg_with_highlight))
})

# Requirement T2 ---------------------------------------------------------------

testthat::context("add_highlight - T2. No errors when one or more strata are highlighted with default parameters.")

testthat::test_that("T2.1 No error when `strata` is a character string found in the plot strata.", {
  gg <- adtte %>%
    visR::estimate_KM(strata = "TRTP") %>%
    visR::visr()

  gg %>%
    visR::add_highlight(strata = "Placebo") %>%
    testthat::expect_error(NA)

  testthat::skip_on_cran()
  gg %>%
    visR::add_highlight(strata = "Placebo") %>%
    vdiffr::expect_doppelganger(title = "add_highlight_T2_1_no_error_when_strata_is_string")
})

testthat::test_that("T2.2 An error when `strata` is a character string not found in the plot strata.", {
  gg <- adtte %>%
    visR::estimate_KM(strata = "TRTP") %>%
    visR::visr()

  expected_error <- "The strata you specified has not been found in the provided plot.\n  Available strata: Placebo, Xanomeline High Dose, Xanomeline Low Dose\n  Please adjust and rerun."

  gg %>%
    visR::add_highlight(strata = "visR") %>%
    testthat::expect_error(expected_error)
})

testthat::test_that("T2.3 No error when `strata` is a `list` with a single non-character element.", {
  expected_error <- "A 'strata' must be either a single character string or a list of them."

  adtte %>%
    visR::estimate_KM("SEX") %>%
    visR::visr() %>%
    visR::add_highlight(list(1)) %>%
    testthat::expect_error(expected_error)
})

testthat::test_that("T2.4 No error when `strata` is a `list` or `vector` of character strings found in the plot strata.", {
  strata_list <- list("Placebo", "Xanomeline Low Dose")
  strata_vector <- c("Placebo", "Xanomeline Low Dose")

  gg <- adtte %>%
    visR::estimate_KM(strata = "TRTP") %>%
    visR::visr()

  gg %>%
    visR::add_highlight(strata = strata_list) %>%
    testthat::expect_error(NA)

  gg %>%
    visR::add_highlight(strata = strata_vector) %>%
    testthat::expect_error(NA)

  testthat::skip_on_cran()

  gg %>%
    visR::add_highlight(strata = strata_list) %>%
    vdiffr::expect_doppelganger(title = "add_highlight_T2_4_no_error_when_strata_is_string_list")

  gg %>%
    visR::add_highlight(strata = strata_vector) %>%
    vdiffr::expect_doppelganger(title = "add_highlight_T2_4_no_error_when_strata_is_string_vec") # 'vector' shortened because of tarball size
})

testthat::test_that("T2.5 An error when `strata` is a `list` or `vector` that holds non-character-string elements.", {
  strata <- c(1, 2, 3)

  gg <- adtte %>%
    visR::estimate_KM(strata = "TRTP") %>%
    visR::visr()

  expected_error <- "When 'strata' is a list, all elements must be character strings."

  gg %>%
    visR::add_highlight(strata = strata) %>%
    testthat::expect_error(expected_error)
})

testthat::test_that("T2.6 An error when `strata` is not a character string or a list.", {
  gg <- adtte %>%
    visR::estimate_KM(strata = "TRTP") %>%
    visR::visr()

  expected_error <- "A 'strata' must be either a single character string or a list of them."

  gg %>%
    visR::add_highlight(strata = NA) %>%
    testthat::expect_error(expected_error)

  gg %>%
    visR::add_highlight(strata = 1) %>%
    testthat::expect_error(expected_error)
})

testthat::test_that("T2.7 An error when `strata` is `NULL` or missing.", {
  gg <- adtte %>%
    visR::estimate_KM(strata = "TRTP") %>%
    visR::visr()

  gg %>%
    visR::add_highlight(strata = NULL) %>%
    testthat::expect_error()

  gg %>%
    visR::add_highlight() %>%
    testthat::expect_error()
})

testthat::test_that("T2.8 An error when `strata` is not a character string or a list.", {
  gg <- adtte %>%
    visR::estimate_KM(strata = "TRTP") %>%
    visR::visr()

  expected_error <- "A 'strata' must be either a single character string or a list of them."

  gg %>%
    visR::add_highlight(strata = NA) %>%
    testthat::expect_error(expected_error)

  gg %>%
    visR::add_highlight(strata = 1) %>%
    testthat::expect_error(expected_error)
})

# Requirement T3 ---------------------------------------------------------------

testthat::context("add_highlight - T3. The opacity of the background strata can be changed through `bg_alpha`.")

testthat::test_that("T3.1 No error when `bg_alpha` is a `numberic`.", {
  gg <- adtte %>%
    visR::estimate_KM(strata = "TRTP") %>%
    visR::visr()

  gg %>%
    visR::add_highlight(
      strata = "Placebo",
      bg_alpha = 0.2
    ) %>%
    testthat::expect_error(NA)
})

testthat::test_that("T3.2 An error when `bg_alpha` is a not a `numberic`.", {
  gg <- adtte %>%
    visR::estimate_KM(strata = "TRTP") %>%
    visR::visr()

  expected_error <- "The `bg_alpha` must be a `numeric`."

  gg %>%
    visR::add_highlight(
      strata = "Placebo",
      bg_alpha = "visR"
    ) %>%
    testthat::expect_error(expected_error)
})

testthat::test_that("T3.3 An error when `bg_alpha` is outside of [0, 1].", {
  gg <- adtte %>%
    visR::estimate_KM(strata = "TRTP") %>%
    visR::visr()

  expected_error <- "The `bg_alpha` must be a numeric value between 0 and 1."

  gg %>%
    visR::add_highlight(
      strata = "Placebo",
      bg_alpha = -1
    ) %>%
    testthat::expect_error(expected_error)

  gg %>%
    visR::add_highlight(
      strata = "Placebo",
      bg_alpha = 2
    ) %>%
    testthat::expect_error(expected_error)
})

testthat::test_that("T3.4 The alpha of the background strata changes with `bg_alpha`.", {
  gg <- adtte %>%
    visR::estimate_KM(strata = "TRTP") %>%
    visR::visr()

  testthat::skip_on_cran()

  gg %>%
    visR::add_highlight(
      strata = "Placebo",
      bg_alpha = 0
    ) %>%
    vdiffr::expect_doppelganger(title = "add_highlight_T3_4_bg_alpha_is_0")

  gg %>%
    visR::add_highlight(
      strata = "Placebo",
      bg_alpha = 0.4
    ) %>%
    vdiffr::expect_doppelganger(title = "add_highlight_T3_4_bg_alpha_is_0_4")

  gg %>%
    visR::add_highlight(
      strata = "Placebo",
      bg_alpha = 1.0
    ) %>%
    vdiffr::expect_doppelganger(title = "add_highlight_T3_4_bg_alpha_is_1")
})

# Requirement T4 ---------------------------------------------------------------

testthat::context("add_highlight - T4. The function modifies the underlying data structure than is interpreted during plotting.")

testthat::test_that("T4.1 The function adds the alpha channel to the hex-encoded colour.", {
  gg <- adtte %>%
    visR::estimate_KM(strata = "TRTP") %>%
    visR::visr()

  gg_with_highlight <- gg %>% visR::add_highlight(strata = "Placebo")

  gg_data <- ggplot2::ggplot_build(gg)$data[[1]]
  gg_with_highlight_data <- ggplot2::ggplot_build(gg_with_highlight)$data[[1]]

  gg_colours <- gg_data$colour %>% unique()
  gg_with_highlight_colours <- gg_with_highlight_data$colour %>% unique()

  # Remove #RRGGBB from #RRGGBBAA colours
  alphas <- unlist(lapply(gg_colours, function(c) {
    gsub(c, "", gg_with_highlight_colours[gg_colours == c])
  }))

  testthat::expect_identical(alphas, c("FF", "33", "33"))
})

testthat::test_that("T4.2 The function also reduces the alpha value of the confidence intervals introduced by `add_CI`.", {
  ci_alpha <- 0.5
  bg_alpha <- 0.2

  gg <- adtte %>%
    visR::estimate_KM(strata = "TRTP") %>%
    visR::visr() %>%
    visR::add_CI(alpha = ci_alpha)

  gg_with_highlight <- gg %>%
    visR::add_highlight(strata = "Placebo", bg_alpha = bg_alpha)

  gg_CI_data <- ggplot2::ggplot_build(gg)$data[[2]]
  gg_with_highlight_CI_data <- ggplot2::ggplot_build(gg_with_highlight)$data[[2]]

  gg_CI_fills <- gg_CI_data$fill %>% unique()
  gg_with_highlight_CI_fills <- gg_with_highlight_CI_data$fill %>% unique()

  # Remove #RRGGBB from #RRGGBBAA colours
  gg_CI_fills_numeric <- gsub("#[A-Z0-9]{6}", "", gg_CI_fills) %>%
    sapply(function(s) {
      .convert_alpha(hex_alpha = s)
    }) %>%
    as.vector()

  gg_with_highlight_CI_fills_numeric <- gsub(
    pattern = "#[A-Z0-9]{6}",
    replacement = "",
    x = gg_with_highlight_CI_fills
  ) %>%
    sapply(function(s) {
      .convert_alpha(hex_alpha = s)
    }) %>%
    as.vector()

  testthat::expect_equal(
    gg_CI_fills_numeric,
    rep(ci_alpha, length(gg_CI_fills_numeric))
  )

  # To not over-engineer the test here, we take for granted that the foreground
  # strata is the first of the three in the evaluation order
  testthat::expect_equal(gg_with_highlight_CI_fills_numeric[1], ci_alpha)
  testthat::expect_equal(gg_with_highlight_CI_fills_numeric[2], ci_alpha * bg_alpha)
  testthat::expect_equal(gg_with_highlight_CI_fills_numeric[3], ci_alpha * bg_alpha)
})

# END OF CODE -------------------------------------------------------------
