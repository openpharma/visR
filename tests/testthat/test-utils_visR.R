#' @title Specifications test-utils_visR.R
#' @section Last updated by: Tim Treis (tim.treis@@outlook.de)
#' @section Last update date: 2022-02-09T15:22:32
#'
#' @section List of tested specifications
#' T1. `align_plots()` accepts a list of `ggplot` objects.
#' T1.1 No error when a list of `ggplot` objects is passed.
#' T1.2 An error when `NULL` is passed.
#' T1.3 An error when a list containing non-`ggplot` objects is passed.
#' T2. `align_plots()` aligns multiple `ggplot` objects, taking the legend into account.
#' T2.1 Columns are added to the grob-converted plot.
#' T2.2 Equal widths are assigned to all grob-converted plots.
#' T2.3 The y-axis label of the main plot is aligned to the y-axis label.
#' T2.4 The final plot shows aligned plots taking the legend into account.
#' T3. The function `legendopts()` translates the input to a `ggplot2`-compatible list.
#' T3.1 No error when no arguments are specified.
#' T3.2 A list is returned when no arguments are specified.
#' T3.3 No error when `legend_position` is 'bottom', 'right', 'top', 'left' or 'none'.
#' T3.4 When `legend_position` is 'bottom' and `legend_orientation` is `NULL`, the parameters in the returned object anchor it bottom-centered.
#' T3.5 When `legend_position` is 'right' and `legend_orientation` is `NULL`, the parameters in the returned object anchor it right-centered.
#' T3.6 When `legend_position` is 'top' and `legend_orientation` is `NULL`, the parameters in the returned object anchor it top-centered.
#' T3.7 When `legend_position` is 'left' and `legend_orientation` is `NULL`, the parameters in the returned object anchor it left-centered.
#' T3.8 When `legend_position` is 'none', the parameters in the returned object disable the legend.
#' T3.9 When `legend_orientation` is not `NULL`, it is used as `$leg_opts$orientation` in the returned object.
#' T3.10 When `legend_position` is not a character, but a vector with at least length 2, the first two elements are used as x/y positions for the legend.
#' T3.11 A warning when `legend_position` is a vector with a length greater than 2.
#' T3.12 An error when `legend_position` is not a `character` or a vector with a length of at least 2.
#' T4. `.convert_alpha` can convert hex-encoded alpha values between its two representations.
#' T4.1 A two-letter character string is returned when `numeric_alpha` is specified but not `hex_alpha`.
#' T4.2 A numeric is returned when `numeric_alpha` is not specified but `hex_alpha`.
#' T4.3 An error when `numeric_alpha` and `hex_alpha` are both specified.
#' T4.4 Ab error when neither `numeric_alpha` nor `hex_alpha` are specified.
#' T4.5 No error when `numeric_alpha` is a `numeric` between 0 and 1.
#' T4.6 An error when `numeric_alpha` is a `numeric` outside of [0, 1].
#' T4.7 An error when `numeric_alpha` is not a `numeric`.
#' T4.8 No error when `hex_alpha` is a two-letter character string.
#' T4.9 An error when `hex_alpha` is a character string longer than two letters.
#' T4.10 When `numeric_alpha` is `NULL` or `NA`, the hex-encoded alpha = 1 (00) is returned.
#' T5. `.replace_hex_alpha` modified the alpha value of a hex-encoded colour.
#' T5.1 No error when `colour` is a #RRGGBBAA string and `new_alpha` is a two-letter string.
#' T5.2 An error when either `colour` or `new_alpha` or none of both are specified.
#' T5.3 An error when `new_alpha` is not a two-character string.
#' T5.4 An error when `colour` is not a string.
#' T5.5 An error when `colour` is a string but not in the format of #RRGGBBAA.
#' T5.6 The function replaces the AA part #RRGGBBAA in `colour` with `new_alpha`.
#' T6. `.get_alpha_from_hex_colour` extracts the numerical value of a hex-encoded colour.
#' T6.1 A numerical value is returned when `hex_colour` is a #RRGGBBAA `character` string.
#' T6.2 An error when `hex_color` is misisng
#' T6.3 An error when `hex_color` is not a character string.
#' T6.4 An error when `hex_color` is not a character string with length 9.
#' T6.5 An error when `hex_color` is a character string with length 9 but doesn't have # at the first position.

# Requirement T1 ----------------------------------------------------------

testthat::context("utils_visr - T1. `align_plots()` accepts a list of `ggplot` objects.")

testthat::test_that("T1.1 No error when a list of `ggplot` objects is passed.", {
  gg_sex <- adtte %>%
    visR::estimate_KM("SEX") %>%
    visR::visr()

  gg_trtp <- adtte %>%
    visR::estimate_KM("TRTP") %>%
    visR::visr()

  gg_list <- list(gg_sex, gg_trtp)

  testthat::expect_error(visR::align_plots(gg_list), NA)
})

testthat::test_that("T1.2 An error when `NULL` is passed.", {
  testthat::expect_error(visR::align_plots())
  testthat::expect_error(visR::align_plots(pltlist = NULL))
})

testthat::test_that("T1.3 An error when a list containing non-`ggplot` objects is passed.", {
  gg_sex <- adtte %>%
    visR::estimate_KM("SEX") %>%
    visR::visr()

  gg_trtp <- adtte %>%
    visR::estimate_KM("TRTP") %>%
    visR::visr()

  gg_list <- list(gg_sex, gg_trtp, 3)

  testthat::expect_error(visR::align_plots(gg_list))
})

# Requirement T2 ---------------------------------------------------------------

testthat::context("utils_visr - T2. `align_plots()` aligns multiple `ggplot` objects, taking the legend into account.")

testthat::test_that("T2.1 Columns are added to the grob-converted plot.", {
  # From ggplot2 3.5.0 onwards ggplots have stable gtable dimensions with
  # regards to legend placement
  skip_if(utils::packageVersion("ggplot2") >= "3.5.0")

  gg_sex <- adtte %>%
    visR::estimate_KM("SEX") %>%
    visR::visr()

  gg_trtp <- adtte %>%
    visR::estimate_KM("TRTP") %>%
    visR::visr(legend = "none")


  pltlist <- list(gg_sex, gg_trtp) %>%
    visR::align_plots()

  grob_orig <- ggplot2::ggplotGrob(gg_trtp)

  testthat::expect_false(dim(grob_orig)[2] == dim(pltlist[[2]])[2])
  testthat::expect_true(dim(pltlist[[1]])[2] == dim(pltlist[[2]])[2])
})

testthat::test_that("T2.2 Equal widths are assigned to all grob-converted plots.", {
  gg_sex <- adtte %>%
    visR::estimate_KM("SEX") %>%
    visR::visr()

  gg_trtp <- adtte %>%
    visR::estimate_KM("TRTP") %>%
    visR::visr(legend = "none")


  pltlist <- list(gg_sex, gg_trtp) %>%
    visR::align_plots()

  testthat::expect_true(identical(pltlist[[1]]$widths, pltlist[[2]]$widths))
})

testthat::test_that("T2.3 The y-axis label of the main plot is aligned to the y-axis label.", {
  gg_sex_trtp <- adtte %>%
    visR::estimate_KM(strata = c("SEX", "TRTP")) %>%
    visR::visr(legend_position = "none") %>%
    add_risktable(group = "statlist")

  testthat::skip_on_cran()
  gg_sex_trtp %>%
    vdiffr::expect_doppelganger(title = "utils_visr_T2_3_yaxis_labels_aligned_when_no_legend")
})

testthat::test_that("T2.4 The final plot shows aligned plots taking the legend into account.", {
  gg_sex_trtp <- adtte %>%
    visR::estimate_KM(strata = c("SEX", "TRTP")) %>%
    visR::visr() %>%
    add_risktable(group = "statlist")

  testthat::skip_on_cran()
  gg_sex_trtp %>%
    vdiffr::expect_doppelganger(title = "utils_visr_T2_4_yaxis_labels_aligned_when_legend")
})

# Requirement T3 ---------------------------------------------------------------

testthat::context("utils_visr - T3. The function `legendopts()` translates the input to a `ggplot2`-compatible list.")

testthat::test_that("T3.1 No error when no arguments are specified.", {
  testthat::expect_error(visR:::legendopts(), NA)
})

testthat::test_that("T3.2 A list is returned when no arguments are specified.", {
  opts <- visR:::legendopts()

  testthat::expect_true(inherits(opts, "list"))
})

testthat::test_that("T3.3 No error when `legend_position` is 'bottom', 'right', 'top', 'left' or 'none'.", {
  for (pos in c("bottom", "right", "top", "left", "none")) {
    testthat::expect_error(visR:::legendopts(legend_position = pos), NA)
  }
})

testthat::test_that("T3.4 When `legend_position` is 'bottom' and `legend_orientation` is `NULL`, the parameters in the returned object anchor it bottom-centered.", {
  legend <- visR:::legendopts(
    legend_position = "bottom",
    legend_orientation = NULL
  )

  testthat::expect_true(legend$leg_opts$xanchor == "center")
  testthat::expect_true(legend$leg_opts$x == 0.5)
  testthat::expect_true(legend$leg_opts$y == -0.2)
  testthat::expect_true(legend$leg_opts$orientation == "h")
  testthat::expect_true(legend$showlegend == TRUE)
})

testthat::test_that("T3.5 When `legend_position` is 'right' and `legend_orientation` is `NULL`, the parameters in the returned object anchor it right-centered.", {
  legend <- visR:::legendopts(
    legend_position = "right",
    legend_orientation = NULL
  )

  testthat::expect_true(legend$leg_opts$yanchor == "center")
  testthat::expect_true(legend$leg_opts$x == 1.2)
  testthat::expect_true(legend$leg_opts$y == 0.5)
  testthat::expect_true(legend$leg_opts$orientation == "v")
  testthat::expect_true(legend$showlegend == TRUE)
})

testthat::test_that("T3.6 When `legend_position` is 'top' and `legend_orientation` is `NULL`, the parameters in the returned object anchor it top-centered.", {
  legend <- visR:::legendopts(
    legend_position = "top",
    legend_orientation = NULL
  )

  testthat::expect_true(legend$leg_opts$xanchor == "center")
  testthat::expect_true(legend$leg_opts$x == 0.5)
  testthat::expect_true(legend$leg_opts$y == 1.2)
  testthat::expect_true(legend$leg_opts$orientation == "h")
  testthat::expect_true(legend$showlegend == TRUE)
})

testthat::test_that("T3.7 When `legend_position` is 'left' and `legend_orientation` is `NULL`, the parameters in the returned object anchor it left-centered.", {
  legend <- visR:::legendopts(
    legend_position = "left",
    legend_orientation = NULL
  )

  testthat::expect_true(legend$leg_opts$yanchor == "center")
  testthat::expect_true(legend$leg_opts$x == -1.0)
  testthat::expect_true(legend$leg_opts$y == 0.5)
  testthat::expect_true(legend$leg_opts$orientation == "v")
  testthat::expect_true(legend$showlegend == TRUE)
})

testthat::test_that("T3.8 When `legend_position` is 'none', the parameters in the returned object disable the legend.", {
  legend <- visR:::legendopts(
    legend_position = "none",
    legend_orientation = NULL
  )

  testthat::expect_true(is.null(legend$leg_opts))
  testthat::expect_false(legend$showlegend)
})

testthat::test_that("T3.9 When `legend_orientation` is not `NULL`, it is used as `$leg_opts$orientation` in the returned object.", {
  for (orientation in c(1, "visR", c(1, 2, 3))) {
    tmp <- visR:::legendopts(legend_orientation = orientation)

    testthat::expect_equal(orientation, tmp$leg_opts$orientation)
  }
})

testthat::test_that("T3.10 When `legend_position` is not a character, but a vector with at least length 2, the first two elements are used as x/y positions for the legend.", {
  suppressWarnings(legend <- visR:::legendopts(legend_position = c(1, 2)))

  testthat::expect_true(legend$leg_opts$x == 1)
  testthat::expect_true(legend$leg_opts$y == 2)
})

testthat::test_that("T3.11 A warning when `legend_position` is a vector with a length greater than 2.", {
  expected_warning <- "The provided vector for the legend position contains more than 2 elements, only using the first two."
  visR:::legendopts(legend_position = c(1, 2, 3)) %>%
    testthat::expect_warning(expected_warning)
})

testthat::test_that("T3.12 An error when `legend_position` is not a `character` or a vector with a length of at least 2.", {
  visR:::legendopts(legend_position = 1) %>% testthat::expect_error()
  visR:::legendopts(legend_position = c(1)) %>% testthat::expect_error()
  visR:::legendopts(legend_position = c()) %>% testthat::expect_error()
})


# Requirement T4 ---------------------------------------------------------------

testthat::context("utils_visr - T4. `.convert_alpha` can convert hex-encoded alpha values between its two representations.")

testthat::test_that("T4.1 A two-letter character string is returned when `numeric_alpha` is specified but not `hex_alpha`.", {
  visR:::.convert_alpha(numeric_alpha = 0.5) %>%
    testthat::expect_error(NA)

  res <- visR:::.convert_alpha(numeric_alpha = 0.5)

  testthat::expect_true(nchar(res) == 2)
  testthat::expect_true(inherits(res, "character"))
})

testthat::test_that("T4.2 A numeric is returned when `numeric_alpha` is not specified but `hex_alpha`.", {
  visR:::.convert_alpha(hex_alpha = "FF") %>%
    testthat::expect_error(NA)

  testthat::expect_true(inherits(visR:::.convert_alpha(hex_alpha = "FF"), "numeric"))
})

testthat::test_that("T4.3 An error when `numeric_alpha` and `hex_alpha` are both specified.", {
  expected_error <- "Please choose either `numeric_alpha` or `hex_alpha`."

  visR:::.convert_alpha(numeric_alpha = 0.5, hex_alpha = "FF") %>%
    testthat::expect_error(expected_error)
})

testthat::test_that("T4.4 Ab error when neither `numeric_alpha` nor `hex_alpha` are specified.", {
  expected_error <- "Either `numeric_alpha` or `hex_alpha` has to be specified."

  visR:::.convert_alpha() %>%
    testthat::expect_error(expected_error)
})

testthat::test_that("T4.5 No error when `numeric_alpha` is a `numeric` between 0 and 1.", {
  visR:::.convert_alpha(numeric_alpha = 0.5) %>%
    testthat::expect_error(NA)
})

testthat::test_that("T4.6 An error when `numeric_alpha` is a `numeric` outside of [0, 1].", {
  expected_error <- "Please enter a numeric value between 0 and 1 for `numeric_alpha`."

  visR:::.convert_alpha(numeric_alpha = -0.5) %>%
    testthat::expect_error(expected_error)

  visR:::.convert_alpha(numeric_alpha = 1.5) %>%
    testthat::expect_error(expected_error)
})

testthat::test_that("T4.7 An error when `numeric_alpha` is not a `numeric`.", {
  expected_error <- "Please enter a numeric value between 0 and 1 for `numeric_alpha`."

  visR:::.convert_alpha(numeric_alpha = "visR") %>%
    testthat::expect_error(expected_error)
})

testthat::test_that("T4.8 No error when `hex_alpha` is a two-letter character string.", {
  visR:::.convert_alpha(hex_alpha = "FF") %>%
    testthat::expect_error(NA)
})

testthat::test_that("T4.9 An error when `hex_alpha` is a character string longer than two letters.", {
  expected_error <- "Please specify a two-letter character string for `hex_alpha`."

  visR:::.convert_alpha(hex_alpha = "visR") %>%
    testthat::expect_error(expected_error)
})

testthat::test_that("T4.10 When `numeric_alpha` is `NULL` or `NA`, the hex-encoded alpha = 1 (00) is returned.", {
  visR:::.convert_alpha(numeric_alpha = NULL) %>%
    testthat::expect_identical("00")

  visR:::.convert_alpha(numeric_alpha = NA) %>%
    testthat::expect_identical("00")
})

# Requirement T5 ---------------------------------------------------------------

testthat::context("utils_visr - T5. `.replace_hex_alpha` modified the alpha value of a hex-encoded colour.")

testthat::test_that("T5.1 No error when `colour` is a #RRGGBBAA string and `new_alpha` is a two-letter string.", {
  visR:::.replace_hex_alpha(colour = "#FFFFFF00", new_alpha = "FF") %>%
    testthat::expect_error(NA)
})

testthat::test_that("T5.2 An error when either `colour` or `new_alpha` or none of both are specified.", {
  expected_error <- "Please provide a `colour` and a `new_alpha` in hex representation as strings."

  visR:::.replace_hex_alpha(colour = "#FFFFFF00") %>%
    testthat::expect_error(expected_error)

  visR:::.replace_hex_alpha(new_alpha = "FF") %>%
    testthat::expect_error(expected_error)

  visR:::.replace_hex_alpha() %>%
    testthat::expect_error(expected_error)
})

testthat::test_that("T5.3 An error when `new_alpha` is not a two-character string.", {
  expected_error <- "Please provide a two-character string for the hex representation of the new alpha."

  visR:::.replace_hex_alpha(colour = "#FFFFFF00", new_alpha = 1) %>%
    testthat::expect_error(expected_error)

  visR:::.replace_hex_alpha(colour = "#FFFFFF00", new_alpha = "visR") %>%
    testthat::expect_error(expected_error)
})

testthat::test_that("T5.4 An error when `colour` is not a string.", {
  expected_error <- "Please provide a hex colour as a string."

  visR:::.replace_hex_alpha(colour = 1, new_alpha = "FF") %>%
    testthat::expect_error(expected_error)
})

testthat::test_that("T5.5 An error when `colour` is a string but not in the format of #RRGGBBAA.", {
  expected_error <- "Please provide a hex colour in the format #RRGGBBAA."

  # Covers case of too long strings
  visR:::.replace_hex_alpha(colour = "this_string_is_too_long", new_alpha = "FF") %>%
    testthat::expect_error(expected_error)

  # Covers case of nchar == 9 but no # in first position
  visR:::.replace_hex_alpha(colour = "visRvisRv", new_alpha = "FF") %>%
    testthat::expect_error(expected_error)
})

testthat::test_that("T5.6 The function replaces the AA part #RRGGBBAA in `colour` with `new_alpha`.", {
  colour <- "#FFFFFFAA"
  new_alpha <- "00"

  new_colour <- visR:::.replace_hex_alpha(colour = colour, new_alpha = new_alpha)

  testthat::expect_equal(substr(colour, 1, 7), substr(new_colour, 1, 7))
  testthat::expect_equal(new_alpha, substr(new_colour, 8, 9))
})

# Requirement T6 ---------------------------------------------------------------

testthat::context("utils_visr - T6. `.get_alpha_from_hex_colour` extracts the numerical value of a hex-encoded colour.")

testthat::test_that("T6.1 A numerical value is returned when `hex_colour` is a #RRGGBBAA `character` string.", {
  visR:::.get_alpha_from_hex_colour(hex_colour = "#FFFFFF04") %>%
    testthat::expect_error(NA)

  alpha <- visR:::.get_alpha_from_hex_colour(hex_colour = "#FFFFFF04")
  testthat::expect_true(inherits(alpha, "numeric"))
})

testthat::test_that("T6.2 An error when `hex_color` is misisng", {
  expected_error <- "Please provide a colour in hex representation as a string for `hex_colour`."

  visR:::.get_alpha_from_hex_colour() %>%
    testthat::expect_error(expected_error)
})

testthat::test_that("T6.3 An error when `hex_color` is not a character string.", {
  expected_error <- "Please provide a colour in hex representation as a string for `hex_colour`."

  visR:::.get_alpha_from_hex_colour(hex_colour = 1) %>%
    testthat::expect_error(expected_error)
})

testthat::test_that("T6.4 An error when `hex_color` is not a character string with length 9.", {
  expected_error <- "Please provide a hex colour in the format #RRGGBBAA."

  visR:::.get_alpha_from_hex_colour(hex_colour = "visR") %>%
    testthat::expect_error(expected_error)
})

testthat::test_that("T6.5 An error when `hex_color` is a character string with length 9 but doesn't have # at the first position.", {
  expected_error <- "Please provide a hex colour in the format #RRGGBBAA."

  visR:::.get_alpha_from_hex_colour(hex_colour = "visRvisRv") %>%
    testthat::expect_error(expected_error)
})

# END OF CODE -------------------------------------------------------------
