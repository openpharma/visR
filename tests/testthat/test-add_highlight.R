#' @title Specifications add_highlight
#' @section Last updated by: Tim Treis
#' @section Last update date: 06-Auust-2021

# Specifications ---------------------------------------------------------------

#' T1. No errors when confidence intervals are added to the plots. 

# Requirement T1 ---------------------------------------------------------------

testthat::context("add_highlight - T1. The function changes the alpha values of strata found in the plot.")

testthat::test_that("T1.1 No error when `add_highlight` is called on a `ggsurvfit` object.", {
  
  gg <- adtte %>%
    visR::estimate_KM(strata = "TRTP") %>%
    visR::visr()
  
  testthat::expect_true("ggsurvfit" %in% class(gg))
  
  gg %>%
    visR::add_highlight(strata = "TRTP=Placebo") %>%
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
  
  visR::add_highlight(stats::lm(data = adtte, "AGE ~ TRTDUR")) %>%
    testthat::expect_error()
  
})

testthat::test_that("T1.4 An error when `add_highlight` is called on a `ggplot` but non-`ggsurvfit` object.", {
  
  gg <- adtte %>%
    ggplot2::ggplot(ggplot2::aes(x = AGE, y = TRTDUR)) +
    ggplot2::geom_point()
  
  testthat::expect_true("ggplot" %in% class(gg))
  testthat::expect_false("ggsurvfit" %in% class(gg))
  
  gg %>% add_highlight() %>% testthat::expect_error()
  
})

# Requirement T2 ---------------------------------------------------------------

testthat::context("add_highlight - T2. No errors when one or more strata are highlighted with default parameters.")

testthat::test_that("T2.1 No error when `strata` is a character string found in the plot strata.", {
  
  gg <- adtte %>%
    visR::estimate_KM(strata = "TRTP") %>%
    visR::visr()
  
  gg %>%
    visR::add_highlight(strata = "TRTP=Placebo") %>%
    testthat::expect_error(NA)
  
  testthat::skip_on_cran()
  gg %>% 
    visR::add_highlight(strata = "TRTP=Placebo") %>%
    vdiffr::expect_doppelganger(title = "add_highlight_T2_1_no_error_when_strata_is_a_character_string_found_in_the_plot_strata")  
  
})

testthat::test_that("T2.2 An error when `strata` is a character string not found in the plot strata.", {
  
  gg <- adtte %>%
    visR::estimate_KM(strata = "TRTP") %>%
    visR::visr()
  
  expected_error <- "The strata you specified has not been found in the provided plot.\n  Available strata: TRTP=Placebo, TRTP=Xanomeline High Dose, TRTP=Xanomeline Low Dose\n  Please adjust and rerun."
  
  gg %>%
    visR::add_highlight(strata = "visR") %>%
    testthat::expect_error(expected_error)
  
})

testthat::test_that("T2.3 No error when `strata` is a `list` or `vector` of character strings found in the plot strata.", {
  
  strata_list <- list("TRTP=Placebo", "TRTP=Xanomeline Low Dose")
  strata_vector <- c("TRTP=Placebo", "TRTP=Xanomeline Low Dose")
  
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
    vdiffr::expect_doppelganger(title = "add_highlight_T2_2_no_error_when_strata_is_a_list_of_character_strings_found_in_the_plot_strata") 
  
  gg %>% 
    visR::add_highlight(strata = strata_vector) %>%
    vdiffr::expect_doppelganger(title = "add_highlight_T2_2_no_error_when_strata_is_a_vector_of_character_strings_found_in_the_plot_strata")  
  
})

testthat::test_that("T2.4 An error when `strata` is a `list` or `vector` that holds non-character-string elements.", {
  
  strata <- c(1, 2, 3)
  
  gg <- adtte %>%
    visR::estimate_KM(strata = "TRTP") %>%
    visR::visr()
  
  expected_error <- "When 'strata' is a list, all elements must be character strings."
  
  gg %>%
    visR::add_highlight(strata = strata) %>%
    testthat::expect_error(expected_error)
  
})

testthat::test_that("T2.5 An error when `strata` is not a character string or a list.", {
  
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

testthat::test_that("T2.6 An error when `strata` is `NULL` or missing.", {
  
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

testthat::test_that("T2.7 An error when `strata` is not a character string or a list.", {
  
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

testthat::context("add_highlight - T3. The opacity of the background strata can be changed through `bg_alpha_multiplier`.")

testthat::test_that("T3.1 No error when `bg_alpha_multiplier` is a `numberic`.", {
  
  gg <- adtte %>%
    visR::estimate_KM(strata = "TRTP") %>%
    visR::visr()
  
  gg %>%
    visR::add_highlight(strata = "TRTP=Placebo",
                        bg_alpha_multiplier = 0.2) %>%
    testthat::expect_error(NA)
  
})

testthat::test_that("T3.2 An error when `bg_alpha_multiplier` is a not a `numberic`.", {
  
  gg <- adtte %>%
    visR::estimate_KM(strata = "TRTP") %>%
    visR::visr()
  
  expected_error <- "The `bg_alpha_multiplier` must be a `numeric`."
  
  gg %>%
    visR::add_highlight(strata = "TRTP=Placebo",
                        bg_alpha_multiplier = "visR") %>%
    testthat::expect_error(expected_error)
  
})

testthat::test_that("T3.3 An error when `bg_alpha_multiplier` is outside of [0, 1].", {
  
  gg <- adtte %>%
    visR::estimate_KM(strata = "TRTP") %>%
    visR::visr()
  
  expected_error <- "The `bg_alpha_multiplier` must be a numeric value between 0 and 1."
  
  gg %>%
    visR::add_highlight(strata = "TRTP=Placebo",
                        bg_alpha_multiplier = -1) %>%
    testthat::expect_error(expected_error)
  
  gg %>%
    visR::add_highlight(strata = "TRTP=Placebo",
                        bg_alpha_multiplier = 2) %>%
    testthat::expect_error(expected_error)
  
})

# END OF CODE ------------------------------------------------------------------
