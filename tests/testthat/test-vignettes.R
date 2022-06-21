#' @title Specifications test-vignettes.R
#' @section Last updated by: Tim Treis (tim.treis@@outlook.de)
#' @section Last update date: 2022-03-01T16:22:07
#'
#' @section List of tested specifications
#' T1. Data loaded from external sources exists.
#' T1.1 Data from Bankar et al. is still hosted on GitHub.

# Requirement T1 ----------------------------------------------------------

testthat::context("vignettes - T1. Data loaded from external sources exists.")

testthat::test_that("T1.1 Data from Bankar et al. is still hosted on GitHub.", {

  # Our interactive vignette loads public data so that people can follow along
  # This test ensures that it is still publicly available

  bankar_link <- "https://raw.githubusercontent.com/vntkumar8/covid-survival/main/data/final.csv"

  check <- suppressWarnings(
    try(
      open.connection(
        url(bankar_link),
        timeout = 5
      ),
      silent = TRUE
    )[1]
  )

  suppressWarnings(
    try(
      close.connection(url(bankar_link)),
      silent = TRUE
    )
  )

  testthat::expect_true(is.null(check))
})

# END OF CODE -------------------------------------------------------------
