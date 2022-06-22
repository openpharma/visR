#' @title Specifications test-CRAN_watchdog.R
#' @section Last updated by: Tim Treis (tim.treis(at)outlook.de)
#' @section Last update date: 2021-10-28 16:29:24
#'
#' @section List of tested specifications
#' T1. Our codebase doesn't violate CRAN style-guidelines.
#' T1.1 TRUE/FALSE are used instead of T/F.
#' T1.2 Each function documentation contains a \value{} tag.
#' T1.3 The existence of packages is not checked through 'installed.packages()'.
#' T1.4 No \dontrun{} tags unless the code actually takes a long time.
#' T1.5 The use of 'options()' is immediately preemptively reverted.

# Requirement T1 ---------------------------------------------------------------

testthat::context("CRAN_watchdog - T1. Our codebase doesn't violate CRAN style-guidelines.")

testthat::test_that("T1.1 TRUE/FALSE are used instead of T/F.", {
  test_files <- .get_visR_files(
    functions = TRUE,
    tests = TRUE,
    documentation = TRUE,
    vignettes = TRUE
  )

  patterns <- list(
    "=T,", "=T ", "=T)",
    "=F,", "=F ", "=F)",
    "= T,", "= T ", "= T)",
    "= F,", "= F ", "= F)"
  )

  CRAN_incompabilities <- data.frame()

  for (test_file in test_files) {
    for (pattern in patterns) {
      hits <- base::grep(pattern, base::readLines(test_file, warn = FALSE))

      if (base::length(hits) > 0) {
        tmp <- data.frame("line" = hits)
        tmp[["file"]] <- test_file
        tmp[["issue"]] <- pattern
        CRAN_incompabilities <- base::rbind(CRAN_incompabilities, tmp)
      }
    }
  }

  if (base::nrow(CRAN_incompabilities) > 0) {
    cat(paste0(CRAN_incompabilities, collapse = ";"))
  }

  testthat::expect_true(base::nrow(CRAN_incompabilities) == 0)
})

testthat::test_that("T1.2 Each function documentation contains a \\value{} tag.", {
  test_files <- .get_visR_files(documentation = TRUE)

  # List of files in which we don't expect a return value.
  exceptions <- list(
    "adtte.Rd",
    "brca_cohort.Rd",
    "visR-Global.Rd",
    "visR-package.Rd",
    "reexports.Rd"
  )
  exceptions_collapsed <- paste(exceptions, collapse = "|")

  CRAN_incompabilities <- data.frame()

  for (test_file in test_files) {
    if (sum(grepl(exceptions_collapsed, test_file)) == 0) {
      name_hits <- base::grep("\\\\name\\{", base::readLines(test_file, warn = FALSE))
      value_hits <- base::grep("\\\\value\\{", base::readLines(test_file, warn = FALSE))

      # The number of \name{} and \value{} should be the same since this means
      # that each function has a return value.

      if (length(name_hits) != length(value_hits)) {
        name_hits <- base::ifelse(length(name_hits) == 0, NA, name_hits)
        value_hits <- base::ifelse(length(name_hits) == 0, NA, value_hits)

        tmp <- data.frame("name_line" = name_hits)
        tmp[["value_line"]] <- value_hits
        tmp[["file"]] <- test_file
        CRAN_incompabilities <- base::rbind(CRAN_incompabilities, tmp)
      }
    }
  }

  if (base::nrow(CRAN_incompabilities) > 0) {
    print(CRAN_incompabilities)
  }

  testthat::expect_true(base::nrow(CRAN_incompabilities) == 0)
})

testthat::test_that("T1.3 The existence of packages is not checked through 'installed.packages()'.", {

  # installed.packages might be slow on CRAN servers

  test_files <- .get_visR_files(
    functions = TRUE,
    tests = TRUE,
    documentation = TRUE,
    vignettes = TRUE
  )

  CRAN_incompabilities <- data.frame()

  for (test_file in test_files) {
    hits <- base::grep(
      "\\\\installed.packages",
      base::readLines(test_file, warn = FALSE)
    )

    if (length(hits) > 0) {
      tmp <- data.frame("line" = hits)
      tmp[["file"]] <- test_file
      CRAN_incompabilities <- base::rbind(CRAN_incompabilities, tmp)
    }
  }

  if (base::nrow(CRAN_incompabilities) > 0) {
    print(CRAN_incompabilities)
  }

  testthat::expect_true(base::nrow(CRAN_incompabilities) == 0)
})

testthat::test_that("T1.4 No \\dontrun{} tags unless the code actually takes a long time.", {
  test_files <- .get_visR_files(documentation = TRUE)

  # List of files in which we don't expect a return value.
  exceptions <- list(
    "adtte.Rd",
    "brca_cohort.Rd",
    "visR-Global.Rd"
  )
  exceptions_collapsed <- paste(exceptions, collapse = "|")

  CRAN_incompabilities <- data.frame()

  for (test_file in test_files) {
    if (sum(grepl(exceptions_collapsed, test_file)) == 0) {
      hits <- base::grep(
        "\\\\dontrun\\{",
        base::readLines(test_file, warn = FALSE)
      )

      if (length(hits) > 0) {
        tmp <- data.frame("line" = hits)
        tmp[["file"]] <- test_file
        CRAN_incompabilities <- base::rbind(CRAN_incompabilities, tmp)
      }
    }
  }

  if (base::nrow(CRAN_incompabilities) > 0) {
    print(CRAN_incompabilities)
  }

  testthat::expect_true(base::nrow(CRAN_incompabilities) == 0)
})

testthat::test_that("T1.5 The use of 'options()' is immediately preemptively reverted.", {
  test_files <- .get_visR_files(
    functions = TRUE,
    tests = TRUE,
    documentation = TRUE,
    vignettes = TRUE
  )

  # List of files in which we don't expect a return value.
  exceptions <- list("Time_to_event_analysis.Rmd", "CDISC_ADaM.Rmd")
  exceptions_collapsed <- paste(exceptions, collapse = "|")

  CRAN_incompabilities <- data.frame()

  for (test_file in test_files) {
    if (sum(grepl(exceptions_collapsed, test_file)) == 0) {
      hits <- base::grep("^options\\(", base::readLines(test_file, warn = FALSE))

      if (length(hits) > 0) {
        tmp <- data.frame("line" = hits)
        tmp[["file"]] <- test_file
        CRAN_incompabilities <- base::rbind(CRAN_incompabilities, tmp)
      }
    }
  }

  if (base::nrow(CRAN_incompabilities) > 0) {
    print(CRAN_incompabilities)
  }

  testthat::expect_true(base::nrow(CRAN_incompabilities) == 0)
})
