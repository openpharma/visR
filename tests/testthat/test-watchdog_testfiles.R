#' @title Specifications test-get_risktable.R
#' @section Last updated by: Tim Treis (tim.treis(at)outlook.de)
#' @section Last update date: 2021-10-28 16:29:24
#'
#' @section List of tested specifications
#' T1. Details on last change for test files are recorded.
#' T1.1 executed.

# Requirement T1 ---------------------------------------------------------------

context("validate_watchdog - T1. Details on last change for test files are recorded.")
# skip on GH Actions and CRAN
testthat::skip_if(isTRUE(as.logical(Sys.getenv("CI"))))
testthat::skip_on_cran()
testthat::skip_on_os("windows")

testthat::test_that("T1.1 executed.", {
  test_files <- .get_visR_files(tests = TRUE)

  test_files <- test_files[!(grepl("watchdog", test_files))]
  test_files <- test_files[!(grepl("helper", test_files))]

  last_change_df <- data.frame(
    full_path = test_files,
    name = basename(test_files)
  )

  # Get information from last commit that changed the respective file
  cmd <- "git log -1" # Get only last edit
  cmd <- paste0(cmd, " --pretty=format:'%an (%ae);%ad'") # name-mail-date
  cmd <- paste0(cmd, " --date=format:'%Y-%m-%dT%H:%M:%S' ")

  last_change_df["log"] <- sapply(last_change_df$full_path, function(x) {
    system(paste0(cmd, x), intern = TRUE)
  })

  last_change_df <- last_change_df %>%
    tidyr::separate(log, into = c("last_edit_by", "last_edit_when"), ";") %>%
    dplyr::mutate(last_edit_by = gsub("\\@", "\\@\\@", last_edit_by))

  apply(last_change_df, 1, function(x) {
    file_content <- readChar(x["full_path"], file.info(x["full_path"])$size)

    new_header <- paste0("#' \\@title Specifications ", x["name"], "\n")
    new_header <- paste0(
      new_header,
      "#' \\@section Last updated by: ",
      x["last_edit_by"],
      "\n"
    )
    new_header <- paste0(
      new_header,
      "#' \\@section Last update date: ",
      x["last_edit_when"],
      "\n#'\n"
    )
    new_header <- paste0(
      new_header,
      "#' \\@section List of tested specifications",
      "\n"
    )
    new_header <- paste0(new_header, .get_test_TOC(x["full_path"]), "\n")
    new_header <- paste0(
      new_header,
      "# Requirement T1 ",
      paste0(rep("-", 58), collapse = ""),
      "\n\ntestthat::context"
    )

    file_content <- gsub(
      pattern = ".*\\@title(.+?)\n.+?context",
      replacement = new_header,
      x = file_content
    )

    # Capture everything after the last test's })
    leftovers <- regmatches(
      x = file_content,
      m = gregexpr("}\\)(?!(.|\n)*test)(.|\n)*\\Z", file_content, perl = TRUE)
    )[[1]]

    # Format replacement section
    eoc_section <- paste0(
      "})\n\n",
      "# END OF CODE ",
      paste0(rep("-", 61), collapse = ""),
      "\n"
    )

    file_content <- gsub(
      pattern = leftovers,
      replacement = eoc_section,
      x = file_content
    )

    cat(file_content, file = x["full_path"])
  })

  testthat::expect_true(TRUE)
})
