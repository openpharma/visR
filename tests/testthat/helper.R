#' @title Collections of functions only used in testthat tests

#' A helper function for catching errors in the ggplot2 traceback stack
#' @keywords internal

.check_traceback_stack_for_ggplot_aesthetics_warning <- function() {

  # ggplot returns its errors for aesthetics as overloaded arguments to the
  # a print call (I think?) which makes them non-catchable through testthat.
  # This part retrieves it anyway.

  traceback_stack <- base::.traceback(1)

  # Collapse elements of stack in case they are multiline
  for (i in 1:base::length(traceback_stack)) {
    traceback_stack[i] <- base::paste0(base::paste0(traceback_stack[i]))
  }

  abortive_warning <- base::grep("[^_]Aesthetics", traceback_stack, value = TRUE)

  return(abortive_warning)
}

#' A helper function for map numbers to an arbitrary range
#' @keywords internal

.map_numbers_to_new_range <- function(numbers, lower, upper) {

  # Used to artificially generated a set amount of strata for testing
  # https://stackoverflow.com/a/18303620/10169453

  # Shifting the vector so that min(x) == 0
  numbers <- numbers - min(numbers)
  # Scaling to the range of [0, 1]
  numbers <- numbers / max(numbers)
  # Scaling to the needed amplitude
  numbers <- numbers * (upper - lower)
  # Shifting to the needed level
  return(as.factor(round(numbers + lower, 0)))
}

#' A helper function that returns the full paths of the package files as a vector.
#' It is used as part of the watchdogs documented here at
#' https://github.com/openpharma/visR/wiki/Coding-principles#package-maintenance
#' @keywords internal

.get_visR_files <- function(functions = FALSE,
                            tests = FALSE,
                            documentation = FALSE,
                            vignettes = FALSE,
                            remove_watchdog = TRUE) {
  files <- list()
  wd <- getwd()

  if (functions) {
    R_files <- list.files(
      path = file.path(wd, "/../../R"),
      pattern = "*.R",
      full.names = TRUE
    )
    files <- c(files, unlist(R_files))
  }

  if (tests) {
    test_files <- list.files(
      path = wd,
      pattern = "*.R",
      full.names = TRUE
    )
    files <- c(files, unlist(test_files))
  }

  if (documentation) {
    man_files <- list.files(
      path = file.path(wd, "/../../man"),
      pattern = "*.Rd",
      full.names = TRUE
    )
    files <- c(files, unlist(man_files))
  }

  if (vignettes) {
    vignette_files <- list.files(
      path = file.path(wd, "/../../vignettes"),
      pattern = "*.Rmd",
      full.names = TRUE
    )
    files <- c(files, unlist(vignette_files))
  }

  if (remove_watchdog) {
    files <- files[!grepl("test-watchdog_CRAN.R", files)]
  }

  return(unlist(files))
}

#' A helper function that returns a formatted table containing a table of
#' content based on the `testthat` context and name info given in the file.
#' @keywords internal

.get_test_TOC <- function(path_to_file) {
  txt <- paste(readLines(path_to_file, warn = FALSE), collapse = "\n")
  tests <- gregexpr("testthat::test_that\\(\\\"(.+?)\"", txt)
  contexts <- gregexpr("context\\(\\\"(.+?)\"", txt)

  context_df <- data.frame("pos" = unlist(contexts))
  context_df["match.length"] <- attributes(contexts[[1]])$match.length
  context_df["index.type"] <- attributes(contexts[[1]])$index.type
  context_df["useBytes"] <- attributes(contexts[[1]])$useBytes
  context_df["type"] <- "context"
  context_df["content"] <- regmatches(txt, contexts)

  context_df$content <- base::lapply(context_df$content, function(c) {
    c <- gsub(pattern = "context\\(\"(.+?)T", replacement = "T", x = c)
    c <- gsub(pattern = "\"", replacement = "", x = c)
    c
  }) %>% unlist()

  test_df <- data.frame("pos" = unlist(tests))
  test_df["match.length"] <- attributes(tests[[1]])$match.length
  test_df["index.type"] <- attributes(tests[[1]])$index.type
  test_df["useBytes"] <- attributes(tests[[1]])$useBytes
  test_df["type"] <- "test"
  test_df["content"] <- regmatches(txt, tests)

  test_df$content <- base::lapply(test_df$content, function(c) {
    c <- gsub(pattern = "testthat::test_that\\(\\\"T", replacement = "T", x = c)
    c <- gsub(pattern = "\"", replacement = "", x = c)
    c
  }) %>% unlist()

  matches <- rbind(context_df, test_df)
  matches <- matches[order(matches$pos), ]


  toc <- ""
  toc <- base::lapply(matches$content, function(line) {
    toc <- paste0(toc, "#' ", line, "\n")
  }) %>%
    unlist() %>%
    paste0(collapse = "")

  return(toc)
}

# get_pvalue - Results to compare against ---------------------------------

ref1 <- survival::survdiff(formula = survival::Surv(AVAL, 1 - CNSR) ~ TRTA, data = adtte, rho = 0)
ref2 <- survival::survdiff(formula = survival::Surv(AVAL, 1 - CNSR) ~ TRTA, data = adtte, rho = 1)
ref3 <- survival::survdiff(formula = survival::Surv(AVAL, 1 - CNSR) ~ TRTA, data = adtte, rho = 1.5)
ref4 <- survival::survdiff(formula = survival::Surv(AVAL, 1 - CNSR) ~ TRTA, data = adtte, rho = 2.4)

get_pvalue_ref1 <- data.frame(
  `Equality across strata` = "Log-Rank",
  Chisq = format(round(ref1$chisq, 3), nsmall = 3, width = 6, justify = "right", scientific = FALSE),
  df = length(ref1$n) - 1,
  `p-value` = .pvalformat(stats::pchisq(ref1$chisq, length(ref1$n) - 1, lower.tail = FALSE)),
  check.names = FALSE,
  row.names = NULL,
  stringsAsFactors = FALSE
)
get_pvalue_ref2 <- data.frame(
  `Equality across strata` = "Wilcoxon",
  Chisq = format(round(ref2$chisq, 3), nsmall = 3, width = 6, justify = "right", scientific = FALSE),
  df = length(ref2$n) - 1,
  `p-value` = .pvalformat(stats::pchisq(ref2$chisq, length(ref2$n) - 1, lower.tail = FALSE)),
  check.names = FALSE,
  row.names = NULL,
  stringsAsFactors = FALSE
)
get_pvalue_ref3 <- data.frame(
  `Equality across strata` = "Tarone-Ware",
  Chisq = format(round(ref3$chisq, 3), nsmall = 3, width = 6, justify = "right", scientific = FALSE),
  df = length(ref3$n) - 1,
  `p-value` = .pvalformat(stats::pchisq(ref3$chisq, length(ref3$n) - 1, lower.tail = FALSE)),
  check.names = FALSE,
  row.names = NULL,
  stringsAsFactors = FALSE
)
get_pvalue_ref4 <- data.frame(
  `Equality across strata` = paste0("Harrington and Fleming test (rho = ", 2.4, ")"),
  Chisq = format(round(ref4$chisq, 3), nsmall = 3, width = 6, justify = "right", scientific = FALSE),
  df = length(ref4$n) - 1,
  `p-value` = .pvalformat(stats::pchisq(ref4$chisq, length(ref4$n) - 1, lower.tail = FALSE)),
  check.names = FALSE,
  row.names = NULL,
  stringsAsFactors = FALSE
)

get_pvalue_ref <- base::rbind.data.frame(
  get_pvalue_ref1,
  get_pvalue_ref2,
  get_pvalue_ref3,
  get_pvalue_ref4,
  make.row.names = FALSE
)

get_pvalue_ref134 <- base::rbind.data.frame(
  get_pvalue_ref1,
  get_pvalue_ref3,
  get_pvalue_ref4,
  make.row.names = FALSE
)

get_legend_title <- function(gg) {
  ggb <- ggplot2::ggplot_build(gg)
  ggt <- ggplot2::ggplot_gtable(ggb)

  legend_grob_id <- which(sapply(ggt$grobs, function(x) x$name) == "guide-box")
  legend_grob <- ggt$grobs[[legend_grob_id]]

  legend_title_id <- which(sapply(legend_grob$grobs[[1]]$layout$name, function(x) x) == "title")
  legend_gtree <- legend_grob$grobs[[1]]$grobs[[legend_title_id]]
  legend_title <- legend_gtree$children[[1]]$children[[1]]$label

  return(legend_title)
}
