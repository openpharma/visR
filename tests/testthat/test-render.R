#' @title Specifications test-render.R
#' @section Last updated by: Tim Treis (tim.treis@@outlook.de)
#' @section Last update date: 2022-02-09T15:22:32
#'
#' @section List of tested specifications
#' T1. The function `render.tableone()` properly renders a `render.tableone` object.
#' T1.1 No error when `data` is a `tableone` object.
#' T1.2 An error when `data` is not a `tableone` object.
#' T1.3 An error when `title` is missing.
#' T1.4 No error when `title` is defined.
#' T1.5 An error when `datasource` is missing.
#' T1.6 No error when `datasource` is defined.
#' T1.7 No error when `footnote` is defined.
#' T1.8 No error when `output_format` is 'html' and `engine` is 'gt'.
#' T1.9 No error when `output_format` is 'html' and `engine` is 'kable'.
#' T1.10 No error when `output_format` is 'html' and `engine` is 'dt', 'datatable' or 'datatables'.
#' T1.11 An error when `output_format` is 'latex' and `engine` is not 'gt' or 'kable'.
#' T1.12 An error when `output_format` is an invalid parameter.
#' T1.13 An error when `engine` is an invalid parameter.
#' T1.14 No error when `output_format` is 'latex' and `engine` is 'kable'.
#' T1.16 No error when `engine` is in ['dt', 'datatable', 'datatables'] and download_format` is in ['copy', 'csv', 'excel'].
#' T1.17 A warning when `engine` is not in ['dt', 'datatable', 'datatables'] and download_format` is in ['copy', 'csv', 'excel'].
#' T1.18 A warning when `download_format` is not 'copy', 'csv' or 'excel'.
#' T2. The function `render.risktable()` properly renders a `risktable` object.
#' T2.1 No error when `data` is a `risktable` object.
#' T2.2 An error when `data` is not a `risktable` object.
#' T2.3 An error when `title` is missing.
#' T2.4 No error when `title` is defined.
#' T2.5 An error when `datasource` is missing.
#' T2.6 No error when `datasource` is defined.
#' T2.7 No error when `footnote` is defined.
#' T2.8 No error when `output_format` is 'html' and `engine` is 'gt'.
#' T2.9 No error when `output_format` is 'html' and `engine` is 'kable'.
#' T2.10 No error when `output_format` is 'html' and `engine` is 'dt', 'datatable' or 'datatables'.
#' T2.11 An error when `output_format` is an invalid parameter.
#' T2.12 An error when `engine` is an invalid parameter.
#' T2.13 No error when `output_format` is 'latex' and `engine` is 'kable'.
#' T2.14 An error when `output_format` is 'latex' and `engine` is 'dt', 'datatable' or 'datatables'.
#' T2.15 No error when `engine` is in ['dt', 'datatable', 'datatables'] and download_format` is in ['copy', 'csv', 'excel'].
#' T2.16 A warning when `engine` is not in ['dt', 'datatable', 'datatables'] and download_format` is in ['copy', 'csv', 'excel'].
#' T2.17 The strata-colnames of the `risktable` object are used as rownames.
#' T2.18 The metric of the risktable is used in the rendered table.
#' T2.19 The values of the evalutated metric are pivoted wide.
#' T3. The function `render.data.frame()` properly renders a `data.frame` object.
#' T3.1 When `engine` is 'gt' and `output_format` is 'latex', a latex `knit_asis` object is returned.
#' T3.2 A warning when `engine` is 'dt', 'datatable' or 'datatables' and `output_format is not 'html'.`
#' T4. The function `check_rendering_input()` only permits valid `output_format` and `engine` options.
#' T4.1 No error when `output_format` is `html` or `latex` and `engine` is `kable`, `gt`, `dt`, `datatable` or `datatables`.
#' T4.2 An error when `output_format` and/or `engine` are missing, `NULL` or `NA`.
#' T4.3 An error when `output_format` is not `html` or `latex` and `engine` is a valid option.
#' T4.4 An error when `engine` is not `kable`, `gt`, `dt`, `datatables` or `datatable` and `output_format` is a valid option.
#' T5. The function `render_datatable.data.frame()` creates an `htmlwidget` of the table.
#' T5.1 No error when `data` is a `data.frame`.
#' T5.2 The returned object is of type `htmlwidget`.
#' T5.3 The `title` is passed along to the HTML widget.
#' T5.4 The `source_cap` is passed along to the HTML widget.
#' T5.5 When `download_format` is not `NULL`, a button is added.
#' T5.6 When `download_format` is `NULL`, no button is added.
#' T6. The function `get_gt.data.frame()` properly passes the input along to `gt::gt()`.
#' T6.1 No error when `data` is a `data.frame`.
#' T6.2 The returned object is of type `gt_tbl`.

# Requirement T1 ----------------------------------------------------------

testthat::context("render - T1. The function `render.tableone()` properly renders a `render.tableone` object.")

testthat::test_that("T1.1 No error when `data` is a `tableone` object.", {
  adtte_tableone <- adtte %>%
    visR::get_tableone()

  testthat::expect_true(inherits(adtte_tableone, "tableone"))
  adtte_tableone %>% visR:::render.tableone(title = NULL, datasource = NULL)
})

testthat::test_that("T1.2 An error when `data` is not a `tableone` object.", {
  adtte_tableone <- adtte %>%
    visR::get_tableone()

  class(adtte_tableone) <- class(adtte_tableone)[class(adtte_tableone) != "tableone"]

  testthat::expect_false(inherits(adtte_tableone, "tableone"))
  adtte_tableone %>%
    visR:::render.tableone(title = NULL, datasource = NULL) %>%
    testthat::expect_error()
})

testthat::test_that("T1.3 An error when `title` is missing.", {
  adtte_tableone <- adtte %>%
    visR::get_tableone()

  adtte_tableone %>%
    visR:::render.tableone(datasource = NULL) %>%
    testthat::expect_error()
})

testthat::test_that("T1.4 No error when `title` is defined.", {
  adtte_tableone <- adtte %>%
    visR::get_tableone()

  adtte_tableone %>%
    visR:::render.tableone(title = NULL, datasource = NULL) %>%
    testthat::expect_error(NA)

  adtte_tableone %>%
    visR:::render.tableone(title = 1, datasource = NULL) %>%
    testthat::expect_error(NA)

  adtte_tableone %>%
    visR:::render.tableone(title = "visR", datasource = NULL) %>%
    testthat::expect_error(NA)

  adtte_tableone %>%
    visR:::render.tableone(title = c(1, 2, 3), datasource = NULL) %>%
    testthat::expect_error(NA)
})

testthat::test_that("T1.5 An error when `datasource` is missing.", {
  adtte_tableone <- adtte %>%
    visR::get_tableone()

  adtte_tableone %>%
    visR:::render.tableone(title = NULL) %>%
    testthat::expect_error()
})

testthat::test_that("T1.6 No error when `datasource` is defined.", {
  adtte_tableone <- adtte %>%
    visR::get_tableone()

  adtte_tableone %>%
    visR:::render.tableone(title = NULL, datasource = NULL) %>%
    testthat::expect_error(NA)

  adtte_tableone %>%
    visR:::render.tableone(title = NULL, datasource = 1) %>%
    testthat::expect_error(NA)

  adtte_tableone %>%
    visR:::render.tableone(title = NULL, datasource = "visR") %>%
    testthat::expect_error(NA)

  adtte_tableone %>%
    visR:::render.tableone(title = NULL, datasource = c(1, 2, 3)) %>%
    testthat::expect_error(NA)
})

testthat::test_that("T1.7 No error when `footnote` is defined.", {
  adtte_tableone <- adtte %>%
    visR::get_tableone()

  adtte_tableone %>%
    visR:::render.tableone(
      title = NULL,
      datasource = NULL,
      footnote = NULL
    ) %>%
    testthat::expect_error(NA)

  adtte_tableone %>%
    visR:::render.tableone(
      title = NULL,
      datasource = NULL,
      footnote = 1
    ) %>%
    testthat::expect_error(NA)

  adtte_tableone %>%
    visR:::render.tableone(
      title = NULL,
      datasource = NULL,
      footnote = "visR"
    ) %>%
    testthat::expect_error(NA)

  adtte_tableone %>%
    visR:::render.tableone(
      title = NULL,
      datasource = NULL,
      footnote = c(1, 2, 3)
    ) %>%
    testthat::expect_error(NA)
})

testthat::test_that("T1.8 No error when `output_format` is 'html' and `engine` is 'gt'.", {
  adtte_tableone <- adtte %>%
    visR::get_tableone()

  adtte_tableone %>%
    visR:::render.tableone(
      title = NULL,
      datasource = NULL,
      output_format = "html",
      engine = "gt"
    ) %>%
    testthat::expect_error(NA)
})

testthat::test_that("T1.9 No error when `output_format` is 'html' and `engine` is 'kable'.", {
  adtte_tableone <- adtte %>%
    visR::get_tableone()

  adtte_tableone %>%
    visR:::render.tableone(
      title = NULL,
      datasource = NULL,
      output_format = "html",
      engine = "kable"
    ) %>%
    testthat::expect_error(NA)
})

testthat::test_that("T1.10 No error when `output_format` is 'html' and `engine` is 'dt', 'datatable' or 'datatables'.", {
  adtte_tableone <- adtte %>%
    visR::get_tableone()

  adtte_tableone %>%
    visR:::render.tableone(
      title = NULL,
      datasource = NULL,
      output_format = "html",
      engine = "dt"
    ) %>%
    testthat::expect_error(NA)

  adtte_tableone %>%
    visR:::render.tableone(
      title = NULL,
      datasource = NULL,
      output_format = "html",
      engine = "datatable"
    ) %>%
    testthat::expect_error(NA)

  adtte_tableone %>%
    visR:::render.tableone(
      title = NULL,
      datasource = NULL,
      output_format = "html",
      engine = "datatables"
    ) %>%
    testthat::expect_error(NA)
})

testthat::test_that("T1.11 An error when `output_format` is 'latex' and `engine` is not 'gt' or 'kable'.", {
  adtte_tableone <- adtte %>%
    visR::get_tableone()

  expected_error <- "Currently, 'latex' output is only implemented with 'gt' or 'kable' as a table engine."
  adtte_tableone %>%
    visR:::render.tableone(
      title = NULL,
      datasource = NULL,
      output_format = "latex",
      engine = "dt"
    ) %>%
    testthat::expect_error(expected_error)
})

testthat::test_that("T1.12 An error when `output_format` is an invalid parameter.", {
  adtte_tableone <- adtte %>%
    visR::get_tableone()

  adtte_tableone %>%
    visR:::render.tableone(
      title = NULL,
      datasource = NULL,
      output_format = NULL
    ) %>%
    testthat::expect_error()

  adtte_tableone %>%
    visR:::render.tableone(
      title = NULL,
      datasource = NULL,
      output_format = 1
    ) %>%
    testthat::expect_error()

  adtte_tableone %>%
    visR:::render.tableone(
      title = NULL,
      datasource = NULL,
      output_format = "visR"
    ) %>%
    testthat::expect_error()
})

testthat::test_that("T1.13 An error when `engine` is an invalid parameter.", {
  adtte_tableone <- adtte %>%
    visR::get_tableone()

  adtte_tableone %>%
    visR:::render.tableone(
      title = NULL,
      datasource = NULL,
      engine = NULL
    ) %>%
    testthat::expect_error()

  adtte_tableone %>%
    visR:::render.tableone(
      title = NULL,
      datasource = NULL,
      engine = 1
    ) %>%
    testthat::expect_error()

  adtte_tableone %>%
    visR:::render.tableone(
      title = NULL,
      datasource = NULL,
      engine = "visR"
    ) %>%
    testthat::expect_error()
})

testthat::test_that("T1.14 No error when `output_format` is 'latex' and `engine` is 'kable'.", {
  adtte_tableone <- adtte %>%
    visR::get_tableone()

  adtte_tableone %>%
    visR:::render.tableone(
      title = NULL,
      datasource = NULL,
      output_format = "latex",
      engine = "kable"
    ) %>%
    testthat::expect_error(NA)
})

testthat::test_that("T1.16 No error when `engine` is in ['dt', 'datatable', 'datatables'] and download_format` is in ['copy', 'csv', 'excel'].", {
  adtte_tableone <- adtte %>%
    visR::get_tableone()

  for (engine in c("dt", "datatable", "datatables")) {
    for (download_format in c("copy", "csv", "excel")) {
      adtte_tableone %>%
        visR:::render.tableone(
          title = NULL,
          datasource = NULL,
          engine = engine,
          download_format = download_format
        ) %>%
        testthat::expect_error(NA)
    }
  }
})

testthat::test_that("T1.17 A warning when `engine` is not in ['dt', 'datatable', 'datatables'] and download_format` is in ['copy', 'csv', 'excel'].", {
  adtte_tableone <- adtte %>%
    visR::get_tableone()

  for (engine in c("gt", "kable")) {
    for (download_format in c("copy", "csv", "excel")) {
      adtte_tableone %>%
        visR:::render.tableone(
          title = NULL,
          datasource = NULL,
          engine = engine,
          download_format = download_format
        ) %>%
        testthat::expect_warning()
    }
  }
})

testthat::test_that("T1.18 A warning when `download_format` is not 'copy', 'csv' or 'excel'.", {
  adtte_tableone <- adtte %>%
    visR::get_tableone()

  expected_warning <- "Currently, only 'copy', 'csv' and 'excel' are supported as 'download_format'."
  adtte_tableone %>%
    visR:::render.tableone(
      title = NULL,
      datasource = NULL,
      engine = "dt",
      download_format = "visR"
    ) %>%
    testthat::expect_warning(expected_warning)
})

# Requirement T2 ---------------------------------------------------------------

testthat::context("render - T2. The function `render.risktable()` properly renders a `risktable` object.")

testthat::test_that("T2.1 No error when `data` is a `risktable` object.", {
  adtte_risktable <- adtte %>%
    visR::estimate_KM("SEX") %>%
    visR::get_risktable()

  testthat::expect_true(inherits(adtte_risktable, "risktable"))
  adtte_risktable %>%
    visR:::render.risktable(title = NULL, datasource = NULL) %>%
    testthat::expect_error(NA)
})

testthat::test_that("T2.2 An error when `data` is not a `risktable` object.", {
  adtte_risktable <- adtte %>%
    visR::estimate_KM("SEX") %>%
    visR::get_risktable()

  class(adtte_risktable) <- class(adtte_risktable)[class(adtte_risktable) != "risktable"]

  testthat::expect_false(inherits(adtte_risktable, "risktable"))
  adtte_risktable %>%
    visR:::render.risktable(title = NULL, datasource = NULL) %>%
    testthat::expect_error()
})

testthat::test_that("T2.3 An error when `title` is missing.", {
  adtte_risktable <- adtte %>%
    visR::estimate_KM("SEX") %>%
    visR::get_risktable()

  adtte_risktable %>%
    visR:::render.risktable(datasource = NULL) %>%
    testthat::expect_error()
})

testthat::test_that("T2.4 No error when `title` is defined.", {
  adtte_risktable <- adtte %>%
    visR::estimate_KM("SEX") %>%
    visR::get_risktable()

  adtte_risktable %>%
    visR:::render.risktable(
      title = NULL,
      datasource = NULL
    ) %>%
    testthat::expect_error(NA)

  adtte_risktable %>%
    visR:::render.risktable(
      title = 1,
      datasource = NULL
    ) %>%
    testthat::expect_error(NA)

  adtte_risktable %>%
    visR:::render.risktable(
      title = "visR",
      datasource = NULL
    ) %>%
    testthat::expect_error(NA)

  adtte_risktable %>%
    visR:::render.risktable(
      title = c(1, 2, 3),
      datasource = NULL
    ) %>%
    testthat::expect_error(NA)
})

testthat::test_that("T2.5 An error when `datasource` is missing.", {
  adtte_risktable <- adtte %>%
    visR::estimate_KM("SEX") %>%
    visR::get_risktable()

  adtte_risktable %>%
    visR:::render.risktable(title = NULL) %>%
    testthat::expect_error()
})

testthat::test_that("T2.6 No error when `datasource` is defined.", {
  adtte_risktable <- adtte %>%
    visR::estimate_KM("SEX") %>%
    visR::get_risktable()

  adtte_risktable %>%
    visR:::render.risktable(
      title = NULL,
      datasource = NULL
    ) %>%
    testthat::expect_error(NA)

  adtte_risktable %>%
    visR:::render.risktable(
      title = NULL,
      datasource = 1
    ) %>%
    testthat::expect_error(NA)

  adtte_risktable %>%
    visR:::render.risktable(
      title = NULL,
      datasource = "visR"
    ) %>%
    testthat::expect_error(NA)

  adtte_risktable %>%
    visR:::render.risktable(
      title = NULL,
      datasource = c(1, 2, 3)
    ) %>%
    testthat::expect_error(NA)
})

testthat::test_that("T2.7 No error when `footnote` is defined.", {
  adtte_risktable <- adtte %>%
    visR::estimate_KM("SEX") %>%
    visR::get_risktable()

  adtte_risktable %>%
    visR:::render.risktable(
      title = NULL,
      datasource = NULL,
      footnote = NULL
    ) %>%
    testthat::expect_error(NA)

  adtte_risktable %>%
    visR:::render.risktable(
      title = NULL,
      datasource = NULL,
      footnote = 1
    ) %>%
    testthat::expect_error(NA)

  adtte_risktable %>%
    visR:::render.risktable(
      title = NULL,
      datasource = NULL,
      footnote = "visR"
    ) %>%
    testthat::expect_error(NA)

  adtte_risktable %>%
    visR:::render.risktable(
      title = NULL,
      datasource = NULL,
      footnote = c(1, 2, 3)
    ) %>%
    testthat::expect_error(NA)
})

testthat::test_that("T2.8 No error when `output_format` is 'html' and `engine` is 'gt'.", {
  adtte_risktable <- adtte %>%
    visR::estimate_KM("SEX") %>%
    visR::get_risktable()

  adtte_risktable %>%
    visR:::render.risktable(
      title = NULL,
      datasource = NULL,
      output_format = "html",
      engine = "gt"
    ) %>%
    testthat::expect_error(NA)
})

testthat::test_that("T2.9 No error when `output_format` is 'html' and `engine` is 'kable'.", {
  adtte_risktable <- adtte %>%
    visR::estimate_KM("SEX") %>%
    visR::get_risktable()

  adtte_risktable %>%
    visR:::render.risktable(
      title = NULL,
      datasource = NULL,
      output_format = "html",
      engine = "kable"
    ) %>%
    testthat::expect_error(NA)
})

testthat::test_that("T2.10 No error when `output_format` is 'html' and `engine` is 'dt', 'datatable' or 'datatables'.", {
  adtte_risktable <- adtte %>%
    visR::estimate_KM("SEX") %>%
    visR::get_risktable()

  adtte_risktable %>%
    visR:::render.risktable(
      title = NULL,
      datasource = NULL,
      output_format = "html",
      engine = "dt"
    ) %>%
    testthat::expect_error(NA)

  adtte_risktable %>%
    visR:::render.risktable(
      title = NULL,
      datasource = NULL,
      output_format = "html",
      engine = "datatable"
    ) %>%
    testthat::expect_error(NA)

  adtte_risktable %>%
    visR:::render.risktable(
      title = NULL,
      datasource = NULL,
      output_format = "html",
      engine = "datatables"
    ) %>%
    testthat::expect_error(NA)
})

testthat::test_that("T2.11 An error when `output_format` is an invalid parameter.", {
  adtte_risktable <- adtte %>%
    visR::estimate_KM("SEX") %>%
    visR::get_risktable()

  adtte_risktable %>%
    visR:::render.risktable(
      title = NULL,
      datasource = NULL,
      output_format = NULL
    ) %>%
    testthat::expect_error()

  adtte_risktable %>%
    visR:::render.risktable(
      title = NULL,
      datasource = NULL,
      output_format = 1
    ) %>%
    testthat::expect_error()

  adtte_risktable %>%
    visR:::render.risktable(
      title = NULL,
      datasource = NULL,
      output_format = "visR"
    ) %>%
    testthat::expect_error()
})

testthat::test_that("T2.12 An error when `engine` is an invalid parameter.", {
  adtte_risktable <- adtte %>%
    visR::estimate_KM("SEX") %>%
    visR::get_risktable()

  adtte_risktable %>%
    visR:::render.risktable(
      title = NULL,
      datasource = NULL,
      engine = NULL
    ) %>%
    testthat::expect_error()

  adtte_risktable %>%
    visR:::render.risktable(
      title = NULL,
      datasource = NULL,
      engine = 1
    ) %>%
    testthat::expect_error()

  adtte_risktable %>%
    visR:::render.risktable(
      title = NULL,
      datasource = NULL,
      engine = "visR"
    ) %>%
    testthat::expect_error()
})

testthat::test_that("T2.13 No error when `output_format` is 'latex' and `engine` is 'kable'.", {
  adtte_risktable <- adtte %>%
    visR::estimate_KM("SEX") %>%
    visR::get_risktable()

  adtte_risktable %>%
    visR:::render.risktable(
      title = NULL,
      datasource = NULL,
      output_format = "latex",
      engine = "kable"
    ) %>%
    testthat::expect_error(NA)
})

testthat::test_that("T2.14 An error when `output_format` is 'latex' and `engine` is 'dt', 'datatable' or 'datatables'.", {
  adtte_risktable <- adtte %>%
    visR::estimate_KM("SEX") %>%
    visR::get_risktable()

  adtte_risktable %>%
    visR:::render.risktable(
      title = NULL,
      datasource = NULL,
      output_format = "latex",
      engine = "dt"
    ) %>%
    testthat::expect_error()

  adtte_risktable %>%
    visR:::render.risktable(
      title = NULL,
      datasource = NULL,
      output_format = "latex",
      engine = "datatable"
    ) %>%
    testthat::expect_error()

  adtte_risktable %>%
    visR:::render.risktable(
      title = NULL,
      datasource = NULL,
      output_format = "latex",
      engine = "datatables"
    ) %>%
    testthat::expect_error()
})

testthat::test_that("T2.15 No error when `engine` is in ['dt', 'datatable', 'datatables'] and download_format` is in ['copy', 'csv', 'excel'].", {
  adtte_risktable <- adtte %>%
    visR::estimate_KM("SEX") %>%
    visR::get_risktable()

  for (engine in c("dt", "datatable", "datatables")) {
    for (download_format in c("copy", "csv", "excel")) {
      adtte_risktable %>%
        visR:::render.risktable(
          title = NULL,
          datasource = NULL,
          engine = engine,
          download_format = download_format
        ) %>%
        testthat::expect_error(NA)
    }
  }
})

testthat::test_that("T2.16 A warning when `engine` is not in ['dt', 'datatable', 'datatables'] and download_format` is in ['copy', 'csv', 'excel'].", {
  adtte_risktable <- adtte %>%
    visR::estimate_KM("SEX") %>%
    visR::get_risktable()

  for (engine in c("gt", "kable")) {
    for (download_format in c("copy", "csv", "excel")) {
      adtte_risktable %>%
        visR:::render.risktable(
          title = NULL,
          datasource = NULL,
          engine = engine,
          download_format = download_format
        ) %>%
        testthat::expect_warning()
    }
  }
})

testthat::test_that("T2.17 The strata-colnames of the `risktable` object are used as rownames.", {
  adtte_risktable <- adtte %>%
    visR::estimate_KM("SEX") %>%
    visR::get_risktable()

  gg <- adtte_risktable %>%
    visR:::render.risktable(title = NULL, datasource = NULL)
  gg_data <- gg["_data"] %>% as.data.frame()

  strata_names <- colnames(adtte_risktable)[3:length(colnames(adtte_risktable))]

  testthat::expect_identical(strata_names, gg_data[, 1])
})

testthat::test_that("T2.18 The metric of the risktable is used in the rendered table.", {
  adtte_risktable_at_risk <- adtte %>%
    visR::estimate_KM("SEX") %>%
    visR::get_risktable(statlist = "n.risk")

  adtte_risktable_censored <- adtte %>%
    visR::estimate_KM("SEX") %>%
    visR::get_risktable(statlist = "n.censor")

  adtte_risktable_events <- adtte %>%
    visR::estimate_KM("SEX") %>%
    visR::get_risktable(statlist = "n.event")

  gg_at_risk <- adtte_risktable_at_risk %>%
    visR:::render.risktable(title = NULL, datasource = NULL)
  gg_at_risk_data <- gg_at_risk["_data"] %>% as.data.frame()

  gg_censored <- adtte_risktable_censored %>%
    visR:::render.risktable(title = NULL, datasource = NULL)
  gg_censored_data <- gg_censored["_data"] %>% as.data.frame()

  gg_events <- adtte_risktable_events %>%
    visR:::render.risktable(title = NULL, datasource = NULL)
  gg_events_data <- gg_events["_data"] %>% as.data.frame()

  testthat::expect_identical(levels(gg_at_risk_data[, 2]), "At risk")
  testthat::expect_identical(levels(gg_censored_data[, 2]), "Censored")
  testthat::expect_identical(levels(gg_events_data[, 2]), "Events")
})

testthat::test_that("T2.19 The values of the evalutated metric are pivoted wide.", {
  adtte_risktable <- adtte %>%
    visR::estimate_KM("SEX") %>%
    visR::get_risktable()

  gg <- adtte_risktable %>%
    visR:::render.risktable(title = NULL, datasource = NULL)
  gg_data <- gg["_data"] %>% as.data.frame()
  female_vals <- as.numeric(t(gg_data)[3:length(gg_data), 1])
  male_vals <- as.numeric(t(gg_data)[3:length(gg_data), 2])

  testthat::expect_identical(adtte_risktable[, "F"], female_vals)
  testthat::expect_identical(adtte_risktable[, "M"], male_vals)
})

# Requirement T3 ---------------------------------------------------------------

testthat::context("render - T3. The function `render.data.frame()` properly renders a `data.frame` object.")

testthat::test_that("T3.1 When `engine` is 'gt' and `output_format` is 'latex', a latex `knit_asis` object is returned.", {
  latex_table <- adtte %>%
    visR:::render.data.frame(
      title = NULL,
      datasource = NULL,
      engine = "gt",
      output_format = "latex"
    )

  testthat::expect_true(inherits(latex_table, "knit_asis"))
})

testthat::test_that("T3.2 A warning when `engine` is 'dt', 'datatable' or 'datatables' and `output_format is not 'html'.`", {
  expected_warning <- "DT engine only supports html output and not latex - falling back to html. Please pick a different engine to create other outputs"
  adtte %>%
    visR:::render.data.frame(
      title = NULL,
      datasource = NULL,
      engine = "dt",
      output_format = "latex"
    ) %>%
    testthat::expect_warning(expected_warning)

  adtte %>%
    visR:::render.data.frame(
      title = NULL,
      datasource = NULL,
      engine = "datatable",
      output_format = "latex"
    ) %>%
    testthat::expect_warning(expected_warning)

  adtte %>%
    visR:::render.data.frame(
      title = NULL,
      datasource = NULL,
      engine = "datatables",
      output_format = "latex"
    ) %>%
    testthat::expect_warning(expected_warning)
})

# Requirement T4 ---------------------------------------------------------------

testthat::context("render - T4. The function `check_rendering_input()` only permits valid `output_format` and `engine` options.")

testthat::test_that("T4.1 No error when `output_format` is `html` or `latex` and `engine` is `kable`, `gt`, `dt`, `datatable` or `datatables`.", {
  for (output_format in c("html", "latex")) {
    for (engine in c("kable", "gt", "dt", "datatable", "datatables")) {
      visR:::check_rendering_input(
        output_format = output_format,
        engine = engine
      ) %>%
        testthat::expect_error(NA)
    }
  }
})

testthat::test_that("T4.2 An error when `output_format` and/or `engine` are missing, `NULL` or `NA`.", {
  arg_missing_waring <- "Please provide an output_format and an engine."
  visR:::check_rendering_input(output_format = "visR") %>%
    testthat::expect_error(arg_missing_waring)
  visR:::check_rendering_input(engine = "visR") %>%
    testthat::expect_error(arg_missing_waring)
  visR:::check_rendering_input(output_format = "html", engine = NULL) %>%
    testthat::expect_error(arg_missing_waring)
  visR:::check_rendering_input(engine = "kable", output_format = NULL) %>%
    testthat::expect_error(arg_missing_waring)
  visR:::check_rendering_input(engine = NULL, output_format = NULL) %>%
    testthat::expect_error(arg_missing_waring)

  expected_error <- "Currently implemented output engines are kable, gt and jquery datatables \\(DT\\). NA is not yet supported."
  visR:::check_rendering_input(output_format = "html", engine = NA) %>%
    testthat::expect_error(expected_error)

  expected_error <- "Currently supported output formats are html and latex. NA is not yet supported."
  visR:::check_rendering_input(engine = "kable", output_format = NA) %>%
    testthat::expect_error()
})

testthat::test_that("T4.3 An error when `output_format` is not `html` or `latex` and `engine` is a valid option.", {
  expected_error <- "Currently supported output formats are html and latex. visR is not yet supported."
  visR:::check_rendering_input(engine = "kable", output_format = "visR") %>%
    testthat::expect_error(expected_error)
})

testthat::test_that("T4.4 An error when `engine` is not `kable`, `gt`, `dt`, `datatables` or `datatable` and `output_format` is a valid option.", {
  expected_error <- "Currently implemented output engines are kable, gt and jquery datatables \\(DT\\). visR is not yet supported."
  visR:::check_rendering_input(output_format = "html", engine = "visR") %>%
    testthat::expect_error(expected_error)
})

# Requirement T5 ---------------------------------------------------------------

testthat::context("render - T5. The function `render_datatable.data.frame()` creates an `htmlwidget` of the table.")

testthat::test_that("T5.1 No error when `data` is a `data.frame`.", {
  adtte %>%
    visR:::render_datatable.data.frame(
      title = "visR",
      download_format = "csv",
      source_cap = "visR"
    ) %>%
    testthat::expect_error(NA)
})

testthat::test_that("T5.2 The returned object is of type `htmlwidget`.", {
  tmp <- adtte %>%
    visR:::render_datatable.data.frame(
      title = "visR_title",
      download_format = "csv",
      source_cap = "visR_source_cap"
    )

  testthat::expect_true(inherits(tmp, "htmlwidget"))
})

testthat::test_that("T5.3 The `title` is passed along to the HTML widget.", {
  widget_title <- "visR_title"

  tmp <- adtte %>%
    visR:::render_datatable.data.frame(
      title = widget_title,
      download_format = "csv",
      source_cap = "visR_source_cap"
    )

  testthat::expect_true(grepl(widget_title, tmp$x$caption))
})

testthat::test_that("T5.4 The `source_cap` is passed along to the HTML widget.", {
  source_cap <- "visR_source_cap"

  tmp <- adtte %>%
    visR:::render_datatable.data.frame(
      title = "visR_title",
      download_format = "csv",
      source_cap = source_cap
    )

  testthat::expect_true(grepl(source_cap, tmp$x$options$drawCallback))
})

testthat::test_that("T5.5 When `download_format` is not `NULL`, a button is added.", {
  download_format <- "visR_csv"

  tmp <- adtte %>%
    visR:::render_datatable.data.frame(
      title = "visR_title",
      download_format = download_format,
      source_cap = "visR_source_cap"
    )

  testthat::expect_equal(tmp$x$options$buttons[[1]], download_format)
})

testthat::test_that("T5.6 When `download_format` is `NULL`, no button is added.", {
  tmp <- adtte %>%
    visR:::render_datatable.data.frame(
      title = "visR_title",
      download_format = NULL,
      source_cap = "visR_source_cap"
    )

  testthat::expect_false("buttons" %in% names(tmp$x$options))
})

# Requirement T6 ---------------------------------------------------------------

testthat::context("render - T6. The function `get_gt.data.frame()` properly passes the input along to `gt::gt()`.")

testthat::test_that("T6.1 No error when `data` is a `data.frame`.", {
  adtte %>%
    visR:::get_gt.data.frame() %>%
    testthat::expect_error(NA)
})

testthat::test_that("T6.2 The returned object is of type `gt_tbl`.", {
  tmp <- adtte %>%
    visR:::get_gt.data.frame()

  testthat::expect_true(inherits(tmp, "gt_tbl"))
})

# END OF CODE -------------------------------------------------------------
