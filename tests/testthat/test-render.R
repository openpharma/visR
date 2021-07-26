#' @title Specifications render
#' @section Last updated by: Tim Treis
#' @section Last update date: 25-JULY-2021

# Specifications ---------------------------------------------------------------

#' T1. The function `render.tableone()` properly renders a `tableone` object.
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
#' T1.11 An error when `output_format` is an invalid parameter.
#' T1.12 An error when `engine` is an invalid parameter.
#' T1.13 No error when `output_format` is 'latex' and `engine` is 'kable'.
#' T1.14 A warning when `output_format` is 'latex' and `engine` is 'dt', 'datatable' or 'datatables'.
#' T1.15 No error when `engine` is in ['dt', 'datatable', 'datatables'] and download_format` is in ['copy', 'csv', 'excel'].
#' T1.16 A warning when `engine` is not in ['dt', 'datatable', 'datatables'] and download_format` is in ['copy', 'csv', 'excel'].

# Requirement T1 ---------------------------------------------------------------

context("tableone - T1. The function `render.tableone()` properly renders a `tableone` object.")

testthat::test_that("T1.1 No error when `data` is a `tableone` object.", {
  
  adtte_tableone <- adtte %>%
    visR::get_tableone() 
  
  testthat::expect_true("tableone" %in% class(adtte_tableone))
  adtte_tableone %>% visR:::render.tableone(title = NULL, datasource = NULL) %>%
    testthat::expect_error(NA)
  
})

testthat::test_that("T1.2 An error when `data` is not a `tableone` object.", {
  
  adtte_tableone <- adtte
  
  testthat::expect_false("tableone" %in% class(adtte_tableone))
  adtte_tableone %>% visR:::render.tableone(title = NULL, datasource = NULL) %>%
    testthat::expect_error()
  
})

testthat::test_that("T1.3 An error when `title` is missing.", {
  
  adtte_tableone <- adtte %>%
    visR::get_tableone() 
  
  adtte_tableone %>% visR:::render.tableone(datasource = NULL) %>%
    testthat::expect_error()
  
})

testthat::test_that("T1.4 No error when `title` is defined.", {
  
  adtte_tableone <- adtte %>%
    visR::get_tableone() 
  
  adtte_tableone %>% visR:::render.tableone(title = NULL, datasource = NULL) %>%
    testthat::expect_error(NA)
  
  adtte_tableone %>% visR:::render.tableone(title = 1, datasource = NULL) %>%
    testthat::expect_error(NA)
  
  adtte_tableone %>% visR:::render.tableone(title = "visR", datasource = NULL) %>%
    testthat::expect_error(NA)
  
  adtte_tableone %>% visR:::render.tableone(title = c(1, 2, 3), datasource = NULL) %>%
    testthat::expect_error(NA)
  
})

testthat::test_that("T1.5 An error when `datasource` is missing.", {
  
  adtte_tableone <- adtte %>%
    visR::get_tableone() 
  
  adtte_tableone %>% visR:::render.tableone(title = NULL) %>%
    testthat::expect_error()
  
})

testthat::test_that("T1.6 No error when `datasource` is defined.", {
  
  adtte_tableone <- adtte %>%
    visR::get_tableone() 
  
  adtte_tableone %>% visR:::render.tableone(title = NULL, datasource = NULL) %>%
    testthat::expect_error(NA)
  
  adtte_tableone %>% visR:::render.tableone(title = NULL, datasource = 1) %>%
    testthat::expect_error(NA)
  
  adtte_tableone %>% visR:::render.tableone(title = NULL, datasource = "visR") %>%
    testthat::expect_error(NA)
  
  adtte_tableone %>% visR:::render.tableone(title = NULL, datasource = c(1, 2, 3)) %>%
    testthat::expect_error(NA)
  
})

testthat::test_that("T1.7 No error when `footnote` is defined.", {
  
  adtte_tableone <- adtte %>%
    visR::get_tableone() 
  
  adtte_tableone %>% visR:::render.tableone(title = NULL, datasource = NULL, footnote = NULL) %>%
    testthat::expect_error(NA)
  
  adtte_tableone %>% visR:::render.tableone(title = NULL, datasource = NULL, footnote = 1) %>%
    testthat::expect_error(NA)
  
  adtte_tableone %>% visR:::render.tableone(title = NULL, datasource = NULL, footnote = "visR") %>%
    testthat::expect_error(NA)
  
  adtte_tableone %>% visR:::render.tableone(title = NULL, datasource = NULL, footnote = c(1, 2, 3)) %>%
    testthat::expect_error(NA)
  
})

testthat::test_that("T1.8 No error when `output_format` is 'html' and `engine` is 'gt'.", {
  
  adtte_tableone <- adtte %>%
    visR::get_tableone() 
  
  adtte_tableone %>% visR:::render.tableone(title = NULL, 
                                            datasource = NULL, 
                                            output_format = "html", 
                                            engine = "gt") %>%
    testthat::expect_error(NA)
  
})

testthat::test_that("T1.9 No error when `output_format` is 'html' and `engine` is 'kable'.", {
  
  adtte_tableone <- adtte %>%
    visR::get_tableone() 
  
  adtte_tableone %>% visR:::render.tableone(title = NULL, 
                                            datasource = NULL, 
                                            output_format = "html", 
                                            engine = "kable") %>%
    testthat::expect_error(NA)
  
})

testthat::test_that("T1.10 No error when `output_format` is 'html' and `engine` is 'dt', 'datatable' or 'datatables'.", {
  
  adtte_tableone <- adtte %>%
    visR::get_tableone() 
  
  adtte_tableone %>% visR:::render.tableone(title = NULL, 
                                            datasource = NULL, 
                                            output_format = "html", 
                                            engine = "dt") %>%
    testthat::expect_error(NA)
  
  adtte_tableone %>% visR:::render.tableone(title = NULL, 
                                            datasource = NULL, 
                                            output_format = "html", 
                                            engine = "datatable") %>%
    testthat::expect_error(NA)
  
  adtte_tableone %>% visR:::render.tableone(title = NULL, 
                                            datasource = NULL, 
                                            output_format = "html", 
                                            engine = "datatables") %>%
    testthat::expect_error(NA)
  
})

testthat::test_that("T1.11 An error when `output_format` is an invalid parameter.", {
  
  adtte_tableone <- adtte %>%
    visR::get_tableone() 
  
  adtte_tableone %>% visR:::render.tableone(title = NULL, 
                                            datasource = NULL, 
                                            output_format = NULL) %>%
    testthat::expect_error()
  
  adtte_tableone %>% visR:::render.tableone(title = NULL, 
                                            datasource = NULL, 
                                            output_format = 1) %>%
    testthat::expect_error()
  
  adtte_tableone %>% visR:::render.tableone(title = NULL, 
                                            datasource = NULL, 
                                            output_format = "visR") %>%
    testthat::expect_error()
  
})

testthat::test_that("T1.12 An error when `engine` is an invalid parameter.", {
  
  adtte_tableone <- adtte %>%
    visR::get_tableone() 
  
  adtte_tableone %>% visR:::render.tableone(title = NULL, 
                                            datasource = NULL, 
                                            engine = NULL) %>%
    testthat::expect_error()
  
  adtte_tableone %>% visR:::render.tableone(title = NULL, 
                                            datasource = NULL, 
                                            engine = 1) %>%
    testthat::expect_error()
  
  adtte_tableone %>% visR:::render.tableone(title = NULL, 
                                            datasource = NULL, 
                                            engine = "visR") %>%
    testthat::expect_error()
  
})

testthat::test_that("T1.13 No error when `output_format` is 'latex' and `engine` is 'kable'.", {
  
  adtte_tableone <- adtte %>%
    visR::get_tableone() 
  
  adtte_tableone %>% visR:::render.tableone(title = NULL, datasource = NULL, output_format = "latex", engine = "kable") %>%
    testthat::expect_error(NA)
  
})

testthat::test_that("T1.14 A warning when `output_format` is 'latex' and `engine` is 'dt', 'datatable' or 'datatables'.", {
  
  adtte_tableone <- adtte %>%
    visR::get_tableone() 
  
  adtte_tableone %>% visR:::render.tableone(title = NULL, 
                                            datasource = NULL, 
                                            output_format = "latex", 
                                            engine = "dt") %>%
    testthat::expect_warning()
  
  adtte_tableone %>% visR:::render.tableone(title = NULL, 
                                            datasource = NULL, 
                                            output_format = "latex", 
                                            engine = "datatable") %>%
    testthat::expect_warning()
  
  adtte_tableone %>% visR:::render.tableone(title = NULL, 
                                            datasource = NULL, 
                                            output_format = "latex", 
                                            engine = "datatables") %>%
    testthat::expect_warning()
  
})

testthat::test_that("T1.15 No error when `engine` is in ['dt', 'datatable', 'datatables'] and download_format` is in ['copy', 'csv', 'excel'].", {
  
  adtte_tableone <- adtte %>%
    visR::get_tableone() 
  
  for (engine in c("dt", "datatable", "datatables")) {
    
    for (download_format in c("copy", "csv", "excel")) {
      
      adtte_tableone %>% visR:::render.tableone(title = NULL, 
                                                datasource = NULL, 
                                                engine = engine, 
                                                download_format = download_format) %>%
        testthat::expect_error(NA)
      
    }
  }
  
})

testthat::test_that("T1.16 A warning when `engine` is not in ['dt', 'datatable', 'datatables'] and download_format` is in ['copy', 'csv', 'excel'].", {
  
  adtte_tableone <- adtte %>%
    visR::get_tableone() 
  
  for (engine in c("gt", "kable")) {
    
    for (download_format in c("copy", "csv", "excel")) {
      
      adtte_tableone %>% visR:::render.tableone(title = NULL, 
                                                datasource = NULL, 
                                                engine = engine, 
                                                download_format = download_format) %>%
        testthat::expect_warning()
      
    }
  }
  
})

# END --------------------------------------------------------------------------
