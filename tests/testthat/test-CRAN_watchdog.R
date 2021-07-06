#' @title Specifications CRAN_watchdog
#' @section Last updated by:
#' Tim Treis
#' @section Last update date:
#' 05-JULY-2021

# Specifications ----------------------------------------------------------
#' T1. Our codebase doesn't violate CRAN style-guidelines
#' T1.1 TRUE/FALSE are used instead of T/F
#' T1.2 Each function documentation contains a \value{} tag 

# Requirement T1 ----------------------------------------------------------

context("CRAN_watchdog - T1. Our codebase doesn't violate CRAN style-guidelines")

testthat::test_that("T1.1 TRUE/FALSE are used instead of T/F",{
  
  test_files <- base::list.files(path = base::getwd(), 
                                 pattern = "*.R", 
                                 full.names = FALSE)
  
  # Remove this file
  test_files <- test_files[test_files != "test-CRAN_watchdog.R"] 
  
  patterns <- list("=T,",  "=T ",  "=T)",
                   "=F,",  "=F ",  "=F)",
                   "= T,", "= T ", "= T)",
                   "= F,", "= F ", "= F)")
  
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
  
  testthat::expect_true(base::nrow(CRAN_incompabilities) == 0)
  
})

testthat::test_that("T1.2 Each function documentation contains a \value{} tag ",{
  
  test_files <- base::list.files(path = base::paste0(base::getwd(), "/../../man"), 
                                 pattern = "*.Rd", 
                                 full.names = TRUE)
  
  # List of files in which we don't expect a return value.
  
  exceptions <- list("adtte.Rd",
                     "brca_cohort.Rd",
                     "visR-Global.Rd")
  exceptions_collapsed <- paste(exceptions, collapse = "|")
  
  CRAN_incompabilities <- data.frame()
  
  for (test_file in test_files) {
    
    if (sum(grepl(exceptions_collapsed, test_file)) == 0) {
      
      name_hits <- base::grep("\\\\name\\{", base::readLines(test_file, warn = FALSE))
      value_hits <- base::grep("\\\\value\\{", base::readLines(test_file, warn = FALSE))
      
      # The number of \name{} and \value{} should be the same since this means 
      # that each function has a return value.
      
      if (length(name_hits) != length(value_hits)) {
        
        name_hits  <- base::ifelse(length(name_hits) == 0, NA, name_hits)
        value_hits <- base::ifelse(length(name_hits) == 0, NA, value_hits)
        
        tmp <- data.frame("name_line" = name_hits)
        tmp[["value_line"]] <- value_hits
        tmp[["file"]] <- test_file
        CRAN_incompabilities <- base::rbind(CRAN_incompabilities, tmp)
        
      }
    }
  }
  
  print(CRAN_incompabilities)
  
  testthat::expect_true(base::nrow(CRAN_incompabilities) == 0)
  
})

# END OF CODE ----------------------------------------------------------

