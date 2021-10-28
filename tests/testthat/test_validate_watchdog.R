#' @title Specifications validate_watchdog
#' @section Last updated by: Tim Treis
#' @section Last update date: 27-OCTOBER-2021

# Specifications ---------------------------------------------------------------
#' 
#' T1. Details on last change for test files are recorded.
#' T1.1 executed.
#' 
# Requirement T1 ---------------------------------------------------------------

context("validate_watchdog - T1. Details on last change for test files are recorded.")

testthat::test_that("T1.1 - executed.",{

  test_files <- get_visR_files(tests = TRUE)
  
  test_files <- test_files[!(grepl("watchdog", test_files))]
  test_files <- test_files[!(grepl("helper", test_files))]
  
  last_change_df <- data.frame(full_path = test_files,
                               name = basename(test_files))
  
  # Get information from last commit that changed the respective file
  cmd <- "git log -1 --pretty=format:'%an (%ae);%ad' --date=format:'%Y-%m-%d %H:%M:%S' "
  
  last_change_df["log"] <- sapply(last_change_df$full_path, function(x) {
    system(paste0(cmd, x), intern = TRUE)
  })
  
  last_change_df <- last_change_df %>% 
    tidyr::separate(log, into = c("last_edit_by", "last_edit_when"), sep = ";") %>%
    dplyr::mutate(last_edit_by = gsub("\\@", "(at)", last_edit_by))
  
  apply(last_change_df, 1, function(x) {
    
    file_content <- readChar(x["full_path"], file.info(x["full_path"])$size)
    
    new_header <- paste0("#' \\@title Specifications ", x["name"], "\n")
    new_header <- paste0(new_header, "#' \\@section Last updated by: ", x["last_edit_by"], "\n")
    new_header <- paste0(new_header, "#' \\@section Last update date: ", x["last_edit_when"], "\n#'\n")
    new_header <- paste0(new_header, "#' \\@section List of tested specifications", "\n")
    new_header <- paste0(new_header, .get_test_TOC(x["full_path"]), "\n")
    new_header <- paste0(new_header, "#' Requirement T1 ", paste0(rep("-", 102), collapse = ""), "\n\ncontext")
    
    file_content <- gsub(".*\\@title(.+?)\n.+?context", new_header, file_content)

    cat(file_content, file = x["full_path"])
    
  })
  
  testthat::expect_true(TRUE)

})


# END OF CODE ------------------------------------------------------------------
