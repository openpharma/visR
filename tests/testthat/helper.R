#' @title Collections of functions only used in testthat tests

#' A helper function for catching errors in the ggplot2 traceback stack
#' @keywords internal

check_traceback_stack_for_ggplot_aesthetics_warning <- function() {
  
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

map_numbers_to_new_range <- function(numbers, lower, upper) {
  
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

#' A helper function to get all files in the ~/R/ folder
#' @keywords internal

get_files_in_R_folder <- function() {
  
  R_files <- base::list.files(path = base::paste0(base::getwd(), "/../../R"), 
                                pattern = "*.R", 
                                full.names = TRUE)
  
  return(unlist(R_files))
  
}

#' A helper function to get all files in the ~/tests/testthat/ folder
#' @keywords internal

get_files_in_test_folder <- function() {
  
  test_files <- base::list.files(path = base::getwd(), 
                                 pattern = "*.R", 
                                 full.names = TRUE)
  
  return(unlist(test_files))
  
}

#' A helper function to get all files in the ~/man/ folder
#' @keywords internal

get_files_in_man_folder <- function() {
  
  man_files <- base::list.files(path = base::paste0(base::getwd(), "/../../man"), 
                                pattern = "*.Rd", 
                                full.names = TRUE)
  
  return(unlist(man_files))
  
}

#' A helper function to get all files in the ~/vignettes/ folder
#' @keywords internal

get_files_in_vignettes_folder <- function() {
  
  vignette_files <- base::list.files(path = base::paste0(base::getwd(), "/../../vignettes"), 
                                     pattern = "*.Rmd", 
                                     full.names = TRUE)
  
  return(unlist(vignette_files))
  
}

#' A helper function to conditionally retrieve files of the package
#' @keywords internal

get_visR_files <- function(functions = FALSE,
                           tests = FALSE,
                           documentation = FALSE,
                           vignettes = FALSE,
                           remove_watchdog = TRUE) {
  
  files <- list()
  
  if (functions)     {files <- c(files, get_files_in_R_folder())}
  if (tests)         {files <- c(files, get_files_in_test_folder())}
  if (documentation) {files <- c(files, get_files_in_man_folder())}
  if (vignettes)     {files <- c(files, get_files_in_vignettes_folder())}
  
  if (remove_watchdog) {files <- files[!grepl("test-CRAN_watchdog.R", files)] }
  
  return(unlist(files))
  
}

#' A helper function that compares the width of the grobs comprising the two given ggplot objects
#' @keywords internal

check_grob_width_equal <- function(gg_A, gg_B) {
  
  gg_A_grob <- ggplot2::ggplotGrob(gg_A)
  gg_B_grob <- ggplot2::ggplotGrob(gg_B)
  
  gg_A_grob_widths <- gg_A_grob$widths
  gg_B_grob_widths <- gg_B_grob$widths
  
  widths <- cbind(as.character(gg_A_grob_widths), as.character(gg_B_grob_widths))
  tmp <- c()
  
  for (i in 1:nrow(widths)) {
    
    if (widths[i, 1] == widths[i, 2]) {
      
      tmp <- c(tmp, TRUE)
      
    } else {
      
      tmp <- c(tmp, FALSE)
      
    }
  }
  
  return(length(tmp[tmp != TRUE]))
  
}

# get_pvalue - Results to compare against ---------------------------------

  ref1 <- survival::survdiff(formula = survival::Surv(AVAL, 1 - CNSR) ~ TRTA, data = adtte, rho=0)
  ref2 <- survival::survdiff(formula = survival::Surv(AVAL, 1 - CNSR) ~ TRTA, data = adtte, rho=1)
  ref3 <- survival::survdiff(formula = survival::Surv(AVAL, 1 - CNSR) ~ TRTA, data = adtte, rho=1.5)
  ref4 <- survival::survdiff(formula = survival::Surv(AVAL, 1 - CNSR) ~ TRTA, data = adtte, rho=2.4)

  get_pvalue_ref1 <- data.frame(
    `Equality across strata` = "Log-Rank",
    Chisq = format(round(ref1$chisq,3), nsmall = 3, width = 6, justify = "right", scientific = FALSE),
    df = length(ref1$n)-1,
    `p-value` = .pvalformat(stats::pchisq(ref1$chisq, length(ref1$n) - 1, lower.tail = FALSE)),
    check.names = FALSE,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
  get_pvalue_ref2 <- data.frame(
    `Equality across strata` = "Wilcoxon",
    Chisq = format(round(ref2$chisq,3), nsmall = 3, width = 6, justify = "right", scientific = FALSE),
    df = length(ref2$n)-1,
    `p-value` = .pvalformat(stats::pchisq(ref2$chisq, length(ref2$n) - 1, lower.tail = FALSE)),
    check.names = FALSE,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
  get_pvalue_ref3 <- data.frame(
    `Equality across strata` = "Tarone-Ware",
    Chisq = format(round(ref3$chisq,3), nsmall = 3, width = 6, justify = "right", scientific = FALSE),
    df = length(ref3$n)-1,
    `p-value` = .pvalformat(stats::pchisq(ref3$chisq, length(ref3$n) - 1, lower.tail = FALSE)),
    check.names = FALSE,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
  get_pvalue_ref4 <- data.frame(
    `Equality across strata` = paste0("Harrington and Fleming test (rho = ", 2.4, ")"),
    Chisq = format(round(ref4$chisq,3), nsmall = 3, width = 6, justify = "right", scientific = FALSE),
    df = length(ref4$n)-1,
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
