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


# get_pvalue - Results to compare against ---------------------------------

  ref1 <- survival::survdiff(formula = survival::Surv(AVAL, 1 - CNSR) ~ TRTA, data = adtte, rho=0)
  ref2 <- survival::survdiff(formula = survival::Surv(AVAL, 1 - CNSR) ~ TRTA, data = adtte, rho=1)
  ref3 <- survival::survdiff(formula = survival::Surv(AVAL, 1 - CNSR) ~ TRTA, data = adtte, rho=1.5)
  ref4 <- survival::survdiff(formula = survival::Surv(AVAL, 1 - CNSR) ~ TRTA, data = adtte, rho=2.4)

  get_pvalue_ref1 <- data.frame(
    `Equality across strata` = "Log-Rank",
    Chisq = ref1$chisq,
    df = length(ref1$n)-1,
    `p-value` = .pvalformat(stats::pchisq(ref1$chisq, length(ref1$n) - 1, lower.tail = FALSE)),
    check.names = FALSE,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
  get_pvalue_ref2 <- data.frame(
    `Equality across strata` = "Wilcoxon",
    Chisq = ref2$chisq,
    df = length(ref2$n)-1,
    `p-value` = .pvalformat(stats::pchisq(ref2$chisq, length(ref2$n) - 1, lower.tail = FALSE)),
    check.names = FALSE,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
  get_pvalue_ref3 <- data.frame(
    `Equality across strata` = "Tarone-Ware",
    Chisq = ref3$chisq,
    df = length(ref3$n)-1,
    `p-value` = .pvalformat(stats::pchisq(ref3$chisq, length(ref3$n) - 1, lower.tail = FALSE)),
    check.names = FALSE,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
  get_pvalue_ref4 <- data.frame(
    `Equality across strata` = paste0("Harrington and Fleming test (rho = ", 2.4, ")"),
    Chisq = ref4$chisq,
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
    make.row.names = F
  )

  get_pvalue_ref134 <- base::rbind.data.frame(
    get_pvalue_ref1,
    get_pvalue_ref3,
    get_pvalue_ref4,
    make.row.names = F
  )