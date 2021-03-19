#' @title Collections of functions only used in testthat tests
#' @section Last updated by:
#' Tim Treis
#' @section Last update date:
#' 18-MAR-2021

#' A helper function for catching errors in the ggplot2 traceback stack
#' @keywords internal
#' @export
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
#' @export
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
