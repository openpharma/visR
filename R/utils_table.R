#' Calculate summary statistics for a vector
#'
#' Calculates several summary statistics for a vector depending on the vector class
#'
#' @param x an object
#' @return A summarized version of the input.
#' @export
summarize_long <- function(x) UseMethod("summarize_long")


#' Create variable summary for factors
#'
#' @param x an object of class "factor"
#' @return Long list of summary statistics for the input factors.
#' @export
summarize_long.factor <- function(x){
  x1 <- forcats::fct_explicit_na(x, na_level = "Missing")

  dat <- tibble::enframe(x1) %>%
    dplyr::group_by(value) %>%
    dplyr::summarise(N = dplyr::n()) %>%
    dplyr::mutate(`%` = round(100 * N/sum(N), 3)) %>%
    tidyr::pivot_wider(names_from = value, values_from = c("N", "%"), names_sep=" ") %>%
    as.list()
  list(dat)
}

#' Create variable summary for numeric variables
#'
#' @param x an object of class "integer"
#' @return Long list of summary statistics for the input.
#' @export
summarize_long.integer <- function(x){
  summarize_long.numeric(x)
}

#' Create variable summary for numeric variables
#'
#' @param x an object of class "numeric"
#' @return Long list of summary statistics for the input.
#' @export
summarize_long.numeric <- function(x){
  dat <- list(
    mean = mean(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    Q1 = quantile(x, probs=0.25, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    Q3 = quantile(x, probs=0.75, na.rm = TRUE),
    max = max(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE)
  )
  list(dat)
}

#' Create variable summary for all other variable types
#'
#' @param x an object of any other class
#' @return List of counts for unique and missing values in `x`.
#' @export
summarize_long.default <- function(x){
  dat <- list(
    unique_values = length(unique(x)),
    nmiss = sum(is.na(x))
  )
  list(dat)
}

#' Create abbreviated variable summary for table1
#'
#' This function creates summaries combines multiple summary measures in a single formatted string.
#'
#' @param x a vector to be summarized
#' @return A summarized less detailed version of the input.
#' @export
summarize_short <- function(x) UseMethod("summarize_short")

#' Create variable summary for factors
#'
#' Calculates N and % of occurrence for each factor value
#'
#' @param x an object of class "factor"
#' @return Short list of summary statistics for the input factors.
#' @export
summarize_short.factor <- function(x){
  x1 <- forcats::fct_explicit_na(x, na_level = "Missing")

  dat <- tibble::enframe(x1) %>%
    dplyr::group_by(value) %>%
    dplyr::summarise(N = dplyr::n()) %>%
    dplyr::mutate(`n (%)` = paste0(N, " (", format(100 * N/sum(N), digits = 3, trim=TRUE), "%)")) %>%
    dplyr::select(-N) %>%
    tidyr::pivot_wider(names_from = value, values_from = c("n (%)"), names_sep=" ") %>%
    as.list()
  list(dat)
}

#' Create variable summary for numeric variables
#'
#' Calculates mean (standard deviation), median (IQR), min-max range and N/% missing elements
#' for a numeric vector.
#'
#' @param x an object of class "numeric"
#' @return Short list of summary statistics for the input.
#' @export
summarize_short.numeric <- function(x){
  dat <- list(
    `Mean (SD)` = paste0(format(mean(x, na.rm = TRUE), digits = 3), " (", format(sd(x, na.rm = TRUE), digits = 3), ")"),
    `Median (IQR)` = paste0(format(median(x, na.rm = TRUE), digits = 3), " (", format(quantile(x, probs=0.25, na.rm = TRUE), digits = 3),
                            "-", format(quantile(x, probs=0.75, na.rm = TRUE), digits = 3), ")"),
    `Min-max` = paste0(format(min(x, na.rm = TRUE), digits = 3), "-", format(max(x, na.rm = TRUE), digits = 3)),
    Missing = paste0(format(sum(is.na(x)), digits = 3),
                     " (", format(100 * sum(is.na(x))/length(x), trim=TRUE, digits = 3), "%)")
  )
  list(dat)
}

#' Create variable summary for integer variables
#'
#' Calculates mean (standard deviation), median (IQR), min-max range and N/% missing elements
#' for a integer vector.
#'
#' @param x an object of class "integer"
#' @return Short list of summary statistics for the input.
#' @export
summarize_short.integer <- function(x){
  summarize_short.numeric(x)
}

#' Create variable summary for all other variable types
#'
#' @param x an object of any other class
#' @return List of counts for unique and missing values in `x`.
#' @export
summarize_short.default <- function(x){
  dat <- list(
    `Unique values` = format(length(unique(x))),
    `Missing (%)` = paste0(format(sum(is.na(x))), " (", format(100 * sum(is.na(x))/length(x), trim=TRUE), "%)")
  )
  list(dat)
}