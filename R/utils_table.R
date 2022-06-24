#' @title Calculate summary statistics from a vector
#'
#' @description
#' `r lifecycle::badge("questioning")`
#' Calculates several summary statistics. The summary statistics
#' depend on the vector class
#'
#' @param x an object
#' @return A summarized version of the input.
#' @rdname summarize_long
#' @export
summarize_long <- function(x) {
  UseMethod("summarize_long")
}


#' @rdname summarize_long
#' @method summarize_long factor
#' @export
summarize_long.factor <- function(x) {
  x1 <- forcats::fct_explicit_na(x, na_level = "Missing")

  dat <- tibble::enframe(x1) %>%
    dplyr::group_by(value) %>%
    dplyr::summarise(N = dplyr::n()) %>%
    dplyr::mutate(`%` = round(100 * N / sum(N), 3)) %>%
    tidyr::pivot_wider(names_from = value, values_from = c("N", "%"), names_sep = " ") %>%
    as.list()
  list(dat)
}

#' @method summarize_long integer
#' @rdname summarize_long
#' @export
summarize_long.integer <- function(x) {
  summarize_long.numeric(x)
}

#' @method summarize_long numeric
#' @rdname summarize_long
#' @export
summarize_long.numeric <- function(x) {
  dat <- list(
    mean = mean(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    Q1 = quantile(x, probs = 0.25, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    Q3 = quantile(x, probs = 0.75, na.rm = TRUE),
    max = max(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE)
  )
  list(dat)
}

#' @method summarize_long default
#' @rdname summarize_long
#' @export
summarize_long.default <- function(x) {
  dat <- list(
    unique_values = length(unique(x)),
    nmiss = sum(is.na(x))
  )
  list(dat)
}

#' @title Create abbreviated variable summary for table1
#'
#' @description
#' `r lifecycle::badge("questioning")`
#' This function creates summaries combines multiple summary
#' measures in a single formatted string. Create variable summary for numeric variables. Calculates mean
#' (standard deviation), median (IQR), min-max range and N/% missing elements
#' for a numeric vector.
#'
#' Create variable summary for integer variables
#' Calculates mean (standard deviation), median (IQR), min-max range
#' and N/% missing elements for a integer vector.
#'
#' @param x a vector to be summarized
#' @return A summarized less detailed version of the input.
#' @rdname summarize_short
#' @export
summarize_short <- function(x) {
  UseMethod("summarize_short")
}


#' @method summarize_short factor
#' @rdname summarize_short
#' @export
summarize_short.factor <- function(x) {
  x1 <- forcats::fct_explicit_na(x, na_level = "Missing")

  dat <- tibble::enframe(x1) %>%
    dplyr::group_by(value) %>%
    dplyr::summarise(N = dplyr::n()) %>%
    dplyr::mutate(`n (%)` = paste0(N, " (", format(100 * N / sum(N), digits = 3, trim = TRUE), "%)")) %>%
    dplyr::select(-N) %>%
    tidyr::pivot_wider(names_from = value, values_from = c("n (%)"), names_sep = " ") %>%
    as.list()
  list(dat)
}

#' @method summarize_short numeric
#' @rdname summarize_short
#' @export
summarize_short.numeric <- function(x) {
  dat <- list(
    `Mean (SD)` = paste0(format(mean(x, na.rm = TRUE), digits = 3), " (", format(sd(x, na.rm = TRUE), digits = 3), ")"),
    `Median (IQR)` = paste0(
      format(median(x, na.rm = TRUE), digits = 3), " (", format(quantile(x, probs = 0.25, na.rm = TRUE), digits = 3),
      "-", format(quantile(x, probs = 0.75, na.rm = TRUE), digits = 3), ")"
    ),
    `Min-max` = paste0(format(min(x, na.rm = TRUE), digits = 3), "-", format(max(x, na.rm = TRUE), digits = 3)),
    Missing = paste0(
      format(sum(is.na(x)), digits = 3),
      " (", format(100 * sum(is.na(x)) / length(x), trim = TRUE, digits = 3), "%)"
    )
  )
  list(dat)
}

#' @method summarize_short integer
#' @rdname summarize_short
#' @export
summarize_short.integer <- function(x) {
  summarize_short.numeric(x)
}

#' @method summarize_short default
#' @rdname summarize_short
#' @export
summarize_short.default <- function(x) {
  dat <- list(
    `Unique values` = format(length(unique(x))),
    `Missing (%)` = paste0(format(sum(is.na(x))), " (", format(100 * sum(is.na(x)) / length(x), trim = TRUE), "%)")
  )
  list(dat)
}
