#' @title Calculate summary statistics
#'
#' @description
#' `r lifecycle::badge("questioning")`
#' S3 method for creating a table of summary statistics.
#' The summary statistics can be used for presentation in tables such as table one or baseline and demography tables.
#'
#' The summary statistics estimated are conditional on the variable type: continuous, binary, categorical, etc.
#'
#' By default the following summary stats are calculated:
#' * Numeric variables: mean, min, 25th-percentile, median, 75th-percentile, maximum, standard deviation
#' * Factor variables: proportion of each factor level in the overall dataset
#' * Default: number of unique values and number of missing values
#'
#' @param data The dataset to summarize as dataframe or tibble
#' @param strata Stratifying/Grouping variable name(s) as character vector. If NULL, only overall results are returned
#' @param overall If TRUE, the summary statistics for the overall dataset are also calculated
#' @param summary_function A function defining summary statistics for numeric and categorical values
#' @details It is possible to provide your own summary function. Please have a loot at summary for inspiration.
#'
#' @note All columns in the table will be summarized. If only some columns shall be used, please select only those
#' variables prior to creating the summary table by using dplyr::select()
#'
#' @examples
#'
#' # Example using the ovarian data set
#'
#' survival::ovarian %>%
#'   dplyr::select(-fustat) %>%
#'   dplyr::mutate(
#'     age_group = factor(
#'       dplyr::case_when(
#'         age <= 50 ~ "<= 50 years",
#'         age <= 60 ~ "<= 60 years",
#'         age <= 70 ~ "<= 70 years",
#'         TRUE ~ "> 70 years"
#'       )
#'     ),
#'     rx = factor(rx),
#'     ecog.ps = factor(ecog.ps)
#'   ) %>%
#'   dplyr::select(age, age_group, everything()) %>%
#'   visR::get_tableone()
#'
#' # Examples using ADaM data
#'
#' # display patients in an analysis set
#' adtte %>%
#'   dplyr::filter(SAFFL == "Y") %>%
#'   dplyr::select(TRTA) %>%
#'   visR::get_tableone()
#'
#' ## display overall summaries for demog
#' adtte %>%
#'   dplyr::filter(SAFFL == "Y") %>%
#'   dplyr::select(AGE, AGEGR1, SEX, RACE) %>%
#'   visR::get_tableone()
#'
#' ## By actual treatment
#' adtte %>%
#'   dplyr::filter(SAFFL == "Y") %>%
#'   dplyr::select(AGE, AGEGR1, SEX, RACE, TRTA) %>%
#'   visR::get_tableone(strata = "TRTA")
#'
#' ## By actual treatment, without overall
#' adtte %>%
#'   dplyr::filter(SAFFL == "Y") %>%
#'   dplyr::select(AGE, AGEGR1, SEX, EVNTDESC, TRTA) %>%
#'   visR::get_tableone(strata = "TRTA", overall = FALSE)
#'
#' @rdname get_tableone
#' @export
#'
get_tableone <- function(data, strata = NULL, overall = TRUE, summary_function = summarize_short) {
  UseMethod("get_tableone")
}

#' @rdname get_tableone
#' @method get_tableone default
#' @return object of class tableone. That is a list of data specified summaries
#'   for all input variables.
#' @export
get_tableone.default <- function(data, strata = NULL, overall = TRUE, summary_function = summarize_short) {
  summary_FUN <- match.fun(summary_function)

  if (overall & !is.null(strata)) {
    overall_table1 <- get_tableone(data, strata = NULL, overall = FALSE, summary_function = summary_function) %>%
      dplyr::filter(!(variable %in% strata))

    combine_dfs <- TRUE
  } else {
    combine_dfs <- FALSE
  }

  if (is.null(strata)) {
    data <- data %>%
      dplyr::mutate(all = "Total")
    strata <- c("all")
  }

  data <- data %>%
    dplyr::group_by(!!!dplyr::syms(strata))

  data_ns <- data %>%
    dplyr::summarise(summary = dplyr::n()) %>%
    tidyr::pivot_wider(names_from = dplyr::any_of(strata), values_from = "summary") %>%
    dplyr::mutate(variable = "Sample", summary_id = "N")

  data_summary <- data %>%
    dplyr::summarise_all(summary_FUN) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_longer(cols = setdiff(names(.), strata), names_to = "variable", values_to = "summary") %>%
    tidyr::unnest_longer(summary) %>%
    tidyr::pivot_wider(names_from = dplyr::any_of(strata), values_from = "summary")

  data_table1 <- rbind(data_ns, data_summary) %>%
    dplyr::rename(statistic = summary_id) %>%
    dplyr::select(variable, statistic, everything())

  if (overall & combine_dfs) {
    data_table1 <- overall_table1 %>% dplyr::left_join(data_table1, by = c("variable", "statistic"))
  }

  class(data_table1) <- c("tableone", class(data_table1))

  return(data_table1)
}
