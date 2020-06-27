#' Create Summary Table (also known as Table 1)
#' 
#' @description Create a summary table of descriptive statistics from a data frame or tibble. 
#' 
#' By default the following summary stats are calculated:
#' * Numeric variables: mean, min, 25th-percentile, median, 75th-percentile, maximum, standard deviation
#' * Factor variables: proportion of each factor level in the overall data set
#' * Default: number of unique values and number of missing values
#'
#' @param dat The data set to summarize as data frame or tibble
#' @param group_columns Stratifying/Grouping variable name(s) as character vector.
#' If NULL, only overall results are returned
#' @param overall If TRUE, the summary statistics for the overall data set are also calculated 
#' @param summary_function A function defining summary statistics for numeric and categorical values
#' 
#' @details It is possible to provide your own summary function. Please have a loot at vr_summary for inspiration.
#' 
#' @note All columns in the table will be summarized. 
#' If only some columns shall be used, please select only those
#' variables prior to creating the summary table by using dplyr::select()
#' 
#' @export
#' 
#' @examples
#' library(survival)
#' library(dplyr)
#' ovarian %>% 
#' select(-fustat) %>% 
#'   mutate(age_group = factor(case_when(age <= 50 ~ "<= 50 years",
#'                                       age <= 60 ~ "<= 60 years",
#'                                       age <= 70 ~ "<= 70 years",
#'                                       TRUE ~ "> 70 years")),
#'          rx = factor(rx),
#'          ecog.ps = factor(ecog.ps)) %>% 
#'   select(age, age_group, everything()) %>% 
#'   vr_create_tableone()

vr_create_tableone <- function(dat,
                               group_columns = NULL, overall = TRUE,
                               summary_function = vr_summarize_tab1) {
  
  if (is.null(group_columns) & !overall) {
    stop("Overall and group columns can not both be missing/False")
  }
  
  summary_FUN <- match.fun(summary_function)
  
  if(overall) {
    table1_overall <- create_tableone_data(dat, summary_function = summary_FUN)
  }
  
  if(!is.null(group_columns)){
    table1_grouped <- create_tableone_data(dat,
                                           group_columns = group_columns,
                                           summary_function = summary_FUN)
  }
  
  # create final table based on user input
  if(overall & is.null(group_columns)) {
    table1 <- table1_overall
    
  } else if (!overall & !is.null(group_columns)) {
    table1 <- table1_grouped
    
  } else {
    table1 <- table1_grouped %>%
      left_join(table1_overall, by = c("variable", "statistic"))
    
  }
  
  # add labels if they exist
  
  return(table1)
  
}

#' Create summary table one data
#'
#' @param dat The data set to summarize as data frame or tibble
#' @param group_columns Stratifying/Grouping variable name(s) as character vector.
#' If NULL, only overall results are returned
#' @param summary_function A function defining summary statistics for
#' numeric and categorical values
#' @noRd

create_tableone_data <- function(dat, group_columns = NULL,
                                 summary_function = vr_summarize_tab1) {
  
  if(is.null(group_columns)){
    dat <- dat %>% 
      dplyr::mutate(all = "Overall")
    group_columns <- c("all")
  }
  
  dat <- dat %>%
    dplyr::group_by(.dots = group_columns)
  
  dat_n <- dat %>%
    dplyr::summarise(summary = dplyr::n()) %>%
    tidyr::pivot_wider(
      names_from = tidyselect::any_of(group_columns),
      values_from = "summary"
    ) %>%
    dplyr::mutate(variable = "Sample", summary_id = "N")
  
  dat_summary <- dat %>%
    dplyr::summarise_all(summary_function) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_longer(
      cols = setdiff(names(.), group_columns),
      names_to = "variable",
      values_to = "summary"
    ) %>%
    tidyr::unnest_longer(summary) %>%
    tidyr::pivot_wider(
      names_from = tidyselect::any_of(group_columns),
      values_from = "summary"
    )
  
  dat_table1 <- rbind(dat_n, dat_summary) %>%
    dplyr::rename(statistic = summary_id) %>%
    dplyr::select(variable, statistic, everything())
  
  return(dat_table1)
}