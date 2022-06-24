#' @title Apply list of inclusion/exclusion criteria to a patient-level dataframe
#'
#' @description
#' `r lifecycle::badge("questioning")`
#' Apply list of inclusion/exclusion criteria to a patient-level dataframe
#'
#' @param data \code{data.frame}. Data set to be filtered
#' @param criteria_conditions \code{character} dplyr-filter compatible conditions
#'  of the filtering criteria. These conditions will be applied to filter the input
#'  data set and obtain final analysis data set
#'
#' @return Filtered data frame
#' @export
#'
#' @examples
#' adtte_filtered <- visR::apply_attrition(adtte,
#'   criteria_conditions = c(
#'     "TRTP=='Placebo'", "AGE>=75",
#'     "RACE=='WHITE'", "SITEID==709"
#'   )
#' )
#'
apply_attrition <- function(data, criteria_conditions) {
  if (missing(data)) {
    stop("Please provide input dataframe.")
  }
  if (missing(criteria_conditions)) {
    stop("Please provide a character vector with conditions to filter input data.")
  }

  if (rlang::is_empty(criteria_conditions)) {
    stop("Please provide a non-empty list of filtering critera as strings for the data.")
  }

  filters <- paste(criteria_conditions, collapse = ") & (") %>% paste0("(", ., ")")
  adf <- data %>%
    dplyr::filter(eval(parse(text = filters)))
  return(adf)
}
