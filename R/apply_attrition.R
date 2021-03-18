#' Apply list of inclusion/exclusion criteria to a patient-level dataframe
#'
#' @param data dataframe. Data set to be filtered
#' @param criteria_conditions character vector. Dplyr-filter compatible 
#' conditions of the filtering criteria.
#' These conditions will be applied to filter the input data set and obtain final
#' analysis data set
#'
#' @return Filtered data frame
#' @export
#'
#' @examples
apply_attrition <- function(data, criteria_conditions){
    filters <- paste(criteria_conditions, collapse = ") & (") %>% paste0("(", .,")")
    adf <- data %>% 
        dplyr::filter(eval(parse(text=filters)))
    return(adf)
}