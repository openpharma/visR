#' Summary Table (Table One)
#'
#' @description Create and render a summary table for a dataset
#'
#' Calculate summary statistics and present them in a formatted table
#'
#' @param data The dataframe or tibble to visualise
#' @param title Table title to include in the rendered table
#' @param datasource String specifying the datasource underlying the data set
#' @param strata Character vector with column names to use for
#' stratification in the summary table. Default: NULL
#' @param overall If TRUE, the summary statistics for the overall dataset are also calculated 
#' @param summary_function A function defining summary statistics for numeric and categorical values
#' Pre-implemented functions are summarize and summarize_tab1
#' @param ... Pass options to render_table
#'
#' @examples 
#' 
#'  ## Set meta-data for table including title and data source
#'  t1_title <- "My table one title"
#'  t1_ds <- "My table one data source"
#'  
#'  adtte %>% 
#'    filter(SAFFL == "Y") %>%
#'    select(AGE, AGEGR1, SEX, EVNTDESC, TRTA ) %>%   
#'    tableone(strata = "TRTA", overall = F, title = t1_title, datasource = t1_ds)
#'
#' @rdname tableone
#'
#' @export

tableone <- function(
    data,
    title,
    datasource,
    # abbreviations = "",
    # variable_definitions = "",
    strata = NULL,
    overall = T,
    summary_function = summarize_tab1,
    ...
    # engine = "gt"
) {
    tab1_rendered <- get_tableone(data, strata = strata, summary_function = summary_function, overall = overall) %>%
        render(title = title, datasource = datasource, ...)
    return(tab1_rendered)
}
