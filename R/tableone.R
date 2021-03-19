#' @title Display a summary Table (i.e. table one)
#'
#' @description Wrapper function to produce a summary table (i.e. Table One). 
#' Create and render a summary table for a dataset. 
#' A typical example of a summary table are "table one", the first table in an applied medical research manuscript.
#'
#' Calculate summary statistics and present them in a formatted table
#'
#' @param data The dataframe or tibble to visualize
#' @param title Table title to include in the rendered table. Input is a text string. 
#' @param footnote Table footnote to include in the rendered table. Input is a text string. 
#' @param datasource String specifying the datasource underlying the data set
#' @param strata Character vector with column names to use for
#' stratification in the summary table. Default: NULL , which indicates no stratification. 
#' @param overall If TRUE, the summary statistics for the overall dataset are also calculated 
#' @param summary_function A function defining summary statistics for numeric and categorical values
#' Pre-implemented functions are summarize and summarize_tab1
#' @param ... Pass options to render_table
#'
#' @examples 
#' 
#' library(dplyr)
#' library(visR)
#' 
#' 
#' # metadata for table
#' t1_title <- "My table one title"
#' t1_ds <- "My table one data source"
#' t1_fn <- "My table one footnote"
#' 
#' 
#' ## table by treatment - without overall and render with DT 
#' adtte %>%
#'    dplyr::filter(SAFFL == "Y") %>%
#'    dplyr::select(AGE, AGEGR1, SEX, EVNTDESC, TRTA) %>%
#'    visR::tableone(
#'       strata = "TRTA",
#'       overall = F,
#'       title = t1_title,
#'       datasource = t1_ds,
#'       footnote = t1_fn,
#'       engine = "DT"
#'    )
#' 
#' ## table by treatment - without overall and render with GT 
#' adtte %>%
#'    dplyr::filter(SAFFL == "Y") %>%
#'    dplyr::select(AGE, AGEGR1, SEX, EVNTDESC, TRTA) %>%
#'    visR::tableone(
#'       strata = "TRTA",
#'       overall = F,
#'       title = t1_title,
#'       datasource = t1_ds,
#'       footnote = t1_fn,
#'       engine = "gt"
#'    )
#' 
#' ## table by treatment - without overall and render with kable
#' adtte %>%
#'    dplyr::filter(SAFFL == "Y") %>%
#'    dplyr::select(AGE, AGEGR1, SEX, EVNTDESC, TRTA) %>%
#'    visR::tableone(
#'       strata = "TRTA",
#'       overall = F,
#'       title = t1_title,
#'       datasource = t1_ds,
#'       footnote = t1_fn,
#'       engine = "kable"
#'    )
#' 
#' ## table by treatment - without overall and render with kable as 
#' ## a latex table format rather than html 
#' adtte %>%
#'    dplyr::filter(SAFFL == "Y") %>%
#'    dplyr::select(AGE, AGEGR1, SEX, EVNTDESC, TRTA) %>%
#'    visR::tableone(
#'       strata = "TRTA",
#'       overall = F,
#'       title = t1_title,
#'       datasource = t1_ds,
#'       footnote = t1_fn,
#'       output_format = "latex",
#'       engine = "kable"
#'    )
#' 
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
