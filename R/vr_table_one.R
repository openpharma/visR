#' Summary Table (Table One) 
#' 
#' @description Create and render a summary table for a dataset
#' 
#' Calculate summary statistics and present them in a formatted table
#'
#' @param data The dataframe or tibble to visualise
#' @param title Table title to include in the rendered table
#' @param caption Table caption to include in the rendered table
#' @param datasource String specifying the datasource underlying the data set
#' @param groupCols Character vector with column names to use for stratification in the summary table. Default: NULL
#' @param summary_function A function to summarize variables of different types. Pre-implemented functions are vr_summarize and vr_summarize_tab1
#' @param engine Rendering engine to use for creating output. Currently implemented are gt, kable and jquery datatable (DT)
#' 
#' @export

vr_table_one <- function(
    data, 
    title, 
    datasource,
    caption = "",
    # abbreviations = "", 
    # variable_definitions = "", 
    groupCols = NULL,
    summary_function = vr_summarize_tab1,
    ...
    # engine = "gt"
) {
    tab1_rendered <- vr_create_tableone(data, groupCols = groupCols, summary_function = summary_function) %>% 
        vr_render_tableone(title = title, caption = caption, datasource = datasource, ...)
    return(tab1_rendered)
}
