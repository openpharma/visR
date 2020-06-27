#' Summary Table (Table One)
#'
#' @description Create and render a summary table for a data set
#'
#' Calculate summary statistics and present them in a formatted table
#'
#' @param dat The data set to summarize as data frame or tibble
#' @param title Table title to include in the rendered table
#' @param caption Table caption to include in the rendered table
#' @param data_source String specifying the datas ource underlying the data set
#' @param group_columns Stratifying/Grouping variable name(s) as character vector.
#' If NULL, only overall results are returned
#' @param summary_function A function to summarize variables of different types.
#' Pre-implemented functions are vr_summarize and vr_summarize_tab1
#' @param ... Pass options to vr_render_tableone
#'
#' @export

vr_table_one <- function(
    dat,
    title,
    data_source,
    caption = "",
    # abbreviations = "",
    # variable_definitions = "",
    group_columns = NULL,
    summary_function = vr_summarize_tab1,
    ...
    # engine = "gt"
) {
    tab1_rendered <- vr_create_tableone(
        dat,
        group_columns = group_columns,
        summary_function = summary_function
    ) %>%
        vr_render_tableone(
            title = title,
            caption = caption,
            data_source = data_source,
            ...
        )
    
    return(tab1_rendered)
}
