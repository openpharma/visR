vr_table_one <- function(
    data, 
    title, 
    dataset,
    caption = "",
    subtitle = "",
    abbreviations = "", 
    variable_definitions = "", 
    groupCols = NULL,
    summary_function = vr_summarize,
    ...
) {
    # create table
    tab1_df <- vr_create_tableone(data, groupCols = groupCols, summary_function = summary_function)
    
    # render table
    tab1_rendered <- tab1_df %>% 
        vr_render_tableone(title = title, subtitle = subtitle, caption = caption, datasource = dataset, ...) %>% 
    
    return(tab1_rendered)
}
