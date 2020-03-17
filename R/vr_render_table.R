#' Render a dataframe or tibble
#' 
#' @description Render a previously created dataframe or tibble to html, rtf or latex
#' 
#' Create formatted table
#'
#' @param df The dataframe or tibble to visualise
#' @param title Table title to include in the rendered table
#' @param subtitle Table title to include in the rendered table
#' @param caption Table caption to include in the rendered table
#' @param output_format If TRUE, the summary statistics for the overall dataset are also calculated 
#' @param engine If html is selected as output format, one can chose between using kable, gt and DT as engine to create the output table
#' 
#' @export
vr_render_table <- function(df, title, caption, datasource, subtitle="", output_format="html", engine="gt"){
  # TODO: add code to support jquery datatable output for html, as well as all functionality for rtf and latex output
  # TODO: do we need a routine for falling back on minimal?
  # TODO: do we need features to further specify styling of the table?
  
  # stop if output format is not supported
  if(!tolower(output_format) %in% c("html", "rtf", "latex")){
    stop(paste("Output format needs to be html, rtf or latex and not", output_format))
  }
  
  # stop if engine format is not supported
  if(!tolower(engine) %in% c("kable", "gt", "dt", "datatables")){
    stop(paste("Output engine needs to be kable, gt or datatables (DT) and not", engine))
  }
  
  # HTML output 
  if(output_format == "html"){
    if(tolower(engine) == "kable"){
      table_out <- df %>% knitr::kable(format = "html", 
                                       caption = caption,
                                       digits = 2) %>% 
        kableExtra::collapse_rows(valign="top")
    }
    
    if(tolower(engine) == "gt"){
      numcols <- df %>% dplyr::select_if(is.numeric) %>% names()
      table_out <- df %>% 
        gt::gt() %>% 
        gt::fmt_number(
          columns = numcols,
          decimals = 2
        )%>% 
        gt::tab_header(title = title) %>% 
        # add metadata
        gt::tab_source_note(source_note = paste("Source:", datasource)) %>% 
        # add formatting
        gt::tab_options(data_row.padding = gt::px(4))
    }
  }
  
  # Latex output 
  if(output_format == "latex"){
    table_out <- df %>% knitr::kable(format = "latex", 
                                     caption = caption,
                                     digits = 2) %>% 
      kableExtra::collapse_rows(valign="top")
  }
  
  
  return(table_out)
}
