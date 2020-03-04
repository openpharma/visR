#' Render Summary Table
#' 
#' @description Render a previously created summary table  
#' 
#' Create formatted table
#'
#' @param table1_df The summary table of a dataset created by `vr_create_tableone`
#' @param caption Table caption to include in the rendered table
#' @param output_format If TRUE, the summary statistics for the overall dataset are also calculated 
#' @param engine If html is selected as output format, one can chose between using kable, gt and DT as engine to create the output table
#' 
#' @export
vr_render_tableone <- function(table1_df, caption, output_format="html", engine="gt"){
  # TODO: add code to support datatable output for html, as well as all functionality for rtf and latex output
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
      table1_out <- table1_df %>% knitr::kable(format = "html", 
                                               caption = caption,
                                               digits = 2) %>% 
        kableExtra::collapse_rows(valign="top")
    }
    
    if(tolower(engine) == "gt"){
      numcols <- table1_df %>% dplyr::select_if(is.numeric) %>% names()
      table1_out <- table1_df %>% 
        gt::gt(groupname_col = "variable",
               rowname_col = "statistic") %>% 
        gt::fmt_number(
          columns = numcols,
          decimals = 2
        )%>% 
        # no decimal points for sample count
        gt::fmt_number(
          columns = numcols,
          rows = grepl("^N$", statistic),
          decimals = 0
        ) %>% 
        gt::tab_header(
          title = caption
        )
    }
  }
  
  # Latex output 
  if(output_format == "latex"){
    table1_out <- table1_df %>% knitr::kable(format = "latex", 
                                             caption = caption,
                                             digits = 2) %>% 
      kableExtra::collapse_rows(valign="top")
  }
    
  
  return(table1_out)
}
