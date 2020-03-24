#' Render Summary Table
#' 
#' @description Render a previously created summary table  
#' 
#' Create formatted table
#'
#' @param table1_df The summary table of a dataset created by `vr_create_tableone`
#' @param title Table title to include in the rendered table
#' @param caption Table caption to include in the rendered table
#' @param datasource String specifying the datasource underlying the data set
#' @param output_format String specifying the format to render the table. Supported formats are: html and latex 
#' @param engine If html is selected as output format, one can chose between using kable, gt and DT as engine to create the output table
#' @param download_format Vector of file formats to use for download buttons - passed on to DT buttons. NULL if no download button should be created. Default: c('copy', 'csv', 'excel') 
#' 
#' @export
vr_render_tableone <- function(table1_df, title, caption, datasource, 
                               output_format="html", engine="gt", 
                               download_format = c('copy', 'csv', 'excel')){
  # TODO: add code to support rtf
  # TODO: do we need features to further specify styling of the table?

  # stop if output format is not supported
  if(!tolower(output_format) %in% c("html", "latex")){ #"rtf", 
    stop(paste("Currently supported output formats are html and latex.", output_format, "is not yet supported."))
  }
  
  # stop if engine format is not supported
  if(!tolower(engine) %in% c("kable", "gt", "dt", "datatables")){
    stop(paste("Currently implemented output engines are kable, gt and jquery datatables (DT).", engine, "is not yet supported."))
  }
  joined_caption <- title
  if(caption != ""){
    joined_caption <- paste(title, "-", caption)
  }
  
  #--------------------
  # Kable output 
  if(tolower(engine) == "kable"){
    if(tolower(output_format) %in% c("html", "latex")){
      table1_out <- table1_df %>% 
        knitr::kable(format = output_format,
                     caption = joined_caption,
                     digits = 2,
                     booktabs = T) %>% 
        kableExtra::collapse_rows(valign="top") %>% 
        kableExtra::footnote(general = datasource,
                             general_title = "Data Source:")
    }
    else{
      warning(paste("Supported output format of the kable engine are html and latex and not", output_format, " - falling back to html"))
      vr_render_tableone(table1_df=table1_df, title=title, caption=caption, datasource=datasource,
                         output_format="html", engine=engine, download_format=download_format)
    }
  }
  
  #--------------------
  # GT output
  else if(tolower(engine) == "gt"){
    if(!tolower(output_format) %in% c("html", "latex")){
      warning(paste("Supported output format of the gt engine are html and latex and not", output_format, " - falling back to html"))
    }
    
    table1_out <- vr_render_tableone_gt(table1_df, title, caption, datasource)
      
    if(output_format == "latex"){
    # note: after this step, the table is not a gt object anymore and thus cannot be further styled
      table1_out <- table1_out %>% gt::as_latex()
    }
  }
  
  #--------------------
  # jQuery DT output
  else if(tolower(engine) %in% c("dt", "datatables", "datatable")){
    if(!tolower(output_format) %in% c("html")){
      warning(paste("DT engine only supports html output and not", output_format,
                    "- falling back to html. Please pick a different engine to create other outputs"))
    }
    
    # WIP: we may want to create a custom container to allow for stratification of more than one value and merge cells in the description
    # sketch = htmltools::withTags(table(
    #   DT::tableHeader(colnames(table1_df)),
    #   DT::tableFooter(paste("Source:", datasource))
    # ))
    
    source_cap <- c(
      "function(settings){",
      "  var datatable = settings.oInstance.api();",
      "  var table = datatable.table().node();",
      paste("  var caption = 'Data Source:", datasource, "'"),
      "  $(table).append('<caption style=\"caption-side: bottom\">' + caption + '</caption>');",
      "}"
    )
    
    # may need some adjustment to also allow creation of DT in loops
    if(is.null(download_format)){
      table1_out <- table1_df %>% 
        DT::datatable(caption = joined_caption,
                      filter = "none",
                      # container = sketch,
                      options = list(paging=FALSE, 
                                     ordering=FALSE,
                                     info=FALSE,
                                     drawCallback = DT::JS(source_cap)))
    }
    else{
      table1_out <- table1_df %>% 
        DT::datatable(caption = joined_caption,
                      filter = "none",
                      # container = sketch,
                      extensions = 'Buttons',
                      options = list(paging=FALSE,
                                     info=FALSE,
                                     ordering=FALSE,
                                     drawCallback = DT::JS(source_cap),
                                     dom = 'Bfrtip', 
                                     buttons = download_format))
    }
  }
  
  #--------------------
  return(table1_out)
}


vr_render_tableone_gt <- function(table1_df, title, caption, datasource){
  # identify numeric columns for special formatting later
  numcols <- table1_df %>% dplyr::select_if(is.numeric) %>% names()
  # create gt table 
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
      title = title
    ) %>% 
    # add metadata
    gt::tab_source_note(source_note = paste("Data Source:", datasource)) %>% 
    # add formatting
    gt::tab_options(data_row.padding = gt::px(4))
  
  # add caption as subtitle if one is provided
  if(caption != ""){
    table1_out <- table1_out %>% 
      gt::tab_header(
        title = title,
        subtitle = caption
      )
  }
  
  return(table1_out)
}
