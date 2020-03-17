#' Render a dataframe or tibble
#' 
#' @description Render a previously created dataframe or tibble to html, rtf or latex
#' 
#' Create formatted table
#'
#' @param data The dataframe or tibble to visualise
#' @param title Table title to include in the rendered table
#' @param caption Table caption to include in the rendered table
#' @param datasource String specifying the datasource underlying the data set
#' @param output_format If TRUE, the summary statistics for the overall dataset are also calculated 
#' @param engine If html is selected as output format, one can chose between using kable, gt and DT as engine to create the output table
#' 
#' @export
vr_render_table <- function(data, title, caption, datasource, output_format="html", engine="gt"){
  # TODO: add code for rtf output
  # TODO: do we need a routine for falling back on minimal?
  # TODO: do we need features to further specify styling of the table?
  
  # stop if output format is not supported
  if(!tolower(output_format) %in% c("html", "latex")){ #"rtf", 
    stop(paste("Currently supported output formats are html and latex.", output_format, "is not yet supported."))
  }
  
  # stop if engine format is not supported
  if(!tolower(engine) %in% c("kable", "gt", "dt", "datatables")){
    stop(paste("Currently implemented output engines are kable, gt and jquery datatables (DT).", engine, "is not yet supported."))
  }
  
  # create composite title/caption for those engines that do not support both
  joined_caption <- title
  if(caption != ""){
    joined_caption <- paste(title, "-", caption)
  }
  
  
  #--------------------
  # Kable output 
  if(tolower(engine) == "kable"){
    if(tolower(output_format) %in% c("html", "latex")){
      table_out <- data %>% 
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
      vr_render_tableone(data=data, title=title, caption=caption, datasource=datasource,
                         output_format="html", engine=engine, download_format=download_format)
    }
  }
  
  #--------------------
  # GT output
  else if(tolower(engine) == "gt"){
    if(!tolower(output_format) %in% c("html", "latex")){
      warning(paste("Supported output format of the gt engine are html and latex and not", output_format, " - falling back to html"))
    }
    
    table_out <- vr_render_gt(data, title, caption, datasource)
    
    if(output_format == "latex"){
      # note: after this step, the table is not a gt object anymore and thus cannot be further styled
      table_out <- table_out %>% gt::as_latex()
    }
  }
  
  #--------------------
  # jQuery DT output
  else if(tolower(engine) %in% c("dt", "datatables", "datatable")){
    if(!tolower(output_format) %in% c("html")){
      warning(paste("DT engine only supports html output and not", output_format,
                    "- falling back to html. Please pick a different engine to create other outputs"))
    }

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
      table_out <- data %>% 
        DT::datatable(caption = joined_caption,
                      options = list(drawCallback = DT::JS(source_cap)))
    }
    else{
      table_out <- data %>% 
        DT::datatable(caption = joined_caption,
                      extensions = 'Buttons',
                      options = list(drawCallback = DT::JS(source_cap),
                                     dom = 'Bfrtip', 
                                     buttons = download_format))
    }
  }
  
  #--------------------
  return(table_out)
}


vr_render_gt <- function(data, title, caption, datasource){
  # identify numeric columns for special formatting later
  numcols <- data %>% dplyr::select_if(is.numeric) %>% names()
  # create gt table 
  table1_out <- data %>% 
    gt::gt() %>% 
    gt::fmt_number(
      columns = numcols,
      decimals = 2
    )%>% 
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
