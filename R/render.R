#' Render a dataframe or tibble
#'
#' @description Render a previously created dataframe to html,
#' rtf or latex
#'
#'
#' @param data The dataframe or tibble to visualise
#' @param title Table title to include in the rendered table
#' @param datasource String specifying the datasource underlying the data set
#' @param footnote String specifying additional information to be displayed in the table note alongside the data source and specifications of statistical tests.
#' @param output_format If TRUE, the summary statistics for the overall dataset
#' are also calculated
#' @param engine If html is selected as output format, one can chose between
#' using kable, gt and DT as engine to create the output table
#' @param download_format How can users download it
#'
#' @export
#' 
render <- function(data,
                            title,
                            datasource,
                            footnote = "",
                            output_format="html",
                            engine="gt",
                            download_format = c('copy', 'csv', 'excel')){
  UseMethod("render")
}

#' @rdname render
#' @method render tableone
#' @export
render.tableone <- function(
  data,
  title,
  datasource,
  footnote = "",
  output_format="html",
  engine="gt",
  download_format = c('copy', 'csv', 'excel')){
  
  if(!("risktable" %in% class(data))){
    sample <- data[data$variable == "Sample", ]
    sample <- sample[3:length(sample)]
    sample_names = colnames(sample)
    new_sample_names <- sapply(1:length(sample), function(i){
      vec = c(sample_names[i], " (N=", sample[i], ")")
      paste(vec, collapse = "") 
    })
    colnames(data) <- c(colnames(data)[1:2], new_sample_names)
    data <- data[data$variable != "Sample", ]
  }
  
  render.data.frame(data=data, title=title, datasource=datasource, footnote=footnote, output_format=output_format, engine=engine, download_format=download_format)
}

#' @rdname render
#' @method render risktable
#' @export
render.risktable <- function(
  data,
  title,
  datasource,
  footnote = "",
  output_format="html",
  engine="gt",
  download_format = c('copy', 'csv', 'excel')){
  
  strata <- colnames(data)[3:ncol(data)]
  if (!is.null(attributes(data)$title) & length(attributes(data)$title) == length(strata)){
    data <- data %>% rename_at(vars(strata), ~ attributes(data)$title)
    strata <- colnames(data)[3:ncol(data)]  
  }
  y_lables <- unique(data$y_values)
  coln <- colnames(data)[1:2]
  complete_tab <- c()
  for (s in strata){
    tab <- 
      data[c(coln, s)] %>%
      tidyr::pivot_wider(names_from = "time", values_from=s)
    tab$variable <- s
    complete_tab <- rbind(complete_tab, tab)
  }
  colnames(complete_tab) <- c("statistic",colnames(tab)[2:ncol(tab)])
  class(complete_tab) <- c("tableone", class(complete_tab))
  class(complete_tab) <- c("risktable", class(complete_tab))
  complete_tab <- complete_tab %>% select(variable, statistic, everything())
  render.tableone(complete_tab,
                  title,
                  datasource,
                  footnote,
                  output_format,
                  engine,
                  download_format)
  
}



#' @rdname render
#' @method render data.frame
#' @export
render.data.frame <- function(
  data,
  title,
  datasource,
  footnote = "",
  output_format="html",
  engine="gt",
  download_format = c('copy', 'csv', 'excel')){
  # TODO: add code for rtf output
  # TODO: do we need a routine for falling back on minimal?
  # TODO: do we need features to further specify styling of the table?
  
  check_rendering_input(output_format, engine)


  #--------------------
  # Kable output
  if(tolower(engine) == "kable"){
    if(tolower(output_format) %in% c("html", "latex")){
      table_out <- data %>%
        knitr::kable(format = output_format,
                     caption = title,
                     digits = 2,
                     booktabs = T) %>%
        kableExtra::collapse_rows(valign="top") %>%
        kableExtra::footnote(general = footnote, 
                             general_title = "Additional Note:") %>%
        kableExtra::footnote(general = datasource,
                             general_title = "Data Source:") 
        
    }
    else{
      warning(paste("Supported output format of the kable engine are html and latex and not", output_format, " - falling back to html"))
      render(data=data, title=title, datasource=datasource,
                         output_format="html", engine=engine, download_format=download_format)
    }
  }

  #--------------------
  # GT output
  else if(tolower(engine) == "gt"){
    if(!tolower(output_format) %in% c("html", "latex")){
      warning(paste("Supported output format of the gt engine are html and latex and not", output_format, " - falling back to html"))
    }

    table_out <- render_gt(data=data, title=title, datasource=datasource, footnote=footnote)

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
    
    # WIP: we may want to create a custom container to allow for stratification of more than one value and merge cells in the description
    # sketch = htmltools::withTags(table(
    #   DT::tableHeader(colnames(data)),
    #   DT::tableFooter(paste("Source:", datasource))
    # ))
    footnote_1 <- paste0("Data Source: ", datasource)
    caption_datasource <- paste("  var caption = 'Data Source:", datasource)
    caption_compl <- paste(caption_datasource, footnote, "'", sep="; ")
    source_cap <- c(
      "function(settings){",
      "  var datatable = settings.oInstance.api();",
      "  var table = datatable.table().node();",
      caption_compl,
      #paste("  var caption = 'Data Source:", datasource, "'"),
      "  $(table).append('<caption style=\"caption-side: bottom\">' + caption + '</caption>');",
      #"  $(table).lastElementChild.replaceWith('<caption style=\"caption-side: bottom\">' + caption + '</caption>');",
      "}"
    )

    # may need some adjustment to also allow creation of DT in loops
    table_out <- render_datatable(data, title, download_format, source_cap)
  }

  #--------------------
  return(table_out)
}


### Functions for datatable
render_datatable <- function(data, title, download_format, source_cap){
  UseMethod("render_datatable")
}

render_datatable.tableone <- function(data, title, download_format, source_cap){
  if(is.null(download_format)){
    table_out <- data %>% 
      DT::datatable(caption = title,
                    filter = "none",
                    # container = sketch,
                    options = list(paging=FALSE, 
                                   ordering=FALSE,
                                   info=FALSE,
                                   drawCallback = DT::JS(source_cap)))
  } else {
    table_out <- data %>% 
      DT::datatable(caption = title,
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
  return(table_out)
}

render_datatable.data.frame <- function(data, title, download_format, source_cap){
  if(is.null(download_format)){
    table_out <- data %>%
      DT::datatable(caption = title,
                    options = list(
                    drawCallback = DT::JS(source_cap)))
  } else {
    table_out <- data %>%
      DT::datatable(caption = title,
                    extensions = 'Buttons',
                    options = list(drawCallback = DT::JS(source_cap),
                                   dom = 'Bfrtip',
                                   buttons = download_format))
  }
  return(table_out)
}


### Functions for gt rendering
render_gt <- function(data, title, datasource, footnote){
  # identify numeric columns for special formatting later
  numcols <- data %>% dplyr::select_if(is.numeric) %>% names()
  # create gt table
  table_out <- data %>%
    get_gt(numcols) %>%
    gt::fmt_number(
      columns = numcols,
      decimals = 2
    )%>% add_metadata_gt(title=title, datasource=datasource, footnote=footnote)
  return(table_out)
}

# Create initial gt object
get_gt <- function(data, numcols){
  UseMethod("get_gt")
}
get_gt.tableone <- function(data, numcols){
  gt <- gt::gt(data, groupname_col = "variable",
         rowname_col = "statistic")%>%
         # no decimal points for sample count
         gt::fmt_number(
           columns = numcols,
           rows = grepl("^N$", statistic),
           decimals = 0)
  return(gt)
}
get_gt.data.frame <- function(data, numcols){
  gt <- gt::gt(data)
  return(gt)
}

# add metadata to gt
add_metadata_gt <- function(gt, title, datasource, footnote){
  table_out <- gt %>% gt::tab_header(title = title)
  
  table_out <- table_out %>%
    # add metadata
    gt::tab_source_note(source_note = paste("Data Source:", datasource)) %>%
    gt::tab_source_note(source_note = footnote) %>%
    # add formatting
    gt::tab_options(data_row.padding = gt::px(4))
  
  return(table_out)
}

### Check if the input works
check_rendering_input <- function(output_format, engine){
  
  # stop if output format is not supported
  if(!tolower(output_format) %in% c("html", "latex")){ #"rtf",
    stop(paste("Currently supported output formats are html and latex.", output_format, "is not yet supported."))
  }
  
  # stop if engine format is not supported
  if(!tolower(engine) %in% c("kable", "gt", "dt", "datatables", "datatable")){
    stop(paste("Currently implemented output engines are kable, gt and jquery datatables (DT).", engine, "is not yet supported."))
  }
}