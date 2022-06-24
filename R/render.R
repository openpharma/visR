#' @title Render a data.frame, risktable, or tableone object as a table
#'
#' @description
#' `r lifecycle::badge("questioning")`
#' Render a previously created data.frame, tibble or tableone object to html, rtf or latex
#'
#' @param data Input data.frame or tibble to visualize
#' @param title Specify the title as a text string to be displayed in the rendered table.
#' Default is no title.
#' @param datasource String specifying the data source underlying the data set.
#' Default is no title.
#' @param footnote String specifying additional information to be displayed as a footnote
#' alongside the data source and specifications of statistical tests.
#' @param output_format Type of output that is returned, can be "html" or "latex".
#' Default is "html".
#' @param engine If "html" is selected as `output_format`, one can chose between
#' using `kable`, `gt` and `DT` as engine to render the output table. Default is "gt".
#' @param download_format Options formats generated for downloading the data.
#' Default is a list "c('copy', 'csv', 'excel')".
#'
#' @return A table data structure with possible interactive functionality depending on the choice of the engine.
#' @rdname render
#' @export
render <- function(data,
                   title = "",
                   datasource,
                   footnote = "",
                   output_format = "html",
                   engine = "gt",
                   download_format = c("copy", "csv", "excel")) {
  UseMethod("render")
}


#' @inheritParams render
#' @export
#' @method render tableone
#'
render.tableone <- function(data,
                            title,
                            datasource,
                            footnote = "",
                            output_format = "html",
                            engine = "gt",
                            download_format = NULL) {
  if (!inherits(data, "tableone")) {
    stop("Please provide a valid `tableone` object.")
  }

  if (missing(title)) {
    stop("Please provide a valid `title`.")
  }

  if (missing(datasource)) {
    stop("Please provide a valid `datasource`.")
  }

  if (!(output_format %in% c("html", "latex"))) {
    stop("Invalid output_format. Currently, 'html' and 'latex' are supported.")
  } else if (output_format == "latex" & !(engine %in% c("gt", "kable"))) {
    stop("Currently, 'latex' output is only implemented with 'gt' or 'kable' as a table engine.")
  }

  download_formats <- c()

  if (engine %in% c("dt", "datatable", "datatables")) {
    if ("copy" %in% download_format) {
      download_formats <- c(download_formats, "copy")
    }
    if ("csv" %in% download_format) {
      download_formats <- c(download_formats, "csv")
    }
    if ("excel" %in% download_format) {
      download_formats <- c(download_formats, "excel")
    }

    for (f in download_format) {
      if (!(is.null(f)) & !(f %in% c("copy", "csv", "excel"))) {
        warning("Currently, only 'copy', 'csv' and 'excel' are supported as 'download_format'.")
      }
    }
  } else {
    if (!is.null(download_format)) {
      warning("Currently, 'download_format' is only supported for the following engines: dt, datatable, datatables.")
    }
  }

  if (!inherits(data, "risktable")) {
    sample <- data[data$variable == "Sample", ]
    sample <- sample[3:length(sample)]
    sample_names <- colnames(sample)
    new_sample_names <- sapply(1:length(sample), function(i) {
      vec <- c(sample_names[i], " (N=", sample[i], ")")
      paste(vec, collapse = "")
    })
    colnames(data) <- c(colnames(data)[1:2], new_sample_names)
    data <- data[data$variable != "Sample", ]
  }

  render.data.frame(
    data,
    title,
    datasource,
    footnote,
    output_format,
    engine,
    download_format
  )
}

#' @inheritParams render
#' @export
#'
#' @method render risktable
render.risktable <- function(data,
                             title,
                             datasource,
                             footnote = "",
                             output_format = "html",
                             engine = "gt",
                             download_format = NULL) {
  if (!inherits(data, "risktable")) {
    stop("Please provide a valid `risktable` object.")
  } else {

    # Many tidyr operations don't work on non-standard class objects. Therefore,
    # we remove the class and add it back later.
    class(data) <- class(data)[class(data) != "risktable"]
  }

  if (missing(title)) {
    stop("Please provide a valid `title`.")
  }

  if (missing(datasource)) {
    stop("Please provide a valid `datasource`.")
  }

  strata <- colnames(data)[3:ncol(data)]

  if (!is.null(attributes(data)$title) & length(attributes(data)$title) == length(strata)) {
    data <- data %>% dplyr::rename_at(dplyr::vars(strata), ~ attributes(data)$title)
    strata <- colnames(data)[3:ncol(data)]
  }

  y_lables <- unique(data$y_values)
  coln <- colnames(data)[1:2]
  complete_tab <- c()

  for (s in strata) {
    tab <- data[c(coln, s)] %>%
      tidyr::pivot_wider(names_from = "time", values_from = s)
    tab$variable <- s
    complete_tab <- rbind(complete_tab, tab)
  }

  colnames(complete_tab) <- c("statistic", colnames(tab)[2:ncol(tab)])
  class(complete_tab) <- c("tableone", class(complete_tab))
  class(complete_tab) <- c("risktable", class(complete_tab))
  complete_tab <- complete_tab %>%
    dplyr::select(variable, statistic, dplyr::everything())

  render.tableone(
    complete_tab,
    title,
    datasource,
    footnote,
    output_format,
    engine,
    download_format
  )
}


#' @inheritParams render
#'
#' @method render data.frame
#' @export
render.data.frame <- function(data,
                              title,
                              datasource,
                              footnote = "",
                              output_format = "html",
                              engine = "gt",
                              download_format = c("copy", "csv", "excel")) {
  # TODO: add code for rtf output
  # TODO: do we need a routine for falling back on minimal?
  # TODO: do we need features to further specify styling of the table?

  check_rendering_input(output_format, engine)

  # Kable output
  if (tolower(engine) == "kable") {
    if (tolower(output_format) %in% c("html", "latex")) {
      table_out <- data %>%
        knitr::kable(
          format = output_format,
          caption = title,
          digits = 2,
          booktabs = TRUE
        ) %>%
        kableExtra::collapse_rows(valign = "top") %>%
        kableExtra::footnote(
          general = footnote,
          general_title = "Additional Note:"
        ) %>%
        kableExtra::footnote(
          general = datasource,
          general_title = "Data Source:"
        )
    } else {

      # Currently can't be triggered due to check_rendering_input()
      # Uncommented for possible later use with rtf
      # warning(paste("Supported output format of the kable engine are html and latex and not", output_format, " - falling back to html"))
      # render(data = data,
      #        title = title,
      #        datasource = datasource,
      #        output_format = "html",
      #        engine = engine,
      #        download_format = download_format)
    }
  }

  #--------------------
  # GT output
  else if (tolower(engine) == "gt") {
    if (!tolower(output_format) %in% c("html", "latex")) {

      # Currently can't be triggered due to check_rendering_input()
      # Uncommented for possible later use with rtf
      # warning(paste("Supported output format of the gt engine are html and latex and not", output_format, " - falling back to html"))
    }

    table_out <- render_gt(data = data, title = title, datasource = datasource, footnote = footnote)

    if (output_format == "latex") {
      # note: after this step, the table is not a gt object anymore and thus cannot be further styled
      table_out <- table_out %>% gt::as_latex()
    }
  }

  #--------------------
  # jQuery DT output
  else if (tolower(engine) %in% c("dt", "datatables", "datatable")) {
    if (!tolower(output_format) %in% c("html")) {
      warning(paste(
        "DT engine only supports html output and not", output_format,
        "- falling back to html. Please pick a different engine to create other outputs"
      ))
    }

    # WIP: we may want to create a custom container to allow for stratification of more than one value and merge cells in the description
    # sketch = htmltools::withTags(table(
    #   DT::tableHeader(colnames(data)),
    #   DT::tableFooter(paste("Source:", datasource))
    # ))
    caption_datasource <- paste("  var caption = 'Data Source:", datasource, "';")
    source_cap <- c(
      "function(settings){",
      "  var datatable = settings.oInstance.api();",
      "  var table = datatable.table().node();",
      "  var n_captions = $(table).find('caption').length + 1;",
      caption_datasource,
      "  if (n_captions <= 1) { $(table).append('<caption style=\"caption-side: bottom\">' + caption + '</caption>')};",
      "}"
    )

    # may need some adjustment to also allow creation of DT in loops
    table_out <- render_datatable(data, title, download_format, source_cap)
  }

  #--------------------
  return(table_out)
}





#' @title Experimental internal function to help render a data.frame, risktable or tableone object as a datatable
#'
#' @description
#' `r lifecycle::badge("questioning")`
#' Render a previously created datatable to html, rtf or latex
#' @param data Input data.frame or tibble to visualize
#' @param title Specify the title as a text string to be displayed in the rendered table.
#' Default is no title.
#' @param download_format Options formats generated for downloading the data.
#' Default is a list "c('copy', 'csv', 'excel')".'
#' @param source_cap String automatically compiled of data source and captions.
#' @return A table data structure with possible interactive functionality depending on the choice of the engine.
#' @rdname render_datatable
#' @noRd
render_datatable <- function(data, title, download_format, source_cap) {
  UseMethod("render_datatable")
}


#' @inheritParams render_datatable
#'
#' @method render_datatable tableone
#' @noRd
render_datatable.tableone <- function(data, title, download_format, source_cap) {
  if (is.null(download_format)) {
    table_out <- data %>%
      DT::datatable(
        caption = title,
        filter = "none",
        # container = sketch,
        options = list(
          paging = FALSE,
          ordering = FALSE,
          info = FALSE,
          drawCallback = DT::JS(source_cap)
        )
      )
  } else {
    table_out <- data %>%
      DT::datatable(
        caption = title,
        filter = "none",
        # container = sketch,
        extensions = "Buttons",
        options = list(
          paging = FALSE,
          info = FALSE,
          ordering = FALSE,
          drawCallback = DT::JS(source_cap),
          dom = "Bfrtip",
          buttons = download_format
        )
      )
  }

  return(table_out)
}

#' @inheritParams render_datatable
#'
#' @method render_datatable data.frame
#' @noRd
render_datatable.data.frame <- function(data, title, download_format, source_cap) {
  if (is.null(download_format)) {
    table_out <- data %>%
      DT::datatable(
        caption = title,
        options = list(
          drawCallback = DT::JS(source_cap)
        )
      )
  } else {
    table_out <- data %>%
      DT::datatable(
        caption = title,
        extensions = "Buttons",
        options = list(
          drawCallback = DT::JS(source_cap),
          dom = "Bfrtip",
          buttons = download_format
        )
      )
  }

  return(table_out)
}



#' @title Experimental function to render to a gt table.
#'
#' @description Render a previously created datatable to gt
#' `r lifecycle::badge("questioning")`
#' @param data Input data.frame or tibble to visualize
#' @param title Specify the title as a text string to be displayed in the rendered table.
#' Default is no title.
#' @param datasource String specifying the data source underlying the data set.
#' Default is no title.
#' @param footnote String specifying additional information to be displayed as a footnote
#' alongside the data source and specifications of statistical tests.
#' @return A gt object.
#' @rdname render_gt
#' @noRd
render_gt <- function(data, title, datasource, footnote) {
  # identify numeric columns for special formatting later
  numcols <- data %>%
    dplyr::select_if(is.numeric) %>%
    names()
  # create gt table
  table_out <- data %>%
    get_gt(numcols) %>%
    gt::fmt_number(
      columns = numcols,
      decimals = 2
    ) %>%
    add_metadata_gt(
      title = title,
      datasource = datasource,
      footnote = footnote
    )

  return(table_out)
}


#' @title Internal function Get gt object
#' @description Internal function Get gt object for tableone
#' `r lifecycle::badge("questioning")`
#'
#' @param data input data set
#' @param numcols number of columns
#' @noRd
#' @return gt object
get_gt <- function(data, numcols) {
  UseMethod("get_gt")
}




#' @inheritParams get_gt
#'
#' @method get_gt tableone
#' @noRd
get_gt.tableone <- function(data, numcols) {
  gt <- gt::gt(data,
    groupname_col = "variable",
    rowname_col = "statistic"
  ) %>%
    # no decimal points for sample count
    gt::fmt_number(
      columns = numcols,
      rows = grepl("^N$", statistic),
      decimals = 0
    )

  return(gt)
}

#' @inheritParams get_gt
#'
#' @method get_gt data.frame
#'
get_gt.data.frame <- function(data, numcols) {
  gt <- gt::gt(data)

  return(gt)
}


#' @title Internal function to add metadata to a gt object
#' @description Internal function to add metadata to a gt object
#' `r lifecycle::badge("questioning")`
#'
#' @param gt input gt object
#' @param title Specify the title as a text string to be displayed in the rendered table.
#' Default is no title.
#' @param datasource String specifying the data source underlying the data set.
#' Default is no title.
#' @param footnote String specifying additional information to be displayed as a footnote
#' alongside the data source and specifications of statistical tests.
#' @noRd
#' @return gt object
add_metadata_gt <- function(gt, title, datasource, footnote) {
  table_out <- gt %>% gt::tab_header(title = title)

  table_out <- table_out %>%
    # add metadata
    gt::tab_source_note(source_note = paste("Data Source:", datasource)) %>%
    gt::tab_source_note(source_note = footnote) %>%
    # add formatting
    gt::tab_options(data_row.padding = gt::px(4))

  return(table_out)
}



#' @title Internal function to check if the input works
#' `r lifecycle::badge("questioning")`
#' @param output_format format for output i.e. html
#' @param engine engine to render output.
#' @noRd
#' @return Warning message
check_rendering_input <- function(output_format = NULL, engine = NULL) {
  if (missing(output_format) | is.null(output_format) | missing(engine) | is.null(engine)) {
    stop("Please provide an output_format and an engine.")
  }

  # stop if output format is not supported
  if (!tolower(output_format) %in% c("html", "latex")) { # "rtf",
    stop(paste("Currently supported output formats are html and latex.", output_format, "is not yet supported."))
  }

  # stop if engine format is not supported
  if (!tolower(engine) %in% c("kable", "gt", "dt", "datatables", "datatable")) {
    stop(paste("Currently implemented output engines are kable, gt and jquery datatables (DT).", engine, "is not yet supported."))
  }
}
