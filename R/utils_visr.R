#' @title Align multiple ggplot graphs, taking into account the legend
#'
#' @description This function aligns multiple \code{ggplot} graphs by making them the same width by taking into account the legend width.
#'
#' @param pltlist A list of plots (TODO: provide more detail)
#'
#' @return List of \code{ggplot} with equal width.
#'
#' @references \url{https://stackoverflow.com/questions/26159495/align-multiple-ggplot-graphs-with-and-without-legends}
#'
#' @examples
#' \donttest{
#' library(ggplot2)
#' library(gtable)
#' library(cowplot)
#'
#' ## create 2 graphs
#' p1 <- ggplot2::ggplot(adtte, ggplot2::aes(x = as.numeric(AGE), fill = "Age")) +
#'   ggplot2::geom_histogram(bins = 15)
#' p2 <- ggplot2::ggplot(adtte, ggplot2::aes(x = as.numeric(AGE))) +
#'  ggplot2::geom_histogram(bins = 15)
#'
#' ## default alignment does not take into account legend size
#' cowplot::plot_grid(plotlist = list(p1,p2), align = "none", nrow=2)
#'
#' ## align_plots() takes into account legend width
#' cowplot::plot_grid(plotlist = align_plots(pltlist = list(p1, p2)), align = "none", nrow=2)
#' }
#' @export


align_plots <- function(pltlist) {
  
  if (missing(pltlist) | is.null(pltlist)) {
    
    base::stop("Please provide a list of valid `ggplot` objects.")
    
  } 
  
  for (plt in pltlist) {
    
    if (!("ggplot" %in% class(plt))) {
      
      base::stop("Not all elements of the provided list are `ggplot` objects.")
      
    }
  }
  
  .LegendWidth <- function(x)
    x$grobs[[8]]$grobs[[1]]$widths[[4]]

  plots.grobs <- lapply(pltlist, ggplot2::ggplotGrob)
  max.widths <-
    do.call(grid::unit.pmax, lapply(plots.grobs, "[[", "widths"))
  legends.widths <- lapply(plots.grobs, .LegendWidth)

  max.legends.width <-
    base::suppressWarnings(do.call(max, legends.widths))

  plots.grobs.eq.widths <- lapply(plots.grobs, function(x) {
    x$widths <- max.widths
    x
  })

  plots.grobs.eq.widths.aligned <-
    lapply(plots.grobs.eq.widths, function(x) {
      if (gtable::is.gtable(x$grobs[[8]])) {
        x$grobs[[8]] <-
          gtable::gtable_add_cols(x$grobs[[8]], unit(abs(diff(
            c(LegendWidth(x), max.legends.width)
          )), "mm"))
      }
      x
    })

  plots.grobs.eq.widths.aligned
}

#' @title Translates options for legend into a list that can be passed to ggplot2
#'
#' @description This function takes the legend position and orientation, defined by the user and puts them into a list for ggplot2.
#'
#' @param legend_position  Default = "right".
#' @param legend_orientation Default = NULL.
#'
#' @return List of legend options for ggplot2.

legendopts <- function(legend_position = "right",
                       legend_orientation = NULL) {
  ## replace default eg "h" if user specified something else
  .ucoalesce <- function(x, default) {
    ifelse(is.null(x), default, x)
  }

  showlegend <- TRUE

  if (is.character(legend_position)) {
    if (legend_position == "bottom") {
      leg_opts <- list(
        xanchor = "center",
        x = 0.5,
        y = -0.2,
        orientation = .ucoalesce(legend_orientation, "h")
      )
    } else if (legend_position == "right") {
      leg_opts <- list(
        yanchor = "center",
        x = 1.2,
        y = 0.5,
        orientation = .ucoalesce(legend_orientation, "v")
      )
    } else if (legend_position == "top") {
      leg_opts <- list(
        xanchor = "center",
        x = 0.5,
        y = 1.2,
        orientation = .ucoalesce(legend_orientation, "h")
      )
    } else if (legend_position == "left") {
      leg_opts <- list(
        yanchor = "center",
        x = -1.0,
        y = 0.5,
        orientation = .ucoalesce(legend_orientation, "v")
      )
    } else if (legend_position == "none") {
      showlegend <-  FALSE
      leg_opts <- NULL
    }
  } else {
    leg_opts <- list(x = legend_position[1],
                     y = legend_position[2])
  }

  return(list(leg_opts = leg_opts, showlegend = showlegend))
}


#' @title Create labels for flowchart
#'
#' @description This function creates lables with a maximal character length per line by combining content of two dataframe columns
#'
#' @param data A dataframe
#' @param description_column_name \code{character} The column name containing description part of the new label
#' @param value_column_name \code{character} The column name containing the sample size part of the new label
#' @param complement_column_name \code{character} The column name containing a complement description part (will result in a second label)
#' @param wrap_width \code{integer} for the maximal character count per line
#'
#' @return The input dataframe extended by two columns containing the label and complement label
#'
#' @keywords internal
#' @noRd
.get_labels <- function(data, description_column_name, value_column_name, complement_column_name="", wrap_width=50){

  label <- complement_label <- NULL

  plotting_data <- data %>%
    dplyr::rowwise() %>%
    # below needs update to description_column_name instead of Criteria
    dplyr::mutate(label = paste(strwrap(get(description_column_name), width = wrap_width), collapse = "\n")) %>%
    # below needs update to value_column_name instead of `Remaining N`
    dplyr::mutate(label = sprintf("%s\nN = %d", label, get(value_column_name)))


  if(complement_column_name != ""){
    plotting_data <- plotting_data %>%
      # below needs update to complement_column_name instead of Complement
      dplyr::mutate(complement_label = paste(strwrap(get(complement_column_name), width = wrap_width), collapse = "\n")) %>%
      dplyr::ungroup() %>%
      # below needs update to value_column_name instead of `Remaining N`
      dplyr::mutate(complement_label = sprintf("%s\nN = %d",
                                               complement_label,
                                               dplyr::lag(get(value_column_name)) - get(value_column_name)))
  }else{
    plotting_data <- plotting_data %>%
      dplyr::ungroup() %>%
      dplyr::mutate(complement_label = sprintf("%s N = %d", "Excluded",
                                               dplyr::lag(get(value_column_name)) - get(value_column_name)))
  }

  return(plotting_data)
}

#' @title Calculate the size labels on the
#'
#' @description Calculate the text width and maximal text width of both the label and complement labels
#'
#' @param data Dataframe with label and complement label strings
#' @param label The column containing attrition labels
#' @param complement_label The column containing complement description labels
#'
#' @return The input dataframe extended by several columns containing the label and complement label height and width
#'
#' @keywords internal
#' @noRd
#'
.get_labelsizes <- function(data, label, complement_label){
  labelheight <- labelwidth <- complementheight <- complementwidth <- maxwidth <- maxheight <- NULL

  plotting_data <- data %>%
    dplyr::mutate(labelwidth = graphics::strwidth({{label}}, units = "inch"),
                  complementwidth = graphics::strwidth({{complement_label}}, units = "inch"),
                  maxwidth = max(labelwidth, complementwidth),
                  labelheight = graphics::strheight({{label}}, units = "inch"),
                  complementheight = graphics::strheight({{complement_label}}, units = "inch"),
                  maxheight = max(labelheight, complementheight)) %>%
    dplyr::select(labelwidth, complementwidth, maxwidth, labelheight, complementheight, maxheight, dplyr::everything())
  return(plotting_data)
}

#' @title Create coordinates for each row in the attrition table
#'
#' @description This function creates lables with a maximal character length per line by combining content of two dataframe columns
#'
#' @param data A dataframe containing the attrition data
#' @param box_width \code{integer} The width of the boxes in the flow charts (in canvas coordinates)
#' @param box_height \code{integer} The height of the boxes in the flow charts (in canvas coordinates)
#' @param field_height \code{float} The width of the boxes in the flow charts (in canvas coordinates)
#'
#' @return The input dataframe extended by columns containing x and y coordinates for included and excluded counts
#'
#' @keywords internal
#' @noRd
.get_coordinates <- function(data, box_width, box_height, field_height){

  y <- ymin <- ymax <- down_ystart <- down_yend <- x <- side_xend <- side_y <- NULL

  plotting_data <- data %>%
    dplyr::ungroup() %>%
    dplyr::mutate(x = 50,
                  y = 100 - dplyr::row_number() * field_height + box_height/2) %>%
    # coordinates of text box
    dplyr::mutate(
      box_width = box_width,
      box_height = box_height,
      ymin = y - (box_height/2),
      ymax = y + (box_height/2)) %>%
    # coordinates of down arrow
    dplyr::mutate(down_ystart = dplyr::lag(ymin),
                  down_yend = ymax) %>%
    # coordinates of side arrow
    dplyr::mutate(side_y = down_ystart - 0.5*(down_ystart-down_yend),
                  side_xstart = x,
                  side_xend = x + (box_width/2) + 10) %>%
    # complement coordinates
    dplyr::mutate(cx = side_xend + (box_width/2) ,
                  cy = side_y)

  return(plotting_data)

}

