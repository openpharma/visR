#' @title Align multiple ggplot graphs, taking into account the legend
#'
#' @description This function aligns multiple \code{ggplot} graphs by making them the same width by taking into account the legend width.
#'
#' @author Steven Haesendonckx
#'
#' @param pltlist A list of plots (TODO: provide more detail)
#'
#' @return List of \code{ggplot} with equal width.
#'
#' @references \url{https://stackoverflow.com/questions/26159495/align-multiple-ggplot-graphs-with-and-without-legends}
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(gtable)
#' library(cowplot)
#'
#' ## create 2 graphs
#' p1 <- ggplot(adtte, aes(x = as.numeric(AGE), fill = "Age")) +
#'   geom_histogram(bins = 15)
#' p2 <- ggplot(adtte, aes(x = as.numeric(AGE))) +
#'  geom_histogram(bins = 15)
#'
#' ## default alignment does not take into account legend size
#' cowplot::plot_grid(plotlist = list(p1,p2), align = "none", nrow=2)
#'
#' ## Alignplots takes into account legend width
#' cowplot::plot_grid(plotlist = AlignPlots(pltlist = list(p1, p2)), align = "none", nrow=2)
#' }

AlignPlots <- function(pltlist = NULL) {
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
#' @author Steven Haesendonckx
#'
#' @param legend_position  Deafult = "right".
#' @param legend_orientation Deafult = NULL.
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
