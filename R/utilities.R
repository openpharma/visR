### function that will find the "lhs" symbol in the pipeline, given that this function is called from with a pipeline
 ## https://github.com/tidyverse/magrittr/issues/115#issuecomment-173894787
the_lhs <- function() {
  parents <- lapply(sys.frames(), parent.env)

  is_magrittr_env <-
    vapply(parents, identical, logical(1), y = environment(`%>%`))

  if (any(is_magrittr_env)) {
    deparse(get("lhs", sys.frames()[[max(which(is_magrittr_env))]]))
  }
}

# https://stackoverflow.com/questions/26159495/align-multiple-ggplot-graphs-with-and-without-legends
AlignPlots <- function(pltlist = NULL) {
  
  .LegendWidth <- function(x) x$grobs[[8]]$grobs[[1]]$widths[[4]]

  plots.grobs <- lapply(pltlist, ggplotGrob)
  max.widths <- do.call(grid::unit.pmax, lapply(plots.grobs, "[[", "widths"))
  legends.widths <- lapply(plots.grobs, .LegendWidth)
  
  max.legends.width <- base::suppressWarnings(do.call(max, legends.widths))
  
  plots.grobs.eq.widths <- lapply(plots.grobs, function(x) {
    x$widths <- max.widths
    x
  })
  
  plots.grobs.eq.widths.aligned <- lapply(plots.grobs.eq.widths, function(x) {
    if (is.gtable(x$grobs[[8]])) {
      x$grobs[[8]] <- gtable_add_cols(x$grobs[[8]], unit(abs(diff(c(LegendWidth(x), max.legends.width))),"mm"))
    }
    x
  })

  plots.grobs.eq.widths.aligned
}
