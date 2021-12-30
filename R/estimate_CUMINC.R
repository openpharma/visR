#' Competing Events Cumulative Incidence
#'
#' @param CNSR Column name indicating the outcome and censoring statuses.
#' Column must be a factor and the first level indicates censoring, the
#' next level is the outcome of interest, and the remaining levels are the
#' competing events.
#' @inheritParams estimate_KM
#' @inheritParams visr
#' @inheritParams add_CI.ggsurvfit
#' @inheritParams add_risktable.ggsurvfit
#' @inheritParams get_risktable
#' @return
#' @name estimate_CUMINC
#'
#' @examples
#' estimate_CUMINC(
#'   tidycmprsk::trial,
#'   strata = "trt",
#'   CNSR = "death_cr",
#'   AVAL = "ttdeath"
#' ) %>%
#'   visr() %>%
#'   add_CI() %>%
#'   add_risktable()
NULL

#' @export
#' @rdname estimate_CUMINC
estimate_CUMINC <- function(data
                            ,strata = NULL
                            ,CNSR
                            ,AVAL
                            ,conf.int = 0.95
                            ,...){
  # check for installation of tidycmprsk package
  rlang::check_installed(pkg = "tidycmprsk", version = "0.1.0.9003")
  rlang::check_installed(pkg = "glue")
  if (packageVersion("hardhat") <= "0.1.6") {
    message("`estimate_CUMINC()` requires >v0.1.6 of the {hardhat} pcakage.")
    message("Install with `devtools::install_github('tidymodels/hardhat')`")
    return(invisible())
  }

  # checking/prepping inputs ---------------------------------------------------
  strata <- strata %||% "1" %>% paste(collapse = " + ")

  cuminc <-
    tidycmprsk::cuminc(
      formula = as.formula(glue::glue("survival::Surv({AVAL}, {CNSR}) ~ {strata}")),
      data = data,
      conf.level = conf.int,
      ...
    )

  # only keeping outcome of interest
  cuminc$tidy <-
    cuminc$tidy %>%
    dplyr::filter(.data$outcome %in% names(cuminc$failcode)[1])

  # adding strata column if not already present
  if (!"strata" %in% names(cuminc$tidy)) {
    cuminc$tidy <- dplyr::mutate(cuminc$tidy, strata = "Overall")
  }

  cuminc
}


#' @export
#' @rdname estimate_CUMINC
visr.tidycuminc <- function(x = NULL
                            ,x_label = "Time"
                            ,y_label = "Cumulative Incidence"
                            ,x_units = NULL
                            ,x_ticks = pretty(x$tidy$time, 10)
                            ,y_ticks = pretty(c(0, 1), 5)
                            ,legend_position = "right"
                            ,...){
  if (!is.null(x_units)) {
    x_label <- paste0(x_label, " (", x_units, ")")
  }

  # Plotit -----------------------------------------------------
  yscaleFUN <- function(x) sprintf("%.2f", x)

  gg <-
    x$tidy %>%
    dplyr::rename(est = estimate) %>%
    ggplot2::ggplot(ggplot2::aes(x = time,
                                 group = strata,
                                 fill = strata)) +
    ggplot2::geom_step(ggplot2::aes(y = est, col = strata)) +
    ggplot2::scale_x_continuous(breaks = x_ticks,
                                limits = c(min(x_ticks), max(x_ticks))) +
    ggplot2::xlab(x_label) +
    ggplot2::scale_y_continuous(breaks = y_ticks,
                                labels = yscaleFUN,
                                limits = c(min(y_ticks), max(y_ticks))) +
    ggplot2::ylab(y_label) +
    ggplot2::theme(legend.position = legend_position) +
    ggplot2::theme(legend.key = ggplot2::element_blank()) +
    NULL

  class(gg) <- append(class(gg), "ggtidycuminc")
  attr(gg, "tidycuminc") <- x

  gg
}


#' @export
#' @rdname estimate_CUMINC
add_CI.ggtidycuminc <- function(gg,
                                alpha = 0.1,
                                style = c("ribbon", "step"),
                                linetype = 2, ...){
  style <- match.arg(style)

  gg_gb <- ggplot2::ggplot_build(gg)
  strata_colours <- unique(gg_gb$data[[1]]$colour)

  if (style == "ribbon"){
    gg <-
      gg +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = conf.low,
                     ymax = conf.high),
        na.rm = TRUE,
        show.legend = FALSE) +
      ggplot2::scale_fill_manual(values = ggplot2::alpha(strata_colours, alpha))
  }

  if (style == "step"){
    gg <-
      gg +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin   = conf.low,
                     ymax   = conf.high,
                     colour = strata),
        outline.type = "both",
        linetype = linetype,
        show.legend = FALSE,
        na.rm = TRUE) +
      ggplot2::scale_fill_manual(values = ggplot2::alpha(strata_colours, 0))
  }

  return(gg)
}

#' @export
#' @rdname estimate_CUMINC
get_risktable.ggtidycuminc <- function(x
                                       ,times = NULL
                                       ,statlist = c("n.risk")
                                       ,label = NULL
                                       ,group = "strata"
                                       ,collapse = FALSE
                                       ,...) {

  # list of statistics and their labels
  if (!is.null(label)) {
    lst_stat_labels <- as.list(label) %>% stats::setNames(statlist)
  }
  else {
    lst_stat_labels_default <-
      list(n.risk = "At Risk",
           n.event = "N Event",
           n.censor = "N Censored",
           cumulative.event = "Cum. N Event",
           cumulative.censor = "Cum. N Censored")
    lst_stat_labels <- lst_stat_labels_default[statlist]
  }

  tidycmprsk::tidy(attr(x, "tidycuminc"), times = times)  %>%
    # dplyr::mutate(strata = ifelse("strata" %in% names(.), strata, "Overall")) %>%
    # dplyr::filter(.data$outcome %in% names(attr(x, "tidycuminc")$failcode)[1]) %>%
    dplyr::select(time, strata, n.risk, n.event, cumulative.event, n.censor, cumulative.censor) %>%
    tidyr::pivot_longer(cols = -c(time, strata)) %>%
    tidyr::pivot_wider(
      id_cols = c(time, name),
      values_from = "value",
      names_from = "strata"
    ) %>%
    dplyr::mutate(
      y_values = dplyr::recode(name, !!!lst_stat_labels)
    ) %>%
    dplyr::filter(.data$name %in% .env$statlist) %>%
    dplyr::select(time, y_values, dplyr::everything(), -name)
}

#' @export
#' @rdname estimate_CUMINC
add_risktable.ggtidycuminc <- function(gg
                                       ,times = NULL
                                       ,statlist = c("n.risk")
                                       ,label = NULL
                                       ,group = "strata"
                                       ,collapse = FALSE
                                       ,...){

  # Obtain the relevant table -----------------------------------------------
  tidy_object <- gg$data
  cuminc_object <- attr(gg, "tidycuminc")

  ggbld <- ggplot2::ggplot_build(gg)

  graphtimes <- as.numeric(ggbld$layout$panel_params[[1]]$x$get_labels())
  times <- times %||% graphtimes

  final <- get_risktable(gg
                         ,times
                         ,statlist
                         ,label
                         ,group
                         ,collapse)

  level_title <- names(final) %>% setdiff(c("time", "y_values"))

  # Plot requested tables below using list approach with map function -------
  tbls <-
    base::Map(
      function(level_title) {
        ggrisk <- ggplot2::ggplot(final,
                                  ggplot2::aes(
                                    x = time,
                                    y = stats::reorder(y_values, dplyr::desc(y_values)),
                                    label = format(get(level_title), nsmall = 0) # = value columns
                                  )
        ) +
          ggplot2::geom_text(size = 3.0, hjust = 0.5, vjust = 0.5, angle = 0, show.legend = FALSE) +
          ggplot2::theme_bw() +
          ggplot2::scale_x_continuous(breaks = graphtimes,
                                      limits = c(min(graphtimes), max(graphtimes))) +

          ggplot2::theme(axis.title.x = ggplot2::element_text(size = 8,
                                                              vjust = 1,
                                                              hjust = 1),
                         panel.grid.major = ggplot2::element_blank(),
                         panel.grid.minor = ggplot2::element_blank(),
                         panel.border = ggplot2::element_blank(),
                         axis.line = ggplot2::element_blank(),
                         axis.text.x = ggplot2::element_blank(),
                         axis.ticks = ggplot2::element_blank(),
                         axis.text.y = ggplot2::element_text(size = 8, colour = "black", face = "plain"),
                         plot.margin = ggplot2::unit(c(1, 0, 0, 0), "lines"),
                         plot.title = ggplot2::element_text(hjust = 0, vjust = 0),
                         legend.position = "none"
          ) +
          ggplot2::xlab(NULL) +
          ggplot2::ylab(NULL)

        if (!is.na(level_title) && !is.null(level_title)){
          ggrisk <- ggrisk +
            ggplot2::ggtitle(level_title) +
            ggplot2::theme(plot.title = ggplot2::element_text(size = 10))
        }

        return(ggrisk)
      },
      level_title = as.list(level_title)
    )

  # Align plot and table by adjusting width ---------------------------------

  gglist <-
    list(gg) %>%
    base::append(tbls)

  ggA <-
    gglist %>%
    align_plots()

  # Create plot and add class -----------------------------------------------

  ## cowplot allows to align according to an axis (+left) and change the height
  ggB <-
    cowplot::plot_grid(plotlist = ggA,
                       align = "none",
                       nrow = length(ggA),
                       rel_heights = c(1-(8/50 * (length(ggA)-1)), rep(8/50, length(ggA)-1))
    )

  class(ggB) <- c(class(ggB), "ggtidycuminc")

  # Add individual components -----------------------------------------------

  components <- append(list(gg), tbls)
  names(components) = c("visR_plot", title)
  ggB[["components"]] <- components

  return(ggB)
}

#' @export
#' @rdname estimate_CUMINC
add_CNSR.ggtidycuminc <- add_CNSR.ggsurvfit

`%||%` <- function (x, y) if (rlang::is_null(x)) y else x
