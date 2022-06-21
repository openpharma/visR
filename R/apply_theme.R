#' @title Applies a theme to a ggplot object.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' Takes in the styling options defined through `visR::define_theme` and applies them to a plot.
#'
#' @param gg object of class \code{ggplot}
#' @param visR_theme_dict nested list containing possible font options
#'
#' @return object of class \code{ggplot}
#'
#' @examples
#'
#' library(visR)
#'
#' theme <- visR::define_theme(
#'   strata = list(
#'     "SEX" = list(
#'       "F" = "red",
#'       "M" = "blue"
#'     ),
#'     "TRTA" = list(
#'       "Placebo" = "cyan",
#'       "Xanomeline High Dose" = "purple",
#'       "Xanomeline Low Dose" = "brown"
#'     )
#'   ),
#'   fontsizes = list(
#'     "axis" = 12,
#'     "ticks" = 10,
#'     "legend_title" = 10,
#'     "legend_text" = 8
#'   ),
#'   fontfamily = "Helvetica",
#'   grid = FALSE,
#'   bg = "transparent",
#'   legend_position = "top"
#' )
#'
#'
#' gg <- adtte %>%
#'   visR::estimate_KM(strata = "SEX") %>%
#'   visR::visr() %>%
#'   visR::apply_theme(theme)
#' gg
#'
#' @export

apply_theme <- function(gg, visR_theme_dict = NULL) {

  # Manually define colour-blind friendly palette, taken from
  # http://mkweb.bcgsc.ca/biovis2012/krzywinski-visualizing-biological-data.pdf
  coldefault <- c(
    grDevices::rgb(0, 0, 0, maxColorValue = 255), #  1
    grDevices::rgb(73, 0, 146, maxColorValue = 255), #  6
    grDevices::rgb(146, 0, 0, maxColorValue = 255), # 11
    grDevices::rgb(0, 146, 146, maxColorValue = 255), #  3
    grDevices::rgb(182, 109, 255, maxColorValue = 255), #  8
    grDevices::rgb(219, 209, 0, maxColorValue = 255), # 13
    grDevices::rgb(255, 182, 119, maxColorValue = 255), #  5
    grDevices::rgb(182, 219, 255, maxColorValue = 255), # 10
    grDevices::rgb(255, 255, 109, maxColorValue = 255), # 15

    grDevices::rgb(0, 73, 73, maxColorValue = 255), #  2
    grDevices::rgb(0, 109, 219, maxColorValue = 255), #  7
    grDevices::rgb(146, 73, 0, maxColorValue = 255), # 12
    grDevices::rgb(255, 109, 182, maxColorValue = 255), #  4
    grDevices::rgb(109, 182, 255, maxColorValue = 255), #  9
    grDevices::rgb(36, 255, 36, maxColorValue = 255) # 14
  )

  skipcolordef <- FALSE
  skipcolor <- TRUE

  font_family <- ggplot2::element_text(family = "Helvetica")

  legend_title <- ggplot2::element_text(size = 12)
  legend_text <- ggplot2::element_text(size = 10)
  legend_position <- NULL

  axis_text <- ggplot2::element_text(size = 10)
  axis_title <- ggplot2::element_text(
    size = 12,
    margin = ggplot2::margin(
      t = 6,
      r = 6,
      b = 6,
      l = 6,
      unit = "pt"
    )
  )

  panel_grid_major <- ggplot2::element_blank()
  panel_grid_minor <- ggplot2::element_blank()
  panel_background <- ggplot2::element_rect(fill = "transparent")

  plot_background <- ggplot2::element_rect(fill = "transparent")

  if (!is.null(visR_theme_dict)) {
    if (!inherits(visR_theme_dict, "visR_theme")) {
      base::message("It is recommended to generate the theme object through `visR::define_theme`. Attempting to use the provided object anyway.")
    }

    if ("strata" %in% base::names(visR_theme_dict)) {
      cols <- c()
      skipcolordef <- TRUE
      skipcolor <- FALSE

      named_strata <- base::names(visR_theme_dict[["strata"]])

      for (s in named_strata) {
        values <- visR_theme_dict[["strata"]][[s]]

        for (v in names(values)) {
          name <- v
          cols[[name]] <- values[[v]]
        }
      }

      cols <- unlist(cols)

      # find group used in plot and extract levels from the data => select these from cols
      # if these levels were not defined, use default as present in plot
      colneed <- as.character(unique(gg$data[[gg$labels$group]]))

      # Take title to match strata in theme
      lvl1 <- lapply(visR_theme_dict[["strata"]], unlist)
      lvl2 <- lapply(lvl1, function(x) any(colneed %in% names(x)))
      ttl <- names(which(lvl2 == TRUE))

      # from the strata in the theme, which were used in the estimation
      if (!any(colneed %in% names(cols))) {
        skipcolor <- TRUE
      }

      if (length(colneed) > length(coldefault)) {
        ## too many strata, keep as is
        # layer <- ggplot2::layer_data(gg)
        # cols <- layer[unique(layer[["group"]]), "colour"]
        # names(cols) <- colneed
        skipcolordef <- FALSE
        skipcolor <- TRUE
      } else if (length(intersect(names(cols), colneed)) > 0) {
        cols <- cols[intersect(names(cols), colneed)]
      } else if (length(colneed) <= length(coldefault)) {
        cols <- coldefault[1:length(colneed)]
        names(cols) <- colneed
        skipcolordef <- FALSE
      }
    }

    # fonts and text -----------------------------------------------------------

    if ("fontsizes" %in% base::names(visR_theme_dict)) {
      if (is.numeric(visR_theme_dict[["fontsizes"]])) {
        default_fontsize <- visR_theme_dict[["fontsizes"]]
        axis_margin <- base::floor(default_fontsize / 2)
        axis_title <- ggplot2::element_text(
          size = default_fontsize,
          margin = ggplot2::margin(
            t = axis_margin,
            r = axis_margin,
            b = axis_margin,
            l = axis_margin,
            unit = "pt"
          )
        )
        axis_text <- ggplot2::element_text(size = default_fontsize)
        legend_title <- ggplot2::element_text(size = default_fontsize)
        legend_text <- ggplot2::element_text(size = default_fontsize)
      } else if (is.list(visR_theme_dict[["fontsizes"]])) {
        if ("axis" %in% names(visR_theme_dict[["fontsizes"]])) {
          axis_title_fontsize <- visR_theme_dict[["fontsizes"]][["axis"]]
          axis_margin <- base::floor(axis_title_fontsize / 2)
          axis_title <- ggplot2::element_text(
            size = axis_title_fontsize,
            margin = ggplot2::margin(
              t = axis_margin,
              r = axis_margin,
              b = axis_margin,
              l = axis_margin,
              unit = "pt"
            )
          )
        }

        if ("ticks" %in% names(visR_theme_dict[["fontsizes"]])) {
          axis_text <- ggplot2::element_text(size = visR_theme_dict[["fontsizes"]][["ticks"]])
        }

        if ("legend_title" %in% names(visR_theme_dict[["fontsizes"]])) {
          legend_title <- ggplot2::element_text(size = visR_theme_dict[["fontsizes"]][["legend_title"]])
        }

        if ("legend_text" %in% names(visR_theme_dict[["fontsizes"]])) {
          legend_text <- ggplot2::element_text(size = visR_theme_dict[["fontsizes"]][["legend_text"]])
        }
      }
    }

    if ("fontfamily" %in% base::names(visR_theme_dict)) {
      font_family <- ggplot2::element_text(family = visR_theme_dict[["fontfamily"]])
    }

    # grid ---------------------------------------------------------------------

    if ("grid" %in% base::names(visR_theme_dict)) {
      if (is.logical(visR_theme_dict[["grid"]])) {
        if (visR_theme_dict[["grid"]] == FALSE) {
          panel_grid_major <- ggplot2::element_blank()
          panel_grid_minor <- ggplot2::element_blank()
        } else if (visR_theme_dict[["grid"]] == TRUE) {
          panel_grid_major <- ggplot2::element_line()
          panel_grid_minor <- ggplot2::element_line()
        }
      } else if (is.list(visR_theme_dict[["grid"]])) {
        if ("major" %in% base::names(visR_theme_dict[["grid"]])) {
          if (visR_theme_dict[["grid"]][["major"]] == FALSE) {
            panel_grid_major <- ggplot2::element_blank()
          } else if (visR_theme_dict[["grid"]][["major"]] == TRUE) {
            panel_grid_major <- ggplot2::element_line()
          } else {
            base::warning("Invalid argument for `theme[[\"grid\"]][[\"major\"]]`. Please use `TRUE` or `FALSE`. Setting to default `FALSE`.")
          }
        }

        if ("minor" %in% base::names(visR_theme_dict[["grid"]])) {
          if (visR_theme_dict[["grid"]][["minor"]] == FALSE) {
            panel_grid_minor <- ggplot2::element_blank()
          } else if (visR_theme_dict[["grid"]][["minor"]] == TRUE) {
            panel_grid_minor <- ggplot2::element_line()
          } else {
            base::warning("Invalid argument for `theme[[\"grid\"]][[\"minor\"]]`. Please use `TRUE` or `FALSE`. Setting to default `FALSE`.")
          }
        }

        if (!("major" %in% base::names(visR_theme_dict[["grid"]])) &
          !("minor" %in% base::names(visR_theme_dict[["grid"]]))) {
          base::warning("Could find neither `theme[[\"grid\"]][[\"major\"]]` nor `theme[[\"grid\"]][[\"minor\"]]`. Setting to default `FALSE`.")
          panel_grid_major <- ggplot2::element_blank()
          panel_grid_minor <- ggplot2::element_blank()
        }
      }
    } else {
      panel_grid_major <- ggplot2::element_blank()
      panel_grid_minor <- ggplot2::element_blank()
    }

    # background ---------------------------------------------------------------

    if ("bg" %in% base::names(visR_theme_dict)) {
      bg_colour <- visR_theme_dict[["bg"]]
      panel_background <- ggplot2::element_rect(fill = bg_colour)
      plot_background <- ggplot2::element_rect(fill = bg_colour)
    }

    # legend position ----------------------------------------------------------

    if ("legend_position" %in% base::names(visR_theme_dict)) {
      legend_position <- visR_theme_dict[["legend_position"]]
    } else {
      ggb <- ggplot2::ggplot_build(gg)
      legend_position <- ggb$plot$theme$legend.position
    }
  }

  if (is.null(legend_position)) {
    ggb <- ggplot2::ggplot_build(gg)
    legend_position <- ggb$plot$theme$legend.position
  }

  # Reset background
  gg <- gg + ggplot2::theme_minimal()

  if (!skipcolor) {
    gg <- gg +
      ggplot2::scale_colour_manual(
        labels = names(cols),
        values = cols,
        aesthetics = c("colour", "fill"), na.value = "grey50"
      ) +
      ggplot2::guides(color = ggplot2::guide_legend(ttl))
  } else if (!skipcolordef) {

    ## apply color friendly palette
    if (length(unique(ggplot2::layer_data(gg)[["group"]])) > length(coldefault)) {
      warning(paste0(length(coldefault), " is the max. number of strata supported."))
    } else {
      gg <- gg +
        ggplot2::scale_colour_manual(
          values = coldefault,
          aesthetics = c("colour", "fill"), na.value = "grey50"
        )
    }
  }

  gg <- gg +
    ggplot2::theme(
      text = font_family,
      axis.title.x = axis_title,
      axis.title.y = axis_title,
      axis.text = axis_text,
      legend.title = legend_title,
      legend.text = legend_text,
      panel.grid.major = panel_grid_major,
      panel.grid.minor = panel_grid_minor,
      panel.background = panel_background,
      plot.background = plot_background,
      legend.position = legend_position
    )


  return(gg)
}

# END OF CODE -------------------------------------------------------------
