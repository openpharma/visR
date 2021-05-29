#' @title Provides a simple wrapper for theming
#'
#' @description This function collects several lists if they are present. If absent, reasonable defaults are used.
#'
#' @param strata list containing the different strata and name:colour value pairs
#' @param fontsizes list containing the font sizes for different options
#' @param fontfamily string with the name of a supported font
#' @param grid boolean that specifies whether the grid should be drawn or not
#' @param bg string giving the colour for the background of the plot
#' @param legend_position string indicating the legend position
#'
#' @return Nested list with styling preferences for a ggplot object
#'
#' @examples
#'
#' theme <- visR::define_theme(strata = list("SEX" = list("F" = "red",
#'                                                        "M" = "blue"),
#'                                           "TRTA" = list("Placebo" = "cyan",
#'                                                         "Xanomeline High Dose" = "purple",
#'                                                         "Xanomeline Low Dose" = "brown")),
#'                             fontsizes = list("axis" = 12,
#'                                              "ticks" = 10,
#'                                              "legend_title" = 10,
#'                                              "legend_text" = 8),
#'                             fontfamily = "Helvetica",
#'                             grid = list("major" = FALSE,
#'                                         "minor" = FALSE),
#'                             bg = "transparent",
#'                             legend_position = "top")
#'
#' @export

define_theme <- function(strata = NULL,
                         fontsizes = NULL,
                         fontfamily = "Helvetica",
                         grid = FALSE,
                         bg = "transparent",
                         legend_position = NULL) {
  theme <- list()
  
  if (!base::is.null(strata)) {
    
    if (base::is.list(strata)) {
      
      if (base::length(base::names(strata)) > 0) {
        
        theme[["strata"]] <- strata
      
      } else {
        
        base::warning("Invalid argument for `strata`. Please provide a named list as described in the documentation. Setting strata to `NULL`.")
        theme[["strata"]] <- NULL
        
      }
    }
  } 
  
  if (!base::is.null(fontsizes)) {
    
    if (base::is.list(fontsizes)) {
      
      if (base::length(base::names(fontsizes)) > 0) {
        
        theme[["fontsizes"]] <- fontsizes
        
      } else {
        
        base::warning("Invalid argument for `fontsizes`. Please provide a named list for the individual plot elements as described in the documentation. Setting fontsizes to `NULL`.")
        theme[["fontsizes"]] <- NULL
        
      }
      
    } else if (base::is.numeric(fontsizes)) {
      
      base::message("Setting all fontsizes to the provided numeric value. It is recommended to use a named list as described in the documentation.")
      theme[["fontsizes"]] <- fontsizes
      
    } else {
      
      base::warning("Invalid argument for `fontsizes`. Please provide a named list as described in the documentation. Setting fontsizes to `NULL`.")
      theme[["fontsizes"]] <- NULL
      
    }
  } 
  
  if (!base::is.character(fontfamily)) {
    
    base::warning("Invalid argument for `fontfamily`. Please provide the name of a valid font family as a string. Setting to default `Helvetica`.")
    theme[["fontfamily"]] <- "Helvetica"
    
  } else {
    
    theme[["fontfamily"]] <- fontfamily
    
  }
  
  if (base::is.logical(grid)) {
    
    if (grid == TRUE) {
      
      theme[["grid"]] <- list("major" = TRUE,
                              "minor" = FALSE)
      
    } else {
      
      theme[["grid"]] <- grid 
      
    }
    
  } else if (is.list(grid)) {
    
    if (("major" %in% names(grid)) | ("minor" %in% names(grid))) {
      
      theme[["grid"]] <- grid
      
    } else {
      
      base::warning("Invalid argument for `grid`. Please use a boolean or a list to indicate whether you want a background grid. Setting to default `FALSE`.")
      theme[["grid"]] <- FALSE
      
    }
    
  } else {
    
    base::warning("Invalid argument for `grid`. Please use a boolean or a list to indicate whether you want a background grid. Setting to default `FALSE`.")
    theme[["grid"]] <- FALSE
    
  }
  
  if (!base::is.character(bg)) {
    
    base::warning("Invalid argument for `bg`. Please provide the name of a valid colour as a string. Setting to default `transparent`.")
    theme[["bg"]] <- "transparent"
    
  } else {
    
    theme[["bg"]] <- bg
    
  }
  
  if (base::is.null(legend_position) | base::is.character(legend_position)) {
    
    theme[["legend_position"]] <- legend_position
    
  } else {
    
    base::warning("Invalid argument for `legend_position`. Setting it to default \"right\".")
    theme[["legend_position"]] <- "right"
    
  }
  
  base::class(theme) <- base::append(base::class(theme), "visR_theme")
  
  return(theme)
  
}

#' @title Applies a theme to a ggplot object.
#'
#' @description Takes in the styling options defined through `visR::define_theme` and applies them to a plot.
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
#' theme <- visR::define_theme(strata = list("SEX" = list("F" = "red",
#'                                                        "M" = "blue"),
#'                                           "TRTA" = list("Placebo" = "cyan",
#'                                                         "Xanomeline High Dose" = "purple",
#'                                                         "Xanomeline Low Dose" = "brown")),
#'                             fontsizes = list("axis" = 12,
#'                                              "ticks" = 10,
#'                                              "legend_title" = 10,
#'                                              "legend_text" = 8),
#'                             fontfamily = "Helvetica",
#'                             grid = FALSE,
#'                             bg = "transparent",
#'                             legend_position = "top")
#'
#'
#' gg <- adtte %>%
#'   visR::estimate_KM(strata = "SEX") %>%
#'   visR::visr() %>%
#'   visR::add_CI() %>%
#'   visR::apply_theme(theme)
#' gg
#'
#' @export

apply_theme <- function(gg, visR_theme_dict = NULL) {
  
  # Manually define colour-blind friendly palette, taken from
  # http://mkweb.bcgsc.ca/biovis2012/krzywinski-visualizing-biological-data.pdf
  # skipping black
  cols <- c(
    grDevices::rgb(0,  73,  73, maxColorValue = 255),
    grDevices::rgb(0, 146, 146, maxColorValue = 255),
    grDevices::rgb(255, 109, 182, maxColorValue = 255),
    grDevices::rgb(255, 182, 119, maxColorValue = 255),
    grDevices::rgb(73,   0, 146, maxColorValue = 255),
    grDevices::rgb(0, 109, 219, maxColorValue = 255),
    grDevices::rgb(182, 109, 255, maxColorValue = 255),
    grDevices::rgb(109, 182, 255, maxColorValue = 255),
    grDevices::rgb(182, 219, 255, maxColorValue = 255),
    grDevices::rgb(146,   0,   0, maxColorValue = 255),
    grDevices::rgb(146,  73,   0, maxColorValue = 255),
    grDevices::rgb(219, 209,   0, maxColorValue = 255),
    grDevices::rgb(36, 255,  36, maxColorValue = 255),
    grDevices::rgb(255, 255, 109, maxColorValue = 255)
  )
  
  font_family = ggplot2::element_text(family = "Helvetica")
  axis_title = ggplot2::element_text(size = 12)
  axis_text = ggplot2::element_text(size = 10)
  legend_title = ggplot2::element_text(size = 12)
  legend_text = ggplot2::element_text(size = 10)
  legend_position = "right"
  panel_grid_major = ggplot2::element_blank()
  panel_grid_minor = ggplot2::element_blank()
  panel_background = ggplot2::element_rect(fill = "transparent")
  plot_background = ggplot2::element_rect(fill = "transparent")
  
  if (!is.null(visR_theme_dict)) {
    
    if (!("visR_theme" %in% base::class(visR_theme_dict))) {
      
      base::message("It is recommended to generate the theme object through `visR::define_theme`. Attempting to use the provided object anyway.")
      
    }
    
    
    
    if ("strata" %in% base::names(visR_theme_dict)) {
      
      cols <- c()
      named_strata <- base::names(visR_theme_dict[["strata"]])
      
      for (s in named_strata) {
        values <- visR_theme_dict[["strata"]][[s]]
        
        for (v in names(values)) {
          name <- paste0(s, "=", v)
          cols[[name]] <- values[[v]]
        }
      }
      
      cols <- unlist(cols)
      
    } 
    
    
    # fonts and text -----------------------------------------------------------
    
    if ("fontsizes" %in% base::names(visR_theme_dict)) {
      
      if ("axis" %in% names(visR_theme_dict[["fontsizes"]])) {
        axis_title = ggplot2::element_text(size = visR_theme_dict[["fontsizes"]][["axis"]])
        
      }
      
      if ("ticks" %in% names(visR_theme_dict[["fontsizes"]])) {
        axis_text = ggplot2::element_text(size = visR_theme_dict[["fontsizes"]][["ticks"]])
        
      }
      
      if ("legend_title" %in% names(visR_theme_dict[["fontsizes"]])) {
        legend_title = ggplot2::element_text(size = visR_theme_dict[["fontsizes"]][["legend_title"]])
        
      }
      
      if ("legend_text" %in% names(visR_theme_dict[["fontsizes"]])) {
        legend_text = ggplot2::element_text(size = visR_theme_dict[["fontsizes"]][["legend_text"]])
        
      }
    }
    
    if ("fontfamily" %in% base::names(visR_theme_dict)) {
      font_family = ggplot2::element_text(family = visR_theme_dict[["fontfamily"]])
      
    }
    
    
    
    # grid ---------------------------------------------------------------------
    
    if ("grid" %in% base::names(visR_theme_dict)) {
      
      if (is.logical(visR_theme_dict[["grid"]])) {
        
        if (visR_theme_dict[["grid"]] == FALSE) {
          
          panel_grid_major = ggplot2::element_blank()
          panel_grid_minor = ggplot2::element_blank()
          
        } else if (visR_theme_dict[["grid"]] == TRUE) {
          
          panel_grid_major = ggplot2::element_line()
          panel_grid_minor = ggplot2::element_line()
          
        }
        
      } else if (is.list(visR_theme_dict[["grid"]])) {
        
        if ("major" %in% base::names(visR_theme_dict[["grid"]])) {
          
          if (visR_theme_dict[["grid"]][["major"]] == FALSE) {
            
            panel_grid_major = ggplot2::element_blank()
            
          } else if (visR_theme_dict[["grid"]][["major"]] == TRUE) {
            
            panel_grid_major = ggplot2::element_line()
            
          } else {
            
            base::warning("Invalid argument for `theme[[\"grid\"]][[\"major\"]]`. Please use `TRUE` or `FALSE`. Setting to default `FALSE`.")
            
          }
        }
        
        if ("minor" %in% base::names(visR_theme_dict[["grid"]])) {
          
          if (visR_theme_dict[["grid"]][["minor"]] == FALSE) {
            
            panel_grid_minor = ggplot2::element_blank()
            
          } else if (visR_theme_dict[["grid"]][["minor"]] == TRUE) {
            
            panel_grid_minor = ggplot2::element_line()
            
          } else {
            
            base::warning("Invalid argument for `theme[[\"grid\"]][[\"minor\"]]`. Please use `TRUE` or `FALSE`. Setting to default `FALSE`.")
            
          }
        }
        
        if (!("major" %in% base::names(visR_theme_dict[["grid"]])) & 
            !("minor" %in% base::names(visR_theme_dict[["grid"]]))) {
          
          base::warning("Could find neither `theme[[\"grid\"]][[\"major\"]]` nor `theme[[\"grid\"]][[\"minor\"]]`. Setting to default `FALSE`.")
          panel_grid_major = ggplot2::element_blank()
          panel_grid_minor = ggplot2::element_blank()
          
        }
        
      }
    } else {
      panel_grid_major = ggplot2::element_blank()
      panel_grid_minor = ggplot2::element_blank()
    }
    
    # background ---------------------------------------------------------------
    
    if ("bg" %in% base::names(visR_theme_dict)) {
      bg_colour <- visR_theme_dict[["bg"]]
      panel_background = ggplot2::element_rect(fill = bg_colour)
      plot_background = ggplot2::element_rect(fill = bg_colour)
    }
    
    # legend position ----------------------------------------------------------
    
    if ("legend_position" %in% base::names(visR_theme_dict)) {
      legend_position <- visR_theme_dict[["legend_position"]]
    }
    
  }
  
  # Reset background
  gg <- gg + ggplot2::theme_minimal()
  
  gg <- gg +
    ggplot2::scale_colour_manual(values = cols,
                                 aesthetics = c("colour", "fill")) +
    ggplot2::theme(
      text = font_family,
      axis.title = axis_title,
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

