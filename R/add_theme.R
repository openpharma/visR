#' @title Provides a simple wrapper for theming
#'
#' @description This function collects several lists if they are present. If absent, reasonable defaults are used.
#'
#' @author Tim Treis
#'
#' @param strata list containing the different strata and name:colour value pairs
#' @param fontsizes list containing the font sizes for different options
#' @param fontfamily string with the name of a supported font
#' @param grid boolean that specifies whether the grid should be drawn or not
#' @param bg string giving the colour for the background of the plot
#'
#' @return Nested list with styling preferences for a ggplot object
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
#'                                              "ticks" = 10),
#'                             fontfamily = "Helvetica",
#'                             grid = FALSE,
#'                             bg = "transparent")
#'
#' @export

define_theme <- function(strata = NULL,
                         fontsizes = NULL,
                         fontfamily = "Helvetica",
                         grid = FALSE,
                         bg = "transparent") {
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
  
  if (!base::is.logical(grid)) {
    
    base::warning("Invalid argument for `grid`. Please use a boolean to indicate whether you want a background grid. Setting to default `FALSE`.")
    theme[["grid"]] <- FALSE
    
  } else {
    
    theme[["grid"]] <- grid
    
  }
  
  if (!base::is.character(bg)) {
    
    base::warning("Invalid argument for `bg`. Please provide the name of a valid colour as a string. Setting to default `transparent`.")
    theme[["bg"]] <- "transparent"
    
  } else {
    
    theme[["bg"]] <- bg
    
  }
  
  base::class(theme) <- base::append(base::class(theme), "visR_theme")
  
  return(theme)
  
}

#' @title Applies a theme to a ggplot object.
#'
#' @description Takes in the styling options defined through `visR::define_theme` and applies them to a plot.
#'
#' @author Tim Treis
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
#'                                              "ticks" = 10),
#'                             fontfamily = "Helvetica",
#'                             grid = FALSE,
#'                             bg = "transparent")
#'
#'
#' gg <- adtte %>%
#'   visR::estimate_KM(strata = "SEX") %>%
#'   visR::plot() %>%
#'   visR::add_CI() %>%
#'   add_theme(theme)
#' gg
#'
#' @export

add_theme <- function(gg, visR_theme_dict = NULL) {
  
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
    }
    
    if ("fontfamily" %in% base::names(visR_theme_dict)) {
      font_family = ggplot2::element_text(family = visR_theme_dict[["fontfamily"]])
      
    }
    
    
    
    # grid ---------------------------------------------------------------------
    
    if ("grid" %in% base::names(visR_theme_dict)) {
      if (visR_theme_dict[["grid"]] == FALSE) {
        panel_grid_major = ggplot2::element_blank()
        panel_grid_minor = ggplot2::element_blank()
        
      }
    }
    
    # background ---------------------------------------------------------------
    
    if ("bg" %in% base::names(visR_theme_dict)) {
      bg_colour <- visR_theme_dict[["bg"]]
      panel_background = ggplot2::element_rect(fill = bg_colour)
      plot_background = ggplot2::element_rect(fill = bg_colour)
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
      panel.grid.major = panel_grid_major,
      panel.grid.minor = panel_grid_minor,
      panel.background = panel_background,
      plot.background = plot_background
    )
  
  return(gg)
}
