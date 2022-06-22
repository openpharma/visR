#' @title Provides a simple wrapper for themes
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'  This function collects several lists if they are present. If absent, reasonable defaults are used.
#'  When strata are not defined in the theme, they default to grey50 and will not be presented in the legend.
#' @param strata named list containing the different strata and name:colour value pairs
#' @param fontsizes named list containing the font sizes for different options
#' @param fontfamily string with the name of a supported font
#' @param grid boolean that specifies whether the major and minor grid should be drawn. The drawing of major and minor
#'   gridlines can be manipulated separately by using a boolean indicator in a named `list` with elements `major`
#'   and `minor`.
#' @param bg string defining the colour for the background of the plot
#' @param legend_position string defining the legend position. Valid options are NULL, 'top' 'bottom' 'right' 'left'
#'
#' @return Nested list with styling preferences for a ggplot object
#'
#' @examples
#'
#' theme <- visR::define_theme(
#'   strata = list("SEX" = list(
#'     "F" = "red",
#'     "M" = "blue"
#'   )),
#'   fontsizes = list(
#'     "axis" = 12,
#'     "ticks" = 10,
#'     "legend_title" = 10,
#'     "legend_text" = 8
#'   ),
#'   fontfamily = "Helvetica",
#'   grid = list(
#'     "major" = FALSE,
#'     "minor" = FALSE
#'   ),
#'   bg = "transparent",
#'   legend_position = "top"
#' )
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
        base::warning("Invalid argument for `strata`. Please provide a named list as described in the documentation. Setting strata to `NULL` (which results in no specific theming for stratification).")
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
  } else if (base::is.character(fontfamily) & (base::length(fontfamily) > 1)) {
    base::warning(paste0("Invalid amount of arguments for `fontfamily`. Using the first one: ", fontfamily[[1]]))
    theme[["fontfamily"]] <- fontfamily[[1]]
  } else if (base::is.character(fontfamily) &
    (base::length(fontfamily) == 1) &
    (base::nchar(fontfamily) == 0)) {
    base::warning("Invalid argument for `fontfamily`. Please provide the name of a valid font family as a string. Setting to default `Helvetica`.")
    theme[["fontfamily"]] <- "Helvetica"
  } else {
    theme[["fontfamily"]] <- fontfamily
  }

  if (base::is.logical(grid)) {
    if (grid == TRUE) {
      theme[["grid"]] <- list(
        "major" = TRUE,
        "minor" = FALSE
      )
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

  if (base::is.null(legend_position) | isTRUE(legend_position %in% c("top", "right", "left", "bottom"))) {
    theme[["legend_position"]] <- legend_position
  } else {
    base::warning("Invalid argument for `legend_position`. Setting it to default \"right\".")
    theme[["legend_position"]] <- "right"
  }

  base::class(theme) <- c("visR_theme", class(theme))

  return(theme)
}

# END OF CODE -------------------------------------------------------------
