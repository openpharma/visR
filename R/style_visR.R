#' Add visR theme to ggplot chart
#'
#' @param gg ggplot object to style
#'
#' @return
#' @export
#' @keywords visR
#'
#' @examples
#' 
#' library(ggplot2)

library(visR)
library(ggplot2)
library(thematic)

source("/shared_data/google-drive/Projekte/visR_feature_styling/R/style_visR.R")
source("/shared_data/google-drive/Projekte/visR_feature_styling/R/plot.R")

style_visR <- function(gg, ...){
  UseMethod("style_visR")
}

style_visR.ggsurvfit <- function(gg, 
                                 theme = "visR", 
                                 gg_color = NULL,
                                 gg_fill = NULL,
                                 ...) {
  
  # To check whether it's a theme object or a string
  if (length(theme) == 1) {
    
    if (theme == "visR") {
      
      gg_themed <- gg + 
        ggsci::scale_color_nejm() + 
        ggsci::scale_fill_nejm() + 
        ggplot2::theme_bw() +
        NULL
      
    }
    
  } else {
  
    gg_classes <- invisible(class(theme))
      
    if (("theme" %in% gg_classes) & ("gg" %in% gg_classes)) {
    
      gg_themed <- gg + theme
      
    } else if ("thematic_theme" %in% gg_classes) {
      
      thematic::thematic_set_theme(theme)
      
      gg_themed <- invisible(ggplot2::ggplot_build(gg)$plot)
      
      thematic::thematic_set_theme(NULL)
      
    } else {
      
      warning("No theme specified")
      gg_themed <- invisible(ggplot2::ggplot_build(gg)$plot)
      
    }
    
    if (!missing(gg_color)) {
      
      gg_themed <- gg_themed + gg_color
      
    }
    
    if (!missing(gg_fill)) {
      
      gg_themed <- gg_themed + gg_fill
      
    }
    
  }
  
  return(gg_themed)
  
}


gg <- adtte %>%
  visR::estimate_KM(strata = "SEX") %>%
  visR::plot() %>%
  visR::add_CI() %>%
  visR::add_CNSR()
gg

gg %>% style_visR()

gg %>% style_visR(ggplot2::theme_classic())

theme <- thematic::thematic_theme(bg = "#222222", 
                                  fg = "white", 
                                  accent = "#0CE3AC", 
                                  font = "Oxanium")

gg %>% style_visR(theme)

gg %>%
  style_visR(theme,
             gg_color = ggsci::scale_color_ucscgb(),
             gg_fill = ggsci::scale_fill_jco())
