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
#' 
style_visR <- function(gg) {
  font <- "sans"
  
  gg <- gg +
    
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        family = font,
        size = 12,
        face = "bold",
        color = "#222222"
      ),
      
      plot.subtitle = ggplot2::element_text(
        family = font,
        size = 10,
        margin = ggplot2::margin(9, 0, 9, 0)
      ),
      
      plot.caption = ggplot2::element_blank(),
      
      legend.text = ggplot2::element_text(
        family = font,
        size = 8,
        color = "#222222"
      ),
      
      strip.text = ggplot2::element_text(size  = 8,  hjust = 0)
    )
  
  return(gg)
}
