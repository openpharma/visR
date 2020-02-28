#' Display a simple forest plot given a tidy data set
#'
#' @param td tidy data set 
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' library(RBesT) 
#' library(tidyverse) 
#' 
#' example(crohn) 
#' vr_tidy_rbest(map_crohn)
#' map_crohn %>% vr_tidy_rbest() %>% filter(model == "meta") %>%
#' vr_plot_forest()
#'
#' map_crohn %>% vr_tidy_rbest() %>% filter(model == "stratified") %>%
#' vr_plot_forest()
#'
#' map_crohn %>% vr_tidy_rbest() %>% vr_plot_forest() + facet_wrap(~ model)
#' 
vr_plot_forest <- function(td){
  gg <- 
    td %>% 
    ggplot2::ggplot(aes( x = reorder(study, -row_id), 
              y = estimate, 
              ymin = conf.low, 
              ymax = conf.high )
         ) +
    ggplot2::geom_pointrange(show.legend = FALSE, width = 1) + 
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    ggtitle("Update the title", subtitle = "Fill out the population")
  return(gg)
}




