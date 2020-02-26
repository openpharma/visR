#' Render a forest plot given a tibble
#'
#' @param x tidied tibble 
#' @param prob probability range for uncertainty interval
#'
#' @return td tidied tibble
#' @export
#'
#' @examples
#' library(RBesT)
#' vr_tidy_rbest(map_crohn)
#' 
#' map_crohn %>% vr_tidy_rbest() %>% filter(model == "meta") %>% ggplot(aes( x =
#' study, y = estimate, ymin = conf.low, ymax = conf.high )) +
#' geom_pointrange(show.legend = FALSE, width = 1) + coord_flip()
#' 
vr_plot_forest <- function(x, prob = 0.95){
}