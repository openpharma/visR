#' Plot step lines based on a survival or time to event analysis
#' 
#' The function assumes a tidied tibble dataset with the following variables:
#' time 
#' estimate
#' strata
#'
#' @param x tidy tibble dataset. Based on broom tidied dataset. 
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' library(survival)
#' library(magrittr)
#' library(dplyr)
#' library(ggplot2)
#' library(broom)
#' survival::survfit(Surv(time, status) ~ sex, data = lung)%>%
#'   tidy() %>%
#'   visr_plot_surv()
visr_plot_surv <- function(x) {
  df <- x
  gg <-
    df %>%
    ggplot2::ggplot(aes(time, estimate, group = strata)) +
    ggplot2::geom_line() +
    ggplot2::geom_linerange(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
    ggplot2::geom_point(
      data = df %>% dplyr::filter(n.censor > 0),
      aes(time, estimate),
      alpha = 0.5
    ) +
    ggtitle("Update the title", subtitle = "Fill out the population")
  return(gg)
}


#' Function to summarise risk by interval
#' See code from here https://mcfromnz.wordpress.com/2012/05/05/kaplan-meier-survival-plot-with-at-risk-table-by-sub-groups/
#'
#' @param x surv object to summarise  
#' @param timeby intervals to disaply the risk table
#' 
#' @return tidy tibble
#' @export
#'
#' @examples
#' library(survival)
#' library(magrittr)
#' library(dplyr)
#' library(ggplot2)
#' library(broom)
#' fit <- survival::survfit(Surv(time, status) ~ sex, data = lung)
#' visr_summary_surv(fit, 50)
visr_summary_surv <- function(x, timeby){
  times <- seq(0, max(x$time), by = timeby)
  df <- 
    tibble::tibble(strata = summary(x, times = times, extend = TRUE)$strata,
                   time = summary(x, times = times, extend = TRUE)$time,
                   n.risk = summary(x, times = times, extend = TRUE)$n.risk) 
  return(df)
} 




#' Create a ggplot of a risk table
#'
#' @param x survfit object
#' @param timeby Integer to indicate range to summarise risk table
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' library(survival)
#' library(magrittr)
#' library(dplyr)
#' library(ggplot2)
#' library(broom)
#' fit <- survival::survfit(Surv(time, status) ~ sex, data = lung)
#' visr_plot_risk(fit, 50) %>%
#' ggplot(aes(x = time, y = strata_id, label = n.risk, group= strata)) + 
#' geom_text() 
visr_plot_risk <- function(x, timeby = 10){
  df <- visr_summary_surv(x, timeby) %>%
    dplyr::mutate(strata_id = group_indices(., strata) - 1)
  return(df)  
}