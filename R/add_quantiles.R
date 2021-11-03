#' @title Add quantile indicators to visR plot
#'
#' @description Method to add lines indicating the specified quantiles of the survival curve.
#'
#' @param gg visR object
#' @param ... other arguments passed on to the method to modify \code{\link[ggplot2]{geom_line}}
#'
#' @examples
#' 
#' library(visR)
#' 
#' adtte %>%
#'   estimate_KM("SEX") %>%
#'   visr() %>%
#'   add_quantiles() 
#' 
#' adtte %>%
#'   estimate_KM("SEX") %>%
#'   visr() %>%
#'   add_quantiles(c(0.25, 0.50, 0.75)) 
#' 
#' adtte %>%
#'   estimate_KM("SEX") %>%
#'   visr() %>%
#'   add_quantiles(c(0.25, 0.50, 0.75)) 
#'
#' @return Lines indicating the quantiles overlayed on a visR ggplot
#'
#' @rdname add_quantiles
#'
#' @export

add_quantiles <- function(gg, ...){
  UseMethod("add_quantiles", gg)
}

#' @param gg A ggplot created with visR
#' @param quantiles vector of quantiles to be displayed on the probability scale, default: 0.5
#' @param linetype string indicating the linetype as described in the aesthetics of ggplot2 \code{\link[ggplot2]{geom_line}}, default: mixed (horizontal lines are solid, vertical ones are dashed)
#' @param linecolour string indicating the linetype as described in the aesthetics of ggplot2 \code{\link[ggplot2]{geom_line}}, default: strata, (horizontal lines are grey50, vertical ones are the same colour as the respective strata)
#' @param alpha numeric value between 0 and 1 as described in the aesthetics of ggplot2 \code{\link[ggplot2]{geom_line}}, default: 1
#' @param ... other arguments passed on to the method to modify \code{\link[ggplot2]{geom_line}}
#'
#' @rdname add_quantiles
#' @method add_quantiles ggsurvfit
#' @export

add_quantiles.ggsurvfit <- function(gg, 
                                    quantiles = 0.5,
                                    linetype = "mixed",
                                    linecolour = "strata",
                                    alpha = 1, 
                                    ...) {
  
  if (!is.numeric(quantiles)) {
    
    stop("Invalid argument for `quantiles`, must be numeric. Setting it to 0.5.")
    quantiles <- 0.5
    
  }
  
  if (!is.character(linetype)) {
    
    stop("Invalid argument for `linetype`, must be a character string. Setting it to default 'mixed'.")
    linetype <- "mixed"
    
  }
  
  if (!is.character(linecolour)) {
    
    stop("Invalid argument for `linecolour`, must be a character string. Setting it to default 'strata'.")
    linecolour <- "strata"
    
  }
  
  if ((alpha > 1) | (alpha < 0)) {
    
    warning("Invalid `alpha` argument, must be between 0 and 1. Setting it to 1.0.")
    alpha <- 1.0
    
  }
  
  model <- eval(gg$data$call[[1]])
  
  model_quantiles <- model %>% 
    get_quantile(probs = quantiles) %>% 
    dplyr::filter(quantity == "quantile") 
  
  cols_to_pivot <- colnames(model_quantiles) %>%
    setdiff(c("strata", "quantity"))
  
  model_quantiles_long <- model_quantiles %>% 
    tidyr::pivot_longer(cols = cols_to_pivot) %>%
    dplyr::rename(quantile = name) %>%
    dplyr::mutate(group = paste(strata, quantile, sep = "_")) %>%
    dplyr::rename(n = value) %>%
    dplyr::mutate(surv = 1 - (as.numeric(quantile))/100) %>%
    dplyr::select(-quantity)
  
  horizontal_helper <- model_quantiles_long %>%
    dplyr::mutate(n = 0)
  
  vertical_helper <- model_quantiles_long %>%
    dplyr::mutate(surv = 0)
  
  if (linetype == "mixed") {
    
    linetype_horizontal <- "solid"
    linetype_vertical <- "dashed"
    
  } else {
    
    linetype_horizontal <- linetype
    linetype_vertical <- linetype
    
  }
  
  if (linecolour == "strata") {
    
    linecolour_horizontal <- "grey50"
    linecolour_vertical <- "strata"
    
  } else {
    
    linecolour_horizontal <- linecolour
    linecolour_vertical <- linecolour
    
  }
  
  # Draw non-overlapping horizontal lines
  gg <- gg +
    ggplot2::geom_line(data = rbind(model_quantiles_long, horizontal_helper) %>%
                         dplyr::group_by(surv) %>%
                         dplyr::filter(n == 0 | n == max(n)),
                       ggplot2::aes(x = n, 
                                    y = surv, 
                                    group = group), 
                       linetype = linetype_horizontal,
                       colour = linecolour_horizontal,
                       alpha = alpha)
  
  if (linecolour_vertical == "strata") {
    
    gg <- gg +
      ggplot2::geom_line(data = rbind(model_quantiles_long, vertical_helper), 
                         ggplot2::aes(x = n, 
                                      y = surv, 
                                      group = group,
                                      colour = strata), 
                         linetype = linetype_vertical,
                         alpha = alpha)
    
  } else {
    
    gg <- gg +
      ggplot2::geom_line(data = rbind(model_quantiles_long, vertical_helper), 
                         ggplot2::aes(x = n, 
                                      y = surv, 
                                      group = group), 
                         linetype = linetype_vertical, 
                         colour = linecolour_vertical,
                         alpha = alpha)
    
  }
  
  return(gg)
}
