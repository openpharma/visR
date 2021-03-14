#' @title Add pointwise Confidence Interval to a an object created by visR through an S3 method
#'
#' @description S3 method for adding a pointwise confidence interval to a object created with visR.
#'     No default method is available at the moment.
#'
#' @author Steven Haesendonckx
#'
#' @param gg visR object
#' @param ... other arguments passed on to the method to modify \code{\link[ggplot2]{geom_ribbon}}
#'
#' @examples
#' library(survival)
#' library(dplyr)
#' library(tidyr)
#' library(ggplot2)
#'
#' survfit_object <- survival::survfit(data = adtte, Surv(AVAL, 1-CNSR) ~ TRTP)
#' vr_plot(survfit_object) %>%
#'   add_CI(alpha = 0.1, style = "step", linetype = 3)
#'
#' @return Pointwise confidence interval overlayed on a visR ggplot
#'
#' @rdname add_CI
#'
#' @export

add_highlight <- function(gg, ...){
  UseMethod("add_highlight")
}

#' @param gg A ggplot created with visR
#' @param strata aesthetic of ggplot2 \code{\link[ggplot2]{geom_ribbon}}. Default is 0.1.
#' @param alpha aesthetic of ggplot2 \code{\link[ggplot2]{geom_ribbon}}. Default is "ribbon".
#'
#' @rdname add_highlight
#' @method add_highlight ggsurvfit
#' @export

add_highlight.ggsurvfit <- function(gg, 
                                    strata, 
                                    new_alpha = 0.5,
                                    ...){
  
  if (missing(strata)) {
    
    warning("A strata to highlight has to be specified.")
    return(NULL)
  
  }
  
  # Extract names of strata objects
  gg_gb <- ggplot2::ggplot_build(gg)
  gg_gtable <- ggplot2::ggplot_gtable(gg_gb)
  gg_guidebox_id <- base::which(sapply(gg_gtable$grobs, function(x) x$name) == "guide-box")
  gg_table_grob <- gg_gtable$grobs[[gg_guidebox_id]]$grobs[[1]]
  
  # Get IDs of elements containing strata labels
  strata_label_ids <- base::grep("label", gg_table_grob$layout$name)
  strata_labels <- c()
  
  for (id in strata_label_ids) {
    
    label <- gg_table_grob$grobs[[id]]$children[[1]]$children[[1]]$label
    label <- base::strsplit(label, "=")[[1]][2]
    strata_labels <- c(strata_labels, label)
    
  }
  
  if (!(strata %in% strata_labels)) {
    
    msg <- "The strata you specified has not been found in the provided plot.\n"
    msg <- base::paste0(msg, 
                  "  Available strata: ", 
                  base::paste(strata_labels, collapse = ", "), 
                  "\n")
    msg <- base::paste0(msg, "  Please adjust and rerun.")
    
    warning(msg)
    return(NULL)
    
  } else {
    
    # Which HEX code does the specified `alpha` correspond to?
    alpha_decimal = base::round((alpha * 100) * (255/100))
    alpha_hex = base::as.hexmode(alpha_decimal)
    
    # Which group in the ggplot data object corresponds to the desired strata?
    highlight_strata_id <- base::which(strata == strata_labels)
    
    # Replace the previous hex alpha values with the new one
    for (i in range(1:length(gg_gb$data))) {
      
      if ("colour" %in% colnames(gg_gb$data[[i]])) {
        
        # Covers the `style = step` case
        gg_gb$data[[i]] = gg_gb$data[[i]] %>% 
          dplyr::mutate(colour = base::ifelse(group != highlight_strata_id,
                                              base::gsub("(#[0-9a-fA-F]{6})([0-9a-fA-F]{2})", 
                                                         paste0("\\1", alpha_hex), 
                                                         colour),
                                              colour))
        
      }
      
      if ("fill" %in% colnames(gg_gb$data[[i]])) {
        
        # Covers the `style = ribbon` case
        # Here, ggplot specifies an `alpha` column which apparently overrides the
        # alpha part of the hex color code
        gg_gb$data[[i]] = gg_gb$data[[i]] %>% 
          dplyr::mutate(alpha = base::ifelse(group != highlight_strata_id,
                                             alpha * new_alpha,
                                             alpha))
        
      }
      
    }
    
  }
  
  gg <- cowplot::ggdraw() + cowplot::draw_grob(ggplot2::ggplot_gtable(gg_gb))
  
  return(gg)
}
