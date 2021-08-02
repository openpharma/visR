#' @title Highlight a specific strata
#'
#' @description S3 method for highlighting a specific strata by lowering the opacity of all other strata.
#'
#' @author Tim Treis
#'
#' @param gg visR object
#' @param ... other arguments passed on to the method 
#' 
#' @examples
#' library(visR)
#'
#' adtte %>%
#'   visR::estimate_KM("SEX") %>%
#'   visR::visr() %>%
#'   visR::add_CI(alpha = 0.4) %>%
#'   visR::add_highlight("SEX=M", bg_alpha_multiplier = 0.2)
#'
#' @return The input `ggplot2` object with adjusted `alpha` values
#'
#' @rdname add_highlight
#'
#' @export

add_highlight <- function(gg, ...){
  UseMethod("add_highlight", gg)
}

#' @param gg A ggplot created with visR
#' @param strata Name of the strata to be highlighted as shown in the legend.
#' @param bg_alpha_multiplier Factor with which the `alpha` values of all but the specified strata will be multiplied.
#'
#' @rdname add_highlight
#' @method add_highlight ggsurvfit
#' @export

add_highlight.ggsurvfit <- function(gg, 
                                    strata, 
                                    bg_alpha_multiplier = 0.2,
                                    ...) {
  
  # Ugly hack to suppress CRAN warning as described here:
  # https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/#option-two
  alpha <- colour <- fill <- group <- NULL
  
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
    return(gg)
    
  } else {
    
    # Which group(s) in the ggplot data object corresponds to the bg strata?
    bg_strata_ids <- base::which(strata != strata_labels)
    
    # Replace the previous hex alpha values with the new one
    for (i in 1:length(gg_gb$data)) {
      
      if ("ymin" %in% colnames(gg_gb$data[[i]])) {
        
        # Check whether colour contains an alpha value
        if (nchar(gg_gb$data[[i]]$fill[[1]])) {
          
          gg_gb$data[[i]] <- gg_gb$data[[i]] %>%
            dplyr::rowwise() %>%
            dplyr::mutate(alpha = get_alpha_from_hex_colour(fill)) %>%
            as.data.frame()
          
        }
        
        gg_gb$data[[i]] <- gg_gb$data[[i]] %>%
          dplyr::rowwise() %>%
          dplyr::mutate(alpha = base::ifelse(is.na(alpha), 1, alpha)) %>%
          dplyr::mutate(alpha = base::ifelse(group %in% bg_strata_ids, 
                                             alpha * bg_alpha_multiplier, 
                                             alpha)) %>%
          dplyr::mutate(fill = replace_hex_alpha(fill, numeric_alpha_to_hex(alpha))) %>%
          as.data.frame()
        
        strata_colours <- unique(gg_gb$data[[i]]$fill)

        suppressMessages(gg <- gg + ggplot2::scale_fill_manual(values = strata_colours))
        
      } else {
        
        gg_gb$data[[i]] <- gg_gb$data[[i]] %>%
          dplyr::rowwise() %>%
          dplyr::mutate(alpha = base::ifelse(is.na(alpha), 1, alpha)) %>%
          dplyr::mutate(alpha = base::ifelse(group %in% bg_strata_ids, 
                                             alpha * bg_alpha_multiplier, 
                                             alpha)) %>%
          dplyr::mutate(colour = paste0(colour, numeric_alpha_to_hex(alpha))) %>%
          as.data.frame()
        
        strata_colours <- unique(gg_gb$data[[i]]$colour)

        suppressMessages(gg <- gg + ggplot2::scale_color_manual(values = strata_colours))
        
      }
    }
  }
  
  return(gg)
  
}
