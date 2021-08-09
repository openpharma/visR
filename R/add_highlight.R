#' @title Highlight a specific strata
#'
#' @description S3 method for highlighting a specific strata by lowering the opacity of all other strata.
#'
#' @param gg visR object
#' @param ... other arguments passed on to the method 
#' 
#' @examples
#'
#' adtte %>%
#'   visR::estimate_KM(strata = "SEX") %>%
#'   visR::visr() %>%
#'   visR::add_CI(alpha = 0.4) %>%
#'   visR::add_highlight(strata = "SEX=M", bg_alpha_multiplier = 0.2)
#'
#' strata = c("TRTP=Placebo", "TRTP=Xanomeline Low Dose")
#' 
#' adtte %>%
#'   visR::estimate_KM(strata = "TRTP") %>%
#'   visR::visr() %>%
#'   visR::add_CI(alpha = 0.4) %>%
#'   visR::add_highlight(strata = strata, bg_alpha_multiplier = 0.2)
#'
#' @return The input `ggsurvfit` object with adjusted `alpha` values
#'
#' @rdname add_highlight
#'
#' @export

add_highlight <- function(gg, ...){
  UseMethod("add_highlight", gg)
}

#' @param gg A ggsurvfit, ideatlly created with visR
#' @param strata Name of the strata to be highlighted as shown in the legend.
#' @param bg_alpha_multiplier Factor with which the `alpha` values of all but the specified strata will be multiplied.
#'
#' @rdname add_highlight
#' @method add_highlight ggsurvfit
#' @export

add_highlight.ggsurvfit <- function(gg = NULL, 
                                    strata = NULL, 
                                    bg_alpha_multiplier = 0.2,
                                    ...) {
  
  # Ugly hack to suppress CRAN warning as described here:
  # https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/#option-two
  alpha <- colour <- fill <- group <- NULL
  
  if (!("ggplot" %in% class(gg))) {
    
    stop("A 'ggplot' has to be specified for 'gg'.")
    
  } 
  
  if (missing(strata) | length(strata) == 0) {
    
    stop("Please specify one or more 'strata' to highlight.")
    
  } 
  
  if (length(strata) == 1) {
    
    if (class(strata) == "list") {
      
      if (class(strata[[1]]) != "character") {
        
        stop("A 'strata' must be either a single character string or a list of them.")
        
      }
      
    } else if (!(class(strata) == "character")) {
      
      stop("A 'strata' must be either a single character string or a list of them.")
      
    }
    
  } else if (length(strata) > 1) {
    
    if (is.list(strata)) {
      
      strata <- unlist(strata)
      
    }
    
    base::sapply(strata, function(s) {
      
      if (class(s) != "character") {
        
        stop("When 'strata' is a list, all elements must be character strings.")
        
      }
      
    })
    
  }
  
  if (!is.numeric(bg_alpha_multiplier)) {
    
    stop("The `bg_alpha_multiplier` must be a `numeric`.")
    
  }
  
  if (bg_alpha_multiplier > 1 | bg_alpha_multiplier < 0) {
    
    stop("The `bg_alpha_multiplier` must be a numeric value between 0 and 1.")
    
  }
  
  # Extract names of strata objects
  gg_gb <- ggplot2::ggplot_build(gg)
  gg_gtable <- ggplot2::ggplot_gtable(gg_gb)
  gg_guidebox_id <- base::which(base::sapply(gg_gtable$grobs, 
                                             function(x) x$name) == "guide-box")
  gg_table_grob <- gg_gtable$grobs[[gg_guidebox_id]]$grobs[[1]]
  
  # Get IDs of elements containing strata labels
  strata_label_ids <- base::grep("label", gg_table_grob$layout$name)
  
  extract_strata_name_by_id <- function(gg_table_grob, id) {
    
    label <- gg_table_grob$grobs[[id]]$children[[1]]$children[[1]]$label
    
    return(label)
    
  }
  
  strata_labels <- base::sapply(strata_label_ids, 
                                extract_strata_name_by_id,
                                gg_table_grob = gg_table_grob)
  
  base::sapply(c(strata), function(s) {
      
      if (!(s %in% strata_labels)) {
        
        msg <- "The strata you specified has not been found in the provided plot.\n"
        msg <- base::paste0(msg, 
                            "  Available strata: ", 
                            base::paste(strata_labels, collapse = ", "), 
                            "\n")
        msg <- base::paste0(msg, "  Please adjust and rerun.")
        
        stop(msg)
        
      }
    })
    
  # Which group(s) in the ggplot data object corresponds to the bg strata?
  bg_strata_ids <- unique(gg_gb$data[[1]]$group)[!(strata_labels %in% strata)]
  
  # Replace the previous hex alpha values with the new one
  for (i in 1:base::length(gg_gb$data)) {
    
    if ("ymin" %in% base::colnames(gg_gb$data[[i]])) {
      
      # Check whether colour contains an alpha value
      if (base::nchar(gg_gb$data[[i]]$fill[[1]])) {
        
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
        dplyr::mutate(fill = replace_hex_alpha(fill, convert_alpha(numeric_alpha = alpha))) %>%
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
        dplyr::mutate(colour = paste0(colour, convert_alpha(numeric_alpha = alpha))) %>%
        as.data.frame()
      
      strata_colours <- unique(gg_gb$data[[i]]$colour)

      suppressMessages(gg <- gg + ggplot2::scale_color_manual(values = strata_colours))
      
    }
  }
  
  return(gg)
  
}