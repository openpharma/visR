#' @title Highlight a specific strata
#'
#' @description S3 method for adding a pointwise confidence interval to a object created with visR.
#'     No default method is available at the moment.
#'
#' @author Tim Treis
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
#' @rdname add_highlight
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
                                    bg_alpha_multiplier = 0.5,
                                    ...){
  
  if (missing(strata)) {
    
    warning("A strata to highlight has to be specified.")
    return(NULL)
  
  }
  
  get_alpha_from_hex_colour <- function(hex_colour) {
    
    if (missing(hex_colour) | !is.character(hex_colour)) {
      
      stop("Please provide a colour in hex representation as a string for `hex_colour`.")
      
    }
    
    if (!nchar(hex_colour) == 9) {
      
      stop("Please provide a hex colour in the format #RRGGBBAA.")
      
    } else {
      
      colour_parts <- strsplit(hex_colour, "")[[1]]
      
      if (colour_parts[1] != "#") {
        
        stop("Please provide a hex colour in the format #RRGGBBAA.")
        
      } else {
        
        alpha <- col2rgb(hex_colour, alpha = TRUE)["alpha",][[1]]
        alpha <- round(alpha/255, 2) 
        
        return(alpha)
        
      }
    }
  }
  
  numeric_alpha_to_hex <- function(numeric_alpha = NULL) {
    
    # Two separate ifs so that is.na(NULL) doesn't cause an error
    if (is.null(numeric_alpha)) { return("00") }
    if (is.na(numeric_alpha)) { return("00") }
  
    if (is.numeric(numeric_alpha)) {
      
      if (numeric_alpha > 1 | numeric_alpha < 0) {
        
        stop("Please enter a numeric value between 0 and 1.")
        
      } else {
        
        alpha_decimal = base::round((numeric_alpha * 100) * (255/100))
        alpha_hex = base::format(base::as.hexmode(alpha_decimal), 
                                 width = 2,
                                 upper.case = TRUE)
        
        return(alpha_hex)
        
      }
      
    } else {
      
      stop("Please enter a numeric value between 0 and 1.")
      
    }

  }
  
  replace_hex_alpha <- function(colour, new_alpha) {
    
    if (missing(colour) | missing(new_alpha)) {
      
      stop("Please provide a `colour` and a `new_alpha` in hex representation as strings.")
      
    }
    
    if (!(is.character(new_alpha) & nchar(new_alpha) == 2)) {
      
      stop("Please provide a two-character string for the hex representation of the new alpha.")
      
    }
    
    if (!(is.character(colour))) {
      
      stop("Please provide a hex colour as a string.")
      
    } else {
      
      if (!nchar(colour) == 9) {
        
        stop("Please provide a hex colour in the format #RRGGBBAA.")
        
      } else {
        
        colour_parts <- strsplit(colour, "")[[1]]
        
        if (colour_parts[1] != "#") {
          
          stop("Please provide a hex colour in the format #RRGGBBAA.")
          
        } else {
          
          colour_red <- paste0(colour_parts[2:3], collapse = "")
          colour_green <- paste0(colour_parts[4:5], collapse = "")
          colour_blue <- paste0(colour_parts[6:7], collapse = "")
          new_alpha <- base::toupper(new_alpha)
          
          new_colour <- paste0("#", colour_red, colour_green, colour_blue, new_alpha)
          
          return(new_colour)
          
        }
      }
    }
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
