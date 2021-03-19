#' @title Add censoring symbols to a visR ggplot through an S3 method
#'
#' @description S3 method for adding censoring symbols to visR ggplot.
#'     No default method is available at the moment.
#'
#' @author Steven Haesendonckx
#'
#' @param gg visR object
#' @param ... other arguments passed on to the method to modify \code{\link[ggplot2]{geom_point}}
#'
#' @examples
#' 
#' library(visR)
#'
#' # Estimate KM curves by treatment group 
#' survfit_object <- survival::survfit(data = adtte, Surv(AVAL, 1-CNSR) ~ TRTP)
#' 
#' ## plot without confidence intervals
#' p <- visR::plot(survfit_object) 
#' p
#'
#' # add censoring to plot
#' p %>% visR::add_CNSR()
#' 
#' # change censor symbol shape
#' p %>% visR::add_CNSR(shape = 1)
#' 
#' # change size and shape
#' p %>% visR::add_CNSR(size = 4, shape = 2)
#' 
#' @return Censoring symbols overlayed on a visR ggplot
#'
#' @rdname add_CNSR
#'
#' @export

add_CNSR <- function(gg, ...){
  UseMethod("add_CNSR")
}

#' @param gg A ggplot created with visR
#' @param shape aesthetic of ggplot2 \code{\link[ggplot2]{geom_point}}. Default is 3.
#' @param size aesthetic of ggplot2 \code{\link[ggplot2]{geom_point}}. Default is 2.
#'
#' @rdname add_CNSR
#' @method add_CNSR ggsurvfit
#' @export

add_CNSR.ggsurvfit <- function(gg, shape = 3, size = 2, ...){
  
  # <= also catches NULL
  if (length(size) <= 1)  {
    
    if (!is.numeric(size)) {
      
      warning("Invalid `size` specified. Setting it to 2.")
      size <- 2
      
    }
    
  } else { 
  
    # We let ggplot deal with this.
    # https://github.com/openpharma/visR/wiki/Don't-do-this
    
  }
  
  # <= also catches NULL
  if (length(shape) <= 1) {
    
    if ((!is.numeric(shape)) && (!is.character(shape))) {
      
      warning("Invalid `shape` specified. Setting it to 3.")
      shape <- 3
      
    }
    
  } else { 
    
    # We let ggplot deal with this.
    # https://github.com/openpharma/visR/wiki/Don't-do-this
    
  }
  
  gg <- gg +
    ggplot2::geom_point(data = base::subset(gg$data, n.censor >= 1), 
                        ggplot2::aes(x = time, 
                                     y = est, 
                                     color = strata),
                        shape = shape, 
                        size = size)

  return(gg)
}
