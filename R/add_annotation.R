#' @title Add annotations to a visR object
#'
#' @description Wrapper around ggplot2::annotation_custom for simplified annotation to ggplot2 plots.
#' This function accepts a string, dataframe, data.table, tibble or customized 
#' objects of class \code{gtable} and places them on the specified location on 
#' the \code{ggplot}. The layout is fixed: bold columnheaders and plain body. 
#' Only the font size and type can be chosen. 
#'
#' @seealso \code{\link[gridExtra]{tableGrob}} \code{\link[ggplot2]{annotation_custom}}
#'
#' @param gg Object of class \code{ggplot}.
#' @param label \code{String, dataframe, data.table, tibble} used to annotate the \code{ggplot}.
#' @param base_size \code{numeric}. Base font size in pt
#' @param base_family \code{character}. Base font family
#' @param xmin x coordinates giving horizontal location of raster in which to fit annotation.
#' @param xmax x coordinates giving horizontal location of raster in which to fit annotation.
#' @param ymin y coordinates giving vertical location of raster in which to fit annotation.
#' @param ymax y coordinates giving vertical location of raster in which to fit annotation.
#'
#' @examples
#' 
#' library(visR)
#' 
#' ## Estimate survival
#' surv_object <- visR::estimate_KM(data = adtte, strata = "TRTP")
#' 
#' ## We want to annotate the survival KM plot with a simple string comment
#' visR::plot(surv_object) %>%
#'   visR::add_annotation(
#'     label = "My simple comment",
#'     base_family = "sans",
#'     base_size = 15,
#'     xmin = 110,
#'     xmax = 180,
#'     ymin = 0.80
#'   )
#' 
#' ## Currently, care needs to be taken on the x-y values relative
#' ## to the plot data area. Here we are plotting outside of the data area. 
#' visR::plot(surv_object) %>%
#'   visR::add_annotation(
#'   label = "My simple comment",
#'   base_family = "sans",
#'   base_size = 15,
#'   xmin = 210,
#'   xmax = 380,
#'   ymin = 1.0
#'   )
#' 
#' 
#' ## We may also want to annotate a KM plot with information
#' ## from additional tests or estimates. This example we annotate
#' ## with p-values contained in a tibble 
#' 
#' ## we calculate p-values for "Equality across strata"
#' lbl <- visR::get_pvalue(surv_object,
#'              statlist = c("test", "p"),
#'              type = "All")
#' 
#' ## display p-values
#' lbl
#' 
#' ## Now annotate survival KM plot with the p-values
#' visR::plot(surv_object) %>%
#'   visR::add_annotation(
#'     label = lbl,
#'     base_family = "sans",
#'     base_size = 9,
#'     xmin = 100,
#'     xmax = 180,
#'     ymin = 0.80
#'   )
#'
#' @return Object of class \code{ggplot} with added annotation with an object of class \code{gtable}.
#'
#' @export

add_annotation <- function(
  gg = NULL,
  label = NULL,
  base_family = "sans",
  base_size = 11,
  xmin = -Inf,
  xmax = Inf,
  ymin = -Inf,
  ymax = Inf
  ){

# User input validation ---------------------------------------------------

  if (!base::inherits(gg, "ggplot")) stop("Error in add_annotation: gg is not of class `ggplot`")
  if (is.null(label)) stop("Error in add_annotation: label does not exist")
  if (!base_family %in% c("sans", "serif", "mono")) stop("Error in add_annotation: Specified font not supported")
  if (!base::any(unlist(lapply(as.list(c(xmin, xmax, ymin, ymax, base_size)), is.numeric)))) stop("Error in add_annotation: One of the coordinates are not numeric.")

  #### If user has custom gtable, skip all the remaining steps ####

# ggtable -----------------------------------------------------------------

  if (base::inherits(label, "gtable")) {

    gg <- gg +
      ggplot2::annotation_custom(label, xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)
    
    
    # Add individual components -----------------------------------------------
    
    components <- append(list(gg), label)
    names(components) = c("visR_plot", "annotation")
    gg[["components"]] <- components
    
    return (gg)

  } else {

    ### Prepare label: turn into dataframe and avoid factors + add manual bolding to avoid parsing issues with `` in colnames

    df <- data.frame(label, stringsAsFactors = FALSE, check.names = FALSE)
    colnames(df) <- as.vector(paste(paste0("bold(\"", colnames(df), "\")")))

    ### Layout of gtable: access and modify options eg tt1$colhead

    ## Use hjust and x to left justify the text
    core_alignment <- matrix(c(0, 1), ncol=max(ncol(df), 2), nrow=nrow(df), byrow=TRUE)[, 1:(1+as.numeric(ncol(df)>1)), drop = FALSE]
    core_x <- core_alignment

    ## TODO: Commented these two lines for RMD CHECK.
    #head_alignment <- matrix(c(0, 1), ncol=max(length(colnames(df)), 2), nrow=1, byrow=TRUE)[, 1:(1+as.numeric(ncol(df)>1)), drop = FALSE]
    #head_x <- head_alignment

    tt1 <-  gridExtra::ttheme_minimal(
      base_size = base_size,
      base_family = base_family,
      parse = TRUE,
      core=list(
        fg_params=list(
          hjust = as.vector(core_alignment),
          x = as.vector(core_x),
          fontface = "plain"
        )
      ),
      colhead = list(
        fg_params = list(
          hjust =  as.vector(core_alignment[1, ]),
          x = as.vector(core_x[1, ]),
          fontface = 2
        )
      )
    )

    
    if (inherits(label, "character")) {
      dfGrob <- gridExtra::tableGrob(df, rows = NULL, theme = tt1, cols = NULL)
    } else {
      dfGrob <- gridExtra::tableGrob(df, rows = NULL, theme = tt1)
    }

    gg <- gg +
      ggplot2::annotation_custom(dfGrob, xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)
    
    # Add individual components -----------------------------------------------
    
    components <- append(list(gg), list(dfGrob$grob))
    names(components) = c("visR_plot", "annotation")
    gg[["components"]] <- components
    
    
    return(gg)
  }
}
