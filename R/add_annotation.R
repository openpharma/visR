#' @title Add annotations to a visR object
#'
#' @description Wrapper around ggplot2::annotation_custom for simplified annotation to ggplot2 plots.
#' This function accepts a string, dataframe, data.table, tibble or customized
#' objects of class \code{gtable} and places them on the specified location on
#' the \code{ggplot}. The layout is fixed: bold column headers and plain body.
#' Only the font size and type can be chosen.
#' Both the initial plot as the individual annotation are stored as attribute `component`
#' in the final object.
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
#' ## Estimate survival
#' surv_object <- visR::estimate_KM(data = adtte, strata = "TRTP")
#'
#' ## We want to annotate the survival KM plot with a simple string comment
#' visR::visr(surv_object) %>%
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
#' visR::visr(surv_object) %>%
#'   visR::add_annotation(
#'     label = "My simple comment",
#'     base_family = "sans",
#'     base_size = 15,
#'     xmin = 210,
#'     xmax = 380,
#'     ymin = 1.0
#'   )
#'
#'
#' ## We may also want to annotate a KM plot with information
#' ## from additional tests or estimates. This example we annotate
#' ## with p-values contained in a tibble
#'
#' ## we calculate p-values for "Equality across strata"
#' lbl <- visR::get_pvalue(surv_object,
#'   statlist = c("test", "pvalue"),
#'   type = "All"
#' )
#'
#' ## display p-values
#' lbl
#'
#' ## Now annotate survival KM plot with the p-values
#' visR::visr(surv_object) %>%
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

add_annotation <- function(gg = NULL,
                           label = NULL,
                           base_family = "sans",
                           base_size = 11,
                           xmin = -Inf,
                           xmax = Inf,
                           ymin = -Inf,
                           ymax = Inf) {

  # User input validation ---------------------------------------------------

  if (!base::inherits(gg, "ggplot")) stop("Error in add_annotation: gg is not of class `ggplot`")
  if (is.null(label)) stop("Error in add_annotation: label does not exist")
  if (!base_family %in% c("sans", "serif", "mono")) stop("Error in add_annotation: Specified font not supported")
  if (!base::any(unlist(lapply(as.list(c(xmin, xmax, ymin, ymax, base_size)), is.numeric)))) stop("Error in add_annotation: One of the coordinates are not numeric.")

  # ggtable -----------------------------------------------------------------

  if (base::inherits(label, "gtable")) {
    gganno <- gg +
      ggplot2::annotation_custom(label, xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)


    ### Add individual components
    components <- append(list(gg), label) # Note: The append changes the structure of the gtable object
    names(components) <- c("visR_plot", names(label))
    gganno[["components"]] <- components

    return(gganno)
  } else {

    ### Prepare label: turn into dataframe and avoid factors + add manual bolding to avoid parsing issues with `` in colnames

    df <- data.frame(lapply(label, as.character), stringsAsFactors = FALSE, check.names = FALSE) # as.character to protect leading zero in numeric

    colnames(df) <- as.vector(paste(paste0("bold(\"", colnames(df), "\")")))

    ### Layout of gtable: access and modify options eg tt1$colhead
    ## First column to left, rest rightaligned

    core_alignment_matrix <- matrix(rep(0, nrow(df) * dim(df)[2]), nrow = nrow(df), ncol = dim(df)[2])
    if (dim(core_alignment_matrix)[2] > 1) {
      core_alignment_matrix[, 2:dim(core_alignment_matrix)[2]] <- 1
    }
    core_alignment_head <- rep(1, dim(core_alignment_matrix)[2])
    core_alignment_head[1] <- 0

    tt1 <- gridExtra::ttheme_minimal(
      base_size = base_size,
      base_family = base_family,
      core = list(
        fg_params = list(
          hjust = as.vector(core_alignment_matrix),
          x = as.vector(core_alignment_matrix),
          fontface = "plain"
        )
      ),
      colhead = list(
        fg_params = list(
          hjust = core_alignment_head,
          x = core_alignment_head,
          fontface = 2,
          parse = TRUE
        )
      )
    )


    if (inherits(label, "character")) {
      dfGrob <- gridExtra::tableGrob(df, rows = NULL, theme = tt1, cols = NULL)
    } else {
      dfGrob <- gridExtra::tableGrob(df, rows = NULL, theme = tt1)
    }

    gganno <- gg +
      ggplot2::annotation_custom(dfGrob, xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)

    ### Add individual components

    components <- append(list(gg), dfGrob) # Note: The append changes the structure of the tableGrob object
    names(components) <- c("visR_plot", names(dfGrob))
    gganno[["components"]] <- components

    return(gganno)
  }
}
