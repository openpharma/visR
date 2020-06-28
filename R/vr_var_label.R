#' Sets the label attribute on a data frame
#'
#' A helper function to apply a set of attribute labels to variables
#' in a data frame. This is useful if you will be passing your data frame
#' to vr_create_tableone because the labels will be used instead of the
#' variable names.
#' @param dat Data frame
#' @param labels A named list, with the name being the variable and the
#' value being the label to apply.
#' @return Returns the data frame after applying the labels
#' @export
#' @examples
#' mtcars_labeled <- vr_var_label(
#'     mtcars,
#'     labels = list(
#'         "mpg" = "Miles Per Gallon",
#'         "cyl" = "Number Of Cylinders"
#'     )
#' )

vr_var_label <- function(dat, labels) {
    
    for(i in seq_along(labels)) {
        attr(dat[[names(labels[i])]], "label") <- labels[[i]]
    }
    
    return(dat)
}
