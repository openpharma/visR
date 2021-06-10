#' Internal function to format pvalues.
#' @param x Input p-value. Numeric value. 
#' @noRd
.pvalformat <- function(x) {
  
  old <- options()
  on.exit(options(old)) 
  options(scipen = 999)
  
  if (x < 0.001)
    "<0.001"
  else if (x > 0.999)
    ">0.999"
  else
    format(round(x, 3),
           digits = 3,
           justify = "right",
           width = 6)
}
