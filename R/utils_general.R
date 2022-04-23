#' Internal function to format pvalues.
#' @param x Input p-value. Numeric value. 
#' @noRd
.pvalformat <- function(x) {
  
  if (x < 0.001)
    "<0.001"
  else if (x > 0.999)
    ">0.999"
  else
    format(round(x, 3),
           nsmall = 3,
           justify = "right",
           width = 6,
           scientific = FALSE)
}

#' @title Update documentation
#' 
#' @description This functions generates the documentation for a package if both /man and /documentation folders exist within the project:
#'   \itemize{
#'     \item{devtools::document(pkg=pkg) is used to generate Rd files}
#'     \item{Rd files are rendered using ?'anyfunction' eg ?visR}
#'     \item{Documentation is collected within 'pkg'.pdf. This pdf is written to the documentation folder}
#'     }
#'
#' @usage update_documentation(pkg = "~/visR")
#' 
#' @param pkg Path to package.
#'
#' @return .Rd files inside man/ folder and an updated pdf within the documentation folder.
#' @noRd
#' 
#' @examples
#' \dontrun{
#'   update_documentation(pkg = "~/visR")
#'  }

update_documentation <- function(pkg = "~/visR", fileNm="visR")
  {

    ## Create Rd files document ####
    devtools::document(pkg = pkg)
  
    ## Look for exported functions
    tmp <- data.frame(lines = trimws(suppressWarnings(readLines(paste0(pkg, "/NAMESPACE"))), which=("both")), stringsAsFactors = F)
    tmp <- data.frame(lines = tmp$lines[which(grepl("export", tmp$lines) == "TRUE")], stringsAsFactors = F)
    tmp <- data.frame(lines = gsub("export(", "", tmp$lines, fixed = TRUE))
    fcts <- as.vector(gsub(")", "", tmp$lines, fixed = TRUE))
    
    ## Render Rd files
    for (j in 1:length(fcts)){
      eval(parse(text=paste0("?",fcts[j])))
      writeLines(paste0("?",fcts[j]))
    }
    
   ## create pdf
   system(paste0("R CMD Rd2pdf  ", pkg, "/man --force --no-preview --output='", pkg, "/documentation/", fileNm, ".pdf'"))
}

