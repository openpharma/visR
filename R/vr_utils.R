#' @export
vr_summarize <- function(x) UseMethod("vr_summarize")

#' Create variable summary for factors
#'
#' @param x 
#'
#' @return
#' @export
vr_summarize.factor <- function(x){
  x1 <- forcats::fct_explicit_na(x, na_level = "Missing")
  
  dat <- tibble::enframe(x1) %>%
    dplyr::group_by(value) %>%
    dplyr::summarise(N = n()) %>%
    dplyr::mutate(`%` = round(100 * N/sum(N), 3)) %>%
    tidyr::pivot_wider(names_from = value, values_from = c("N", "%"), names_sep=" ") %>%
    as.list()
  list(dat)
}

#' Create variable summary for numeric variables
#'
#' @param x 
#'
#' @return 
#' @export
vr_summarize.numeric <- function(x){
  dat <- list(
    mean = mean(x, na.rm = T),
    min = min(x, na.rm = T),
    Q1 = quantile(x, probs=0.25),
    median = median(x, na.rm = T) ,
    Q3 = quantile(x, probs=0.75),
    max = max(x, na.rm = T),
    sd = sd(x, na.rm = T)
  )
  list(dat)
}

#' Create variable summary for all other variable types
#'
#' @param x 
#'
#' @return
#' @export
vr_summarize.default <- function(x){
  dat <- list(
    unique_values = length(unique(x)),
    nmiss = sum(is.na(x))
  )
  list(dat)
}

#' @export
vr_summarize_tab1 <- function(x) UseMethod("vr_summarize_tab1")

#' Create variable summary for factors
#'
#' @param x 
#'
#' @return
#' @export
vr_summarize_tab1.factor <- function(x){
  x1 <- forcats::fct_explicit_na(x, na_level = "Missing")
  
  dat <- tibble::enframe(x1) %>%
    dplyr::group_by(value) %>%
    dplyr::summarise(N = n()) %>%
    dplyr::mutate(`n (%)` = paste0(N, " (", format(100 * N/sum(N), digits = 3, trim=TRUE), "%)")) %>%
    dplyr::select(-N) %>% 
    tidyr::pivot_wider(names_from = value, values_from = c("n (%)"), names_sep=" ") %>%
    as.list()
  list(dat)
}

#' Create variable summary for numeric variables
#'
#' @param x 
#'
#' @return 
#' @export
vr_summarize_tab1.numeric <- function(x){
  dat <- list(
    `Mean (SD)` = paste0(format(mean(x, na.rm = T), digits = 3), " (", format(sd(x, na.rm = T), digits = 3), ")"),
    `Median (IQR)` = paste0(format(median(x, na.rm = T)), " (", format(quantile(x, probs=0.25)), 
                            "-", format(quantile(x, probs=0.75)), ")"),
    `Min-max` = paste0(format(min(x, na.rm = T)), "-", format(max(x, na.rm = T))),
    Missing = paste0(format(sum(is.na(x))), " (", format(100 * sum(is.na(x))/n(), trim=TRUE), "%)")
  )
  list(dat)
}

#' Create variable summary for all other variable types
#'
#' @param x 
#'
#' @return
#' @export
vr_summarize_tab1.default <- function(x){
  dat <- list(
    `Unique values` = format(length(unique(x))),
    `Missing (%)` = paste0(format(sum(is.na(x))), " (", format(100 * sum(is.na(x))/n(), trim=TRUE), "%)")
  )
  list(dat)
}

#' Create a caption for any table
#'
#' @param x 
#'
#' @return 
#' @export
vr_table_caption <-function(caption,
         label = knitr::opts_current$get("label"),
         width = knitr::opts_current$get("out.width")) {
  asis_output(paste(
    '<table>',
    glue::glue(
      "<caption style=\"width:{width}px\">(#tab:{label}){caption}</caption>"
    ),
    '</table>',
    sep = ""
  ))
}

#' Create a caption for any table
#'
#' @param x 
#'
#' @return 
#' @export
vr_table_download <- function(df, filename=NULL, format="csv", button="Download data"){
  
  if(!format %in% c("txt", "tsv", "csv", "xlsx")){
    stop("Output file format has to be either txt, tsv, csv or xlsx.")
  }
  
  if(is.null(knitr::opts_knit$get("fig.path"))){
    fp <- tempdir()
  }
  else{
    fp <- knitr::opts_knit$get("fig.path")
  }
  
  if(is.null(filename)){
    filename <- tempfile(pattern = paste("data-"), tmpdir = getwd(), fileext = paste0(".", format))
  }
  
  if(format %in% c("csv", "tsv", "txt")){
    delim <- switch(format, "csv" = ",", "tsv" = "\t", "txt" = " ")
    write.table(df, file = filename, sep = delim)
  }
  else{
    xlsx::write.xlsx(df, file=filename)
  }
  
  # String result ready to be placed in rmarkdown
  asis_output(paste0('<a class="btn btn-info" role="button" download="', filename, '" href="', filename, '">', button, '</a>'))
  # paste0('<button type="submit" onclick="window.open("', filename, '")">', button, '</button>')
}
