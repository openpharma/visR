#' vr_alluvial_wrangling
#'
#' Create tables with alluvial data.
#'
#' @param data
#' @param id 
#' @param linename 
#' @param linenumber 
#' @param n_common 
#' 
#' @return list
#'
#' @examples
#' result <- data %>% vr_alluvial_wrangling()
#' result$alluvial_plot_data
#' result$linenames_summary
#' 
#' @export

vr_alluvial_wrangling <- function(
  data,
  id = "PatientID",
  linename = "LineName",
  linenumber = "LineNumber",
  n_common = 2
) {
  
  data <- data %>%
    dplyr::select(PatientID,LineName,LineNumber) %>%
    dplyr::rename(
      "id" = id,
      "linename" = linename,
      "linenumber" = linenumber
    )
  
  data_with_no_tx <- vr_add_no_tx(data)
  
  mostcommon_linenames <- vr_mostcommon_linenames(data)
  
  linenames_summary_long <- 
    data_with_no_tx %>%
    dplyr::group_by(linenumber, linename) %>%
    dplyr:: summarise(patient_count = n()) %>%
    dplyr::arrange(linenumber, desc(patient_count)) %>%
    dplyr::mutate(freq = patient_count / sum(patient_count))
  
  alluvial_plot_data <- vr_cut_linenames(data_with_no_tx, mostcommon_linenames) %>%
    arrange(id, linenumber)
  
  linenames_summary <- 
    alluvial_plot_data %>%
    dplyr::group_by(linenumber, linename) %>%
    dplyr:: summarise(patient_count = n()) %>%
    dplyr::arrange(linenumber, desc(patient_count)) %>%
    dplyr::mutate(freq = patient_count / sum(patient_count))
  
  result <- list(
    "alluvial_plot_data" = alluvial_plot_data,
    "linenames_summary" = linenames_summary, 
    "linenames_summary_long" = linenames_summary_long
  )
  
  return(result)
  
}


vr_add_no_tx <- function(data){
  
  minlines <- min(data$linenumber, na.rm = T)
  maxlines <- max(data$linenumber, na.rm = T)
  data_with_no_tx <- NULL
  
  for (i in minlines:maxlines) {
    
    temp <- data %>%
      dplyr::filter(linenumber == i)
    
    temp <- dplyr::left_join(
      unique(data["id"]),
      temp,
      by = "id")
    
    temp$linename <- if_else(
      !is.na(temp$linename),
      paste0(temp$linename, " ", i, "L"),
      paste0("No Tx ", i, "L")
    )
    
    temp$linenumber <- i
    
    data_with_no_tx <- rbind(
      temp,
      data_with_no_tx
    )
  }
  
  return(data_with_no_tx)
  
}

vr_mostcommon_linenames <- function(data){
  
  mostcommon <- data %>%
    dplyr::group_by(linename, linenumber) %>%
    dplyr::summarise(
      n = n()
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(linenumber) %>%
    dplyr::top_n(n_common, n) %>%
    dplyr::arrange(linenumber, desc(n))
  
  return(mostcommon)
  
}

vr_cut_linenames <- function(data, mostcommon_linenames){
  
  mostcommon_linenames <- mostcommon_linenames %>%
    dplyr::mutate(linename = paste0(linename , " ", linenumber, "L"))
  
  data2 <- data %>% 
    left_join(mostcommon_linenames, 
              by = c("linename" = "linename",
                     "linenumber" = "linenumber")
    ) %>%
    dplyr::mutate(
      linename = dplyr::if_else(
        !is.na(n) | stringr::str_detect(linename, "No Tx"),
        linename,
        "Other Tx",
        missing = "No Tx"
      )
    ) %>% select(-n)
  
  return(output_data)
  
}
