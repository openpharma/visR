#' vr_alluvial_wrangling
#'
#' Create tables with alluvial data.
#'
#' @param data a dataframe
#' @param id patient id variable name, Defoult: 'PatientID'
#' @param linename lines of therapy variable name, Defoult: 'LineName'
#' @param linenumber lines of therapy number variable name, Defoult: 'LineNumber' 
#' @param n_common, Number of most common lines of therapy presented in alluvial plot, Default: 2 
#' 
#' @return list
#'
#' @examples
#' 
#' dataset <- NULL
#'
#' for(PatientID in 1:5000){
#'   
#'   max_line <- sample(0:5, 1, prob = c(0.2, 0.4, 0.2, 0.15, 0.04, 0.01))
#'   
#'   if(max_line == 1){ 
#'     
#'     min_line <- sample(c(0, 1), 1, prob = c(0.3, 0.7)) 
#'     
#'   } else{ min_line <- 0 }
#'   
#'   for(LineNumber in min_line:max_line){
#'     
#'     LineName <- sample(c('Treatment_A', 'Treatment_B', 'Treatment_C'), 1, prob = c(0.5, 0.3, 0.2))
#'     
#'     patient_data_line <- data.frame(PatientID = PatientID,
#'                                     LineName = LineName,
#'                                     LineNumber = LineNumber)
#'     
#'     dataset <- rbind(patient_data_line , dataset)
#'     
#'   }
#'   
#' }
#'
#' result <- dataset %>% vr_alluvial_wrangling()
#' result$alluvial_plot_data
#' result$linenames_summary
#' 
#' @export

vr_alluvial_wrangling <- function(
  data,
  id = "PatientID",
  linename = "LineName",
  linenumber = "LineNumber",
  drop_maintenance_therapy = TRUE,
  linenumber_upper_limit = 4,
  n_common = 2
) {
  
  if("IsMaintenanceTherapy" %in% names(data) & drop_maintenance_therapy){
    
    data <- data %>% 
      dplyr::filter(IsMaintenanceTherapy == FALSE)
    
  }
  
  data <- data %>%
    dplyr::rename(
      "id" = id,
      "linename" = linename,
      "linenumber" = linenumber
    ) %>%
    dplyr::select(id, linename, linenumber)
  
  data <- data %>% 
    dplyr::filter(dplyr::between(linenumber, 0, linenumber_upper_limit))
  
  data <- data %>% 
    dplyr::group_by(id, linenumber) %>% 
    dplyr::slice(1)
  
  data_with_no_tx <- vr_add_no_tx(data)
  
  mostcommon_linenames <- vr_mostcommon_linenames(data, n_common)
  
  linenames_summary_long <- 
    data_with_no_tx %>%
    dplyr::group_by(linenumber, linename) %>%
    dplyr:: summarise(patient_count = n()) %>%
    dplyr::arrange(linenumber, desc(patient_count)) %>%
    dplyr::mutate(freq = patient_count / sum(patient_count))
  
  alluvial_plot_data <- vr_cut_linenames(data_with_no_tx, mostcommon_linenames) %>%
    dplyr::arrange(id, linenumber)
  
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
    
    temp$linename <- dplyr::if_else(
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

vr_mostcommon_linenames <- function(data, n_common){
  
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
  
  data <- data %>% 
    dplyr::left_join(mostcommon_linenames, 
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
    ) %>% dplyr::select(-n)
  
  return(data)
  
}
