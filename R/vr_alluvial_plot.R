#' vr_alluvial_plot
#'
#' Create alluvial plot.
#'
#' @param data a dataframe containg 
#' @param id patient id variable name, Default: 'PatientID'
#' @param linename lines of therapy variable name, Default: 'LineName'
#' @param linenumber lines of therapy number variable name, Default: 'LineNumber' 
#' @param n_common Number of most common lines of therapy presented in alluvial plot, Default: 2 
#' @param title plot title, Default: NULL
#' @param interactive, interactive plot, Default: FALSE
#' @param N_unit, Default: 'patients' 
#' @param data_source data source name, Default: 'Flatiron'
#' @param fill_by one_of(c('first_variable', 'last_variable', 'all_flows', 'values')), Default: 'first_variable'
#' @param col_vector_flow HEX color values for flows, Default: easyalluvial::palette_filter( greys = F)
#' @param col_vector_value HEX color values  for y levels/values, Default:RColorBrewer::brewer.pal(9, 'Blues')[c(3,6,4,7,5)]
#' @param linenames_labels_size, Default: 2.5
#' 
#' @return list
#'
#' @examples
#' 
# cohort <-  dplyr::tibble(
#   # create 500 repeated patient ids
#   PatientID = base::sample(x = 1:500, size=5000, replace = T),
#   # with 'a', 'b', 'c', 'd' as line of therapy name
#   LineName = base::sample(c('a', 'b', 'c', 'd'), 5000, replace = T),
#   # linenumver between 0 and 4
#   LineNumber = base::sample(x = 0:4, size=5000, replace=T)
# )
#'
#' vr_alluvial_plot(cohort)
#' 
#' vr_alluvial_plot(data, interactive = TRUE)
#' 
#' vr_alluvial_plot(data, fill_by = "all_flows")
#' 
#' vr_alluvial_plot(data, interactive = TRUE, fill_by = "all_flows")
#' 
#' vr_alluvial_plot(data, col_vector_value =  RColorBrewer::brewer.pal(9, 'Greys')[c(3,6,4,7,5)])
#'
#' @export


vr_alluvial_plot <- function(
  data,
  id = "PatientID",
  linename = "LineName",
  linenumber = "LineNumber",
  n_common = 2,
  title = NULL,
  interactive = FALSE,
  N_unit = "patients",
  data_source = "Flatiron",
  fill_by = "first_variable",
  col_vector_flow = easyalluvial::palette_qualitative() %>% easyalluvial::palette_filter( greys = F),
  col_vector_value =  RColorBrewer::brewer.pal(9, 'Blues')[c(3,6,4,7,5)],
  linenames_labels_size = 2.5
) {
  
  if(!is.data.frame(data)) {
    
    stop('Argument "data" should be a data.frame object', call. = FALSE)
    
  } 
  
  alluvial_data <- vr_alluvial_wrangling(data = data,
                                         linename = linename,
                                         linenumber = linenumber,
                                         n_common = n_common)$alluvial_plot_data
  
  N = length(unique(alluvial_data$id))
  
  data_source_caption <- sprintf("Data Source: %s", data_source)
  
  p = easyalluvial::alluvial_long( dplyr::select(alluvial_data, linenumber, linename, id),
                                   key = linenumber,
                                   value = linename,
                                   id = id,
                                   fill_by = fill_by,
                                   col_vector_flow = col_vector_flow,
                                   col_vector_value = col_vector_value,
                                   stratum_label_size = linenames_labels_size)
  
  if(interactive) {
    
    plot <- parcats::parcats(p, 
                             marginal_histograms = FALSE,
                             hoverinfo = "count",
                             hoveron = "category",
                             data_input = dplyr::select(alluvial_data, linenumber, linename, id))
    
    
    return(plot)
    
  } else {
    
    plot <- p + 
      ylab("Patients count") +
      xlab("Line of therapy") +
      labs(
        title = title, 
        subtitle = sprintf("N [%s] = %d", N_unit, N), 
        caption = paste(data_source_caption)
      )
    
    return(plot)    
    
  }
  
}
