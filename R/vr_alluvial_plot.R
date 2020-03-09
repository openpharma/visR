#' vr_alluvial_plot
#'
#' Create alluvial plot.
#'
#' @param data
#' @param id 
#' @param linename 
#' @param linenumber 
#' @param n_common 
#' @param interactive
#' @param N_unit 
#' @param data_source 
#' @param fill_by
#' 
#' @return list
#'
#' @examples
#' vr_alluvial_plot(data)
#' 
#' vr_alluvial_plot(data, fill_by = "all_flows")
#' 
#' vr_alluvial_plot(data, interactive = TRUE)
#' 
#' vr_alluvial_plot(data, interactive = TRUE, fill_by = "all_flows")
#'
#' @export


vr_alluvial_plot <- function(
  data,
  id = "PatientID",
  linename = "LineName",
  linenumber = "LineNumber",
  n_common = 2,
  interactive = FALSE,
  N_unit = "patients",
  data_source = "Flatiron",
  fill_by = "first_variable"
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
                                   fill_by = fill_by)
  
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
