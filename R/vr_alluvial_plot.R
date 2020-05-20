#' vr_alluvial_plot
#'
#' Create alluvial plot.
#'
#' @param data a dataframe
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
#' @param col_vector_value HEX color values  for y levels/values,.
#' @param linenames_labels_size, Default: 2.5
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
#' vr_alluvial_plot(dataset)
#'
#' vr_alluvial_plot(dataset, interactive = TRUE)
#'
#' vr_alluvial_plot(dataset, fill_by = "all_flows")
#'
#' vr_alluvial_plot(dataset, interactive = TRUE, fill_by = "all_flows")
#'
#' vr_alluvial_plot(dataset, col_vector_value =  RColorBrewer::brewer.pal(9, 'Greys')[c(3,6,4,7,5,8)])
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
  col_vector_value =  RColorBrewer::brewer.pal(9, 'Blues')[c(3,6,4,7,5,8)],
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

  p = dplyr::select(alluvial_data, linenumber, linename, id) %>%
    dplyr::mutate(
      linenumber = paste0("L",linenumber)
    ) %>%
    easyalluvial::alluvial_long(
       key = linenumber,
       value = linename,
       id = id,
       fill_by = fill_by,
       col_vector_flow = col_vector_flow,
       col_vector_value = col_vector_value,
       stratum_label_size = linenames_labels_size
       )

  if(interactive) {

    plot <- parcats::parcats(p,
                             marginal_histograms = FALSE,
                             hoverinfo = "count",
                             hoveron = "category",
                             data_input = dplyr::select(alluvial_data, linenumber, linename, id))


    return(plot)

  } else {

    plot <- p +
      ggplot2::ylab("Patients count") +
      ggplot2::xlab("Line of therapy") +
      ggplot2::labs(
        title = title,
        subtitle = sprintf("N [%s] = %d", N_unit, N),
        caption = paste(data_source_caption)
      )

    return(plot)

  }

}
