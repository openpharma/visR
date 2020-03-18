#' Plot Kaplan-Meier Curve for Existing Tidy Survival Object
#'
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
vr_plt_kaplan_meier <- function(
    broom_object, 
    title = "", 
    abbreviations = NULL, 
    variable_definitions = NULL, 
    N = NULL, 
    N_unit = "patients", 
    time_unit = "days", 
    data_source = NULL, 
    estimate_name = "survival probability"
) {
    
    # Get number of patients
    if (is.null(N)) {
        N <- 
            broom_object  %>% 
            dplyr::group_by(strata) %>% 
            dplyr::filter(row_number() == 1) %>% 
            dplyr::ungroup() %>% 
            dplyr::summarize(N = sum(n.risk)) %>% 
            dplyr::pull(N)
    }

    # Build caption
    data_source_caption <- sprintf("Data Source: %s", data_source)
    abbreviations_caption <- sprintf("Abbreviations: %s", abbreviations)
    variable_definitions_caption <- sprintf("Variable Definitions: %s", 
        variable_definitions)
    
    plot <- ggplot2::ggplot(broom_object, aes(x = time)) + 
        pammtools::geom_stepribbon(aes(ymin = conf.low, ymax = conf.high, fill = strata), 
            alpha = 0.25) + 
        geom_step(aes(y = estimate, col = strata)) + 
        ggplot2::theme_light() + 
        ggsci::scale_color_nejm() + 
        ggsci::scale_fill_nejm() + 
        ggplot2::ylab(estimate_name) + 
        ggplot2::xlab(sprintf("time (%s)", time_unit)) + 
        ggplot2::labs(
            title = title, 
            subtitle = sprintf("N [%s] = %d", N_unit, N), 
            caption = paste(abbreviations_caption, 
                variable_definitions_caption, data_source_caption))
      
    return(plot)
}
