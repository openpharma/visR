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
            group_by(strata) %>% 
            filter(row_number() == 1) %>% 
            ungroup() %>% 
            summarize(N = sum(n.risk)) %>% 
            pull(N)
    }

    # Build caption
    data_source_caption <- sprintf("Data Source: %s", data_source)
    abbreviations_caption <- sprintf("Abbreviations: %s", abbreviations)
    variable_definitions_caption <- sprintf("Variable Definitions: %s", 
        variable_definitions)
    
    plot <- ggplot(broom_object, aes(x = time)) + 
        geom_stepribbon(aes(ymin = conf.low, ymax = conf.high, fill = strata), 
            alpha = 0.25) + 
        geom_step(aes(y = estimate, col = strata)) + 
        theme_light() + 
        scale_color_nejm() + 
        scale_fill_nejm() + 
        ylab(estimate_name) + 
        xlab(sprintf("time (%s)", time_unit)) + 
        labs(
            title = title, 
            subtitle = sprintf("N [%s] = %d", N_unit, N), 
            caption = paste(abbreviations_caption, 
                variable_definitions_caption, data_source_caption))
      
    return(plot)
}
