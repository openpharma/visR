vr_plt_km_risk_table <- function(risk_table_data, time_unit) {
    plot <- 
        ggplot(risk_table_data, aes(x = time)) + 
        geom_text(aes(y = strata_variable, col = strata, label = value)) + 
        xlab(sprintf("time (%s)", time_unit)) + 
        ylab("") + 
        theme_light() + 
        ggsci::scale_color_nejm()
    
    return(plot)
}
