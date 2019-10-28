pvr_stacked_barchart <- function(
    data, 
    group, 
    subgroup, 
    cnt, 
    title = "", 
    abbreviations = "", 
    variable_definitions = "", 
    N_unit = "patients"
) {
    # Setup main data frame
    group_name <- eval(substitute(bquote(group)), data, parent.frame())
    subgroup_name <- eval(substitute(bquote(subgroup)), data, parent.frame())
    data <- data.frame(
        group = factor(eval(substitute(factor(group)), data, parent.frame())), 
        subgroup = eval(substitute(factor(subgroup)), data, parent.frame()), 
        cnt = eval(substitute(as.integer(cnt)), data, parent.frame())
    )

    # Get counts
    N <- sum(data$cnt)
    cnt <- 
        data %>% 
        group_by(group) %>% 
        summarize(cnt = sum(cnt))
    levels(data$group) <- sprintf("%s\n[%d]", levels(data$group), counts$cnt)

    # Setup plotting data frame
    plot_data <- 
        data %>% 
        group_by(group) %>% 
        arrange(subgroup) %>% 
        mutate(
            percent = cnt * 100 / sum(cnt), 
            label_height = 100 - (cumsum(percent) - (0.5 * percent)), 
            label_text = case_when(percent >= 10 ~ sprintf("%d\n(%.1f%%)", cnt, 
            percent), TRUE ~ "")
        )
        
    # Plot the figure
    plot <- ggplot(plot_data, aes(x = group, y = percent)) + 
        geom_bar(stat = "identity", aes(fill = subgroup)) + 
        geom_text(aes(y = label_height, label = label_text), 
            color = "white", cex = 3) + 
        scale_fill_nejm() + 
        theme_linedraw() + 
        labs(
            fill = subgroup_name, 
            caption = abbreviations
        ) + 
        xlab(group_name) + 
        ylab("%") + 
        ggtitle(title, subtitle = sprintf("N [%s] = %d", N_unit, N))
        
    return(plot)
}
