.get_label <- function(cnt, percent, label_mode) {
    if (label_mode == "count") {
        label <- dplyr::case_when(percent >= 5 ~ sprintf("%d", cnt), TRUE ~ "")
    } else if (label_mode == "percent") {
        label <- dplyr::case_when(percent >= 5 ~ sprintf("%.1f%%", percent), TRUE ~ "")
    } else if (label_mode == "both") {
        label <- dplyr::case_when(percent >= 10 ~ sprintf("%d\n(%.1f%%)", cnt,
            percent), TRUE ~ "")
    } else {
        stop(sprintf("Unrecognized label mode: '%s'.", label_mode))
    }
    return(label)
}


vr_stacked_barchart <- function(
    data,
    title = "",
    abbreviations = "",
    variable_definitions = "",
    N_unit = "patients",
    label_mode = "both"
) {
    # Setup main data frame
    column_names <- colnames(data)
    colnames(data) <- c("group", "subgroup", "cnt")
    data$group <- as.factor(data$group)
    data$subgroup <- as.factor(data$subgroup)

    # Get counts
    N <- sum(data$cnt)
    counts <-
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
            label_text = .get_label(cnt, percent, label_mode)
        )

    # Plot the figure
    plot <- ggplot(plot_data, aes(x = group, y = percent)) +
        geom_bar(stat = "identity", aes(fill = subgroup)) +
        geom_text(aes(y = label_height, label = label_text),
            color = "white", cex = 3) +
        scale_fill_nejm() +
        theme_linedraw() +
        labs(
            fill = column_names[2],
            caption = abbreviations
        ) +
        xlab(column_names[1]) +
        ylab("%") +
        ggtitle(title, subtitle = sprintf("N [%s] = %d", N_unit, N))

    return(plot)
}
