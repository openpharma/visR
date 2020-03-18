vr_cross_tab <- function(data, type = "count", overall_row = TRUE, 
    overall_column = TRUE) {
    if (!type %in% c("count", "percent", "both")) {
        stop("Type not recognized.")
    } else if (type %in% c("percent", "both") && overall_column) {
        stop(sprintf("'overall_column' cannot be displayed with type '%s'", 
            type))
    }
    
    # Reformat the counts
    column_names <- colnames(data)
    formula <- as.formula(sprintf("%s ~ %s", column_names[1], column_names[2]))
    cross_tab <- dcast(formula, data = data, value.var = column_names[3], 
        fill = 0)
    cross_tab[,column_names[1]] <- as.character(cross_tab[,column_names[1]])
    cross_tab[,column_names[1]] <- ifelse(!is.na(cross_tab[,column_names[1]]), 
        cross_tab[,column_names[1]], "Missing")
    rownames(cross_tab) <- cross_tab[,column_names[1]]
    cross_tab[,column_names[1]] <- NULL
    colnames(cross_tab)[is.na(colnames(cross_tab))] <- "Missing"

    # Prepare base tables
    if (type %in% c("percent", "both")) {
        percents <- as.data.frame(sapply(1:ncol(cross_tab), function(i) 
            round_percent(cross_tab[,i], decimals = 1)))
        rownames(percents) <- rownames(cross_tab)
        colnames(percents) <- colnames(cross_tab)
        if (overall_row) {
            percents <- rbind(percents, c(colSums(percents)))
            rownames(percents)[nrow(percents)] <- "Overall"
        }
        #if (overall_column) percents$Overall <- rowSums(percents)
    }
    if (overall_row) {
        cross_tab <- rbind(cross_tab, c(colSums(cross_tab)))
        rownames(cross_tab)[nrow(cross_tab)] <- "Overall"
    }
    if (overall_column && type != ("percent")) 
        cross_tab$Overall <- rowSums(cross_tab)

    # Prepare final table
    if (type == "percent") {
        cross_tab <- percents
    } else if (type == "both") {
        both <- as.data.frame(sapply(1:ncol(cross_tab), function(i) 
            sprintf("%.0f (%.0f%%)", cross_tab[,i], percents[,i])))
        rownames(both) <- rownames(cross_tab)
        colnames(both) <- colnames(cross_tab)
        cross_tab <- both
    }
    return(cross_tab)
}
