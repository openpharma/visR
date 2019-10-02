#' Attrition
#' 
#' Simple attrition diagrams for cohort selection
#'
#' @param N_array An array of integers of length M, where M is the number of 
#' steps in your cohort selection.
#' @param descriptions An array of characters of length M describing each 
#' step of the cohort selection (e.g. patients at least 18 years old at 
#' diagnosis).
#' @param complement_descriptions An array of characters of length M - 1 
#' describing the complement of each step in the cohort selection except for 
#' the first (e.g. patients less than 18 years old at diagnosis).
#' @param output_path The path to the output (either a .ps or a .svg file).
#'
pvr_attrition <- function(N_array, descriptions, complement_descriptions, 
    output_path = NULL) {
    dot_string <- "digraph G {\n    rankdir=TB\n    ranksep=0.1;"
    
    for (i in 1:length(N_array)) {
        text <- sprintf("%s (N = %d)", descriptions[i], N_array[i])
        dot_string <- paste(dot_string, 
            sprintf("\n    step_node_%d [label=\"%s\", shape=rect];", i, text))
        
        if (i > 1) {
            dot_string <- paste(dot_string, 
                sprintf("\n    invisible_node_%d -> step_node_%d;", i - 1, i))
        }
        
        if (i < length(N_array)) {
            dot_string <- paste(dot_string, 
                sprintf("\n    subgraph cluster_%d {", i))
            dot_string <- paste(dot_string, 
                sprintf("\n        color=none;", i))
            dot_string <- paste(dot_string, 
                sprintf("\n        { rank=same;", i))
            dot_string <- paste(dot_string, 
                sprintf("\n        invisible_node_%d [label=\"\" height=0 width=0 color=black];", i))
            text <- sprintf("%s (N = %d)", complement_descriptions[i], 
                N_array[i] - N_array[i + 1])
            dot_string <- paste(dot_string, sprintf(
                "\n        exclusion_node_%d [label=\"%s\", shape=rect];", i, 
                text))
            dot_string <- paste(dot_string, sprintf(
                "\n        invisible_node_%d -> exclusion_node_%d;", i, i))
            dot_string <- paste(dot_string, 
                sprintf("\n    }}", i))
            dot_string <- paste(dot_string, 
                sprintf("\n    step_node_%d -> invisible_node_%d [arrowhead=none];", i, i))
        }
    }
    
    dot_string <- paste(dot_string, "\n}")
    plot <- dot(dot_string, file = output_path)
    
    return(plot)
}
