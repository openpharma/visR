.stringWrap <- function(strings, wrap_width) {
    wrapped_strings <- sapply(strings, function(string) 
        paste(strwrap(string, width = wrap_width), collapse = "\\n"))
    return(wrapped_strings)
}


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
#' @param display Whether to display the dot string.
#' @param wrap_width The width of the text for text wrapping, as used in 
#' the strwrap function..
#' @param node_width The width of the node as used in graphviz (dot).
#' @param font_size The label font size as used in graphviz (dot).
#'
pvr_attrition <- function(N_array, descriptions, complement_descriptions, 
    output_path = NULL, display = FALSE, wrap_width = 50, node_width = 3, 
    font_size = 8) {
    descriptions <- .stringWrap(descriptions, wrap_width)
    complement_descriptions <- .stringWrap(complement_descriptions, wrap_width)
    
    dot_string <- "digraph G {\n    rankdir=TB\n    ranksep=0.1;"
    for (i in 1:length(N_array)) {
        text <- sprintf("%s (N = %d)", descriptions[i], N_array[i])
        dot_string <- paste(dot_string, 
            sprintf("\n    step_node_%d [label=\"%s\", shape=rect, width=%d, fontsize=%d];", 
                i, text, node_width, font_size))
        
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
                "\n        exclusion_node_%d [label=\"%s\", shape=rect, width=%d, fontsize=%d];", 
                i, text, node_width, font_size))
            dot_string <- paste(dot_string, sprintf(
                "\n        invisible_node_%d -> exclusion_node_%d;", i, i))
            dot_string <- paste(dot_string, 
                sprintf("\n    }}", i))
            dot_string <- paste(dot_string, 
                sprintf("\n    step_node_%d -> invisible_node_%d [arrowhead=none];", i, i))
        }
    }
    
    dot_string <- paste(dot_string, "\n}")
    if (display) cat(sprintf("DOT:\n%s\n", dot_string))
    dot_string <- gsub("'", "\\'", dot_string, fixed = TRUE)
    dot_string <- gsub("\"", "\\\"", dot_string, fixed = TRUE)
    plot <- dot(dot_string, file = output_path)
    
    return(plot)
}
