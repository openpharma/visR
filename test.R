library(Rgraphviz)
source("R/pvr_attrition.R")


N_array <- c(5000, 4500, 4000, 3000)
descriptions <- c(
    "All patiensts", 
    "Not having exclusion criterion 1", 
    "Not having exclusion criterion 2", 
    "Not having exclusion criterion 3"
)
complement_descriptions <- c(
    "Having exclusion criterion 1.", 
    "Having exclusion criterion 2.", 
    "Having exclusion criterion 3."
)

png("figures/attrition.png", height = 1000, width = 1000)
graph <- pvr_attrition(N_array, descriptions, complement_descriptions)
dev.off()
