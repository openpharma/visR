library(visR)
library(forestmodel)

# First element of levels gets treated as reference

get_model_n_table <- function(coxph_model) {
  
  # Get data from COX PH model
  data <- stats::model.frame(coxph_model)
  
  # Extract strata name based on which the model was calculated
  strata_name <- base::names(coxph_model$assign)
  
  # Extract n assigned to each group within the strata
  counted <- base::table(data[strata_name]) %>% data.frame()
  
  # Format output
  base::colnames(counted) <- c("group", "value")
  counted["strata"] <- strata_name
  counted["metric"] <- "n"
  counted <- counted[, c("strata", "group", "metric", "value")]
  
  return(counted)
  
}

get_model_hr_table <- function(coxph_model) {
  
  # Get data from COX PH model
  coefficients <- broom::tidy(coxph_model, 
                              exponentiate = TRUE,
                              conf.int = TRUE) %>% data.frame()
  data <- stats::model.frame(coxph_model)
  
  # Extract strata name based on which the model was calculated
  strata_name <- base::names(coxph_model$assign)
  
  # Extract group names present in the strata
  groups <- base::table(data[strata_name]) %>% base::names()
  
  # Remove strata name from group names
  coefficients$term <- sapply(coefficients$term, 
                              function(x) base::gsub("TRTP", "", x))
  
  tmp <- data.frame(matrix(ncol = 1, nrow = length(colnames(coefficients))))
  rownames(tmp) <- colnames(coefficients)
  tmp[,1] <- 0
  tmp["term",] <- groups[1] 
  tmp["estimate",] <- 1
  tmp <- t(tmp)
  
  coefficients <- rbind(tmp, coefficients)
  rownames(coefficients) <- groups
  
  # pivot_longer but in base R + dplyr
  value_colnames <- colnames(coefficients)[colnames(coefficients) != "term"]
  output <- lapply(value_colnames,
                   function(x) {
                     
                     tmp <- data.frame(value = coefficients[,x])
                     tmp["strata"] <- strata_name
                     tmp["group"] <- rownames(coefficients)
                     tmp["metric"] <- x
                     tmp
                     
                   })
  
  output <- do.call(rbind, output)
  output <- output[, c("strata", "group", "metric", "value")]
  
  return(output)
  
}

coxph_model <- adtte %>%
  visR::estimate_KM("TRTP") %>%
  visR::get_COX_HR() 

model_n_table <- coxph_model %>% get_model_n_table()
model_hr_table <- coxph_model %>% get_model_hr_table()

rbind(model_n_table, model_hr_table)

cox %>% forestmodel::forest_model()
