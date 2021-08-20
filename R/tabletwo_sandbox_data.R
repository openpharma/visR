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
  
  # pivot_longer but in base R 
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

coxph_trtp_model <- adtte %>%
  visR::estimate_KM("TRTP") %>%
  visR::get_COX_HR() 

model_trtp_n_table <- coxph_trtp_model %>% get_model_n_table()
model_trtp_hr_table <- coxph_trtp_model %>% get_model_hr_table()

coxph_sex_model <- adtte %>%
  visR::estimate_KM("SEX") %>%
  visR::get_COX_HR() 

coxph_sex_model %>% forestmodel::forest_model()

model_sex_n_table <- coxph_sex_model %>% get_model_n_table()
model_sex_hr_table <- coxph_sex_model %>% get_model_hr_table()

tabletwo_tidy <- rbind(model_trtp_n_table,
                       model_trtp_hr_table,
                       model_sex_n_table,
                       model_sex_hr_table)



render_left_table <- function(tidy_data, precision = 3) {
  
  output <- lapply(unique(tidy_data$strata),
                   function(x) {
                     
                     strata_groups <- tidy_data %>%
                       dplyr::filter(strata == x) %>%
                       dplyr::pull(group) %>%
                       base::unique()
                     
                     HR_values <- tidy_data %>%
                       dplyr::filter(strata == x) %>%
                       dplyr::filter(metric == "estimate") %>%
                       dplyr::pull(value) %>%
                       base::as.numeric() %>%
                       base::round(2) %>%
                       base::format(nsmall = 2) 
                     
                     HR_conf_lower_values <- tidy_data %>%
                       dplyr::filter(strata == x) %>%
                       dplyr::filter(metric == "conf.low") %>%
                       dplyr::pull(value) %>%
                       base::as.numeric() %>%
                       base::round(2) %>%
                       base::format(nsmall = 2)
                     
                     HR_conf_upper_values <- tidy_data %>%
                       dplyr::filter(strata == x) %>%
                       dplyr::filter(metric == "conf.high") %>%
                       dplyr::pull(value) %>%
                       base::as.numeric() %>%
                       base::round(2) %>%
                       base::format(nsmall = 2) 
                     
                     p_values <- tidy_data %>%
                       dplyr::filter(strata == x) %>%
                       dplyr::filter(metric == "p.value") %>%
                       dplyr::pull(value) %>%
                       base::as.numeric() 
                     
                     number_patients <- tidy_data %>%
                       dplyr::filter(strata == x) %>%
                       dplyr::filter(metric == "n") %>%
                       dplyr::pull(value) %>%
                       base::as.numeric() 
                     
                     tmp <- data.frame(strata = rep(x, length(strata_groups)),
                                       group = strata_groups,
                                       HR = HR_values,
                                       lower = HR_conf_lower_values,
                                       upper = HR_conf_upper_values,
                                       p = p_values,
                                       n = number_patients,
                                       colour = "white")
                     
                     tmp
                       
                     })
  
  output <- base::do.call(rbind, output)
  
  output["ID"] <- base::seq(base::nrow(output), 1)
  
  # Preformat label for right side of table
  output <- output %>%
    dplyr::mutate(label = paste0(HR, " (", lower, "-", upper, ")")) 
  
  # Convert HR, lower and upper back to numeric for plotting 
  output <- output %>%
    dplyr::mutate(HR = as.numeric(HR)) %>%
    dplyr::mutate(lower = as.numeric(lower)) %>%
    dplyr::mutate(upper = as.numeric(upper))
  
  # Replace reference HR with "Reference"
  output <- output %>%
    dplyr::mutate(label = dplyr::if_else(label == "1.00 (0.00-0.00)",
                                         "Reference",
                                         label)) 
  
  # Replace pval for references
  output <- output %>%
    dplyr::mutate(p = dplyr::if_else(label == "Reference",
                                     "NA",
                                     as.character(p)))   
  
  # Format pvalue
  threshold <- paste0("0.", paste0(rep("0", precision-1), collapse = ""), "1") %>%
    as.numeric()
  
  suppressWarnings(
    output <- output %>%
      dplyr::mutate(p = as.numeric(p)) %>%
      dplyr::mutate(p = dplyr::if_else(p < threshold,
                                       paste0("p < ", threshold),
                                       p %>% 
                                         base::round(precision) %>% 
                                         base::format(nsmall = precision)))
  )

  
  return(output)
  
}

add_dummy_row_for_strata <- function(hr_table) {
  
  dummy_rows <- hr_table %>% 
    dplyr::group_by(strata) %>%
    dplyr::summarise(group = NA,
                     HR = NA,
                     lower = NA,
                     upper = NA,
                     p = NA,
                     n = NA,
                     ID = max(ID),
                     label = NA,
                     colour = "lightgrey") %>%
    data.frame()
  
  output <- rbind(hr_table, dummy_rows)
  
  return(output %>% dplyr::arrange(desc(ID)))
  
}

blankRows <- 2    # blank rows under boxplot
titleSize <- 5
dataSize <- 4
y_strata <- 0.2
y_group <- 1
y_n <- 4

hazard_data <- render_left_table(tabletwo_tidy)

hazard_data <- add_dummy_row_for_strata(hazard_data)

left_table_labels <- c("strata", "group", "n")
left_header_y_pos <- c(y_strata, y_group, y_n)

LfLabels<-data.frame(x = rep(max(hazard_data$ID)+1, times=length(left_table_labels)),
                     y = left_header_y_pos,
                     lab = left_table_labels,
                     hjust = c(0, 0, 1))

right_table_labels <- c("HR (95% CI)", "pval")
right_header_y_pos <- c(max(hazard_data$upper, na.rm = TRUE)*3, max(hazard_data$upper, na.rm = TRUE)*5.5)

RtLabels<-data.frame(x=rep(max(hazard_data$ID)+1, times=length(right_table_labels)),
                     y = right_header_y_pos,
                     lab = right_table_labels,
                     hjust = c(1, 1))

group_p<-ddply(hazard_data,.(strata),summarize,P=mean(p),y=max(ID))

hl_rows<-data.frame(ID=hazard_data[which(hazard_data$colour == "lightgrey"),"ID"],colour="lightgrey")
# hl_rows$ID<-hl_rows$ID+1
hl_rect<-function(colour="lightgrey",alpha=0.5){
  rectGrob(   x = 0, y = 0, width = 1, height = 1, just = c("left","bottom"), gp=gpar(alpha=alpha, fill=colour))
}



haz <- ggplot2::ggplot(hazard_data, ggplot2::aes(factor(ID), HR)) + 
  ggplot2::labs(x = NULL, y = NULL)


left <- haz +
  apply(hazard_data %>% dplyr::filter(is.na(group)), 
        MARGIN = 1, 
        function(x) annotation_custom(hl_rect(x["colour"],alpha=0.4),as.numeric(x["ID"])-0.5,as.numeric(x["ID"])+0.5,-20,20)) +
  coord_flip(ylim=c(0,4), xlim = c(0, max(hazard_data$ID)+1)) + 
  geom_point(aes(x=factor(ID),y=1),shape=3,alpha=0) + 
  geom_text(data=group_p,aes(factor(y), y_strata ,label=strata, fontface="bold"), hjust=0, size=dataSize) +
  geom_text(data=hazard_data,aes(factor(ID), y_group ,label=group), hjust=0, size=dataSize) +
  geom_text(data=hazard_data,aes(factor(ID), y_n, label=n, hjust=1), size=dataSize) +
  geom_text(data=LfLabels,aes(x,y,label=lab, fontface="bold", hjust=hjust), size=titleSize) +
  geom_segment(aes(x = min(ID)-0.5, y = 0, xend = min(ID)-0.5, yend = 4.5)) + 
  theme_bare

left

hr_x_ticks <- pretty(c(min(hazard_data$lower, na.rm = TRUE), max(hazard_data$upper, na.rm = TRUE)))
scaledata<-data.frame(ID=0.5,HR=c(1, hr_x_ticks))

right <- haz + 
  apply(hazard_data %>% dplyr::filter(is.na(group)), 
        MARGIN = 1, 
        function(x) annotation_custom(hl_rect(x["colour"],alpha=0.4),as.numeric(x["ID"])-0.5,as.numeric(x["ID"])+0.5,-20,20)) +  
  geom_point(aes(x=factor(ID),y=HR),shape=22,alpha=1, fill="black", size = 4) + 
  geom_errorbar(data = hazard_data %>% dplyr::filter(HR != 1),
                aes(ymin = lower, ymax = upper, width = 0.4)) + 
  scale_y_log10() + coord_flip(xlim = c(0, max(hazard_data$ID)+1)) +
  # geom_text(data=scaledata,aes(0,HR,label=HR), vjust=0.5, size=dataSize) +
  geom_text(data=RtLabels,aes(x,y,label=lab, fontface="bold",hjust=hjust), vjust=0.5, size=titleSize) +
  geom_text(aes(factor(ID), max(upper, na.rm = TRUE)*3,label=label),vjust=0.5, hjust=1, size=dataSize) +
  # geom_text(data=group_p,aes(factor(y),max(upper, na.rm = TRUE)*3,label=P, fontface="bold"),vjust=0.5, hjust=1, size=dataSize) +
  geom_text(aes(factor(ID),max(upper, na.rm = TRUE)*5.5,label=p),vjust=0.5, hjust=1, size=dataSize) +
  # geom_text(data=LegendLabels,aes(x,y,label=lab, fontface="bold"),hjust=0.5, vjust=1, size=titleSize) +
  geom_point(data=scaledata,aes(x =ID,y = HR),shape=3,size=2) + 
  geom_text(data=scaledata,aes(ID-0.4,HR,label=HR), vjust=0.5, size=dataSize*0.8) +
  # geom_point(aes(2,12),shape=3,alpha=0) + 
  geom_segment(aes(y=1, yend=1, x = min(ID)-0.5, xend=max(ID)+0.5),linetype=2, size=0.5) + 
  geom_segment(aes(x = min(ID)-0.5, y = 0, xend = min(ID)-0.5, yend = max(upper, na.rm = TRUE)*5.5)) + 
  theme_bare

gridExtra::grid.arrange(left,right, widths=c(1,1.5), ncol=2, nrow=1)
