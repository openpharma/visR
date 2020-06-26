library(dplyr)
library(tidyr)
library(survival)
library(ggplot2)
library(here)
library(readr)
library(broom)

source(here("R", "tidyme.R"))
source(here("R", "vr_KM_plot.R"))
source(here("R", "vr_KM_est.R"))
source(here("R", "vr_KM_risktable.R"))


## try to recreate https://www.nejm.org/doi/full/10.1056/NEJMoa1809697 
## or altto https://www.gotoper.com/publications/ajho/2017/2017august/update-on-her2-positive-adjuvant-therapy

## read data - two treatments initially 

ADTTE <- read_csv(here("data", 'psi-vissig-adtte.csv')) %>% 
  filter(TRT01PN %in% c(1, 4))


## estimations 
km <- survfit(Surv(AVAL, CNSR == 0) ~ TRT01P, data = ADTTE) 
diff <- survdiff(Surv(AVAL, CNSR == 0) ~ TRT01P, data = ADTTE, rho = 0)
hr <- coxph(Surv(AVAL, CNSR == 0) ~ TRT01P + STR01 , data = ADTTE)



km %>% tidy() ## to get KM estimates (lower, upper)
summary(km, times = c(1, 500, 1000)) ## risk table 
diff %>% glance() ## to get log rank p-value
hr %>% tidy()  ## to get HR (lower, upper), p-value 


## proposed API
# plot(km) %>%
#   add_confidence_interval(alpha = 0.05) %>%
#   add_risktable(times = c(1, 6, 12), scale = “year”) %>%
#   add_pvalue(log_rank) %>%
#   add_HR() %>%


fit %>% 
  api_plot() %>%  ## plot the KM estimates
  api_add_ci() %>%  ## display CIs
  api_add_risk_table(fit) %>%  ## add risk table - may need to summarise at specified times
  api_add_metadata() %>% ## display titles, labels, footnotes, etc. 
  api_add_hr(hr) %>%   ## display annotation from different analysis
  api_add_pvalue(diff) %>%  ## display annotation from different analysis
  #add_median_survival() %>% ## add line annotations and summary of median survival
  api_style()  ## style 



#------------------------------------------------------
# Below very hacky functions

api_plot <- function(fit){
  
    ## tidy the object first
 graph_data <-   
   broom::tidy(fit) %>%
    dplyr::mutate(group = stringr::str_remove(strata, "TRT01P=")) 

  
 gg <-
   graph_data %>% 
   ggplot(aes(x = time, y = estimate, group = group)) +
   geom_step()   
 
 return(gg)

}


api_add_ci <- function(gg){
  gg <- 
    gg + geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                alpha = 0.1,
                fill = "red") 
  return(gg)
}


api_add_risk_table <- function(gg, fit){
  
  sumfit <- summary(fit, times = c(0, 250, 500, 750, 1000, 1250, 1500, 1750, 2000)) 
  
  risk_data <-
    tibble(
      time =  sumfit$time,
      group = sumfit$strata,
      n.risk =   sumfit$n.risk,
      n.events = sumfit$n.event
    ) %>%
    dplyr::mutate(
      group = stringr::str_remove(group, "TRT01P="),
      label = paste0(n.risk, " (", n.events, ")"),
      y_pos = 0.01,
      group2 = group
    )
    
  ## draw risk set
  gg <- 
    gg + 
    geom_text(
    data = risk_data,
    mapping = aes(
      x = time,
      y = y_pos,
      label = label,
      group = group,
      fill = NULL
    ),
    size = 2.5
  ) 

  return(gg)
}


api_add_metadata <- function(gg){
  
  title <- "Evidence of improved progression-free survival for combo over monotherapy"
  subtitle <- "Kaplan-Meier estimates over time including 95% uncertainty interval"
  source <- "*The number of patients at risk (and events) are displayed the time point reference.
Data source: https://github.com/VIS-SIG/Wonderful-Wednesdays/tree/master/data/2020/2020-04-08"
  y_axis <- "Progression free survival"
  x_axis <- "Time [days]*"
  
  gg <- 
    gg +   ## annotations
    labs(title = title,
         subtitle = subtitle,
         caption = source) +
    xlab(x_axis) +
    ylab(y_axis) 
  
  return(gg)
}

api_style <- function(gg){
  
  gg <- 
    gg +
    # set up basic theme
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      axis.title.y = element_blank(),
      legend.position = "none"
    ) +
    # Set the entire chart region to a light gray color
    theme(panel.border = element_rect(color = "grey", fill = NA, size = 0.35))
  
  return(gg)
}


api_add_hr <- function(gg, hr){

  
  annotation_data <-
    hr %>%
    tidy() %>%
    dplyr::filter(term == "TRT01Ptablemab x 52 weeks") %>%  ### this is hardcoded. point is to find the correct pvalue
    dplyr::mutate(
      hr_label = paste0("Hazard ratio = ", round(estimate , 2), " (", round(conf.low, 2), ", ", round(conf.high, 2), ")")
    )
  
    hr_annotation <- annotation_data$hr_label
  
  gg <- 
    gg + annotate("text", x = 1500, y = 0.9, label = hr_annotation)

  
  return(gg)
}


api_add_pvalue <- function(gg, diff){
  
  
  annotation_data <-
    diff %>%
    glance() %>%
    dplyr::mutate(
      pval_label = paste0("Pvalue = ", round(p.value , 4))
    )
  
  pval_annotation <- annotation_data$pval_label
  
  gg <- 
    gg + annotate("text", x = 1500, y = 0.8, label = pval_annotation)
  
  
  return(gg)
}



  