library(readr)
X_tmp_data_stern <- read_delim("_tmp_data_stern.csv",
                               ";", escape_double = FALSE, trim_ws = TRUE)
stroke <- X_tmp_data_stern


library(tidyverse)
library(RBesT)
stroke %>% glimpse()

set.seed(34563)
map_mcmc <- gMAP(cbind(dead1,alive1) ~ 1 | trial,
                 data=stroke,
                 tau.dist="HalfNormal",
                 tau.prior=1,
                 beta.prior=2,
                 family=binomial)

stroke <- 
  stroke %>%
  mutate(
    rr = dead0 / (dead0 + alive0) / dead1 / (dead1 + alive1),
    se = sqrt(dead0/(dead0*(dead0+alive0)) + dead1/(dead1*(dead1+alive1))),
    or =  (dead1/alive1)/(dead0/alive0) ,
    log_or = log(or),
    selogor = sqrt((1/dead1)+(1/alive1)+(1/dead0)+(1/alive0)),
    ln_rr = log(rr)
  )


map_mcmc <- gMAP(cbind(dead1, alive1) ~ 1 | trial,
                 data=stroke,
                 tau.dist="HalfNormal",
                 tau.prior=1,
                 beta.prior=2,
                 family=binomial)



map_mcmc <- gMAP(cbind(log_or, selogor) ~ 1 | trial,
                 data=stroke,
                 tau.dist="HalfNormal",
                 tau.prior=1,
                 beta.prior=2,
                 family=gaussian)




td <- map_mcmc %>% 
  vr_tidy_rbest() 

td

stroke <- stroke %>% mutate(
  study_id = trial
)


gg <- 
  td %>%
  left_join(stroke, by = "study_id") %>%
  mutate(
    study_label = trialname,
    case_when(
      study == "Gastr06" ~ "Adams and Smith, 2006",
    )

  ) %>%
  vr_plot_forest() 

gg + facet_wrap( ~ model  )


map_mcmc %>% 
  vr_tidy_rbest() %>% 
  filter(model == "meta") %>%
  vr_plot_forest() 




library(RBesT)
set.seed(34563)
map_mcmc_as <- gMAP(cbind(r, n-r) ~ 1 | study,
                 data=AS,
                 tau.dist="HalfNormal",
                 tau.prior=1,
                 beta.prior=2,
                 family=binomial)

td <- map_mcmc_as %>% 
  vr_tidy_rbest() 

td2 <- map_mcmc %>% 
  vr_tidy_rbest() 


td %>% vr_plot_forest()

td2 %>% vr_plot_forest()


td2 %>% 
#  filter(model != "stratified") %>%
  ggplot2::ggplot(aes( x = reorder(study.label, study.id), 
                       y = estimate, 
                       ymin = conf.low, 
                       ymax = conf.high )
  ) +
  ggplot2::geom_pointrange(show.legend = FALSE, width = 1) + 
  ggplot2::coord_flip() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  facet_wrap( ~ study, ncol = 1, scales = "free_y") + 
  ggtitle("Update the title", subtitle = "Fill out the population")


td2 %>% 
  #  filter(model != "stratified") %>%
  ggplot2::ggplot(aes( x = reorder(model, estimate), 
                       y = estimate, 
                       ymin = conf.low, 
                       ymax = conf.high )
  ) +
  ggplot2::geom_pointrange(show.legend = FALSE) + 
  geom_hline(yintercept = 0.5) +
  ggplot2::coord_flip() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  facet_wrap( ~ study, ncol = 1, scales = "free_y") + 
  ggtitle("Update the title", subtitle = "Fill out the population")


td
td2



#----------------------------------
crs %>%
  select(id, chf, afib, diabetes, mi, copd, crohn, ulcercol, ibd) %>%
  pivot_longer(-id, names_to = "comorb", values_to = "value") %>%
  mutate(
    comorb = case_when(
      comorb == "chf" ~ "Congestive heart failure",
      comorb == "afib" ~ "Atrial fibrillation",
      comorb == "diabetes" ~ "Diabetes",
      comorb == "copd" ~ "COPD",
      comorb == "mi" ~ "Myocardial infarction",
      comorb == "crohn" ~ "Crohn's disease",
      comorb == "ulcercol" ~ "Ulcerative colitis",
      comorb == "ibd" ~ "Inflammatory bowel disease"
    ),
    value = case_when(value == "0" ~ "no",
                      value == "1" ~ "yes")
  ) %>%
  group_by(comorb, value) %>%
  summarise(n = n()) %>%
  mutate(
    inc = n / bigN,
    perc = n / bigN * 100,
    plot_lab = paste0('(', n, ', ', round(perc, digits = 1), '%)'),
    axis_lab = paste0(comorb, ' ', '(', n, ', ', round(perc, digits = 1), '%)')
  ) %>%
  filter(value == "yes") %>%
  ggplot(aes(reorder(axis_lab, inc), inc)) +
  geom_point() +
  coord_flip() +
  ggtitle("Proportion of patients reporting a comorbidity at diagnosis") +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.title = element_blank()
  )



#----------------------------------------


col1 <- td %>%  
  ggplot(aes(reorder(study.label, study.id), 1, label = study.label), hjust = 0) +
  geom_text() +
  coord_flip() +
  ggplot2::theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank()
  )

col2 <- td %>% 
  filter(model != "stratified") %>%
  ggplot2::ggplot(aes( x = reorder(study.label, study.id), 
                       y = estimate, 
                       ymin = conf.low, 
                       ymax = conf.high )
  ) +
  ggplot2::geom_pointrange(show.legend = FALSE) + 
  geom_hline(yintercept = 0.2) +
  ggplot2::coord_flip() +
  ggplot2::theme_minimal() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank()
  ) +
  ylab("Measurement [units]")

library(patchwork)
plot <- col1 + col2
plot



