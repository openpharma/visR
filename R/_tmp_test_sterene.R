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

