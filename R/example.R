library(dplyr)
library(tidyr)
library(survival)
library(ggplot2)
library(broom)

load(file = file.path(getwd(), "data/adtte.rda"))
source(paste0(getwd(), "/R/vr_KM_est.R"))
source(paste0(getwd(), "/R/vr_KM_plot.R"))
source(paste0(getwd(), "/R/tidyme.R"))
source(paste0(getwd(), "/R/add_KM_CI.R"))
source(paste0(getwd(), "/R/add_KM_risktable.R"))


## Estimation function: return survfit object because it can be passed to many downstream applications
   survfit_object <- vr_KM_est(data = adtte, strata = NULL, aval = "AVAL", conf.int = F,  timefix=TRUE)
   survfit_object <- vr_KM_est(data = adtte, strata = "TRTP", aval = "AVAL", conf.int = T, timefix=TRUE)

## low level plotting depends on list structure of surv object
   plot(survfit_object)

## clean survfit
 # broom methods glance augment fail + tidy fails if no CI is estimated
   broom:::tidy.survfit(survfit_object)
   glance(survfit_object)
   augment(survfit_object)
   
 # tidyme
   tidy_survobj <- tidyme.survfit(survfit_object)

## rerun the call
   tidy_survobj$call
   eval(tidy_survobj$call[1][[1]])

## Update formula of survival object
   getCall(survfit_object)
   eval(survfit_object)
   (new_survfit_object <- update(survfit_object,  formula = . ~ . + SEX, evaluate = TRUE))

## update analysis of Surv()-depedent objects => survdiff
   Call <- as.list(tidy_survobj$call[1][[1]])
   Call[[1]] <- as.symbol("survdiff")
   Call$conf.int <- NULL # here we can intersect the formals(survdiff) with Call. + need to capture additional options.
   Call$timefix <- NULL
   eval(as.call(Call))

   
## nesting
   library(purrr)
   x <- adtte %>%
      group_by(SEX) %>%
      nest() %>%
      mutate(fit = map(data, ~ vr_KM_est(data = .x, strata = .x[["TRT01P"]]))) %>%
      mutate(tidytbl = map(fit, ~ tidyme.survfit(.x)))%>%
      unnest(tidytbl)

## high level plotting
# (p <-
#   vr_KM_plot(
#       survfit_object = survfit_object
#      ,conf_limits = TRUE                  
#      ,y_label = "Suvival Probability" 
#      ,x_label = "time"                   
#      ,xaxistable=T
#      ,min_at_risk = 0
#      ,time_ticks = seq(50,200,50)
#    )
# )

   
 (gg <- adtte%>%
       vr_KM_est(aval = "AVAL", strata = "SEX") %>%
       vr_KM_plot() %>%
       add_KM_CI() %>%
       add_KM_risktable(min_at_risk = 3)
  )
   
