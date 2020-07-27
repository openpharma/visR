library(dplyr)
library(tidyr)
library(survival)
library(ggplot2)
library(broom)
library(purrr)
library(gtable)
library(cowplot)

load(file = file.path(getwd(), "data/adtte.rda"))

source(paste0(getwd(), "/R/vr_KM_est.R"))
source(paste0(getwd(), "/R/vr_plot.R"))
source(paste0(getwd(), "/R/tidyme.R"))
source(paste0(getwd(), "/R/add_CI.R"))
source(paste0(getwd(), "/R/add_CNSR.R"))
source(paste0(getwd(), "/R/add_COX_HR.R"))
source(paste0(getwd(), "/R/add_risktable.R"))
source(paste0(getwd(), "/R/utilities.R"))


## Estimation function: return survfit object because it can be passed to many downstream applications
   survfit_object <- vr_KM_est(data = adtte, strata = NULL, conf.int = F,  timefix=TRUE)
   survfit_object <- vr_KM_est(data = adtte, strata = "TRTP", conf.int = T, timefix=TRUE)
   survfit_object <- vr_KM_est(data = adtte[adtte$SEX == "F", c("CNSR", "AVAL", "PARAMCD", "PARAM", "SEX", "TRTP")],
                               strata = "TRTP")
   survfit_object$call

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
   nest <- adtte %>%
      group_by(SEX) %>%
      nest() %>%
      mutate(fit = map(data, ~ vr_KM_est(data = .x, strata = .x[["TRT01P"]]))) %>%
      mutate(tidytbl = map(fit, ~ tidyme.survfit(.x)))%>%
      unnest(tidytbl)

   nest$fit[[1]]

## high level plotting
 # in this fashion data = "." in call which needs to be replaced => addition functionality in vr_KM_est
 (gg <- adtte%>%
       vr_KM_est(strata = "SEX") %>%
       vr_plot() %>%
       add_CI() %>%
       add_risktable(min_at_risk = 3)
  )

### in this fashion data = adtte => call can be recycled.  
 (gg <- vr_KM_est(data=adtte, strata = "SEX") %>%
       vr_plot() %>%
       add_CI() %>%
       add_risktable(min_at_risk = 3)
  )

### risk table + censor table
   
  adtte%>%
    vr_KM_est(strata = "SEX") %>%
    vr_plot(legend.position = "bottom") %>%
    add_CI() %>%
    add_CNSR(shape = 3, size = 2) %>%
    add_risktable(min_at_risk = 5,
                  display= c("n.risk", "n.censor"),
                  title = c("At risk", "Censored")
                  )

 
### Hazard Ratio
   adtte%>%
       vr_KM_est(strata = "SEX") %>%
       vr_plot() %>%
       add_COX_HR() 
  
   
### Quantiles
   ## 50pct is flat but not at the end => take halfway plateau
   testq1 <- data.frame(AVAL = c(1,2,3,4),
                       CNSR = c(0,0,0,0))
   
   survfit_object <- vr_KM_est(data = testq1)  
   vr_plot(survfit_object)
   quantile(survfit_object)
   get_quantile(survfit_object)

   ## 50pct is point: take time corresponding to point
   testq2 <- data.frame(AVAL = c(1,2,2,4),
                       CNSR = c(0,0,0,0))
   
   survfit_object <- vr_KM_est(data = testq2)  
   vr_plot(survfit_object)
   quantile(survfit_object)
   get_quantile(survfit_object)

   
   ## 50pct is not existing and 75 is last => 50 and 25 NA should be NA
   testq3 <- data.frame(AVAL = c(1,2,2,4),
                       CNSR = c(0,1,1,1))
   
   survfit_object <- vr_KM_est(data = testq3)  
   vr_plot(survfit_object)
   quantile(survfit_object)
   get_quantile(survfit_object)

   ?survival:::quantile.survfit
    
#### TODO
 ## fun argument in vr_plot: validate transformations
 ## create actual risk table underneath plot instead of list
 ## create actual HR table
 ## method for plotting?
