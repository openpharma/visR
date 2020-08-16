library(dplyr)
library(tidyr)
library(survival)
library(ggplot2)
library(broom)
library(purrr)
library(gtable)
library(cowplot)
library(plotly)
library(survminer)

load(file = file.path(getwd(), "data/adtte.rda"))

source(paste0(getwd(), "/R/vr_KM_est.R"))
source(paste0(getwd(), "/R/vr_plot.R"))
source(paste0(getwd(), "/R/tidyme.R"))
source(paste0(getwd(), "/R/add_CI.R"))
source(paste0(getwd(), "/R/add_CNSR.R"))
source(paste0(getwd(), "/R/add_COX_HR.R"))
source(paste0(getwd(), "/R/add_risktable.R"))
source(paste0(getwd(), "/R/utilities.R"))
source(paste0(getwd(), "/R/get_quantile.R"))
source(paste0(getwd(), "/R/vr_plotly.R"))

# files <- base::list.files(file.path(getwd(), "R"), pattern = "*.R", full.names = TRUE)
# lapply(files, source)


#### Estimation function : return survfit object allowing it to be used in many downstream applications ####

 ## The function stands on its own and expands the object with PARAM/PARAMCD information
   survfit_object <- vr_KM_est(data = adtte, strata = NULL, conf.int = F,  timefix=TRUE)
   survfit_object <- vr_KM_est(data = adtte, strata = "TRTP", conf.int = T)
   survfit_object <- vr_KM_est(data = adtte, strata = c("TRTP", "SEX"))
   survfit_object <- vr_KM_est(data = adtte[adtte$SEX == "F", c("CNSR", "AVAL", "PARAMCD", "PARAM", "SEX", "TRTP")],
                               strata = "TRTP")
   survfit_object$call
  
  ## The function is compatible with pipe, nest and map => the function call remains traceable data = adtte 
   nest <- adtte %>%
      group_by(SEX) %>%
      nest() %>%
      mutate(fit = map(data, ~ vr_KM_est(data = .x, strata = "TRTP"))) %>%
      mutate(tidytbl = map(fit, ~ tidyme.survfit(.x)))%>%
      unnest(tidytbl)

   nest$fit[[1]]
   
   nest <- adtte %>%
      group_by(SEX) %>%
      nest() %>%
      # use survfit => traceability lost
      mutate(fit = map(data, ~ survfit(Surv(AVAL, 1-CNSR) ~ TRTP, data = .x))) %>%
      mutate(tidytbl = map(fit, ~ tidyme.survfit(.x)))%>%
      unnest(tidytbl)  
   
   nest$fit[[1]]
   
   adtte %>%
      vr_KM_est(strata = "TRTP")

 ## Subgroup analysis  
   fit <- adtte %>%
      group_by(SEX)%>%
      group_map(~ vr_KM_est(strata = NULL, data = .x))
   
   names(fit) <- unique(adtte$SEX)
   
   
   ## survminer solution
   fit <- adtte %>%
    surv_group_by("SEX") %>%
    surv_fit(Surv(AVAL, 1-CNSR) ~ TRTP, data = .)
   

#### Plot : relies on list structure of the survfit object ####
   
  ## Low Level Plotting survival
    survival:::plot.survfit(survfit_object)
   
  ## Low Level Plotting cumhaz: survival calculates CI automaticaly based of x$cumhaz and x$std.chaz using normal approximation
  ## The function for the calculation of the CI is not exported => rely MLE -log(S(t))
    survival:::plot.survfit(survfit_object, cumhaz = T, conf.int = 0.95, conf.type = "plain", col = c(1,2,3))
    survival:::survfit_confint(survfit_object$cumhaz, survfit_object$std.chaz, logse = FALSE, conf.type = "plain", conf.int = 0.95, ulimit = FALSE)

  ## High Level Plotting: Survminer uses -log(-S(t))
    ggsurvplot(survfit_object, fun = "cumhaz", conf.int = T)

  ## High Level Plotting: our approach needs work
    vr_plot(survfit_object, fun="cumhaz")%>%
      add_CI()
   
#### Helper function to clean survfit object for plotting ####
  ## broom methods glance augment fail + tidy fails if no CI is estimated => broom is lazy for survival
    broom:::tidy.survfit(survfit_object)
    glance(survfit_object)
    augment(survfit_object)
   
  ## tidyme
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

   
   
#### High Level Plotting ####

  ## Basis
   (gg <- adtte%>%
       vr_KM_est(strata = "SEX") %>%
       vr_plot() %>%
       add_CI() %>%
       add_risktable(min_at_risk = 3)
   )

   (gg <- vr_KM_est(data=adtte, strata = "SEX") %>%
       vr_plot() %>%
       add_CI() %>%
       add_risktable(min_at_risk = 3)
   )

  ## Confidence interval
   
    gg <- adtte%>%
      vr_KM_est(strata = "SEX") %>%
      vr_plot(legend.position = "bottom")

    gg %>%
      add_CI()

    gg %>%
      add_CI(style = "step", linetype = 2)
    
  ## Censor
    gg %>%
      add_CNSR(shape = 3, size = 2)
   
  ## Risk table and Censor table
    adtte %>%
      vr_KM_est(strata = "SEX") %>%
      vr_plot(legend.position = "bottom") %>%
      add_CI() %>%
      add_CNSR(shape = 3, size = 2) %>%
      add_risktable(min_at_risk = 5,
                    display= c("n.risk", "n.censor"),
                    title = c("At risk", "Censored")
                   )
   ## add style
    gg %>%
       style_visR()

 
#### Hazard Ratio ####
    
   adtte %>%
       vr_KM_est(strata = "SEX") %>%
       vr_plot() %>%
       add_COX_HR() 
  
   
#### Quantiles ####
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
   
   
#### plotly ####
   survfit_object <- vr_KM_est(data = adtte, strata = "TRTP")
   
   vr_plotly.default(survfit_object) ## ggplotly
   vr_plotly.survfit(survfit_object) ## plot_ly

   
   
       
#### TODO
 ### Estimation ------
  ## rename vr_surv_est or even surv_est, surv_plot
  ## KM estimation + cumhaz via Nelson-Aalen.
  ## cumhaz via Nelson-Aalen => survival via exp(cumhaz) = fh estimator. Exponentiation with correction for tied events (fh2)
   # The MLE -log(S) is not estimated.
   # Should we accomodate this? If yes how to avoid interference with older methods eg plot.survfit? ctype can only have two values.
   # We could add a new argument chtype = 1/2 => pass to ctype , chtype = 3 => do -log(S)
   # we could allow ctype = 0. If zero we remove it from ... and add it on afterwards. How to document this?
   
 ### plotting vr_plot ------- 
  ## fun argument in vr_plot: validate transformations
   # allow for arbitrary function eg for % - fun = function(y) y*100
   # fun = cumhaz uses MLE estimator -log(S) instead of what was requested by user in survfit => how to approach this?
   
  ## pvalue: survdiff and trend test for multiple factor levels (adjusted p?) + location on plot [survMisc::comp]
  ## add median line
  ## create actual HR table
   
   
 ### plotting vr_plotly
  ## go with own construction vs ggplotly with some modification? ggplotly still under development
  ## Idea: Ristable => put n.censor/n.risk in tooltip, rather than creating a static table
  
   
 ### validate quantiles: compare with SAS
   
   
 ### AFT overlay
 ### COX PH 
   
