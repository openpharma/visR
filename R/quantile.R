### Some options that we need to validate

   ## 50pct is flat but not at the end => take halfway plateau
   testq1 <- data.frame(AVAL = c(1,2,3,4),
                       CNSR = c(0,0,0,0))
   
   survfit_object <- vr_KM_est(data = testq1)  
   vr_plot(survfit_object)

   ## 50pct is point: take time corresponding to point
   testq2 <- data.frame(AVAL = c(1,2,2,4),
                       CNSR = c(0,0,0,0))
   
   survfit_object <- vr_KM_est(data = testq2)  
   vr_plot(survfit_object)
   

   ## 50pct is not existing and 75 is last => 50 and 25 NA should be NA
   testq3 <- data.frame(AVAL = c(1,2,2,4),
                       CNSR = c(0,1,1,1))
   
   survfit_object <- vr_KM_est(data = testq3)  
   vr_plot(survfit_object)
   
   quantile(survfit_object)

   
   
   ?survival:::quantile.survfit
   
   
get_quantile <- function(x, ...){
  UseMethod("get_quantile")
} 

get_quantile.survfit <- function(  x
                                 , probs = c(0.25,0.50,0.75)
                                 , conf.int = TRUE
                                 , tolerance = sqrt(.Machine$double.eps)
                                )
{
  if(! inherits(x, "survfit")) stop("x is not of class `survfit`.")
  
  q <- quantile(x, probs = probs, conf.int = conf.int, tolerance = tolerance, type = 3)
  qdf <- do.call(rbind.data.frame, q)
  qdf$strata <- unlist(lapply(strsplit(rownames(qdf), "=", fixed=FALSE), `[[`, 2))
  qdf$quantity <- unlist(lapply(strsplit(rownames(qdf), "\\.", fixed=FALSE), `[[`, 1))
  
  

  ## focus on fun = surv, but we could do this for more funs
  ## get maximal 1-S(t)
  max_quantile <- tidyme.survfit(x) %>%
    select(strata, surv,lower, upper) %>%
    group_by(strata) %>%
    summarize_if(is.numeric, ~ 1-min(.))%>%
    mutate(strata = unlist(lapply(strsplit(strata, "=", fixed=FALSE), `[[`, 2))) %>%
    rename(quantile = surv)%>%
    pivot_longer(-strata, names_to = "quantity", values_to = "max")

  # if eg surv placebo  <= 25 => quantile should be NA
  
  correction <- qdf %>%
    left_join(max_quantile, by = c("strata", "quantity"))

   probs[1] > correction$max
  
}



   