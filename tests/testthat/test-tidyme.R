#' @title tidyme
#' @section Last updated by:
#' Tim Treis
#' @section Last update date:
#' 19-MAR-2021

# Specifications ---------------------------------------------------------------

#' T1.  The function tidies up an associated object
#' T1.1 The default method returns a tibble, tidies with broom
#' T1.2 The default method does not affect the content of the object, f.e. `visR::tidyme(stats::lm())`
#' T1.3 The default method throws a message and relies on broom::tidy
#' T2   The S3 method, associated with a survival object, returns a tidied survival object
#' T2.1 The S3 method, associated with a survival object, transforms the survival object into a data frame
#' T2.2 The tidied dataframe has the same content as the original survival object

# Requirement T1 ---------------------------------------------------------------

context("tidyme - The function tidies up an associated object")

testthat::test_that("T1.1 The default method returns a tibble, tidies with broom",{
  
  lm_object <- stats::lm(data = adtte, TRTDUR ~ AVAL)
  testthat::expect_s3_class(lm_object, "lm")
  
  lm_tidied <- suppressMessages(visR::tidyme(lm_object))
  testthat::expect_s3_class(lm_tidied, "tbl")
  
  lm_broomed <- broom::tidy(lm_object)
  testthat::expect_identical(lm_tidied, lm_broomed)
  
})

testthat::test_that("T1.2 The default method does not affect the content of the object, f.e. `visR::tidyme(stats::lm())`",{
  
  lm_object <- stats::lm(data = adtte, TRTDUR ~ AVAL)
  lm_tidied <- suppressMessages(visR::tidyme(lm_object))
  
  lm_object_values <- coefficients(summary(lm_object)) %>% as.matrix()
  
  stats_colnames <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  broom_colnames  <- c("estimate", "std.error", "statistic",  "p.value")
  
  for (i in 1:length(stats_colnames)) {
    
    for (j in 1:2) {
      
      testthat::expect_equal(lm_object_values[j, stats_colnames[i]],
                             lm_tidied[j, broom_colnames[i]][[1]]) 
      
    }
    
  }
  
})

testthat::test_that("T1.3 The default method throws a message and relies on broom::tidy",{
  
  lm_object <- stats::lm(data = adtte, TRTDUR ~ AVAL)
  
  testthat::expect_message(visR::tidyme(lm_object))
  
})

# Requirement T2 ---------------------------------------------------------------

context("tidyme - The S3 method, associated with a survival object, returns a tidied survival object")

testthat::test_that("T2.1 The S3 method, associated with a survival object, transforms the survival object into a data frame",{
  
  surv_object <- survival::survfit(data = adtte, Surv(AVAL, 1-CNSR) ~ TRTP)
  testthat::expect_s3_class(surv_object, "survfit")
  
  surv_object_tidied <- visR::tidyme(surv_object)
  testthat::expect_s3_class(surv_object_tidied, "data.frame")
  
})

testthat::test_that("T2.2 The tidied dataframe has the same content as the original survival object",{
  
  surv_object <- survival::survfit(data = adtte, Surv(AVAL, 1-CNSR) ~ TRTP)
  
  # Shape fit into df
  surv_object_df <- base::with(surv_object, data.frame(time, 
                                                       n.risk,
                                                       n.event,
                                                       n.censor,
                                                       surv,
                                                       std.err,
                                                       cumhaz,
                                                       std.chaz,
                                                       type,
                                                       logse,
                                                       conf.int,
                                                       conf.type,
                                                       lower,
                                                       upper))
  
  surv_object_df["call"] <- capture.output(surv_object$call)
  surv_object_df["strata"] <- base::summary(surv_object, censored = T)$strata
  n_per_strata_table <- base::summary(surv_object, censored = T)$table %>%
    data.frame()
  n_per_strata <- n_per_strata_table$records
  n_per_strata <- setNames(as.list(n_per_strata), rownames(n_per_strata_table))
  
  n_strata_list <- c()
  
  for (strata in surv_object_df$strata) {
    
    n_strata_list <- c(n_strata_list, n_per_strata[[strata]])
    
  }
  
  surv_object_df["n.strata"] <- n_strata_list
  
  surv_object_df <- surv_object_df[with(surv_object_df, order(surv)),] 

  surv_object_tidied <- visR::tidyme(surv_object)
  
  surv_object_tidied$call <- base::as.character(surv_object_tidied$call)
  surv_object_tidied <- surv_object_tidied %>% as.data.frame() 
  surv_object_tidied <- surv_object_tidied[with(surv_object_tidied, order(surv)),] 
  surv_object_tidied["strata"] <- as.factor(surv_object_tidied$strata)
  
  testthat::expect_equal(surv_object_df, surv_object_tidied)
  
})

# END OF CODE ------------------------------------------------------------------