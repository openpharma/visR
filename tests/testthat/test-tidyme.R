#' @title tidyme
#' @section Last updated by:
#' Tim Treis
#' @section Last update date:
#' 19-MAR-2021

# Specifications ---------------------------------------------------------------

#' T1. The function tidies up an associated object
#' T1.1 The default method throws a message and relies on broom::tidy
#' T1.2 The default method returns a tibble, tidied with broom
#' T2. The S3 method, associated with a survival object, returns a tidied survival object
#' T2.1 The S3 method, associated with a survival object, transforms the survival object into a data frame
#' T2.2 The tidied dataframe has the same content as the original survival object

# Requirement T1 ---------------------------------------------------------------

context("tidyme - T1. The function tidies up an associated object")

testthat::test_that("T1.1 The default method throws a message and relies on broom::tidy",{
  
  lm_object <- stats::lm(data = adtte, TRTDUR ~ AVAL)
  
  testthat::expect_message(visR::tidyme(lm_object))
  
})

testthat::test_that("T1.2 The default method returns a tibble, tidied with broom",{
  
  lm_object <- stats::lm(data = adtte, TRTDUR ~ AVAL)
  testthat::expect_s3_class(lm_object, "lm")
  
  lm_tidied <- suppressMessages(visR::tidyme(lm_object))
  testthat::expect_s3_class(lm_tidied, "tbl")
  
  lm_broomed <- broom::tidy(lm_object)
  testthat::expect_identical(lm_tidied, lm_broomed)
  
})


# Requirement T2 ---------------------------------------------------------------

context("tidyme - T2. The S3 method, associated with a survival object, returns a tidied survival object")

testthat::test_that("T2.1 The S3 method, associated with a survival object, transforms the survival object into a data frame",{
  
  surv_object <- survival::survfit(data = adtte, Surv(AVAL, 1-CNSR) ~ TRTP)
  surv_object_tidied <- visR::tidyme(surv_object)
  
  testthat::expect_s3_class(surv_object_tidied, "data.frame")
  
})

testthat::test_that("T2.2 The tidied dataframe has the same content as the original survival object",{
  
  surv_object <- survival::survfit(data = adtte, survival::Surv(AVAL, 1-CNSR) ~ TRTP)
  
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
  
  surv_object_tidied <- visR::tidyme(surv_object)

  surv_object_tidied_common <- surv_object_tidied[, base::intersect(colnames(surv_object_tidied), colnames(surv_object_df))] 
  
  testthat::expect_equal(surv_object_df, surv_object_tidied_common)
  
})


# T3. adds more info compared to classifical tidy method 
# T3.1 Adds call, ......

#   surv_object_df["call"] <- capture.output(surv_object$call)
#   surv_object_df["strata"] <- base::summary(surv_object, censored = T)$strata
#   n_per_strata_table <- base::summary(surv_object, censored = T)$table %>%
#     data.frame()
#   n_per_strata <- n_per_strata_table$records
#   n_per_strata <- setNames(as.list(n_per_strata), rownames(n_per_strata_table))
#   
#   n_strata_list <- c()
#   
#   for (strata in surv_object_df$strata) {
#     
#     n_strata_list <- c(n_strata_list, n_per_strata[[strata]])
#     
#   }
#   
#   surv_object_df["n.strata"] <- n_strata_list
#   
#   surv_object_df <- surv_object_df[with(surv_object_df, base::order(surv)),] 
# 
#   surv_object_tidied <- visR::tidyme(surv_object)
#   
#   surv_object_tidied$call <- base::as.character(surv_object_tidied$call)
#   surv_object_tidied <- surv_object_tidied %>% as.data.frame() 
#   surv_object_tidied <- surv_object_tidied[with(surv_object_tidied, base::order(surv)),] 
#   surv_object_tidied["strata"] <- as.factor(surv_object_tidied$strata)
#   

# END OF CODE ------------------------------------------------------------------