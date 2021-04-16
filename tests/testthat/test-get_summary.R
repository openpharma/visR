#' T1. The function accepts a survival object
#' T1.1 No error when `survfit_object` is a survfit object
#' T1.2 An error when `survfit_object` is a data.frame
#' T1.3 An error when `survfit_object` is a tibble
#' T1.4 An error when `survfit_object` is a data.table
#' T1.5 An error when `survfit_object` is a random object
#' T1.6 An error when `survfit_object` is NULL
#' T2. The function accepts a survival object
#' T2.1 An error when `statlist` is NULL
#' T2.2. An error when `statlist` is a random string
#' T2.3. The `strata` in the statlist of `get_summary` is the same as `strata` in the `survfit` object
#' T2.4. The `records` in the statlist of `get_summary` is the same as `n` in the `survfit` object
#' T2.5. The `events` in the statlist of `get_summary` is the same as events in the `survfit` object
#' T2.6. The `median` in the statlist of `get_summary` is the same as the median in the `survfit` object
#' T2.7. The `LCL` in the statlist of `get_summary` is the same as the lower confidence interval in the `survfit` object
#' T2.8. The `UCL` in the statlist of `get_summary` is the same as the upper confidence interval in the `survfit` object
#' T2.9. The `CI` in the statlist of `get_summary` is the same as the confidence interval in the `survfit` object

# Requirement T1 ----------------------------------------------------------

context("get_summary - T1. The function accepts a survival object")

testthat::test_that("T1.1. No error when `survfit_object` is a survfit object",{
  
  survfit_object <-  estimate_KM(adtte, strata = "TRTP")
  testthat::expect_error(visR::get_summary(survfit_object), NA)
  
})

testthat::test_that("T1.2. An error when `survfit_object` is a data.frame",{
  
  survfit_object <-  visR::adtte
  testthat::expect_error(visR::get_summary(survfit_object))
  
})

testthat::test_that("T1.3. An error when `survfit_object` is a tibble",{
  
  survfit_object <-  tibble::as_tibble(visR::adtte)
  testthat::expect_error(visR::get_summary(survfit_object))
  
})

testthat::test_that("T1.4. An error when `survfit_object` is a data.table",{
  
  survfit_object <-  data.table::as.data.table(visR::adtte)
  testthat::expect_error(visR::get_summary(survfit_object))
  
})

testthat::test_that("T1.5. An error when `survfit_object` is a random object",{
  
  survfit_object <-  "A"
  testthat::expect_error(visR::get_summary(survfit_object))
  
})

testthat::test_that("T1.6. An error when `survfit_object` is NULL",{
  
  survfit_object <-  NULL
  testthat::expect_error(visR::get_summary(survfit_object))
  
})


# Requirement T2 ----------------------------------------------------------

context("get_summary - T2. Correct info displayed for different statlists")

testthat::test_that("T2.1. An error when `statlist` is NULL",{
  
  survfit_object <-  estimate_KM(adtte, strata = "TRTP")
  testthat::expect_error(visR::get_summary(survfit_object, statlist = NULL))
  
})

testthat::test_that("T2.2. An error when `statlist` is a random string",{
  
  survfit_object <-  estimate_KM(adtte, strata = "TRTP")
  testthat::expect_error(visR::get_summary(survfit_object, statlist = "A"))
  
})

testthat::test_that("T2.3. The `strata` in the statlist of `get_summary` is the same as `strata` in the `survfit` object",{
  
  survfit_object <-  estimate_KM(adtte, strata = "TRTP")
  testthat::expect_equal(names(survfit_object$strata), 
                         unname(unlist(visR::get_summary(survfit_object, statlist = "strata"))))
  
})

testthat::test_that("T2.4. The `records` in the statlist of `get_summary` is the same as `n` in the `survfit` object",{
  
  survfit_object <-  estimate_KM(adtte, strata = "TRTP")
  testthat::expect_equal(survfit_object$n, 
                         unname(unlist(visR::get_summary(survfit_object, statlist = "records"))))
  
})

testthat::test_that("T2.5. The `events` in the statlist of `get_summary` is the same as events in the `survfit` object",{

  survfit_object <-  estimate_KM(adtte, strata = "TRTP")
  
  df <- data.frame(`n.events` = survfit_object$n.event, 
                   strata = rep(names(survfit_object$strata), 
                                survfit_object$strata))
  
  testthat::expect_equal(aggregate(n.events ~ strata, data=df, sum)$n.events, 
                         unname(unlist(visR::get_summary(survfit_object, statlist = c("events")))))
  
})

testthat::test_that("T2.6. The `median` in the statlist of `get_summary` is the same as the median in the `survfit` object",{
  
  survfit_object <-  estimate_KM(adtte, strata = "TRTP")
  
  df <- data.frame(surv = survfit_object$surv, 
                   time = survfit_object$time, 
                   strata = rep(names(survfit_object$strata), 
                                survfit_object$strata))
  
  suppressWarnings(inds <- aggregate(surv ~ strata, data=df, function(x){min(which(x <= 0.5))}))
  
  medians <- apply(inds, 1, function(ind){
    df[df$strata == as.character(ind[1]), ][as.numeric(ind[2]),"time"]
  })
  
  testthat::expect_equal(medians, 
                         unname(unlist(visR::get_summary(survfit_object, statlist = c("median")))))
  
})

testthat::test_that("T2.7. The `LCL` in the statlist of `get_summary` is the same as the lower confidence interval in the `survfit` object",{
  
  survfit_object <-  estimate_KM(adtte, strata = "TRTP")
  
  df <- data.frame(lower = survfit_object$lower, 
                   time = survfit_object$time, 
                   strata = rep(names(survfit_object$strata), 
                                survfit_object$strata))
  
  suppressWarnings(inds <- aggregate(lower ~ strata, data=df, function(x){min(which(x <= 0.5))}))
  
  lower <- apply(inds, 1, function(ind){
    df[df$strata == as.character(ind[1]), ][as.numeric(ind[2]),"time"]
  })
  
  testthat::expect_equal(lower, 
                         unname(unlist(visR::get_summary(survfit_object, statlist = c("LCL")))))
  
})

testthat::test_that("T2.8. The `UCL` in the statlist of `get_summary` is the same as the upper confidence interval in the `survfit` object",{
  
  survfit_object <-  estimate_KM(adtte, strata = "TRTP")
  
  df <- data.frame(surv = survfit_object$upper, 
                   time = survfit_object$time, 
                   strata = rep(names(survfit_object$strata), 
                                survfit_object$strata))
  
  suppressWarnings(inds <- aggregate(surv ~ strata, data=df, function(x){min(which(x <= 0.5))}))
  
  upper <- apply(inds, 1, function(ind){
    df[df$strata == as.character(ind[1]), ][as.numeric(ind[2]),"time"]
  })
  
  testthat::expect_equal(upper, 
                         unname(unlist(visR::get_summary(survfit_object, statlist = c("UCL")))))
  
})

testthat::test_that("T2.9. The `CI` in the statlist of `get_summary` is the same as the confidence interval in the `survfit` object",{
  
  survfit_object <-  estimate_KM(adtte, strata = "TRTP")
  
  df <- data.frame(lower = survfit_object$lower,
                   upper = survfit_object$upper, 
                   time = survfit_object$time, 
                   strata = rep(names(survfit_object$strata), 
                                survfit_object$strata))
  
  suppressWarnings(inds <- aggregate(upper ~ strata, data=df, function(x){min(which(x <= 0.5))}))
  upper <- apply(inds, 1, function(ind){
    df[df$strata == as.character(ind[1]), ][as.numeric(ind[2]),"time"]
  })
  
  suppressWarnings(inds <- aggregate(lower ~ strata, data=df, function(x){min(which(x <= 0.5))}))
  lower <- apply(inds, 1, function(ind){
    df[df$strata == as.character(ind[1]), ][as.numeric(ind[2]),"time"]
  })
  
  ci <- sapply(1:length(lower), function(i){
    paste0("(", lower[i], ";", upper[i], ")")
  })
  
  testthat::expect_equal(ci, 
                         unname(unlist(visR::get_summary(survfit_object, statlist = c("CI")))))
  
  
  
})

