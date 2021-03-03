#' T1. The function accepts a survival object and ggsurvfit object
#' T1.1 No error when `survfit_object` is a survfit object
#' T1.1 No error when `gg` is a ggsurvfit object
#' T1.2 An error when `survfit_object`/`gg` is a data.frame
#' T1.3 An error when `survfit_object`/`gg` is a tibble
#' T1.4 An error when `survfit_object`/`gg` is a data.table
#' T1.5 An error when `survfit_object`/`gg` is a random object
#' T1.6 An error when `survfit_object`/`gg` is NULL
#' T2. The function accepts elements `n.risk`, `n.event`, and `n.censor` is part of the `statlist`
#' T2.1 No error when `n.risk` is part of the `statlist`
#' T2.2 No error when `n.event` is part of the `statlist`
#' T2.3 No error when `n.censor` is part of the `statlist`
#' T2.4 An error when a random string is part of the `statlist`
#' T3.`labels` and `statlist` elements are correctly related
#' T3.1. Label and value ordering kept
#' T3.2. Correct labels are assigned also when ordering of statlist is changed
#' T3.3. Correct labels are assigned also when labels are shorter
#' T4.Test `breaks` argument
#' T4.1. `breaks` are correctly calculated
#' T4.2. `breaks` are correctly pulled from ggplot
#' T4.3. An error if `breaks` are 0
#' T4.4. An error if `breaks` are not a number
#' T5.Test `group` argument
#' T6.Test `collapse` argument

testthat::test_that("T3.1. Label and value ordering kept",{
  
  survfit_object <- adtte %>% estimate_KM(strata = "TRTP")
  rt <- get_risktable(survfit_object, min_at_risk = 10,
                      statlist= c("n.risk", "n.event", "n.censor"),
                      label = c("At risk", "Event", "Censor"),
                      breaks = 9,
                      group="strata",
                      collapse = F)

  testthat::expect_equal(as.character(rt[rt$time==0,]$y_values), c("At risk", "Event", "Censor"))
  testthat::expect_equal(rt[rt$time==0,]$Placebo , c(86, 0, 0))
  
})

testthat::test_that("T3.2. Correct labels are assigned also when ordering of statlist is changed",{
  
  survfit_object <- adtte %>% estimate_KM(strata = "TRTP")
  rt <- get_risktable(survfit_object, min_at_risk = 10,
                      statlist= c("n.event", "n.risk", "n.censor"),
                      breaks = 9,
                      group="strata",
                      collapse = F)
  
  testthat::expect_equal(as.character(rt[rt$time==0,]$y_values), c("At risk", "Events", "Censored"))
  testthat::expect_equal(rt[rt$time==0,]$Placebo , c(86, 0, 0))
  
})

testthat::test_that("T3.3. Correct labels are assigned also when labels are shorter",{
  
  survfit_object <- adtte %>% estimate_KM(strata = "TRTP")
  rt <- get_risktable(survfit_object, min_at_risk = 10,
                      statlist= c("n.risk", "n.event", "n.censor"),
                      label = c("At risk", "Don't care"),
                      breaks = 10,
                      group="strata",
                      collapse = F)
  
  testthat::expect_equal(as.character(rt[rt$time==0,]$y_values), c("At risk", "Don't care", "Censored"))
  testthat::expect_equal(rt[rt$time==0,]$Placebo , c(86, 0, 0))
  
})
