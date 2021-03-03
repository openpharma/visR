sex <- c("F", "F", "M", "M", "F", "M")
strat <- factor(c("s1", "s2", "s1", "s2", "s1", "s1"))
dose <- c(3,5,2,4,8.51145,7)
dat_test <- data.frame(sex, strat, dose)
t1 = get_tableone(dat_test)
t2 = get_tableone(dat_test, strata = "strat")

equalise_precision_in_string <- function(string, precision=2) {
  
  # Didn't want to modify the rounding precision in visR::get_tableone, so I'm
  # rounding on strings here. 
  # https://stackoverflow.com/a/49996635/10169453
  
  
  suppressWarnings({
    rounded_string <- strsplit(x = string,
                               split = "(?<=[^0-9.])(?=\\d)|(?<=\\d)(?=[^0-9.])",
                               perl=T) %>%
      unlist %>% 
      lapply(function(x){if(!is.na(as.numeric(x))) {
                             x <- round(as.numeric(x),precision) %>%
                                      format(nsmall=precision)
                             x}}) %>%
      paste(collapse="")
  })
  
  return(rounded_string)
  
}

testfun_mean <- function(x){
  num <- paste0(round(mean(x),2), 
                " (", 
                round(sd(x),2), 
                ")")
  paste0(equalise_precision_in_string(num))
}

testfun_median <- function(x){
   num <- paste0(round(median(x),2), 
                 " (", 
                 round(quantile(x, probs=0.25, na.rm = TRUE),2), 
                 "-",
                 round(quantile(x, probs=0.75, na.rm = TRUE),2), 
                 ")")
   paste0(equalise_precision_in_string(num))
}

testfun_range <- function(x){
  num <- paste0(round(min(x),2), 
                "-", 
                round(max(x),2))
  paste0(equalise_precision_in_string(num))
}




test_that("Correct Variable labels", {
  expect_equal(unique(t1$variable), c("Sample", "sex", "strat", "dose"))
})

test_that("summary numeric - overall", {
  x <- dose
  values=t1[t1$variable == "dose", "Total"]
  expect_equal(testfun_mean(x), equalise_precision_in_string(as.character(values[1,])))
  expect_equal(testfun_median(x), equalise_precision_in_string(as.character(values[2,])))
  expect_equal(testfun_range(x), equalise_precision_in_string(as.character(values[3,])))
  warning("Not implemented; missing values??")
})

test_that("summary numeric - strata", {
  x_1 <- dat_test[dat_test$strat == "s1", "dose"]
  values_s1 <- t2[t2$variable == "dose", "s1"]
  expect_equal(testfun_mean(x_1), equalise_precision_in_string(as.character(values_s1[1,])))
  expect_equal(testfun_median(x_1), equalise_precision_in_string(as.character(values_s1[2,])))
  expect_equal(testfun_range(x_1), equalise_precision_in_string(as.character(values_s1[3,])))
  
  x_2 <- dat_test[dat_test$strat == "s2", "dose"]
  values_s2 <- t2[t2$variable == "dose", "s2"]
  expect_equal(testfun_mean(x_2), equalise_precision_in_string(as.character(values_s2[1,])))
  expect_equal(testfun_median(x_2), equalise_precision_in_string(as.character(values_s2[2,])))
  expect_equal(testfun_range(x_2), equalise_precision_in_string(as.character(values_s2[3,])))
  
  warning("Not implemented; missing values??")
})

test_that("summary factor", {
  n_s1=sum(strat == "s1")
  n_s2=sum(strat == "s2")
  expect_equal(as.character(t1[t1$variable == "strat" & t1$statistic == "s1", "Total"]), 
               paste0(n_s1, " (", round((n_s1/(n_s1+n_s2))*100,1), "%)"))
  expect_equal(as.character(t1[t1$variable == "strat" & t1$statistic == "s2", "Total"]), 
               paste0(n_s2, " (", round((n_s2/(n_s1+n_s2))*100,1), "%)"))
})
  
test_that("summary character", {
  warning("Not implemented; missing values??")
})       



## Test tableone
# Simple data frame for testing
# summary
# Numerics -> mean/median
# Factors -> N/percent
# Chars -> Unique values / missing (percent)
# Test create_tableone einmal mit beiden summary functions
# Treat "labelled" class for summary