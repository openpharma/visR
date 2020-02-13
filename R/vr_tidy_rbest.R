#' Returns a tidied tibble of a RBesT gMAP object
#'
#' @param x gMAP object 
#'
#' @return td tidied tibble
#' @export
#'
#' @examples
tidy_gMAP <- function(x, prob = 0.95){
  
  assertthat::assert_that(inherits(x, "gMAP"))
  td <- tibble::tibble()
  low <- (1-prob)/2
  up <- 1-low
  
#--------------------------------------------  
  # stratified model 
  # is the median needed for stratified model

  strat <- as.data.frame(x$est_strat(1-prob))
  strat2 <- cbind(strat[1:2], median=strat$mean, strat[3:4])
  
  df_strat <- tibble(
    study = row.names(strat2),
    estimate = strat2$mean,
    se = strat$se,
    conf.low = strat[,3],
    conf.high = strat[,4],
    model = "stratified"
  )

#---------------------------------------------- 
##  fitted meta model 
  
  fit <- as.data.frame(fitted(x, type="response", probs=c(0.5, low, up)))
  
  df_model <- tibble(
    study = row.names(fit),
    estimate = fit$mean,
    se = fit$sd,
    conf.low = strat[,3],
    conf.high = strat[,4],
    model = "meta"
  )
  
#------------------------------------------  
  
  pred_est <- as.data.frame(
    do.call(
      rbind, 
      summary(x,probs=c(0.5, low, up), type="response")[c("theta.pred", "theta")]
    )
  )
  pred_est2 <- transform(pred_est,  study=c("MAP", "Mean") , model="meta")
  
  est = c("both", "MAP", "Mean", "none")
  pred_est3 <- pred_est2[c("MAP", "Mean") %in% est,]
  pred_est3 
  
  df_meta <- tibble(
    study = pred_est3$study,
    estimate = pred_est3$mean,
    se = pred_est3$sd,
    conf.low = pred_est3[,4],
    conf.high = pred_est3[,5],
    model = pred_est3$model
  )

  #------------------------------------------
  ## merge tibbles in to one
  ## provide index for study
  
  td <- rbind(df_strat, df_model, df_meta) %>%
    as_tibble() %>%
    dplyr::mutate(
      study_id = group_indices(., study),
      row_id = row_number()
      )
  return(td)
}


# TODO: 
# 1. add estimate type 
# 2. add raw data i.e. n, y, etc. 
# 3. think about glue information to y to make more informative labels
# 4. think about how to plot forest plot in different ways
# 5. meta data arounf endpoint type, title, subtitles, data source, time executred, etc. 
# 6. function write to ARD. 
#     - could the meta data be a tibble with the estimates a tibble to keep table strucutre. 
#     - the overarching tibble would contain meta data and unique analysis id. 

library(tidyverse)

tidy_gMAP(map_crohn)


map_crohn %>%
  tidy_gMAP() %>%
  ggplot(aes(x = row_id, y = estimate)) +
  geom_pointrange(aes(y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_point() +
  coord_flip()
  



### this is the endpoint label - 
## TODO: BUILD THIS IN TO META DATA
xlab_str <- switch(x$family$family,
                   gaussian="Response",
                   binomial="Response Rate",
                   poisson="Counting Rate")



map_crohn %>%
  tidy_gMAP() %>%
  ggplot(aes(
    x = study,
    y = estimate,
    ymin = conf.low,
    ymax = conf.high,
    colour = model,
    group = model
  )) +
  geom_pointrange(show.legend = FALSE,
                  position = "dodge",
                  width = 1) +
  coord_flip() +
  #  facet_wrap( ~ model) +
  theme_light()

position_dodge(width = 1)




library(RBesT)
library(tidyverse)
library(ggplot2)
example(crohn)
print(map_crohn)
str(map_crohn)
forest_plot(map_crohn)


ddf <- map_crohn %>%
  tidy_gMAP()

map_crohn %>%
  tidy_gMAP() %>%
  ggplot(aes(y = row_id, x = estimate, label = study)) +
  geom_point() +
  geom_text()



###
# adjust the base font size
theme_set(theme_default(base_size=16))
forest_plot(map_crohn, model="both", est="MAP", size=1) + legend_move("right") +
  labs(title="Forest plot", subtitle="Results of Meta-Analytic-Predictive (MAP) analysis", 
       caption="Plot shows point estimates (posterior medians) with 95% intervals")

### what is the response to be -125?
