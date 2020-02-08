#' Returns a tidied tibble of a RBesT gMAP object
#'
#' @param x gMAP object 
#'
#' @return td tidied tibble
#' @export
#'
#' @examples
tidy_gMAP <- function(x){
  assertthat::assert_that(inherits(x, "gMAP"))
  td <- tibble::tibble(
    study = c("study 1", "study 1", "study 2", "study 2", "MEAN", "MAP"),
    type = c("study", "study", "study", "study", "estimate", "estimate"),
    estimate = c(0.1, 0.2, 0.1, 0.2, 0.1, 0.2),
    estimate.type = c("stratified", "map", "stratified", "map", "stratified", "map"),
    conf.low = c(0.05, 0.05, 0.05, 0.05, 0.05, 0.05),
    conf.high = c(0.15, 0.25, 0.15, 0.25, 0.15, 0.25)
  ) %>%
    dplyr::mutate(
      study_id = group_indices(., study),
      row_id = row_number()
      )
  return(td)
}

library(tidyverse)
map_crohn %>%
  tidy_gMAP() %>%
  ggplot(aes(x = row_id, y = estimate)) +
  geom_pointrange(aes(y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_point() +
  coord_flip()
  


prob <- 0.95
df <- as_tibble(map_crohn$data)
strat <- as.data.frame(map_crohn$est_strat(1-prob))
strat2 <- cbind(strat[1:2], median=strat$mean, strat[3:4])

# stratified model 
# is the median needed for stratified model
df_strat <- tibble(
  study = row.names(strat2),
  estimate = strat2$mean,
  se = strat$se,
  conf.low = strat[3],
  conf.high = strat[4],
  model = "stratified"
)


x <- map_crohn
low <- (1-prob)/2
up <- 1-low
fit <- as.data.frame(fitted(x, type="response", probs=c(0.5, low, up)))
fit

df_model <- tibble(
  study = row.names(fit),
  estimate = fit$mean,
  se = fit$sd,
  conf.low = strat[3],
  conf.high = strat[4],
  model = "meta"
)


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
  conf.low = pred_est3[4],
  conf.high = pred_est3[5],
  model = pred_est3$model
)

df_meta

### merge tibbles in to one
bind_rows(list(df_strat, df_model, df_meta), model)
rbind(df_strat, df_model, df_meta)

### this is the endpoint label
xlab_str <- switch(x$family$family,
                   gaussian="Response",
                   binomial="Response Rate",
                   poisson="Counting Rate")
