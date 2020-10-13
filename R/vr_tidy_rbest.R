#' Returns a tidied RBesT gMAP object
#' 
#' @description
#' This is an experimental function that may be developed over time. 
#' 
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#' @param x RBesT gMAP object
#' @param prob probability range for uncertainty interval
#'
#' @return td tidied tibble
#'
#' @importFrom stats as.formula filter fitted lag median quantile reorder sd time
#' @importFrom  utils write.table
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Commenting out to not depend on RBest - issue with cran availability
#' # library(RBesT)
#' # example(crohn)
#' # vr_tidy_rbest(map_crohn)
#'
#' # map_crohn %>%
#' # vr_tidy_rbest()
#' }
#'
vr_tidy_rbest <- function(x, prob = 0.95){

  assertthat::assert_that(inherits(x, "gMAP"))
  td <- tibble::tibble()
  low <- (1-prob)/2
  up <- 1-low

  #--------------------------------------------
  # stratified model

  strat <- as.data.frame(x$est_strat(1-prob))
  strat2 <- cbind(strat[1:2], median=strat$mean, strat[3:4])

  df_strat <- tibble::tibble(
    study = row.names(strat2),
    estimate = strat2$mean,
    se = strat$se,
    conf.low = strat[,3],
    conf.high = strat[,4],
    model = "stratified"
  )

  #----------------------------------------------
  #  fitted meta model

  fit <- as.data.frame(fitted(x, type="response", probs=c(0.5, low, up)))

  df_model <-  tibble::tibble(
    study = row.names(fit),
    estimate = fit$mean,
    se = fit$sd,
    conf.low = strat[,3],
    conf.high = strat[,4],
    model = "meta"
  )

  #------------------------------------------
  # model averaged study estimates

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

  df_meta <-  tibble::tibble(
    study = pred_est3$study,
    estimate = pred_est3$mean,
    se = pred_est3$sd,
    conf.low = pred_est3[,4],
    conf.high = pred_est3[,5],
    model = pred_est3$model
  )

  #------------------------------------------
  # merge tibbles in to one
  # provide index for study

  td <- rbind(df_strat, df_model, df_meta) %>%
    tidyr::as_tibble() %>%
    dplyr::mutate(
      study_id = dplyr::group_indices(., study),
      row_id = dplyr::row_number()
    )

  #------------------------------------------
  # retrun tidied tibble
  return(td)
}
