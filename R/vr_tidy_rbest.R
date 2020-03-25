#' Tidy RBesT gMAP object
#' 
#' This function takes a gMAP object from a meta or evidence 
#' synthesis analysis and returns a tidy data frame (tibble).
#' 
#'  TODO: 
#'  * Provide more detail for the function
#'  * Provide the data model
#'
#' @param gmap RBesT gMAP object to be tidied for reporting
#' @param prob probability range for the uncertainty interval. Input is a value between [0, 1]. Defailt is a 95% uncertainaty interval.
#'
#' @return tidied tibble based on the broom data model. 
#' The current data model for the tibble includes the following variables:
#' * row_id 
#' * study_id
#' * study    
#' * estimate    
#' * se 
#' * conf.low 
#' * conf.high model
#' 
#'
#'  @export
#'
#' @examples
#' library(dplyr)
#' library(RBesT)
#' library(tidyverse)
#' 
#' example(crohn)
#' vr_tidy_rbest(map_crohn)
#' 
#' map_crohn %>% 
#'   vr_tidy_rbest() 
#' 
vr_tidy_rbest <- function(gmap, prob = 0.95){
  
  assertthat::assert_that(inherits(gmap, "gMAP"))
  td <- tibble::tibble()
  low <- (1 - prob)/2
  up <- 1 - low
  
  #--------------------------------------------  
  # stratified model
  # obtain stratified estimates
  # also initialise meta-data around study label and study id
  
  strat <- as.data.frame(gmap$est_strat(1 - prob))
  strat2 <- cbind(strat[1:2], median = strat$mean, strat[3:4])
  
  df_strat <- tibble(
    study = row.names(strat2),
    estimate = strat2$mean,
    se = strat$se,
    conf.low = strat[,3],
    conf.high = strat[,4],
    model = "stratified"
  ) 


  #---------------------------------------------- 
  #  fitted meta model 
  
  fit <- as.data.frame(fitted(gmap, type = "response", probs = c(0.5, low, up)))
  
  df_model <- tibble(
    study = row.names(fit),
    estimate = fit$mean,
    se = fit$sd,
    conf.low = fit[, 4],
    conf.high = fit[, 5],
    model = "meta"
  ) 
  
  
  #------------------------------------------  
  # obtain model averaged study estimates
  
  pred_est <- as.data.frame(
    do.call(
      rbind, 
      summary(gmap,probs = c(0.5, low, up), type = "response")[c("theta.pred", "theta")]
    )
  )
  
  pred_est2 <- transform(pred_est,  study = c("MAP", "Mean") , model = "meta")
  
  est = c("both", "MAP", "Mean", "none")
  pred_est3 <- pred_est2[c("MAP", "Mean") %in% est,]
  
  
  df_meta <- tibble(
    study = as.character(pred_est3$study),
    estimate = pred_est3$mean,
    se = pred_est3$sd,
    conf.low = pred_est3[, 4],
    conf.high = pred_est3[, 5],
    model = pred_est3$model
  )
  

  
  #--------------------------------------------  
  # obtain and set up meta-data around study label and study id
  # assumption - use order of studies to set the id
  # initialise the label to be study
  
  study_meta <- df_strat %>%
    dplyr::select(study) %>%
    dplyr::mutate(
      study.type = "trial",
      study.type.id = 1
    )
  
  study_summary <- df_meta %>% 
    dplyr::select(study) %>%
    dplyr::mutate(
      study.type = "combined",
      study.type.id = 2
    )
  
  study_meta <-
    dplyr::bind_rows(study_meta, study_summary) %>%
    dplyr::arrange(study.type.id, study, .by_group = TRUE) %>%
    dplyr::mutate(
      study.id = dplyr::group_indices(., study),
      study.label = study
    )

  #------------------------------------------
  # merge tibbles in to one
  # provide index for study
  # capture remaining meta-data for broom data model
  
  td <- rbind(df_strat, df_model, df_meta) %>%
    tidyr::as_tibble() %>% 
    dplyr::left_join(study_meta, by = "study")  %>%
    dplyr::arrange(study.id, model, .by_group = TRUE) 

  #------------------------------------------
  # retrun tidied tibble
  
  return(td)
}
