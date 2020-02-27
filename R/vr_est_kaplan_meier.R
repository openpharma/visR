#' Title
#'
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
vr_est_kaplan_meier <- function(data) {
  
    survfit_object <- survival::survfit(
      survival::Surv(time, status) ~ trt, data = data
      )
    
    broom_object <- broom::tidy(survfit_object)
    return(broom_object)
}
