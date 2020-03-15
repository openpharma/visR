#' Create Tidy Survival Object
#'
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
vr_est_kaplan_meier <- function(data, equation) {
  
    survfit_object <- survival::survfit(
      eval(parse(text=equation)), data = data
      )
    
    broom_object <- broom::tidy(survfit_object)
    return(broom_object)
}
