vr_est_kaplan_meier <- function(data) {
    survfit_object <- survfit(Surv(time, status) ~ trt, data = data)
    broom_object <- tidy(survfit_object)
    return(broom_object)
}
