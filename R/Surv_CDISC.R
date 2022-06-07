#' Create a Survival Object from CDISC Data
#'
#' @description
#' Create a survival object, usually used as a response variable in a model
#' formula. The arguments are named after the CDISC data standard.
#' The `CNSR` argument expects a numeric column/vector that indicates
#' an observation is censored.
#'
#' The `AVAL` and `CNSR` arguments are passed to
#' `survival::Surv(time = AVAL, event = 1 - CNSR, type = 'right', origin = 0)`.
#'
#' @param AVAL The follow-up time. The follow-up time is assumed to originate from zero.
#' When no argument is passed, the default value is a column/vector named `AVAL`
#' @param CNSR The censoring indicator where `1=censored` and `0=death/event`.
#' When no argument is passed, the default value is a column/vector named `CNSR`
#'
#' @return Object of class 'Surv'
#' @seealso [`survival::Surv()`], [`estimate_KM()`]
#' @export
#'
#' @examples
#' library(survival)
#'
#' survfit(Surv_CDISC() ~ SEX, data = adtte)
#' adtte %>%
#'   estimate_KM(formula = Surv_CDISC() ~ SEX)
#'
#' # When using CDSIC data, you can specify arguments or rely on defaults
#' with(adtte, Surv_CDISC(AVAL, CNSR)) %>% head()
#' with(adtte, Surv_CDISC()) %>% head()

Surv_CDISC <- function(AVAL, CNSR) {
  # set default values if not passed by user -----------------------------------
  if (missing(AVAL) && exists("AVAL", envir = rlang::caller_env()))
    AVAL <- get("AVAL", envir = rlang::caller_env())
  else if (missing(AVAL))
    stop("Default 'AVAL' value not found. Specify argument in `Surv_CDISC(AVAL=)`.")
  if (missing(CNSR) && exists("CNSR", envir = rlang::caller_env()))
    CNSR <- get("CNSR", envir = rlang::caller_env())
  else if (missing(CNSR))
    stop("Default 'CNSR' value not found. Specify argument in `Surv_CDISC(CNSR=)`.")

  # pass args to `survival::Surv()` --------------------------------------------
  survival::Surv(time = AVAL, event = 1 - CNSR, type = "right", origin = 0)
}
