#' Create a Survival Object from CDISC Data
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Create a survival object (e.g. `survival::Surv()`), which is usually used as
#' the response variable in survival models.
#' The function is designed to leverage the
#' [CDISC ADaM ADTTE data model](https://www.cdisc.org/standards/foundational/adam/adam-basic-data-structure-bds-time-event-tte-analyses-v1-0)
#' and more explicitly the conventions and controlled vocabulary of the data model.
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

  # checking inputs ------------------------------------------------------------
  if (!is.numeric(AVAL) || !is.numeric(CNSR))
    stop("Expecting arguments 'AVAL' and 'CNSR' to be numeric.")

  if (stats::na.omit(CNSR) %>% setdiff(c(0, 1)) %>% {!rlang::is_empty(.)})
    stop("Expecting 'CNSR' argument to be binary with values `0/1`.")

  if (any(AVAL < 0))
    warning("Values of 'AVAL' are less than zero, which is likely a data error.")

  # pass args to `survival::Surv()` --------------------------------------------
  survival::Surv(time = AVAL, event = 1 - CNSR, type = "right", origin = 0)
}
