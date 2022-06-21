#' Create a Survival Object from CDISC Data
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' The aim of `Surv_CNSR()` is to map the inconsistency in convention between
#' the [survival](https://cran.r-project.org/package=survival) package and
#' [CDISC ADaM ADTTE data model](https://www.cdisc.org/standards/foundational/adam/adam-basic-data-structure-bds-time-event-tte-analyses-v1-0).
#'
#' The function creates a survival object (e.g. `survival::Surv()`) that
#' uses CDISC ADaM ADTTE coding conventions and converts the arguments to the
#' status/event variable convention used in the
#' [survival](https://cran.r-project.org/package=survival) package.
#'
#' The `AVAL` and `CNSR` arguments are passed to
#' `survival::Surv(time = AVAL, event = 1 - CNSR, type = "right", origin = 0)`.
#'
#' @section Details:
#'
#' The `Surv_CNSR()` function creates a survival object utilizing the
#' expected data structure in the CDISC ADaM ADTTE data model,
#' mapping the CDISC ADaM ADTTE coding conventions with the expected
#' status/event variable convention used in the survival package---specifically,
#' the coding convention used for the status/event indicator.
#' The survival package expects the status/event indicator in the
#' following format: `0=alive`, `1=dead`. Other accepted choices are
#' `TRUE`/`FALSE` (`TRUE = death`) or `1`/`2` (`2=death`).
#' A final but risky option is to omit the indicator variable, in which case
#' all subjects are assumed to have an event.
#'
#' The CDISC ADaM ADTTE data model adopts a different coding convention for
#' the event/status indicator. Using this convention, the event/status variable
#' is named `'CNSR'` and uses the following coding: `censor = 1`, `status/event = 0`.
#'
#' @param AVAL The follow-up time. The follow-up time is assumed to originate from zero.
#' When no argument is passed, the default value is a column/vector named `AVAL`.
#' @param CNSR The censoring indicator where `1=censored` and `0=death/event`.
#' When no argument is passed, the default value is a column/vector named `CNSR`.
#'
#' @return Object of class 'Surv'
#' @seealso [`survival::Surv()`], [`estimate_KM()`]
#' @export
#'
#' @examples
#' # Use the `Surv_CNSR()` function with visR functions
#' adtte %>%
#'   visR::estimate_KM(formula = visR::Surv_CNSR() ~ SEX)
#'
#' # Use the `Surv_CNSR()` function with functions from other packages as well
#' survival::survfit(visR::Surv_CNSR() ~ SEX, data = adtte)
#' survival::survreg(visR::Surv_CNSR() ~ SEX + AGE, data = adtte) %>%
#'   broom::tidy()
Surv_CNSR <- function(AVAL, CNSR) {
  # set default values if not passed by user -----------------------------------
  if (missing(AVAL) && exists("AVAL", envir = rlang::caller_env())) {
    AVAL <- get("AVAL", envir = rlang::caller_env())
  } else if (missing(AVAL)) {
    stop("Default 'AVAL' value not found. Specify argument in `Surv_CNSR(AVAL=)`.")
  }
  if (missing(CNSR) && exists("CNSR", envir = rlang::caller_env())) {
    CNSR <- get("CNSR", envir = rlang::caller_env())
  } else if (missing(CNSR)) {
    stop("Default 'CNSR' value not found. Specify argument in `Surv_CNSR(CNSR=)`.")
  }

  # checking inputs ------------------------------------------------------------
  if (!is.numeric(AVAL) || !is.numeric(CNSR)) {
    stop("Expecting arguments 'AVAL' and 'CNSR' to be numeric.")
  }

  if (stats::na.omit(CNSR) %>% setdiff(c(0, 1)) %>%
    {
      !rlang::is_empty(.)
    }) {
    stop("Expecting 'CNSR' argument to be binary with values `0/1`.")
  }

  if (any(AVAL < 0)) {
    warning("Values of 'AVAL' are less than zero, which is likely a data error.")
  }

  # pass args to `survival::Surv()` --------------------------------------------
  survival::Surv(time = AVAL, event = 1 - CNSR, type = "right", origin = 0)
}
