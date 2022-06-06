

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
  survival::Surv(time = AVAL, event = 1 - CNSR)
}
