#' @title Wrapper for Kaplan-Meier Time-to-Event analysis
#'
#' @description
#' This function is a wrapper around `survival::survfit.formula()`
#'    to perform a Kaplan-Meier analysis, assuming right-censored data.
#'    The result is an object of class \code{survfit} which can be used in
#'    downstream functions and methods that rely on the \code{survfit} class.
#'
#' The function can leverage the conventions and controlled vocabulary from
#' [CDISC ADaM ADTTE data model](https://www.cdisc.org/standards/foundational/adam/adam-basic-data-structure-bds-time-event-tte-analyses-v1-0),
#' and also works with standard, non-CDISC datasets through the `formula` argument.
#'
#' @section Estimation of 'survfit' object:
#'
#' The `estimate_KM()` function utilizes the defaults in `survival::survfit()`:
#'    \itemize{
#'      \item{The Kaplan Meier estimate is estimated directly (stype = 1).}
#'      \item{The cumulative hazard is estimated using the Nelson-Aalen estimator (ctype = 1): H.tilde = cumsum(x$n.event/x$n.risk).
#'      The MLE (H.hat(t) = -log(S.hat(t))) can't be requested.}
#'      \item{A two-sided pointwise 0.95 confidence interval is estimated using a log transformation (conf.type = "log").}
#'    }
#'
#' When strata are present, the returned survfit object is supplemented with
#'   the a named list of the stratum and associated label.
#' To support full traceability, the data set name is captured in the named
#'   list and the call is captured within its corresponding environment.
#'
#' @section PARAM/PARAMCD and CDISC:
#'
#' If the data frame includes columns PARAM/PARAMCD (part of the CDISC format),
#'   the function expects the data has been filtered on the parameter of interest.
#'
#' @seealso \code{\link[survival]{survfit.formula} \link[survival]{survfitCI}}
#'
#' @param data A data frame. The dataset is expected to have
#'    one record per subject per analysis parameter. Rows with missing observations included in the analysis are removed.
#' @param AVAL,CNSR,strata These arguments are used to construct a formula to be passed to
#' `survival::survfit(formula=Surv(AVAL, 1-CNSR)~strata)`. These arguments' default values follow the naming conventions in CDISC.
#' - `AVAL` Analysis value for Time-to-Event analysis. Default is `"AVAL"`, as per CDISC ADaM guiding principles.
#' - `CNSR` Censor for Time-to-Event analysis. Default is `"CNSR"`, as per CDISC ADaM guiding principles. It is expected that CNSR = 1
#'    for censoring and CNSR = 0 for the event of interest.
#' - `strata` Character vector, representing the strata for Time-to-Event analysis. When NULL, an overall analysis is performed.
#'    Default is `NULL`.
#' @param ... additional arguments passed on to the ellipsis of the call `survival::survfit.formula(...)`.
#'    Use \code{?survival::survfit.formula} and \code{?survival::survfitCI} for more information.
#' @param formula `r lifecycle::badge('experimental')` formula with `Surv()` on RHS and stratifying variables on the LHS. Use
#' `~1` on the LHS for unstratified estimates. This argument will be passed to `survival::survfit(formula=)`. When this argument is
#' used, arguments AVAL, CNSR, and strata are ignored.
#'
#' @return survfit object ready for downstream processing in estimation or visualization functions and methods.
#'
#' @references \url{https://github.com/therneau/survival}
#'
#' @export
#'
#' @examples
#'
#' ## No stratification
#' visR::estimate_KM(data = adtte)
#'
#' ## Stratified Kaplan-Meier analysis by `TRTP`
#' visR::estimate_KM(data = adtte, strata = "TRTP")
#'
#' ## Stratified Kaplan-Meier analysis by `TRTP` and `SEX`
#' visR::estimate_KM(data = adtte, strata = c("TRTP", "SEX"))
#'
#' ## Stratification with one level
#' visR::estimate_KM(data = adtte, strata = "PARAMCD")
#'
#' ## Analysis on subset of adtte
#' visR::estimate_KM(data = adtte[adtte$SEX == "F", ])
#'
#' ## Modify the default analysis by using the ellipsis
#' visR::estimate_KM(
#'   data = adtte, strata = NULL,
#'   type = "kaplan-meier", conf.int = FALSE, timefix = TRUE
#' )
#'
#' ## Example working with non CDISC data
#' head(survival::veteran[c("time", "status", "trt")])
#'
#' # Using non-CDSIC data
#' visR::estimate_KM(data = survival::veteran, formula = Surv(time, status) ~ trt)
#'
estimate_KM <- function(data = NULL,
                        strata = NULL,
                        CNSR = "CNSR",
                        AVAL = "AVAL",
                        formula = NULL,
                        ...) {

  # Capture input to validate user input for data argument ---------------------

  dots <- rlang::dots_list(...)

  # Validate argument inputs ---------------------------------------------------

  if (is.null(data)) {
    stop(paste0("Data can't be NULL."))
  }

  if (!is.data.frame(data)) {
    stop("Data does not have class `data.frame`.")
  }

  if (is.null(formula)) {
    reqcols <- c(strata, CNSR, AVAL)
    if (!all(reqcols %in% colnames(data))) {
      stop(paste0("Following columns are missing from `data`: ", paste(setdiff(reqcols, colnames(data)), collapse = " "), "."))
    }

    if (!is.numeric(data[[AVAL]])) {
      stop("Analysis variable (AVAL) is not numeric.")
    }

    if (!is.numeric(data[[CNSR]])) {
      stop("Censor variable (CNSR) is not numeric.")
    }
  } else if (!inherits(formula, "formula")) {
    stop("Argument `formula=` must be class 'formula'.")
  }


  # Check formula arguments and add strata object if user passer formula  -----
  if (!is.null(formula)) {
    if (any(!all.vars(formula) %in% names(data))) {
      vars_missing_in_data <-
        all.vars(formula) %>%
        setdiff(names(data)) %>%
        {
          paste(shQuote(., type = "csh"), collapse = ", ")
        }
      paste(
        "The following columns found in `formula=` are missing from the data frame:",
        vars_missing_in_data
      ) %>%
        stop(call. = FALSE)
    }

    # extract strata
    formula_rhs <- formula
    rlang::f_lhs(formula_rhs) <- NULL
    strata <-
      stats::get_all_vars(formula = formula_rhs, data = data) %>%
      names() %>%
      switch(!rlang::is_empty(.),
        .
      ) # convert empty string to NULL
  }

  # construct formula if not passed by user ------------------------------------
  if (is.null(formula)) {
    formula <-
      stats::as.formula(paste0(
        "survival::Surv(", AVAL, ", 1-", CNSR, ") ~ ",
        ifelse(is.null(strata), "1", paste(strata, collapse = " + "))
      ))
  }

  # Remove NA from the analysis ------------------------------------------------
  data <- tidyr::drop_na(data, dplyr::all_of(all.vars(formula)))

  # Ensure the presence of at least one strata -----------------------------

  formula_rhs <-
    ifelse(is.null(strata), "1", paste(strata, collapse = " + "))

  # Calculate survival and add time = 0 to survfit object -------------------
  survfit_object <-
    rlang::inject(survival::survfit(!!formula, data = data, !!!dots)) %>% # immediate resolves call arguments
    survival::survfit0(start.time = 0)

  # convert survfit() call to quo with attached envir --------------------------

  survfit_object$call[[1]] <- rlang::expr(survival::survfit) # adding `survival::` prefix
  survfit_object$call <- rlang::quo(!!survfit_object$call)

  # Add additional metadata ----------------------------------------------------

  if ("PARAM" %in% colnames(data) && length(setdiff(c("PARAMCD", "PARAM"), strata)) == 2) {
    # we expect only one unique value => catch mistakes
    survfit_object[["PARAM"]] <- paste(unique(data[["PARAM"]]), collapse = ", ")
  }

  if ("PARAMCD" %in% colnames(data) && length(setdiff(c("PARAMCD", "PARAM"), strata)) == 2) {
    # we expect only one unique value => catch mistakes
    survfit_object[["PARAMCD"]] <- paste(unique(data[["PARAMCD"]]), collapse = ", ")
  }

  survfit_object$data_name <- .call_list_to_name(as.list(match.call()))

  # Artificial strata for easy downstream processing when strata=NULL ----------

  if (is.null(survfit_object[["strata"]])) {
    survfit_object[["strata"]] <- as.vector(length(survfit_object[["time"]]))

    if (is.null(strata)) {
      # overall analysis
      attr(survfit_object[["strata"]], "names") <- "Overall"
    } else {
      # ~ x with One level in variable present
      attr(survfit_object[["strata"]], "names") <- as.character(paste0(strata, "=", data[1, formula_rhs]))
    }
  }

  # add strata labels - main goal is for populating legend in visR(): label -- level1 strata -- levelx strata
  # these are the LABEL attributes of the stratifying variables (separate from above, which are the levels of the variables)
  # is null, when no stratifying variables present so legend title is not populated as Overall -- overall

  if (!is.null(strata)) {
    survfit_object[["strata_lbls"]] <-
      lapply(as.list(strata), function(x) attr(data[[x]], "label") %||% x) %>%
      rlang::set_names(strata)
  }

  # Return ------------------------------------------------------------------

  survfit_object
}
