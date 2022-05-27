#' @title Wrapper for Kaplan Meier Time-to-Event analysis
#'
#' @description This function is a wrapper around \code{survival::survfit.formula} to perform a Kaplan-Meier analysis, assuming right-censored data.
#'    The function expects that the data has been filtered on the parameter (PARAM/PARAMCD) of interest. All NA values in the CNSR, AVAL and strata
#'    argument are removed.
#'    Alternatively, PARAM/PARAMCD can be used in the \code{strata} argument. \cr
#'    The result is an object of class \code{survfit} which can be used in downstream functions and methods that rely on the \code{survfit} class.
#'    When strata are present, the returned survfit object is supplemented with the a named list of the stratum and associated label, if present.
#'    To support full traceability, the data set name is captured in the named list and the call is captured within its corresponding envrironment.
#'    By default:
#'    \itemize{
#'      \item{The Kaplan Meier estimate is estimated directly (stype = 1).}
#'      \item{The cumulative hazard is estimated using the Nelson-Aalen estimator (ctype = 1): H.tilde = cumsum(x$n.event/x$n.risk).
#'      The MLE (H.hat(t) = -log(S.hat(t))) can't be requested.}
#'      \item{A two-sided pointwise 0.95 confidence interval is estimated using a log transformation (conf.type = "log").}
#'    }
#'
#' @seealso \code{\link[survival]{survfit.formula} \link[survival]{survfitCI}}
#'
#' @param data The name of the dataset for Time-to-Event analysis based on the Analysis Data Model (ADaM) principles. The dataset is expected to have
#'    one record per subject per analysis parameter. Rows in which the analysis variable (AVAL), the censor variable (CNSR) or the strata variables
#'    contain NA, are removed during analysis.
#' @param strata Character vector, representing the strata for Time-to-Event analysis. When NULL, an overall analysis is performed.
#'    Default is NULL.
#' @param AVAL Analysis value for Time-to-Event analysis. Default is "AVAL", as per CDISC ADaM guiding principles.
#' @param CNSR Censor for Time-to-Event analysis. Default is "CNSR", as per CDISC ADaM guiding principles.
#' @param ... additional arguments passed on to the ellipsis of the call \code{survival::survfit.formula(data = data, formula = Surv(AVAL, 1-CNSR) ~ strata), ...)} .
#'    Use \code{?survival::survfit.formula} and \code{?survival::survfitCI} for more information.
#'
#' @return survfit object, extended by elements PARAM/PARAMCD, ready for downstream processing in estimation or visualization functions and methods.
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
#' visR::estimate_KM(data = adtte, strata = NULL,
#'   type = "kaplan-meier", conf.int = FALSE, timefix = TRUE)
#'
#' ## Example working with non CDISC data
#' head(survival::veteran)
#'
#' # convert time and censoring data to ADaM variables
#' # convert censoring status to CDISC principles
#' veteran_adam <- survival::veteran %>%
#'  dplyr::mutate(AVAL = time,
#'                CNSR = dplyr::if_else(status == 1, 0, 1)
#'  )
#'
#' visR::estimate_KM(data = veteran_adam, strata = "trt")

estimate_KM <- function(
    data = NULL
    ,strata = NULL
    ,CNSR = "CNSR"
    ,AVAL = "AVAL"
    ,...
){

  # Capture input to validate user input for data argument ---------------------

  dots <- rlang::dots_list(...)

  # Validate argument inputs ---------------------------------------------------

  if (is.null(data))
    stop(paste0("Data can't be NULL."))

  if (!is.data.frame(data))
    stop("Data does not have class `data.frame`.")

  reqcols <- c(strata, CNSR, AVAL)
  if (! all(reqcols %in% colnames(data))){
    stop(paste0("Following columns are missing from `data`: ", paste(setdiff(reqcols, colnames(data)), collapse = " "), "."))
  }

  if (!is.numeric(data[[AVAL]])){
    stop("Analysis variable (AVAL) is not numeric.")
  }

  if (!is.numeric(data[[CNSR]])){
    stop("Censor variable (CNSR) is not numeric.")
  }

  # Remove NA from the analysis ------------------------------------------------

  data <-
    as.data.frame(data) %>%
    tidyr::drop_na(any_of(c(AVAL, CNSR)))

  if (!is.null(strata)){
    data <- data %>%
      tidyr::drop_na(any_of(strata))
  }

  # Ensure the presence of at least one strata -----------------------------

  formula_rhs <-
    ifelse(is.null(strata), "1", paste(strata, collapse = " + "))

  # Calculate survival and add time = 0 to survfit object -------------------

  formula <- stats::as.formula(paste0("survival::Surv(", AVAL, ", 1-", CNSR, ") ~ ", formula_rhs))

  survfit_object <-
    rlang::inject(survival::survfit(!!formula, data = data, !!!dots)) %>% # immediate resolves call arguments
    survival::survfit0(start.time = 0)

  # convert survfit() call to quo with attached envir --------------------------

  survfit_object$call[[1]] <- rlang::expr(survival::survfit) # adding `survival::` prefix
  survfit_object$call <- rlang::quo(!!survfit_object$call)

  # Add additional metadata ----------------------------------------------------

  if ("PARAM" %in% colnames(data) && length(setdiff(c("PARAMCD", "PARAM"), strata)) == 2){
    # we expect only one unique value => catch mistakes
    survfit_object[["PARAM"]] <- paste(unique(data[["PARAM"]]), collapse = ", ")
  }

  if ("PARAMCD" %in% colnames(data) && length(setdiff(c("PARAMCD", "PARAM"), strata)) == 2){
    # we expect only one unique value => catch mistakes
    survfit_object[["PARAMCD"]] <- paste(unique(data[["PARAMCD"]]), collapse = ", ")
  }

  survfit_object$data_name <- .call_list_to_name(as.list(match.call()))

  # Artificial strata for easy downstream processing when strata=NULL ----------

  if (is.null(survfit_object[["strata"]])) {
    survfit_object[["strata"]] <- as.vector(length(survfit_object[["time"]]))

    if (is.null(strata)){
      # overall analysis
      attr(survfit_object[["strata"]], "names") <- "Overall"
    }
    else {
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
