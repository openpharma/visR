#' @title Wrapper for Cox Proportional Hazard analysis
#'
#' @description This function is a wrapper around \code{survival::coxph} to fit a Cox Proportional Hazards Regression model, assuming right-censored data.
#'    The function expects that the data has been filtered on the parameter (PARAM/PARAMCD) of interest.
#'    The result is an object of class \code{summary.coxph} which can be used in downstream functions and methods that rely on the \code{summary.coxph} class.
#'
#' @param data The name of the dataset for the Cox model is based on the Analysis Data Model (ADaM) principles. The dataset is expected to have
#'    one record per subject per analysis parameter. Rows in which the analysis variable (AVAL) or the sensor variable (CNSR) contain NA, are removed during analysis.
#' @param equation Character vector, representing the covariates used at the Cox model. When NULL, the baseline (null) model is fit.
#'    Default is NULL.
#' @param AVAL Analysis value for the Cox model. Default is "AVAL", as per CDISC ADaM guiding principles.
#' @param CNSR Censor for the Cox model. Default is "CNSR", as per CDISC ADaM guiding principles.
#' @param conf.int Confidence intervals used for the estimation.
#' @param ... additional arguments passed on to the ellipsis of the call \code{survival::coxph(data = data, formula = survival::Surv(AVAL, 1-CNSR) ~ equation), ...)} .
#'    Use \code{?survival::coxph} and \code{?survival::summary.coxph} for more information.
#'
#' @return a summarized coxph model
#'
#' @references \url{https://github.com/therneau/survival}
#'
#' @export
#'
#' @examples
#'
#' ## Null model
#' visR::estimate_cox(data = adtte)
#'
#' ## Univariate Cox regression model using the covariate: `TRTP`
#' # Stratified Kaplan-Meier analysis by `TRTP`
#' visR::estimate_cox(data = adtte, equation = "TRTP")
#'
#' ## Multivariate Cox regression model using the covariates: `TRTP` and `SEX`
#' visR::estimate_cox(data = adtte, equation = c("TRTP", "SEX"))
#'
#' ## Stratified Cox model (strata with one level)
#' visR::estimate_cox(data = adtte, equation = c("AGE", "survival::strata(RACE)"))
#'
#' ## Null model on subset of adtte
#' visR::estimate_cox(data = adtte[adtte$SEX == "F", ])
#'
#' ## Modify the default analysis by using the ellipsis
#' visR::estimate_cox(data = adtte, equation = c( "SEX", "survival::cluster(USUBJID)"),
#'   tier = "breslow", conf.int = 0.9)
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
#' visR::estimate_cox(data = veteran_adam, equation = c("trt", "diagtime"))



estimate_cox <- function(
  data = NULL
  ,equation = NULL
  ,CNSR = "CNSR"
  ,AVAL = "AVAL"
  ,conf.int = 0.95
  ,...
){

  # Capture input to validate user input for data argument -----------------

  ## Get actual data name as symbol
  ### Magrittre pipe returns "." which inactivates recalls to survfit in downstream functions
  ### map passes .x as cox_ccall$data
  ### df: catch expressions that represent base R subsets
  cox_call <- as.list(match.call())
  dots <- list(...)
  df_expr <- cox_call[["data"]]

  ## Validate `data` and capture data name

  if (is.null(data)) stop(paste0("data can't be NULL."))

  if (base::length(base::deparse(cox_call[["data"]])) == 1 && base::deparse(cox_call[["data"]]) %in% c(".", ".x")) {
    df <- the_lhs()
    cox_call[["data"]] <- as.symbol(df)
  } else {
    df <- as.character(sub("\\[.*$", "", deparse(df_expr))[1])
  }

  if (!(inherits(data, "data.frame") | inherits(data, "tibble") | inherits(data, "data.table"))) {
    stop(paste0("data can be of class `data.frame` or `tibble` or `data.table`."))
  }

  data <- as.data.frame(data)

  # Validate columns --------------------------------------------------------

  reqcols <- c(equation, CNSR, AVAL)
  ss <- c("strata", "tt", "frailty", "ridge", "pspline", "cluster", "I", "factor")

  column_names <- gsub("\\([^()]*\\)", "", reqcols) 
  column_names <- column_names[!column_names %in% ss]
  column_names_special <- gsub("\\(([^()]*)\\)|.", "\\1", reqcols, perl=T)
  column_names_special <- column_names_special[column_names_special!=""]
  
  reqcols_fin <- c(column_names, column_names_special)
 
  # Make sure all variables are in dataset
  if (! all(reqcols_fin %in% colnames(data))){
    stop(paste0("Following columns are missing from `data`: ", paste(setdiff(reqcols_fin, colnames(data)), collapse = " "), "."))
  }

  if (!is.numeric(data[[AVAL]])) {
    stop("Analysis variable (AVAL) is not numeric.")
  }

  if (!is.numeric(data[[CNSR]])) {
    stop("Censor variable (CNSR) is not numeric.")
  }


  # Remove NA from the analysis --------------------------------------------

  data <- as.data.frame(data) %>%
    tidyr::drop_na(AVAL, CNSR)


  # Ensure the presence of at least one strata -----------------------------

  if (is.null(equation)) {
    equation <- "1"
  } else {
    equation <- paste(equation, collapse = " + ")
  }


  ## Reverse censoring: see ADaM guidelines versus R survival KM analysis
  library(survival)
  formula <- stats::as.formula(glue::glue(paste0("survival::Surv(", AVAL, ", 1-", CNSR, ") ~ {equation}")))

  survfit_object <- survival::coxph(
    formula, data = data, ...
  )


  # Update Call with original info and dots, similar as update.default ------

  survfit_object$call[[1]] <- quote(survival::coxph)
  survfit_object$call[["formula"]] <- formula
  survfit_object$call[["data"]] <- cox_call$data
  if (length(dots) > 0) {
    names(survfit_object$call)
    names(dots)
    for (i in seq_along(dots)) {
      survfit_object$call[[names(dots)[i]]] <- unlist(dots[i], use.names = FALSE)
    }
  }


  # Add additional metadata -------------------------------------------------

  if ("PARAM" %in% colnames(data) && length(setdiff(c("PARAMCD", "PARAM"), equation)) == 2) {
    # we expect only one unique value => catch mistakes
    survfit_object[["PARAM"]] <- paste(unique(data[["PARAM"]]), collapse = ", ")
  }

  if ("PARAMCD" %in% colnames(data) && length(setdiff(c("PARAMCD", "PARAM"), equation)) == 2) {
    # we expect only one unique value => catch mistakes
    survfit_object[["PARAMCD"]] <- paste(unique(data[["PARAMCD"]]), collapse = ", ")
  }


  # Return ------------------------------------------------------------------


  summary_cox <- summary(survfit_object, conf.int = conf.int)


  return(summary_cox)


  }
