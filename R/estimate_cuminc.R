#' @title Competing Events Cumulative Incidence
#'
#' @description Function creates a cumulative incidence object using the
#'     `tidycmprsk::cuminc()` function.
#'
#' @param AVAL,CNSR,strata These arguments are used to construct a formula to be passed to `tidycmprsk::cuminc(formula=)`.
#' - `AVAL` Analysis value for Time-to-Event analysis. Default is `"AVAL"`, as per CDISC ADaM guiding principles.
#' - `CNSR` Column name indicating the outcome and censoring statuses.
#'          Column must be a factor and the first level indicates censoring, the
#'          next level is the outcome of interest, and the remaining levels are the
#'          competing events. Default is `"CNSR"`
#' - `strata` Character vector, representing the strata for Time-to-Event analysis. When NULL, an overall analysis is performed.
#'    Default is `NULL`.
#' @param conf.int Confidence internal level. Default is 0.95. Parameter is passed to `tidycmprsk::cuminc(conf.level=)`
#' @param ... Additional argument passed to `tidycmprsk::cuminc()`
#' @inheritParams estimate_KM
#' @inheritParams visr
#' @inheritParams add_CI.ggsurvfit
#' @inheritParams add_risktable.ggsurvfit
#' @inheritParams get_risktable
#'
#' @return A cumulative incidence object as explained at https://mskcc-epi-bio.github.io/tidycmprsk/reference/cuminc.html
#'
#' @export
#'
#' @examples
#' cuminc <-
#'   visR::estimate_cuminc(
#'     data = tidycmprsk::trial,
#'     strata = "trt",
#'     CNSR = "death_cr",
#'     AVAL = "ttdeath"
#'   )
#' cuminc
#'
#' cuminc %>%
#'   visR::visr() %>%
#'   visR::add_CI() %>%
#'   visR::add_risktable(statlist = c("n.risk", "cum.event"))
estimate_cuminc <- function(data = NULL,
                            strata = NULL,
                            CNSR = "CNSR",
                            AVAL = "AVAL",
                            conf.int = 0.95,
                            ...) {


  # check for installation of tidycmprsk package -------------------------
  rlang::check_installed("tidycmprsk", version = "0.1.1")
  dots <- rlang::dots_list(...)

  # Validate data --------------------------------------------------------
  if (is.null(data)) stop(paste0("Data can't be NULL."))
  if (!is.numeric(conf.int)) stop(paste0("conf.int needs to be numeric."))
  if (!(0 <= conf.int & conf.int <= 1)) stop(paste0("conf.int needs to between 0 and 1."))

  # Validate columns -----------------------------------------------------
  reqcols <- c(strata, CNSR, AVAL)

  if (!all(reqcols %in% colnames(data))) {
    stop(paste0("Following columns are missing from `data`: ", paste(setdiff(reqcols, colnames(data)), collapse = " "), "."))
  }

  if (!is.numeric(data[[AVAL]])) {
    stop("Analysis variable (AVAL) is not numeric.")
  }

  if (!is.factor(data[[CNSR]])) {
    stop("Censor variable (CNSR) is not a factor")
  }

  # Remove NA from the analysis ------------------------------------------

  data <- data %>%
    tidyr::drop_na(AVAL, CNSR)

  if (!is.null(strata)) {
    data <- data %>%
      tidyr::drop_na(any_of({{ strata }}))
  }

  # Ensure the presence of at least one strata ---------------------------
  strata <- ifelse(is.null(strata), 1, strata %>% paste(collapse = " + "))

  # cuminc ---------------------------------------------------------------
  cuminc <-
    tidycmprsk::cuminc(
      formula = stats::as.formula(paste0("survival::Surv(", AVAL, ", ", CNSR, ") ~ ", strata)),
      data = data,
      conf.level = conf.int,
      ...
    )

  cuminc
}

# this function runs `tidy()` and puts it in the visR format
# 1. only keeps the first outcome
# 2. renames estimate and CI columns
# 3. Add a strata column if not already present
visr_tidy_tidycuminc <- function(x, times = NULL) {
  df_visr_tidy <-
    tidycmprsk::tidy(x, times = times) %>%
    dplyr::filter(.data[["outcome"]] %in% names(x$failcode)[1]) %>%
    # renaming to match column name in the survfit equivalent of these functions
    dplyr::rename(
      est = .data[["estimate"]],
      est.lower = .data[["conf.low"]],
      est.upper = .data[["conf.high"]]
    )

  # adding strata column if not already present
  if (!"strata" %in% names(df_visr_tidy)) {
    df_visr_tidy <- dplyr::mutate(df_visr_tidy, strata = "Overall")
  }

  as.data.frame(df_visr_tidy)
}
