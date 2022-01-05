#' Competing Events Cumulative Incidence
#'
#' @param CNSR Column name indicating the outcome and censoring statuses.
#' Column must be a factor and the first level indicates censoring, the
#' next level is the outcome of interest, and the remaining levels are the
#' competing events.
#' @param conf.int Confidence internal level. Default is 0.95.
#' @inheritParams estimate_KM
#' @inheritParams visr
#' @inheritParams add_CI.ggsurvfit
#' @inheritParams add_risktable.ggsurvfit
#' @inheritParams get_risktable
#'
#' @name estimate_CUMINC
#' @importFrom rlang .data .env
#'
#' @examples
#' estimate_CUMINC(
#'   tidycmprsk::trial,
#'   strata = "trt",
#'   CNSR = "death_cr",
#'   AVAL = "ttdeath"
#' ) %>%
#'   visr() %>%
#'   add_CI() %>%
#'   add_risktable(statlist = c("n.risk", "cumulative.event"))
NULL

#' @export
#' @rdname estimate_CUMINC
estimate_CUMINC <- function(data
                            ,strata = NULL
                            ,CNSR
                            ,AVAL
                            ,conf.int = 0.95
                            ,...){
  # check for installation of tidycmprsk package
  if (!"tidycmprsk" %in% rownames(utils::installed.packages()) ||
      utils::packageVersion("tidycmprsk") < "0.1.0.9003") {
    stop("Install updated version of 'tidycmprsk' with `devtools::install_github('MSKCC-Epi-Bio/tidycmprsk')`")
  }
  if (!"hardhat" %in% rownames(utils::installed.packages()) ||
      utils::packageVersion("hardhat") <= "0.1.6") {
    stop("Install updated version of 'hardhat' with `devtools::install_github('tidymodels/hardhat')`")
  }
  if (!"broom" %in% rownames(utils::installed.packages()) ||
      utils::packageVersion("broom") < "0.7.10.9000") {
    stop("Install updated version of 'broom' with `devtools::install_github('tidymodels/broom')`")
  }
  if (!"glue" %in% rownames(utils::installed.packages())) {
    stop("Install updated version of 'glue' with `install.packages('glue')`")
  }

  # checking/prepping inputs ---------------------------------------------------
  strata <- strata %||% "1" %>% paste(collapse = " + ")

  cuminc <-
    tidycmprsk::cuminc(
      formula = stats::as.formula(glue::glue("survival::Surv({AVAL}, {CNSR}) ~ {strata}")),
      # formula = stats::as.formula(glue::glue("survival::Surv({AVAL}, {CNSR}) ~ {strata}")),
      data = data,
      conf.level = conf.int,
      ...
    )

  # only keeping outcome of interest
  cuminc$tidy_subset <- visr_tidy_tidycuminc(cuminc)

  cuminc
}

# this function runs `tidy()` and puts it in the visR format
# 1. only keeps the first outcome
# 2. renames estimate and CI columns
# 3. Add a strata column if not already present
visr_tidy_tidycuminc <- function(x, times = NULL) {
  df_visr_tidy <-
    tidycmprsk::tidy(x, times = times) %>%
    dplyr::filter(.data$outcome %in% names(x$failcode)[1]) %>%
    # renaming to match column name in the survfit equivalent of these functions
    dplyr::rename(
      est = .data$estimate,
      est.lower = .data$conf.low,
      est.upper = .data$conf.high
    )

  # adding strata column if not already present
  if (!"strata" %in% names(df_visr_tidy)) {
    df_visr_tidy <- dplyr::mutate(df_visr_tidy, strata = "Overall")
  }

  df_visr_tidy
}

# copy of purrr::`%||%` operator
`%||%` <- function (x, y) if (rlang::is_null(x)) y else x

