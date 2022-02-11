#' @title Competing Events Cumulative Incidence
#'
#' @description Function creates a cumulative incidence object using the
#'     `tidycmprsk::cuminc()` function.
#'
#' @param CNSR Column name indicating the outcome and censoring statuses.
#' Column must be a factor and the first level indicates censoring, the
#' next level is the outcome of interest, and the remaining levels are the
#' competing events. Default is `"CNSR"`
#' @param conf.int Confidence internal level. Default is 0.95.
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
#' estimate_cuminc(
#'   tidycmprsk::trial,
#'   strata = "trt",
#'   CNSR = "death_cr",
#'   AVAL = "ttdeath"
#' ) %>%
#'   visr() %>%
#'   add_CI() %>%
#'   add_risktable(statlist = c("n.risk", "cumulative.event"))

estimate_cuminc <- function(data
                            ,strata = NULL
                            ,CNSR = "CNSR"
                            ,AVAL = "AVAL"
                            ,conf.int = 0.95
                            ,...){
  # check for installation of tidycmprsk package
  if (!"tidycmprsk" %in% rownames(utils::installed.packages()) ||
      utils::packageVersion("tidycmprsk") < "0.1.0.9003") {
    message("Install updated version of 'tidycmprsk' with `devtools::install_github('MSKCC-Epi-Bio/tidycmprsk')`")
    return(invisible())
  }
  if (!"hardhat" %in% rownames(utils::installed.packages()) ||
      utils::packageVersion("hardhat") <= "0.1.6") {
    message("Install updated version of 'hardhat' with `devtools::install_github('tidymodels/hardhat')`")
    return(invisible())
  }

  # checking/prepping inputs ---------------------------------------------------
  strata <- strata %||% "1" %>% paste(collapse = " + ")

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

  as.data.frame(df_visr_tidy)
}


