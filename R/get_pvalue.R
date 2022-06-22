#' @title Summarize the test for equality across strata from a survival object using S3 method
#'
#' @description Wrapper around survival::survdiff that tests the null hypothesis of equality across strata.
#'
#' @seealso \code{\link[survival]{survdiff}}
#'
#' @param survfit_object An object of class \code{survfit}
#' @param ptype Character vector containing the type of p-value desired. Current options are "Log-Rank" "Wilcoxon" "Tarone-Ware" "Custom" "All".
#'    "Custom" allows the user to specify the weights on the Kaplan-Meier estimates using the argument `rho`.
#'    The default is "All" displaying all types possible. When `rho` is specified in context of "All", also a custom p-value is displayed.
#' @param statlist Character vector containing the desired information to be displayed. The order of the arguments determines the order in which
#'    they are displayed in the final result. Default is the test name ("test"), Chi-squared test statistic ("Chisq"), degrees of freedom ("df") and
#'    p-value ("pvalue").
#' @param ... other arguments passed on to the method
#'
#' @inheritParams survival::survdiff
#'
#' @return A data frame with summary measures for the Test of Equality Across Strata
#'
#' @examples
#'
#' ## general examples
#' survfit_object <- visR::estimate_KM(data = adtte, strata = "TRTP")
#' visR::get_pvalue(survfit_object)
#' visR::get_pvalue(survfit_object, ptype = "All")
#'
#' ## examples to obtain specific tests
#' visR::get_pvalue(survfit_object, ptype = "Log-Rank")
#' visR::get_pvalue(survfit_object, ptype = "Wilcoxon")
#' visR::get_pvalue(survfit_object, ptype = "Tarone-Ware")
#'
#' ## Custom example - obtain Harrington and Fleming test
#' visR::get_pvalue(survfit_object, ptype = "Custom", rho = 1)
#'
#' ## Get specific information and statistics
#' visR::get_pvalue(survfit_object, ptype = "Log-Rank", statlist = c("test", "Chisq", "df", "pvalue"))
#' visR::get_pvalue(survfit_object, ptype = "Wilcoxon", statlist = c("pvalue"))
#'
#' @export

get_pvalue <- function(survfit_object,
                       ptype = "All",
                       rho = NULL,
                       statlist = c("test", "Chisq", "df", "pvalue"),
                       ...) {

  # Input validation --------------------------------------------------------

  if (!inherits(survfit_object, "survfit")) {
    stop("The function expects an object of class `survfit` as input.")
  }
  if (length(names(survfit_object[["strata"]])) <= 1) {
    stop("Main effect has only 1 level. Test of equality over strata can't be determined.")
  }
  if (!base::any(c("Log-Rank", "Wilcoxon", "Tarone-Ware", "Custom", "All") %in% ptype)) {
    stop("Specify a valid type")
  }
  if ("Custom" %in% ptype & is.null(rho)) {
    stop("ptype = `Custom`. Please, specify rho.")
  }
  if (is.null(statlist) |
    !base::all(statlist %in% c("test", "df", "Chisq", "pvalue"))) {
    stop("Specify valid `statlist` arguments.")
  }

  # Re-use Call from survival object ----------------------------------------

  Call <- as.list(rlang::quo_squash(survfit_object$call))
  NewCall <- append(as.list(parse(text = "survival::survdiff")), Call[names(Call) %in% names(formals(survival::survdiff))])

  if ("All" %in% ptype) {
    ptype <- c("Log-Rank", "Wilcoxon", "Tarone-Ware")
    if (!is.null(rho)) {
      ptype <- c(ptype, "Custom")
    }
  }

  # Summary list ------------------------------------------------------------

  survdifflist <- list(
    `Log-Rank` = rlang::expr(eval(as.call(
      append(!!NewCall, list(rho = 0))
    ))),
    `Wilcoxon` = rlang::expr(eval(as.call(
      append(!!NewCall, list(rho = 1))
    ))),
    `Tarone-Ware` = rlang::expr(eval(as.call(
      append(!!NewCall, list(rho = 1.5))
    ))),
    `Custom` = rlang::expr(eval(as.call(
      append(!!NewCall, list(rho = !!rho))
    )))
  )[ptype]

  survdifflist_eval <-
    lapply(
      survdifflist,
      function(x) {
        tryCatch(
          eval(x, envir = attr(survfit_object$call, ".Environment")),
          error = function(e) {
            error_msg <- as.character(e)
            if (!is_visr_survfit(survfit_object)) {
              error_msg <-
                paste0(
                  "There was an error calculating the p-values.\n",
                  "The 'survfit' object was not created with `visR::estimate_KM()`.\n",
                  "The the error will likely be resolved by re-estimating the ",
                  "'survfit' object with visR.\n",
                  error_msg
                )
            }
            stop(error_msg, call. = FALSE)
          }
        )
      }
    )

  # Statlist ----------------------------------------------------------------

  statlist <- unique(statlist)
  statlist <- base::sub("test", "Equality across strata", statlist, fixed = TRUE)
  statlist <- base::sub("pvalue", "p-value", statlist, fixed = TRUE)
  Nms <- names(survdifflist_eval)

  stat_summary <- list(
    `Equality across strata` = rlang::expr(base::sub(
      "Custom",
      paste0("Harrington and Fleming test (rho = ", rho, ")"),
      Nms,
      fixed = TRUE
    )),
    `Chisq` = rlang::expr(unlist(
      lapply(survdifflist_eval, function(x) {
        format(round(x$chisq, 3), nsmall = 3, justify = "right", width = 6, scientific = FALSE)
      })
    )),
    df = rlang::expr(unlist(
      lapply(survdifflist_eval, function(x) {
        length(x$n) - 1
      })
    )),
    `p-value` = rlang::expr(unlist(
      lapply(survdifflist_eval, function(x) {
        .pvalformat(
          stats::pchisq(x$chisq, length(x$n) - 1, lower.tail = FALSE)
        )
      })
    ))
  )[statlist]

  # Output to dataframe -----------------------------------------------------

  equality <- data.frame(
    lapply(stat_summary, eval, env = environment()),
    check.names = FALSE,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  return(equality)
}
