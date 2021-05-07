#' \code{visR} package
#'
#' visR
#'
#' See the README on
#' \href{https://github.com/openpharma/visR}{GitHub}
#'
#' For "visible binding note solution" see here \href{https://github.com/STAT545-UBC/Discussion/issues/451}
#'
#' @docType package
#' @name visR
#' @importFrom dplyr %>%
NULL


## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1")
  utils::globalVariables(
    c(
      "est.lower",
      "est.upper",
      "strata" ,
      "n.censor",
      "time",
      "est",
      "unit",
      "LegendWidth",
      "any_of",
      "y_values",
      "statistic",
      "quantile",
      "surv",
      "lower",
      "upper",
      "variable",
      "value",
      ".",
      "qmax",
      "tmax",
      "tmin",
      "quantity",
      "vmin",
      "n.risk",
      "label.y",
      "label.x",
      "min_time",
      "n.event",
      "values",
      "matches",
      ":=",
      "summary_id",
      "everything",
      "rename_at",
      "vars",
      "select",
      "N",
      "median",
      "sd",
      "asis_output",
      "write.table",
      "n.risk",
      "n.event",
      "n_distinct",
      "Remaining N",
      "Excluded N",
      "Criteria",
      "Condition"
    )
  )
