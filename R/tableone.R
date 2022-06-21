#' @title Display a summary Table (i.e. table one)
#'
#' @description
#' `r lifecycle::badge("questioning")`
#' Wrapper function to produce a summary table (i.e. Table One).
#' Create and render a summary table for a dataset.
#' A typical example of a summary table are "table one", the first table in an applied medical research manuscript.
#'
#' Calculate summary statistics and present them in a formatted table
#'
#' @param data The dataframe or tibble to visualize
#' @param title Table title to include in the rendered table. Input is a text string.
#' @param footnote Table footnote to include in the rendered table. Input is a text string.
#' @param datasource String specifying the datasource underlying the data set
#' @param strata Character vector with column names to use for
#' stratification in the summary table. Default: NULL , which indicates no stratification.
#' @param overall If TRUE, the summary statistics for the overall dataset are also calculated
#' @param summary_function A function defining summary statistics for numeric and categorical values
#' Pre-implemented functions are summarize_long and summarize_short
#' @param ... Pass options to render_table
#'
#' @section Example Output:
#' \if{html}{tableone(engine = "gt")}
#'
#' \if{html}{\figure{tableone_gt_ex.png}{options: width=65\%}}
#'
#' \if{html}{tableone(engine = "DT")}
#'
#' \if{html}{\figure{tableone_DT_ex.png}{options: width=65\%}}
#'
#' \if{html}{tableone(engine = "kable")}
#'
#' \if{html}{\figure{tableone_kable_html_ex.png}{options: width=65\%}}
#'
#' \if{html}{tableone(engine = "kable", output_format = "latex")}
#'
#' \if{html}{\figure{tableone_kable_latex_ex.png}{options: width=65\%}}
#'
#' @examples
#'
#' # metadata for table
#' t1_title <- "Cohort Summary"
#' t1_ds <- "ADaM Interim Dataset for Time-to-Event Analysis"
#' t1_fn <- "My table one footnote"
#'
#'
#' ## table by treatment - without overall and render with GT
#' tbl_gt <-
#'   adtte %>%
#'   dplyr::filter(SAFFL == "Y") %>%
#'   dplyr::select(AGE, AGEGR1, SEX, EVNTDESC, TRTA) %>%
#'   visR::tableone(
#'     strata = "TRTA",
#'     overall = FALSE,
#'     title = t1_title,
#'     datasource = t1_ds,
#'     footnote = t1_fn,
#'     engine = "gt"
#'   )
#'
#' ## table by treatment - without overall and render with DT
#' tbl_DT <-
#'   adtte %>%
#'   dplyr::filter(SAFFL == "Y") %>%
#'   dplyr::select(AGE, AGEGR1, SEX, EVNTDESC, TRTA) %>%
#'   visR::tableone(
#'     strata = "TRTA",
#'     overall = FALSE,
#'     title = t1_title,
#'     datasource = t1_ds,
#'     footnote = t1_fn,
#'     engine = "DT"
#'   )
#'
#' ## table by treatment - without overall and render with kable
#' tbl_kable_html <-
#'   adtte %>%
#'   dplyr::filter(SAFFL == "Y") %>%
#'   dplyr::select(AGE, AGEGR1, SEX, EVNTDESC, TRTA) %>%
#'   visR::tableone(
#'     strata = "TRTA",
#'     overall = FALSE,
#'     title = t1_title,
#'     datasource = t1_ds,
#'     footnote = t1_fn,
#'     engine = "kable"
#'   )
#'
#' ## table by treatment - without overall and render with kable as
#' ## a latex table format rather than html
#' tbl_kable_latex <-
#'   adtte %>%
#'   dplyr::filter(SAFFL == "Y") %>%
#'   dplyr::select(AGE, AGEGR1, SEX, EVNTDESC, TRTA) %>%
#'   visR::tableone(
#'     strata = "TRTA",
#'     overall = FALSE,
#'     title = t1_title,
#'     datasource = t1_ds,
#'     footnote = t1_fn,
#'     output_format = "latex",
#'     engine = "kable"
#'   )
#'
#' @return A table-like data structure, possibly interactive depending on the choice of the engine
#'
#' @rdname tableone
#'
#' @export

tableone <- function(data,
                     title,
                     datasource,
                     footnote = "",
                     # abbreviations = "",
                     # variable_definitions = "",
                     strata = NULL,
                     overall = TRUE,
                     summary_function = summarize_short,
                     ...
                     # engine = "gt"
) {
  tab1_rendered <- get_tableone(data,
    strata = strata,
    summary_function = summary_function,
    overall = overall
  ) %>%
    render(
      title = title,
      datasource = datasource,
      footnote = footnote,
      ...
    )

  return(tab1_rendered)
}
