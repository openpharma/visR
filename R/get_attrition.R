#' Generate cohort attrition table
#'
#' @description
#' `r lifecycle::badge("questioning")`
#' This is an experimental function that may be developed over time.
#'
#' This function calculates the subjects counts excluded and included
#' for each step of the cohort selection process.
#'
#' @param data Dataframe. It is used as the input data to count the subjects
#' that meets the criteria of interest
#' @param criteria_descriptions \code{character} It contains the descriptions
#' of the inclusion/exclusion criteria.
#' Each element of the vector corresponds to the description of each criterion.
#' @param criteria_conditions \code{character} It contains the corresponding
#' conditions of the criteria.
#' These conditions will be used in the table to compute the counts of the
#' subjects.
#' @param subject_column_name \code{character} The column name of the table that
#' contains the subject id.
#'
#' @usage get_attrition(data, criteria_descriptions, criteria_conditions,
#' subject_column_name)
#' @return The counts and percentages of the remaining and excluded subjects
#' for each step of the cohort selection in a table format.
#'
#' @details criteria_descriptions and criteria_conditions need to be of same length
#'
#' @examples
#' visR::get_attrition(adtte,
#'   criteria_descriptions =
#'     c(
#'       "1. Placebo Group", "2. Be 75 years of age or older.",
#'       "3. White", "4. Site 709"
#'     ),
#'   criteria_conditions = c(
#'     "TRTP=='Placebo'", "AGE>=75",
#'     "RACE=='WHITE'", "SITEID==709"
#'   ),
#'   subject_column_name = "USUBJID"
#' )
#' @export
get_attrition <- function(data,
                          criteria_descriptions,
                          criteria_conditions,
                          subject_column_name) {
  if (!inherits(subject_column_name, "character") || length(subject_column_name) > 1) {
    stop("The 'subject_column_name' argument has to be a string. Please correct the 'subject_column_name' and re-run the function")
  }

  if (!subject_column_name %in% names(data)) {
    stop("The 'subject_column_name' argument doesn't correspond to a column name. Please correct the 'subject_column_name' and re-run the function")
  }

  if (length(criteria_descriptions) != length(criteria_conditions)) {
    stop("Vectors 'criteria_descriptions' and 'criteria_conditions' must have the same length.")
  }

  if (!NA %in% criteria_conditions) {
    criteria_map <- data.frame(cbind(criteria_descriptions, criteria_conditions), stringsAsFactors = FALSE)

    final_cond <- c()
    person_count_master <- c()

    for (each_cond in criteria_map$criteria_conditions) {
      final_cond <- ifelse(is.null(final_cond),
        each_cond,
        paste(paste0("(", final_cond, ")"),
          paste0(paste0("(", each_cond), ")"),
          sep = " & "
        )
      )
      # print(final_cond)
      person_count_temp <- data %>%
        dplyr::filter(eval(parse(text = final_cond))) %>%
        dplyr::select(!!subject_column_name) %>%
        dplyr::n_distinct()
      # print(person_count_temp)

      person_count_master <- c(person_count_master, person_count_temp)
    }

    if (length(person_count_master) > 0) {
      count_master_table <- dplyr::tibble("Remaining N" = person_count_master)
      criterion_0 <- dplyr::tibble(
        criteria_conditions = "none",
        criteria_descriptions = "Total cohort size",
        `Remaining N` = dplyr::select(data, !!subject_column_name) %>% dplyr::n_distinct()
      )

      # generate attrition table
      attrition_table <-
        criterion_0 %>%
        dplyr::bind_rows(cbind(criteria_map, count_master_table)) %>%
        dplyr::mutate(
          `Remaining %` = 100 * `Remaining N` / max(`Remaining N`),
          `Excluded N` = dplyr::lag(`Remaining N`, n = 1L, default = max(`Remaining N`)) - `Remaining N`,
          `Excluded %` = 100 * `Excluded N` / max(`Remaining N`)
        ) %>%
        # rename columns
        dplyr::rename(
          Condition = criteria_conditions,
          Criteria = criteria_descriptions
        ) %>%
        # fix formatting
        dplyr::select(Criteria, Condition, dplyr::everything())

      class(attrition_table) <- c("attrition", class(attrition_table))
      return(attrition_table)
    }
  }
}
