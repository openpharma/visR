#' Generate treatment sequence summaries from one-row-per-patient-line data
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' This is an experimental function that may be developed over time.
#'
#' This function manipulates treatment sequence information given
#' as one-row-per-line into "lodes" form, one row per combination. The outputs
#' of this function can be used as inputs into visr() to create a
#' sankey plot.
#'
#' @param data \code{data.frame} containing one-row-per-patient-line.
#' The data.frame must contain columns for 1) a patient identifier
#' (character or integer), 2) a treatment label (character or integer), and
#' 3) a line number (integer or factor for line labels). Optionally, a fourth
#' column specifying the IDs of patients can be included to display reasons for
#' censorship.
#'
#' See description for more information.
#'
#' @param id \code{character} the column name of the patient id. data[[id]]
#' must be integer/vector
#' @param label \code{character} specifying the column name of the
#' treatment or treatment group. data[[label]] must be character
#' @param line \code{character} specifying the column name of the line
#' number (or line label) columns. data[[line]] must be integer or factor
#' @param mortality_ids \code{character/integer} (optional) vector specifying
#' which patients should be displayed as dead instead of censored.
#' @param which_lines \code{integer/factor} (optional) vector of specific
#' lines to evaluate in the event some lines are not of interest
#' @param n_groups \code{integer} the maximum number of unique
#' treatment labels that will be considered (others will be grouped as "other").
#' Default is 6.
#'
#' @usage get_tx_sequence(data, id, label, line, n_groups)
#' @return Treatment sequencing data in lodes form.
#'
#'
#' @examples
#'
#' lots <- data.frame(
#'   patient = c('abc','abc','xyz','xyz','xyz'),
#'   line_number = c(1,2,1,2,3),
#'   treatment = c('Drug 1','Drug 3', 'Drug 2', 'Drug 4','Drug 3')
#' )
#'
#' visR::get_tx_sequence(
#'   lots,
#'   id = 'patient',
#'   label = 'treatment',
#'   line = 'line_number'
#'   )
#'
#'
#' @export
get_tx_sequence <- function(
  data,
  id,
  label,
  line,
  mortality_ids = NULL,
  which_lines = NULL,
  n_groups = 6
)
{

  # Verify inputs are correct
  if(!'data.frame' %in% class(data)) {
    stop("data must be a data.frame or coercable into data.frame")
  } else {
    data <- as.data.frame(data) # for column referencing
  }

  if(!all(c(id,label,line) %in% colnames(data))) {
    stop(paste0("The specified columns '", id, "', '", label,
                "', and '", line,"' must all be columns in data"))
  }

  if('numeric' %in% class(data[[id]])) {
    if(all(as.integer(data[[id]])==data[[id]])) {
      data[[id]] <- as.integer(data[[id]])
      message(paste0("Converting '", id, "' from numeric to integer"))
    } else {
      stop(paste0("Column '", id, "' must be integer"))
    }
  }

  if('numeric' %in% class(data[[line]])) {
    if(all(as.integer(data[[line]])==data[[line]])) {
      data[[line]] <- as.integer(data[[line]])
      message(paste0("Converting '", line, "' from numeric to integer"))
    } else {
      stop(paste0("Column '", line, "' must be integer"))
    }
  }

  if('factor' %in% class(data[[line]]) & !'ordered' %in% class(data[[line]])) {
    data[[line]] <- factor(data[[line]],
                           levels = levels(data[[line]]),
                           ordered = T)
  }

  if(!is.null(mortality_ids) & 'numeric' %in% class(mortality_ids)) {
    if(all(as.integer(mortality_ids)==mortality_ids)) {
      mortality_ids <- as.integer(mortality_ids)
      message(paste0("Converting mortality_ids from numeric to integer"))
    } else {
      stop(paste0("mortality_ids must be integer"))
    }
  }

  if ('integer' %in% class(data[[line]]) & any(data[[line]])<=0){
    stop(paste0("If integer, the column '", line, "' must be
                positive."))
  }

  if(!any(c('character','integer') %in% class(data[[id]]))){
    stop(paste0("Columns '", id, "' must be of type
    character or integer"))
  }

  if(!any(c('character','integer') %in% class(data[[label]]))){
    stop(paste0("Columns '", label, "' must be of type
    character or integer"))
  }

  if(!any(c('integer','factor') %in% class(data[[line]]))){
    stop(paste0("Column '", line, "' must be integer or factor"))
  }

  if((!any(c('integer','numeric') %in% class(n_groups)) ||
      as.integer(n_groups) != n_groups
    ) || n_groups < 1) {
    stop("n_groups must be a positive integer")
  }

  if(!is.null(mortality_ids) && !any(c("character","integer")
     %in% class(mortality_ids))) {
    stop(paste0("mortality_ids must be an integer or character vector (whichever
         matches the column '", id, "'"))
  }

  if(!is.null(mortality_ids) && !all(mortality_ids %in% data[[id]])) {
    if(!any(mortality_ids %in% data[[id]])) {
      stop("No mortality_ids are in your list of ids!")
    }
    warning("Some of your mortality_ids are not in the data input")
  }

  if(!is.null(which_lines)) {
    if(!all(which_lines %in% unique(data[[line]]))) {
      stop("All lines in which_lines must be present in data")
    }
    if(!unique(data[[line]])[order(unique(data[[line]]))][1] %in%
       which_lines) {
      stop("The first line must be included in which_lines,
           otherwise this may drop patients from your data.
           Try filtering your data first to set a new first line")
    }
    data <- data[data[[line]] %in% which_lines,]
  } else {
    which_lines <- unique(data[[line]])
  }

  # Clean up and rename data
  data <- data[,c(id,label,line)]
  colnames(data) <- c('id','label','line')

  # Confirm data.frame does not contain >1 row per patient-line
  n_pt_lines <- data %>%
    dplyr::group_by(id, line) %>%
    dplyr::summarise(n = dplyr::n(),.groups = "drop")

  if(max(n_pt_lines$n)>1) {
    stop(paste0(
      "data must contain a maximum of 1 patient per line. ",
      "The following '",id,"' have more than one of the same line:",
      paste0(dplyr::filter(n_pt_lines, n>1)[[1]], collapse = ", ")
    ))

  }

  # Confirm patients don't skip lines (1L, NA, 3L) or start on wrong line
  if("integer" %in% class(data$line)) {
    skip_check <- data %>%
      dplyr::group_by(id) %>%
      dplyr::summarise(
        n_lines = max(line) - min(line) + 1,
        n_rows = dplyr::n()
        )
    skipped <- skip_check %>%
      dplyr::filter(n_lines!=n_rows)
    if(NROW(skipped)>0) {
      stop(paste0("The current version of get_tx_sequence() requires each ",
                  "patient to be observed for consecutive lines only. ",
                  "That is, a patient cannot have a line 1 and 3, but no 2. ",
                  "These patients skip lines: ",
                  paste0(skipped$id,collapse = ", ")))
    }
    started_late <- data %>%
      dplyr::distinct(id) %>%
      dplyr::left_join(
        data %>%
          dplyr::group_by(id) %>%
          dplyr::summarise(
            min_line = min(line)
          ), by='id'
      ) %>%
      dplyr::filter(min_line > min(data$line))
    if(NROW(started_late)>0) {
      stop(paste0("The current version of get_tx_sequence() requires each ",
                  "patient to start on the same first line as others. ",
                  "That is, a patient cannot have a line 2 and 3, but no 1. ",
                  "These patients have started late : ",
                  paste0(started_late$id,collapse = ", ")))
    }
  }

  if("character" %in% class(data$line)) {
    skip_check <- data %>%
      dplyr::group_by(id) %>%
      dplyr::summarise(
        n_lines = which(levels(data$line) == max(line)) -
          which(levels(data$line) == min(line)) + 1,
        n_rows = n()
      )
    skipped <- skip_check %>%
      dplyr::filter(n_lines!=n_rows)
    if(NROW(skipped)>0) {
      stop(paste0("The current version of get_tx_sequence() requires each ",
                  "patient to be observed for consecutive lines only. ",
                  "That is, a patient cannot have a line 1 and 3, but no 2. ",
                  "These patients skip lines: ",
                  paste0(skipped$id,collapse = ", ")))
    }
    started_late <- data %>%
      dplyr::distinct(id) %>%
      left_join(
        data %>%
          dplyr::group_by(id) %>%
          dplyr::summarise(
            min_line = min(line)
          ), by='id'
      ) %>%
      dplyr::filter(min_line > levels(data$line)[1])
    if(NROW(started_late)>0) {
      stop(paste0("The current version of get_tx_sequence() requires each ",
                  "patient to start on the same first line as others. ",
                  "That is, a patient cannot have a line 2 and 3, but no 1. ",
                  "These patients have started late : ",
                  paste0(started_late$id,collapse = ", ")))
    }
  }

  # Group n_groups together
  top_therapies <- data %>%
    dplyr::count(label, sort = T) %>%
    dplyr::slice(1:n_groups) %>%
    dplyr::pull(1)

  if (length(top_therapies) < n_groups) {
    message(paste0("There are fewer than ", n_groups, " regimens to combine into groups. Showing all regimens."))
    n_groups <- length(top_therapies)
  }

  data$label <- ifelse(data$label %in% top_therapies,
                          data$label,
                          'Other')

  cats <- c(top_therapies, 'Other')

  # Now cast to lodes form
  data_wide <- data %>%
    tidyr::pivot_wider(id_cols = 'id',
                       names_from = 'line',
                       values_from = 'label')

  if(!is.null(mortality_ids)) {
    data_wide <- data_wide %>%
      dplyr::mutate_at(2:ncol(.),
                       function(z) dplyr::case_when(
                         !is.na(z) ~ z,
                         is.na(z) & .$id %in% mortality_ids ~ "Dead",
                         is.na(z) & !.$id %in% mortality_ids ~ "Censored"
                       ))
  } else {
    data_wide <- data_wide %>%
      dplyr::mutate_at(2:ncol(.),
                       function(z) dplyr::case_when(
                         !is.na(z) ~ z,
                         is.na(z) ~ "Censored"
                       ))
  }

  df_wide_summary <- data_wide %>%
    dplyr::select(-1) %>%
    dplyr::group_by_all() %>%
    dplyr::count(sort=T) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      combo = dplyr::row_number()
    )

  df_wide_lines <- df_wide_summary %>%
    dplyr::select(-n,-combo)

  df_wide_lines <- df_wide_lines[,order(
    unique(data$line)[order(unique(data$line))]
    )]

  df_wide_n_id <- df_wide_summary %>%
    dplyr::select(n,combo)

  tx_sequence <- cbind(df_wide_lines,
                        df_wide_n_id)

  lot_lode <- df_wide_summary %>%
        tidyr::pivot_longer(
          cols = 1:NROW(which_lines),
          names_to = "linenumber",
          values_to = "linename_grouped"
        ) %>%
        dplyr::mutate(
          linename_grouped = factor(linename_grouped,
                                    levels = c(
                                      cats[order(cats)],
                                      "Censored",
                                      "Dead"
                                    )
          )
        )
  if (is.numeric(data$line)) {
    lot_lode <- lot_lode %>%
          dplyr::mutate(
            linenumber = factor(linenumber)
          )
  } else {
    lot_lode <- lot_lode %>%
          dplyr::mutate(
            linenumber = factor(linenumber, levels = levels(data$line))
          )
  }

  # Prepare labels
  label_df1 <- lot_lode %>%
      dplyr::group_by(linenumber) %>%
      dplyr::summarise(
        total_in_line = sum(n[!linename_grouped %in% c("Censored","Dead")]),
        total_inc_cens = sum(n),
        .groups = "drop"
      )

  label_df2 <- lot_lode %>%
      dplyr::group_by(linenumber, linename_grouped) %>%
      dplyr::summarise(
        total_in_line_on_regimen = sum(n),
        .groups = "drop"
      )

     # Add in these labels
    lot_lode <- lot_lode %>%
      dplyr::left_join(label_df1, "linenumber") %>%
      dplyr::left_join(label_df2, c("linenumber", "linename_grouped")) %>%
      dplyr::mutate(
        p_on_regimen = ifelse(
          !linename_grouped %in% c("Censored","Dead"),
          total_in_line_on_regimen / total_in_line,
          NA_real_
        ),
        p_on_regimen_cln = ifelse(
          !linename_grouped %in% c("Censored","Dead"),
          paste0(format(round(100 * p_on_regimen, 1), nsmall = 1), "%"),
          NA_character_
        ),
        label = dplyr::case_when(
          !linename_grouped %in% c("Censored","Dead") ~ paste0(
            linename_grouped, "\nN = ",
            scales::comma(total_in_line_on_regimen, accuracy = 1),
            " (", p_on_regimen_cln, ")"
            ),
          linename_grouped == 'Censored' ~ paste0("Censored\nN = ", scales::comma(total_in_line_on_regimen,
                                                                                  accuracy = 1
                                                                                  )),
          linename_grouped == "Dead" ~ paste0("Dead\nN = ", scales::comma(total_in_line_on_regimen,
                                                                          accuracy = 1
          ))
        ),
        label_select = ifelse(total_in_line_on_regimen / total_inc_cens > .1,
          label,
          ""
        ),
        label_select_na = ifelse(total_in_line_on_regimen / total_inc_cens > .1,
          label,
          NA_character_
        )
      )

  class(tx_sequence) <- c("tx_sequence", class(tx_sequence))
  attributes(tx_sequence)$label_1 <- label_df1
  attributes(tx_sequence)$label_2 <- label_df2
  attributes(tx_sequence)$has_mortality <- !is.null(mortality_ids)
  attributes(tx_sequence)$which_lines <- which_lines
  attributes(tx_sequence)$tx_cats <- cats
  attributes(tx_sequence)$lot_lode <- lot_lode
  attributes(tx_sequence)$n_groups <- n_groups


  return(tx_sequence)

}

