#' @title Obtain risk tables for tables and plots
#'
#' @description Create a risk table from an object using an S3 method.
#'   Currently, no default method is defined.
#'
#' @seealso \code{\link[survival]{summary.survfit}}
#'
#' @param x an object of class `survfit` or `tidycuminc`
#' @param times Numeric vector indicating the times at which the risk set, censored subjects, events are calculated.
#' @param statlist Character vector indicating which summary data to present. Current choices are "n.risk" "n.event"
#'   "n.censor", "cum.event", "cum.censor".
#'   Default is "n.risk".
#' @param label Character vector with labels for the statlist. Default matches "n.risk" with "At risk", "n.event" with
#'   "Events", "n.censor" with "Censored", "cum.event" with "Cum. Event", and "cum.censor" with "Cum. Censor".
#' @param group String indicating the grouping variable for the risk tables.
#'   Current options are:
#'     \itemize{
#'     \item{"strata": groups the risk tables per stratum.
#'       The `label` specifies the label within each risk table. The strata levels
#'       are used for the titles of the risk tables. This is the default}
#'     \item{"statlist": groups the risk tables per statlist.
#'       The `label` specifies the title for each risk table. The strata levels
#'       are used for labeling within each risk table.}
#'     }
#'   Default is "strata".
#' @param collapse Boolean, indicates whether to present the data overall.
#'   Default is FALSE.
#' @param ... other arguments passed on to the method
#'
#' @return return list of attributes the form the risk table i.e.
#'   number of patients at risk per strata
#' @rdname get_risktable
#' @export
#'
get_risktable <- function(x, ...) {
  UseMethod("get_risktable")
}

#' @rdname get_risktable
#' @method get_risktable survfit
#' @export
get_risktable.survfit <- function(x,
                                  times = NULL,
                                  statlist = "n.risk",
                                  label = NULL,
                                  group = c("strata", "statlist"),
                                  collapse = FALSE,
                                  ...) {

  # User input validation ---------------------------------------------------
  group <- match.arg(group)

  if (!base::all(statlist %in% c(
    "n.risk", "n.censor", "n.event",
    "cum.censor", "cum.event"
  ))) {
    stop("statlist argument not valid. Current options are n.risk, n.censor,
         n.event, cum.event, cum.censor")
  }

  if (!is.null(label) & !base::all(is.character(label)) & !base::inherits(label, "factor")) {
    stop("label arguments should be of class `character` or `factor`.")
  }

  if (!base::is.logical(collapse)) {
    stop("Error in get_risktable: collapse is expected to be boolean.")
  }

  if (base::any(times < 0)) {
    stop("Negative times are not valid.")
  }

  # Clean input ------------------------------------------------------------

  tidy_object <- tidyme(x)
  statlist <- unique(statlist)

  # Match amount of elements in label with statlist -------------------------

  if (length(label) <= length(statlist)) {
    vlookup <- data.frame(
      statlist = c(
        "n.risk", "n.censor", "n.event",
        "cum.censor", "cum.event"
      ),
      label = c(
        "At risk", "Censored", "Events",
        "Cum. Censored", "Cum. Events"
      ),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

    if (is.null(label)) label <- NA
    label <- c(label, rep(NA, length(statlist) - length(label)))

    have <- data.frame(cbind(label, statlist),
      check.names = FALSE,
      stringsAsFactors = TRUE
    )

    label_lookup <- vlookup %>%
      dplyr::right_join(have, by = "statlist") %>%
      dplyr::mutate(label = dplyr::coalesce(label.y, label.x)) %>%
      dplyr::select(-label.x, -label.y) %>%
      as.data.frame()
  } else if (length(label) > length(statlist)) {
    label_lookup <- data.frame(
      statlist = statlist,
      label = label[1:length(statlist)],
      check.names = FALSE,
      stringsAsFactors = TRUE
    )
  }


  # Ensure the order of the label corresponds to statlist order-------------

  statlist_order <- factor(statlist, levels = statlist)
  label_lookup[["statlist"]] <- factor(label_lookup[["statlist"]], levels = statlist)
  label_lookup <- label_lookup[order(label_lookup[["statlist"]]), ]

  # Generate time ticks ----------------------------------------------------

  if (is.null(times)) {
    times <- pretty(x$time, 10)
  } else {
    times <- times[order(unique(times))]
  }

  # Summary -----------------------------------------------------------------

  survfit_summary <- summary(x, times = times, extend = TRUE)

  # Risk table per statlist -------------------------------------------------

  ## labels of risk table are strata, titles are specified through `label

  per_statlist <-
    data.frame(
      time = survfit_summary$time,
      strata =
        base::factor(.get_strata(survfit_summary[["strata"]]),
          levels = unique(.get_strata(survfit_summary[["strata"]]))
        ),
      n.risk = survfit_summary[["n.risk"]],
      n.event = survfit_summary[["n.event"]],
      n.censor = survfit_summary[["n.censor"]]
    ) %>%
    dplyr::arrange(.data[["strata"]], .data[["time"]]) %>%
    dplyr::group_by(.data[["strata"]]) %>%
    dplyr::mutate(
      cum.event = cumsum(.data[["n.event"]]),
      cum.censor = cumsum(.data[["n.censor"]])
    ) %>%
    dplyr::ungroup() %>%
    dplyr::rename(y_values = strata) %>%
    as.data.frame()

  final <- per_statlist[, c("time", "y_values", levels(statlist_order))]

  attr(final, "time_ticks") <- times
  attr(final, "title") <- label_lookup[["label"]]
  attr(final, "statlist") <- levels(label_lookup[["statlist"]])

  # Organize the risk tables per strata => reorganize the data --------------

  if (group == "strata" & collapse == FALSE) {
    per_strata <- per_statlist %>%
      dplyr::arrange(time) %>%
      tidyr::pivot_longer(
        cols = c(
          "n.risk", "n.censor", "n.event",
          "cum.censor", "cum.event"
        ),
        names_to = "statlist",
        values_to = "values"
      ) %>%
      tidyr::pivot_wider(names_from = "y_values", values_from = values) %>%
      dplyr::rename(y_values = statlist) %>%
      dplyr::filter(y_values %in% statlist) %>%
      as.data.frame()

    per_strata[["y_values"]] <-
      factor(per_strata[["y_values"]],
        levels = levels(label_lookup[["statlist"]]),
        labels = label_lookup[["label"]]
      )
    per_strata <- per_strata[order(per_strata[["y_values"]]), ]

    title <- levels(per_statlist[["y_values"]])

    final <- per_strata
    attr(final, "time_ticks") <- times
    attr(final, "title") <- title
    attr(final, "statlist") <- title
  }

  # Collapse: start from the group == "statlist" logic ------------------------

  if (collapse == TRUE) {
    collapsed <- per_statlist %>%
      dplyr::arrange(time) %>%
      dplyr::mutate(strata = "Overall") %>%
      dplyr::group_by(time, strata) %>%
      dplyr::summarise(
        n.risk = sum(n.risk),
        n.event = sum(n.event),
        n.censor = sum(n.censor),
        cum.event = sum(.data[["cum.event"]]),
        cum.censor = sum(.data[["cum.censor"]])
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(-strata) %>%
      tidyr::pivot_longer(
        cols = c(
          "n.risk", "n.censor", "n.event",
          "cum.censor", "cum.event"
        ),
        names_to = "y_values",
        values_to = "Overall"
      ) %>%
      dplyr::filter(y_values %in% statlist) %>%
      as.data.frame()

    collapsed[["y_values"]] <- factor(collapsed[["y_values"]], levels = label_lookup[["statlist"]], labels = label_lookup[["label"]])
    collapsed <- collapsed %>%
      dplyr::arrange(y_values, time)

    final <- collapsed

    attr(final, "time_ticks") <- times
    attr(final, "title") <- "Overall"
    attr(final, "statlist") <- "Overall"
  }

  class(final) <- c("risktable", class(final))

  return(final)
}


#' @rdname get_risktable
#' @method get_risktable tidycuminc
#' @export
get_risktable.tidycuminc <- function(x,
                                     times = pretty(x$tidy$time, 10),
                                     statlist = "n.risk",
                                     label = NULL,
                                     group = c("strata", "statlist"),
                                     collapse = FALSE,
                                     ...) {
  # check for installation of tidycmprsk package
  rlang::check_installed("tidycmprsk", version = "0.1.1")
  group <- match.arg(group)

  # list of statistics and their default labels
  lst_stat_labels_default <-
    list(
      n.risk = "At Risk",
      n.event = "Events",
      n.censor = "Censored",
      cum.event = "Cum. Events",
      cum.censor = "Cum. Censored"
    )

  label <-
    .reconcile_statlist_and_labels(
      statlist = statlist,
      label = label,
      default_labels = lst_stat_labels_default
    )

  # named list of stats and labels
  lst_stat_labels <- as.list(label) %>% stats::setNames(statlist)

  tidy <-
    visr_tidy_tidycuminc(x, times = times) %>%
    dplyr::select(dplyr::any_of(c("time", "outcome", "strata", names(lst_stat_labels_default))))

  if (isTRUE(collapse)) {
    tidy <-
      tidy %>%
      dplyr::mutate(strata = "Overall") %>%
      dplyr::group_by(dplyr::across(dplyr::any_of(c("time", "outcome", "strata")))) %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::any_of(c(
            "n.risk", "n.event", "cum.event",
            "n.censor", "cum.censor"
          )),
          ~ sum(., na.rm = TRUE)
        )
      ) %>%
      dplyr::filter(dplyr::row_number() == 1L) %>%
      dplyr::ungroup()
  }

  if (group == "strata" || isTRUE(collapse)) {
    strata_levels <- unique(tidy[["strata"]]) %>%
      sort() %>%
      as.character()

    result <-
      tidy %>%
      dplyr::select(dplyr::any_of(c(
        "time", "strata", "n.risk", "n.event",
        "cum.event", "n.censor", "cum.censor"
      ))) %>%
      tidyr::pivot_longer(cols = -c(.data[["time"]], .data[["strata"]])) %>%
      tidyr::pivot_wider(
        id_cols = c(.data[["time"]], .data[["name"]]),
        values_from = "value",
        names_from = "strata"
      ) %>%
      dplyr::relocate(dplyr::any_of(strata_levels), .after = dplyr::last_col()) %>%
      dplyr::mutate(
        y_values = dplyr::recode(.data[["name"]], !!!lst_stat_labels)
      ) %>%
      dplyr::filter(.data[["name"]] %in% .env[["statlist"]]) %>%
      dplyr::select(.data[["time"]], .data[["y_values"]], dplyr::everything(), -.data[["name"]]) %>%
      dplyr::mutate(y_values = factor(.data[["y_values"]], levels = .env[["label"]])) %>%
      dplyr::arrange(.data[["y_values"]], .data[["time"]]) %>%
      as.data.frame()
    attr(result, "title") <- names(result) %>% setdiff(c("time", "y_values"))
    attr(result, "statlist") <- names(result) %>% setdiff(c("time", "y_values"))
  } else if (group == "statlist") {
    result <-
      tidy %>%
      dplyr::select(.data[["time"]],
        y_values = .data[["strata"]],
        dplyr::any_of(c(
          "n.risk",
          "n.event", "cum.event",
          "n.censor", "cum.censor"
        ))
      ) %>%
      as.data.frame()

    attr(result, "statlist") <- names(lst_stat_labels_default[statlist])
    attr(result, "title") <- lst_stat_labels_default[statlist] %>%
      unlist() %>%
      unname()
  }

  attr(result, "time_ticks") <- unique(result$time) %>% sort()
  class(result) <- c("risktable", class(result))
  result
}

.reconcile_statlist_and_labels <- function(statlist, label, default_labels) {
  # return label as is if length matches statlist
  if (!is.null(label) && length(statlist) == length(label)) {
    return(label)
  }

  # initialize empty vector if NULL
  if (is.null(label)) {
    label <- character(0L)
  }

  # replace labels with defaults if not passed by user
  for (i in seq_along(statlist)) {
    label[i] <-
      dplyr::coalesce(
        label[i],
        default_labels[[statlist[i]]] %||% NA_character_,
        statlist[i]
      )
  }

  return(label[seq_along(statlist)])
}
