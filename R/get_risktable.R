#' @title Obtain risk tables for tables and plots
#'
#' @description Create a risk table from an object using an S3 method.
#'   Currently, no default method is defined.
#'
#' @seealso \code{\link[survival]{summary.survfit}}
#'
#' @param x object to be passed on to the method
#' @param ... other arguments passed on to the method
#' @return return list of attributes the form the risk table i.e.
#'   number of patients at risk per strata
#' @rdname get_risktable
#'
#' @export

get_risktable <- function(x, ...){
  UseMethod("get_risktable")
}

#' @param x an object of class `survfit` or `tidycuminc`
#' @param times Numeric vector indicating the times at which the risk set, censored subjects, events are calculated.
#' @param statlist Character vector indicating which summary data to present. Current choices are "n.risk" "n.event" "n.censor".
#'   Default is "n.risk". Competing risk models also have the option of "cumulative.event" and "cumulative.censor"
#' @param label Character vector with labels for the statlist. Default matches "n.risk" with "At risk", "n.event" with "Events" and "n.censor"
#'   with "Censored".
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
#' @return return list of attributes the form the risk table i.e.
#'   number of patients at risk per strata
#'
#' @rdname get_risktable
#' @method get_risktable survfit
#' @export

get_risktable.survfit <- function(
  x
  ,times = NULL
  ,statlist = c("n.risk")
  ,label = NULL
  ,group = "strata"
  ,collapse = FALSE
  ,...
){

  # User input validation ---------------------------------------------------
  if (!base::all(statlist %in% c("n.risk", "n.censor", "n.event")))
    stop("statlist argument not valid. Current options are n.risk, n.censor and n.event.")

  if (!is.null(label) & !base::all(is.character(label)) & !base::inherits(label, "factor"))
    stop("label arguments should be of class `character` or `factor`.")

  if (!base::is.logical(collapse))
    stop("Error in get_risktable: collapse is expected to be boolean.")

  if (base::any(times < 0))
    stop("Negative times are not valid.")

  if (length(group)>1 | !(base::all(group %in% c("statlist", "strata"))))
    stop("group should equal statlist or strata.")

  # if (min_at_risk > max(x$n.risk)){
  #   tidy_object <- tidyme(x)
  #
  #   max_at_risk <- tidy_object %>%
  #     dplyr::group_by(strata) %>%
  #     dplyr::summarize(risk = max(n.risk))
  #
  #   stop(paste0("min_at_risk larger than the risk available in any strata. Maximum at risk is ", max(max_at_risk[["risk"]]), " in stratum ", max_at_risk[which(max_at_risk["risk"] == max(max_at_risk[["risk"]])), "strata"] ,"."))
  # }

  # Clean input ------------------------------------------------------------

  tidy_object <- tidyme(x)

  statlist <- unique(statlist)

  label <-
    .reconcile_statlist_and_labels(
      statlist = statlist,
      label = label,
      default_labels =
        list(n.risk = "At risk",
             n.censor = "Censored",
             n.event = "Events")
    )

  # # Pull out the max time to consider ---------------------------------------
  #
  #   max_time <-
  #     tidy_object %>%
  #     dplyr::filter(n.risk >= min_at_risk) %>%
  #     dplyr::group_by(strata) %>%
  #     dplyr::summarize(max_time = max(time)) %>%
  #     dplyr::ungroup() %>%
  #     dplyr::summarize(max_time = max(max_time)) %>%
  #     dplyr::pull(max_time)

  # Generate time ticks ----------------------------------------------------

  if (is.null(times)) {
    times <- pretty(x$time, 10)
  }

  # if (max_time %in% times)
  #   times <- times[0 <= times & times <= max_time]
  # else #make sure the min at risk is shown eg when falls between 180 and 200
  #   times <- unique(times[c(which(0 <= times & times <= max_time), min(length(times), max(which(0 <= times & times <= max_time))+1))])

  # Summary -----------------------------------------------------------------

  survfit_summary <- summary(x, times = times, extend = TRUE)

  # Risk table per statlist -------------------------------------------------

  ## labels of risk table are strata, titles are specifified through `label

  per_statlist <- data.frame(
    time = survfit_summary$time,
    strata = base::factor(.get_strata(survfit_summary[["strata"]]), levels = unique(.get_strata(survfit_summary[["strata"]]))),
    n.risk = survfit_summary$n.risk,
    n.event = survfit_summary$n.event
  ) %>%
    ## correct calculation of n.censor
    dplyr::mutate(n.censor = dplyr::lag(n.risk) - (n.risk + n.event)) %>%
    dplyr::mutate(
      n.censor = dplyr::case_when(
        n.censor >= 0 ~ n.censor,
        TRUE ~ 0
      )
    ) %>%
    dplyr::arrange(strata, time)%>%
    dplyr::rename(y_values = strata)%>%
    as.data.frame()

  final <-  per_statlist

  attr(final, 'time_ticks') <- times
  attr(final, "title") <- label
  attr(final, "statlist") <- statlist

  # Organize the risk tables per strata => reorganize the data --------------

  if (group == "strata" & collapse == FALSE){
    per_strata <- per_statlist %>%
      dplyr::arrange(time) %>%
      tidyr::pivot_longer( cols = c("n.risk", "n.event", "n.censor")
                           ,names_to = "statlist"
                           ,values_to = "values") %>%
      tidyr::pivot_wider(names_from = "y_values", values_from = values) %>%
      dplyr::rename(y_values = statlist) %>%
      dplyr::filter(y_values %in% statlist)%>%
      as.data.frame()

    per_strata[["y_values"]] <- factor(per_strata[["y_values"]], levels = statlist, labels = label)
    title <- levels(per_statlist[["y_values"]])

    final <- per_strata
    attr(final, 'time_ticks') <- times
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
        n.risk = sum(n.risk)
        ,n.event = sum(n.event)
        ,n.censor = sum(n.censor)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(-strata) %>%
      tidyr::pivot_longer( cols = c("n.risk", "n.event", "n.censor")
                           ,names_to = "y_values"
                           ,values_to = "Overall") %>%
      dplyr::filter(y_values %in% statlist) %>%
      as.data.frame()

    collapsed[["y_values"]] <- factor(collapsed[["y_values"]], levels = statlist, labels = label)
    collapsed <- collapsed %>%
      dplyr::arrange(y_values, time)

    final <- collapsed

    attr(final, 'time_ticks') <- times
    attr(final, 'title') <- "Overall"
    attr(final, 'statlist') <- "Overall"
  }

  class(final) <- c("risktable", class(final))

  return(final)
}


#' @rdname get_risktable
#' @method get_risktable tidycuminc
#' @export
get_risktable.tidycuminc <- function(x
                                     ,times = pretty(x$tidy$time, 10)
                                     ,statlist = c("n.risk")
                                     ,label = NULL
                                     ,group = "strata"
                                     ,collapse = FALSE
                                     ,...) {
  # list of statistics and their default labels
  lst_stat_labels_default <-
    list(n.risk = "At Risk",
         n.event = "N Event",
         n.censor = "N Censored",
         cumulative.event = "Cum. N Event",
         cumulative.censor = "Cum. N Censored")

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
          dplyr::any_of(c("n.risk", "n.event", "cumulative.event",
                          "n.censor", "cumulative.censor")),
          ~sum(., na.rm = TRUE))
      ) %>%
      dplyr::filter(dplyr::row_number() == 1L) %>%
      dplyr::ungroup()
  }

  if (group %in% "strata" || isTRUE(collapse)) {
    result <-
      tidy %>%
      dplyr::select(dplyr::any_of(c("time", "strata", "n.risk", "n.event",
                                    "cumulative.event", "n.censor", "cumulative.censor"))) %>%
      tidyr::pivot_longer(cols = -c(.data$time, .data$strata)) %>%
      tidyr::pivot_wider(
        id_cols = c(.data$time, .data$name),
        values_from = "value",
        names_from = "strata"
      ) %>%
      dplyr::mutate(
        y_values = dplyr::recode(.data$name, !!!lst_stat_labels)
      ) %>%
      dplyr::filter(.data$name %in% .env$statlist) %>%
      dplyr::select(.data$time, .data$y_values, dplyr::everything(), -.data$name) %>%
      as.data.frame()
    attr(result, "title") <- names(result) %>% setdiff(c("time", "y_values"))
    attr(result, "statlist") <- names(result) %>% setdiff(c("time", "y_values"))
  }
  else if (group %in% "statlist") {
    result <-
      tidy %>%
      dplyr::select(.data$time, y_values = .data$strata,
                    dplyr::any_of(c("n.risk",
                                    "n.event", "cumulative.event",
                                    "n.censor", "cumulative.censor"))) %>%
      as.data.frame()

    attr(result, "statlist") <- names(lst_stat_labels_default[statlist])
    attr(result, "title") <- lst_stat_labels_default[statlist] %>% unlist() %>% unname()
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


