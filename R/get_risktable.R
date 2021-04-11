#' @title Obtain risk tables for tables and plots
#'
#' @description Create a risk table from an object using an S3 method. Currently, no default method is defined.
#' 
#' @seealso \code{\link[survival]{summary}}
#' 
#' @param x object to be passed on to the method
#' @param ... other arguments passed on to the method
#'  
#' @rdname get_risktable
#' 
#' @export

get_risktable <- function(x, ...){
  UseMethod("get_risktable")
} 

#' @param survfit_object an object of class `survfit`
#' @param min_at_risk \code{numeric} The cutoff for number of participants at risk to display. This minimum is applied across strata. 
#'   Default is 0.
#' @param breaks Numeric vector indicating the times at which the risk set, censored subjects, events are calculated.
#' @param statlist Character vector indicating which summary data to present. Current choices are "n.risk" "n.event" "n.censor".
#'   Default is "n.risk".
#' @param label Character vector with labels for the statlist. Default matches "n.risk" with "At risk", "n.event" with "Events" and "n.censor"
#'   with "Censored".
#' @param group String indicating the grouping variable for the risk tables. Current options are:
#'   \itemize{
#'     \item{"strata": groups the risk tables per stratum. The `label` specifies the label within each risk tabel. The strata levels
#'        are used for the titles of the risk tables. This is the default}
#'      \item{"statlist": groups the risk tables per statlist. The `label` specifies the title for each risk tabel. The strata levels
#'        are used for labeling within each risk table.}
#'   } 
#'   Default is "strata".
#' @param collapse Boolean, indicates whether to present the data overall.
#'   Default is FALSE.
#'
#' @rdname get_risktable
#'
#' @rdname get_risktable
#' @method get_risktable survfit
#' @export

get_risktable.survfit <- function(
    survfit_object
   ,min_at_risk = 0
   ,breaks = NULL
   ,statlist = c("n.risk")
   ,label = NULL
   ,group = "strata"
   ,collapse = FALSE
){

# User input validation ---------------------------------------------------

  if (!base::any(statlist %in% c("n.risk", "n.censor", "n.event")))
    stop("statlist argument not valid.")
  
  if (!base::is.logical(collapse))
    stop("Error in get_risktable: collapse is expected to be boolean.")

  if (min_at_risk < 0 | min_at_risk %% 1 != 0)
    stop("min_at_risk needs to be a positive integer.")
  
  if (min_at_risk > max(survfit_object$n.risk)){
    tidy_object <- tidyme(survfit_object)

    max_at_risk <- tidy_object %>% 
      dplyr::group_by(strata) %>%
      dplyr::summarize(risk = max(n.risk))
    
    stop(paste0("min_at_risk larger than the risk available in any strata. Maximum at risk is ", max(max_at_risk[["risk"]]), " in stratum ", max_at_risk[which(max_at_risk["risk"] == max(max_at_risk[["risk"]])), "strata"] ,"."))
  }
  
# Clean input ------------------------------------------------------------
  
  tidy_object <- tidyme(survfit_object)
  
  statlist <- unique(statlist)
  
  if (!is.null(breaks)) breaks <- order(breaks)
  
  if (length(label) <= length(statlist)) {
    vlookup <- data.frame( statlist = c("n.risk", "n.censor", "n.event")
                          ,label = c("At risk", "Censored", "Events")
                          ,check.names = FALSE
                          ,stringsAsFactors = FALSE)

    label <- c(label, rep(NA, length(statlist)-length(label)))
    have <- data.frame( cbind(label, statlist)
                       ,check.names = FALSE
                       ,stringsAsFactors = FALSE)

    label <- vlookup %>%
      dplyr::arrange(statlist) %>%
      dplyr::right_join(have, by = "statlist") %>%
      dplyr::mutate(label = dplyr::coalesce(label.y, label.x)) %>%
      dplyr::select(-label.x, -label.y) %>%
      dplyr::pull(label)

  }


  if (length(label) > length(statlist))
    label <- label[1:length(statlist)]

  
# Pull out the max time to consider ---------------------------------------

  max_time <-
    tidy_object %>%
    dplyr::filter(n.risk >= min_at_risk) %>%
    dplyr::group_by(strata) %>%
    dplyr::summarize(max_time = max(time)) %>%
    dplyr::ungroup() %>%
    dplyr::summarize(min_time = min(max_time)) %>%
    dplyr::pull(min_time)

# Generate time ticks ----------------------------------------------------

  if (is.null(breaks)) {
     times <- pretty(survfit_object$time, 10)
  } else {
    times <- breaks
  }

  times <- times[times <= max_time]


# Summary -----------------------------------------------------------------

  survfit_summary <- summary(survfit_object, times = times, extend = TRUE)

# Risk table per statlist -------------------------------------------------

  ## labels of risk table are strata, titles are specifified through `label

  per_statlist <- data.frame(
    time = survfit_summary$time,
    n.risk = survfit_summary$n.risk,
    n.event = survfit_summary$n.event,
    strata = base::factor(base::sub('.*=', '', survfit_summary$strata), levels = base::sub('.*=', '', levels(survfit_summary$strata)))
  ) %>%
    ## correct calculation of n.censor
    dplyr::mutate(n.censor = dplyr::lag(n.risk) - (n.risk + n.event)) %>%
    dplyr::mutate(n.censor = dplyr::case_when(
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

  return(final)
}

