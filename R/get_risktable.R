#' Create Risk Table
#' 
#' @description Create a risktable from a survival object. 
#' 
#' @param x object to be passed on to the method
#' @param ... other arguments passed on to the method
#' @rdname get_risktable
#' 
#' @export
get_risktable <- function(x){
  UseMethod("get_risktable")
}


#' @param survfit_object a survival object
#' @param min_at_risk \code{numeric} The cutoff for number of subjects to display. Default is 0.
#' @param break_times Single numeric or numeric vector indicating breaks.
#' @param statlist Character vector indicating which summary data to present. Current choices are "n.risk" "n.event" "n.censor".
#' @param label Character vector with labels for the statlist. Default matches "n.risk" with "At risk", "n.event" with "Events" and "n.censor"
#'   with "Censored".
#' @param group String indicating the grouping variable for the risk tables. Current options are:
#'   \itemize{
#'     \item{"strata": groups the risk tables per stratum. The `label` specifies the lables used within each risk table. This is the default}
#'      \item{"statlist": groups the risk tables per statlist. The `label` specifies the title for each risk tabel. The strata levels
#'        are used for labeling within each risk table.}
#'   } "strata" to group the risk tables
#'   per strata, and "statlist" to group the risk tables 
#' @param collapse Boolean, indicates whether to present the data overall, rather than per strata.
#'   Default is FALSE.
#' @param fun Arbitrary function defining a transformation of the survival curve. This argument will also influence the y_ticks and y_label if not specified. 
#'    \itemize{
#'      \item{"surv": survival curve on the probability scale. The default y label will state "Survival probability".}
#'      \item{"log": log survival curve. The default y label will state "log(Survival probability)".}
#'      \item{"event": empirical CDF f(y) = 1-y. The default y label will state "Failure probability".}
#'      \item{"cloglog": complimentary log-log survival f(y) = log(-log(y)). The default y label will state "log(-log(Survival probability))".}
#'      \item{"pct": survival curve, expressed as percentage. The default y label will state "Survival probability".}
#'      \item{"logpct": log survival curve, expressed as percentage. The default y label will state "log(Survival probability".}
#'      \item{"cumhaz": MLE estimate of the cumulative hazard f(y) = -log(y). The default y label will state "cumulative hazard".}
#'    }
#' @rdname get_risktable
#' @method get_risktable default
#' @export
get_risktable.default <- function(survfit_object
                                ,min_at_risk = 0
                                ,break_times = NULL
                                ,statlist = c("n.risk")
                                ,label = "At risk"
                                ,group = "strata"
                                ,collapse = FALSE
                                ,fun = "surv"){
  
  #### User input validation ####
  
  if (min_at_risk < 0 && min_at_risk %% 1 == 0)
    stop("min_at_risk needs to be a positive integer.")
  
  if (length(label) < length(statlist)) {
    vlookup <- data.frame( statlist = c("n.risk", "n.censor", "n.event")
                           ,label = c("At risk", "Censored", "Events")
                           ,check.names = FALSE
                           ,stringsAsFactors = FALSE
    )
    
    label <- c(label, rep(NA, length(statlist)-length(label)))
    have <- data.frame( cbind(label, statlist)
                        ,check.names = FALSE
                        ,stringsAsFactors = FALSE
    )               
    
    label <- vlookup %>%
      dplyr::arrange(statlist) %>%
      dplyr::right_join(have, by = "statlist") %>%
      dplyr::mutate(label = coalesce(label.y, label.x)) %>% 
      dplyr::select(-label.x, -label.y) %>%
      dplyr::pull(label)
  }
  
  
  if (length(label) > length(statlist))
    label <- label[1:length(statlist)]
  
  statlist <- unique(statlist)
  
  tidy_object <- tidyme.survfit(survfit_object)
  
  times <- get_breaks(tidy_object, break_times, min_at_risk)
  
  survfit_summary <- summary(survfit_object, times = times, extend = TRUE)
  
  #### Risk table per statlist: labels of risk table are strata, titles are specifified through `label` ####
  
  per_statlist <- data.frame(
    time = survfit_summary$time,
    n.risk = survfit_summary$n.risk,
    n.event = survfit_summary$n.event,
    strata = base::sub('.*=', '', survfit_summary$strata)
  ) %>%
    ## correct calculation of n.censor
    dplyr::mutate(n.censor = dplyr::lag(n.risk) - (n.risk + n.event)) %>%
    dplyr::mutate(n.censor = case_when(
      n.censor >= 0 ~ n.censor,
      TRUE ~ 0
    )
    ) %>%
    dplyr::arrange(strata, time)%>%
    dplyr::rename(y_values = strata)
  
  final <- per_statlist
  final <-  final %>% select(time, y_values, statlist)
  attr(final, "title") <- label
  attr(final, "statlist") <- statlist
  
  #### Organize the risk tables per strata => reorganize the data ####
  
  if (group == "strata" & collapse == FALSE){
    per_strata <- per_statlist %>%
      dplyr::arrange(time) %>%
      tidyr::pivot_longer( cols = c("n.risk", "n.event", "n.censor")
                           ,names_to = "statlist"
                           ,values_to = "values") %>%
      tidyr::pivot_wider(names_from = "y_values", values_from = values) %>%
      dplyr::rename(y_values = statlist) %>%
      dplyr::filter(y_values %in% statlist)
    
    per_strata[["y_values"]] <- factor(per_strata[["y_values"]], levels = statlist, labels = label) 
    title <- unique(per_statlist[["y_values"]]) #Was: levels(per_statlist[["y_values"]]) but did not work
    final <- per_strata
    attr(final, "title") <- title
    attr(final, "statlist") <- title
  }
  
  #### Collapse: start from the group == "statlist" logic ####
  
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
      dplyr::filter(y_values %in% statlist)
    
    collapsed[["y_values"]] <- factor(collapsed[["y_values"]], levels = statlist, labels = label)
    collapsed <- collapsed %>%
      dplyr::arrange(y_values, time)
     
    final <- collapsed
    attr(final, 'title') <- "Overall"
    attr(final, 'statlist') <- "Overall"
  }
  attr(final, 'min_at_risk') <- min_at_risk
  class(final) <- c("risktable", class(final))
  return(final)
}

get_breaks <- function(tidy_object, break_times = NULL, min_at_risk){
  #### Pull out max time to consider ####
  max_time <-
    tidy_object %>%
    dplyr::filter(n.risk >= min_at_risk) %>%
    dplyr::group_by(strata) %>%
    dplyr::summarize(max_time = max(time)) %>%
    dplyr::ungroup() %>%
    dplyr::summarize(min_time = min(max_time)) %>%
    dplyr::pull(min_time)
  
  #### Time_ticks ####
  if (is.null(break_times)) {
    times <- seq(from = 0, to = max_time+1, by=round(max_time/10))
    warning("No break points defined. Default to 10 breaks. Use argument break_times to define custom break points")
  } else if (length(break_times) == 1) {
    times <- seq(from = 0, to = max_time+1, by=break_times)
  } else {
    times = break_times
  }
  
  #### Time_ticks ####
  times <- times[times <= max_time]
  return(times)
}
