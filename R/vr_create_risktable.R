#' Create Summary Table (also known as Table 1)
#' 
#' @description Create a risk table from a survival object. 
#' 
#' @param survfit_object a survival object
#' @param min_at_risk \code{numeric} The cutoff for number of subjects to display. Default is 0.
#' @param time_ticks Numeric vector with the points along the x-axis at which the summary data needs to be provided.
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
#'   
#' @export

vr_create_risktable <- function(survfit_object
                                ,min_at_risk = 0
                                ,time_ticks = NULL
                                ,statlist = c("n.risk")
                                ,label = ""
                                ,group = "strata"
                                ,collapse = FALSE){
  
  tidy_object <- 
    tidyme.survfit(survfit_object)
  
  #### Pull out max time to consider ####
  max_time <-
    tidy_object %>%
    dplyr::filter(n.risk >= min_at_risk) %>%
    dplyr::group_by(strata) %>%
    dplyr::summarize(max_time = max(time)) %>%
    dplyr::ungroup() %>%
    dplyr::summarize(min_time = min(max_time)) %>%
    dplyr::pull(min_time)
  ###TODO: add transformation and and adjustment from vr_plot??
  
  #### Time_ticks ####
  
  if (is.null(time_ticks)) times <- seq(from = 0, to = max_time+1, by=round(max_time/10))
  
  #### Summary ####
  
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
  
  title <- label
  final <- per_statlist
  
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
    title <- levels(per_statlist[["y_values"]])
    #statlist <- title
    final <- per_strata
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
    
    title <- "Overall"
    statlist <- "Overall"
    final <- collapsed
  }
  class(final) <- c("vr_risktable", class(final))
  final
}
