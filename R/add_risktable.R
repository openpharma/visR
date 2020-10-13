#' @title Add risk tables to visR plots through an S3 method
#'
#' @description S3 method for adding risk tables to visR plots.
#'     No default method is available at the moment.
#'
#' @author Steven Haesendonckx
#'
#' @seealso \code{\link[cowplot]{plot_grid}}
#'
#' @param gg visR object
#' @param ... other arguments passed on to the method
#'
#' @examples
#' \donttest{
#' library(survival)
#' library(dplyr)
#' library(tidyr)
#' library(ggplot2)
#' library(cowplot)
#' library(gtable)
#'
#' ## Display 2 risk tables, 1 per statlist
#' adtte %>%
#'   vr_KM_est(strata = "TRTP") %>%
#'   vr_plot() %>%
#'   add_risktable( min_at_risk = 3
#'                 ,label = c("Subjects at Risk", "Censored")
#'                 ,statlist = c("n.risk", "n.censor")
#'                 ,group = "statlist"
#'                 )
#'                 
#' ## Display 2 risk tables, 1 per stratum
#' adtte %>%
#'   vr_KM_est(strata = "TRTP") %>%
#'   vr_plot() %>%
#'   add_risktable( min_at_risk = 3
#'                 ,label = c("Subjects at Risk", "Censored")
#'                 ,statlist = c("n.risk", "n.censor")
#'                 ,group = "strata"
#'                 )
#'
#' ## Display overall risk table
#' adtte %>%
#'   vr_KM_est(strata = "TRTP") %>%
#'   vr_plot() %>%
#'   add_risktable( min_at_risk = 3
#'                 ,label = c("Subjects at Risk", "Censored")
#'                 ,statlist = c("n.risk", "n.censor")
#'                 ,collapse = TRUE
#'                 )
#'
#' }
#'
#' @return Object of class \code{ggplot} with added risk table.
#'
#' @rdname add_risktable
#'
#' @export

add_risktable <- function(gg, ...){
  UseMethod("add_risktable")
}

#' @param gg visR plot of class `ggsurvfit`
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
#' @rdname add_risktable
#' @method add_risktable ggsurvfit
#' @export

add_risktable.ggsurvfit <- function(
   gg
  ,min_at_risk = 0
  ,time_ticks = NULL
  ,statlist = c("n.risk")
  ,label  = NULL
  ,group = "strata"
  ,collapse = FALSE
){

  #### User input validation ####

  if (inherits(gg, "ggsurvfit")){
    tidy_object <- gg$data
    survfit_object <- eval(gg$data$call[[1]])
    ggbld <- ggplot2::ggplot_build(gg)
    if (is.null(time_ticks)) time_ticks <- as.numeric(ggbld$layout$panel_params[[1]]$x$get_labels())
  } else {
    stop("Error in add_risktable: gg is not of class `ggsurvfit`.")
  }
  if (!base::any(statlist %in% c("n.risk", "n.censor", "n.event")))
    stop("Error in add_risktable: statlist argument not valid.")
  if (!base::is.logical(collapse))
    stop("Error in add_risktable: collapse is expected to be boolean.")

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

  times <- time_ticks[time_ticks <= max_time]
  
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
    statlist <- title
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
  
  #### Plot all requested tables below => use list approach with map function ####

  tbls <-  base::Map(function(statlist, title = NA) {
    ggrisk <- ggplot2::ggplot(final, aes(x = time,
                                         y = stats::reorder(y_values, desc(y_values)),
                                             label = format(get(statlist), nsmall = 0) # = value columns
                                         )
                              ) +
      ggplot2::geom_text(size = 3.5, hjust=0.5, vjust=0.5, angle=0, show.legend = F) +
      ggplot2::theme_bw() +
      ggplot2::scale_x_continuous(breaks = times,
                                  limits = c(min(time_ticks), max(time_ticks))) +
      ggplot2::theme(axis.title.x = element_text(size = 8,
                                                 vjust = 1,
                                                 hjust = 1),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.border = element_blank(),
                     axis.line = element_blank(),
                     axis.text.x = element_blank(),
                     axis.ticks = element_blank(),
                     axis.text.y = element_text(size=8, colour = "black", face = "plain"),
                     plot.margin = unit(c(1,0,0,0), "lines"),
                     plot.title = element_text(hjust = 0, vjust = 0)
                    ) +
      ggplot2::xlab(NULL) +
      ggplot2::ylab(NULL)

      if (!is.na(title) && !is.null(title)){
        ggrisk <- ggrisk +
          ggplot2::ggtitle(title) +
          theme(plot.title = element_text(size = 10))
      }

      return(ggrisk)
    },
    statlist = as.list(statlist),
    title = as.list(title)
  )


  #### Make plots same width ####
  ggA <- list(gg) %>%
    append(tbls) %>%
    AlignPlots()


  #### cowplot allows to align according to an axis (+left) and change the heigth ####
  ggB <- cowplot::plot_grid(plotlist = ggA,
                           align = "none",
                           nrow = length(ggA),
                           rel_heights = c(1-(8/50 * (length(ggA)-1)), rep(8/50, length(ggA)-1))
                          )

  return(ggB)
}
