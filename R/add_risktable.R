## should we make a method out of this?

 KM_object <- vr_KM_est(adtte, strata = "SEX") %>%
    vr_plot()
 min_at_risk = 0
 time_ticks <-NULL

str(KM_object)
KM_object$coordinates$limits


add_risktable <- function(
   KM_object
  ,min_at_risk = 0
  ,time_ticks = NULL
  ,display = c("n.risk", "n.censor") #other options could be n.censor n.event
  ,title  =c("Subjects at risk")
){
  
  #### User input validation ####
  if(inherits(KM_object, "survfit")){
    tidy_object <- tidyme(survfit_object)
    if (is.null(time_ticks)) time_ticks <- pretty(tidy_object[["time"]], 5)
  } else if (inherits(KM_object, "ggsurvfit")){
    tidy_object <- KM_object$data
    survfit_object <- eval(KM_object$data$call[[1]])
    ggbld <- ggplot_build(KM_object)
    if (is.null(time_ticks)) time_ticks <- as.numeric(ggbld$layout$panel_params[[1]]$x$get_labels())
  } else {
    stop("KM object is nor a plot or table created by visR.")
  }
  
  if (min_at_risk < 0 && min_at_risk %% 1 == 0) stop("min_at_risk needs to be a positive integer.")
  
  
  #### Pull out max time to consider ####
  max_time <- 
    tidy_object %>% 
    filter(n.risk >= min_at_risk) %>% 
    group_by(strata) %>% 
    summarize(max_time = max(time)) %>% 
    ungroup() %>% 
    summarize(min_time = min(max_time)) %>% 
    pull(min_time)
  
  #### Time_ticks ####
  times <- time_ticks[time_ticks <= max_time]

  # ## Create bins for at risk calculation => allows us to select max time where time <= time_ticks
  # binned_object <- survfit_object %>%
  #   mutate(bins = as.character(cut(time, c(-Inf, time_ticks), right = FALSE, include.lowest = TRUE)))%>%
  #   mutate(lim = sapply(strsplit(bins, ","), function(x) {
  #       y <- as.numeric(  gsub("\\D", "", x) )
  #       return(max(y[is.finite(y)]))
  #     }))
  # 
  # ## Ensure all combo's are present so at risk at every time tick for every strata can be calculated.
  # ## Include time to avoids missing times replaced by 0 when merged.     
  # filler <- expand(binned_object, strata, nesting(time, bins, lim))
  # 
  # pre_risk_table <- binned_object%>%
  #   right_join(filler, intersect(colnames(filler), colnames(.))) %>%
  #   arrange(strata, time)%>%
  #   mutate( n.risk = ifelse(is.na(n.risk), 0, n.risk)
  #          ,n.event = ifelse(is.na(n.event), 0, n.event)
  #          ,n.censor = ifelse(is.na(n.censor), 0, n.censor)
  #          )
  # 
  # ## Select time 0 to create lower limit record
  # time0_risk_table <- binned_object%>%
  #   group_by(strata)%>%
  #   filter(row_number() == 1)%>%
  #   mutate(lim = sapply(strsplit(bins, ","), function(x) {
  #     y <- as.numeric(  gsub("\\D", "", x) )
  #     return(min(y[is.finite(y)]))
  #   })
  #   , bins = "[-Inf, 0)"
  #   , time = 0
  #   , n.event = 0
  #   , n.censor = 0)
  #   
  # risk_table <- bind_rows(time0_risk_table, pre_risk_table)%>%
  #   group_by(strata) %>%
  #   mutate(n = max(n.risk)) %>%
  #   group_by(strata, lim) %>%
  #   summarise(n=min(n), n.event = sum(n.event), n.censor = sum(n.censor), time=min(lim)) %>%
  #   arrange(strata, time) %>%
  #   mutate(at.risk = n-cumsum(n.event)-cumsum(n.censor))

  #### Build risk table ####
  survfit_summary <- summary(survfit_object, times = times, extend = TRUE)
  
  summary_data <- data.frame(
      time = survfit_summary$time,
      n.risk = survfit_summary$n.risk,
      n.event = survfit_summary$n.event,
      strata = base::sub('.*=', '', survfit_summary$strata)
  ) %>%
    ## correct calculation of n.censor
    dplyr::mutate(n.censor = lag(n.risk) - (n.risk + n.event)) %>%
    dplyr::mutate(n.censor = case_when(
      n.censor >= 0 ~ n.censor,
      TRUE ~ 0
      )
    ) 
  
  if (!inherits(KM_object, "ggsurvfit")) return(summary_data)

  #### Plot all requested tables below => use list approach with map function ####
  .plttbl <- function(display, title){
      ggrisk <- ggplot2::ggplot(summary_data,aes(x = time, y = strata, label = format(get(display), nsmall = 0))) +
      ggplot2::geom_text(size = 3.5, hjust=0.5, vjust=0.5, angle=0, show.legend = F) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.title.x = element_text(size = 10, vjust = 1),
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
        ggplot2::ggtitle(title)
    }
    return(ggrisk)
  }


    # expansion <- ggbld$layout$panel_params[[1]]$x.range #to get perfect alignment between table and xaxis
    # 
    # ggbld <- ggplot_build(ggrisk)
    # expansion <- ggbld$layout$panel_params[[1]]$x.range #to get perfect alignment between table and xaxis
  
  
  ## Issue with passing a list of ggplots to AlignPlots: Save every ggplot in different object and then pass it to AlignPlots
  title <- c(title, rep(NA, (length(display)- length(title))))
  for (i in 1:length(display)){
    nam <- paste0("KM_object", i)
    assign(nam, .plttbl(display[i], title[i]))
  }
  
  ## ggplot2 - make plots equal in columns
  toArrange <- ls(pattern = "KM_object*")
  ggA <- do.call(AlignPlots, mget(toArrange) )
  
  ## cowplot allows to align according to an axis (+left) and change the heigth
  ggB <- cowplot::plot_grid(plotlist = ggA,
                           align = "l",
                           nrow = length(ggA),
                           rel_heights = c(1-(8/50 * (length(ggA)-1)), rep(8/50, length(ggA)-1))
                          )
  ggB
  
  return(ggB)
}


