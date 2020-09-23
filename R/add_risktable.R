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
#' survfit_object <- survival::survfit(data = adtte, Surv(AVAL, 1-CNSR) ~ TRTP)
#' #vr_plot(survfit_object) %>%
#' #  add_risktable(min_at_risk = 3, title = c("blah"), display = c("n.risk", "n.censor"))
#'  
#'  ## TODO: Check the following errors when above example is run
#'  # > Warning: NAs introduced by coercion
#'  # > Warning: NAs introduced by coercion
#'  # Error in data.matrix(data): (list) object cannot be coerced to type 'double'
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
#' @param display Character vector indicating which summary data to present. Current choices are "n.risk" "n.event" "n.censor".
#' @param title Character vector with titles for the summary tables.
#'  
#' @rdname add_risktable
#' @method add_risktable ggsurvfit
#' @export
#' 
add_risktable.ggsurvfit <- function(
   gg
  ,min_at_risk = 0
  ,time_ticks = NULL
  ,display = c("n.risk")
  ,title  =c("Subjects at risk")
){
  
  #### User input validation ####
  
  if (inherits(gg, "ggsurvfit")){
    tidy_object <- gg$data
    survfit_object <- eval(gg$data$call[[1]])
    ggbld <- ggplot_build(gg)
    if (is.null(time_ticks)) time_ticks <- as.numeric(ggbld$layout$panel_params[[1]]$x$get_labels())
  } else {
    stop("Error in add_risktable: gg is not of class `ggsurvfit`.")
  }
  if (!base::any(display %in% c("n.risk", "n.censor", "n.event")))
    stop("Error in add_risktable: Display argument not valid.")
  
  if (min_at_risk < 0 && min_at_risk %% 1 == 0) 
    stop("min_at_risk needs to be a positive integer.")
  
  if (length(title) < length(display)) 
    title <- c(title, rep(NA, length(display)-length(title)))
  if (length(title) > length(display)) 
    title <- title[1:length(display)]
  
  
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

  #### Plot all requested tables below => use list approach with map function ####
  
  tbls <-  base::Map(function(display, title = NA) {
    ggrisk <- ggplot2::ggplot(summary_data,aes(x = time, y = strata, label = format(get(display), nsmall = 0))) +
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
    display = as.list(display),
    title = as.list(c(title, rep(NA, (length(display)- length(title)))))
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