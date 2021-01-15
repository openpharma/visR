#' @title Add risk tables to visR plots through an S3 method
#'
#' @description S3 method for adding risk tables to visR plots.
#'     No default method is available at the moment.
#'
#' @seealso \code{\link[cowplot]{plot_grid}}
#'
#' @param gg visR plot of class `ggsurvfit`
#' @param risktable A risktable created with method 'get_risktable'
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

add_risktable <- function(gg
                          ,min_at_risk = 0
                          ,break_times = NULL
                          ,statlist = c("n.risk")
                          ,label = "At risk"
                          ,group = "strata"
                          ,collapse = FALSE
                          ,fun = "surv"){
  UseMethod("add_risktable")
}

#' @rdname add_risktable
#' @method add_risktable ggsurvfit
#' @export

add_risktable.ggsurvfit <- function(
   gg
   ,min_at_risk = 0
   ,break_times = NULL
   ,statlist = c("n.risk")
   ,label = "At risk"
   ,group = "strata"
   ,collapse = FALSE
   ,fun = "surv"
){


  final <- get_risktable(gg
                         ,min_at_risk
                         ,break_times
                         ,statlist
                         ,label
                         ,group
                         ,collapse
                         ,fun)
  
  time_ticks <- attributes(final)$time_ticks
  times <- as.numeric(unique(final$time))
  statlist <- attributes(final)$statlist
  title <- attributes(final)$title
  
  
  #### Plot all requested tables below => use list approach with map function ####

  tbls <-  base::Map(function(statlist, title = NA) {
    ggrisk <- ggplot2::ggplot(final, aes(x = time,
                                         y = stats::reorder(y_values, desc(y_values)),
                                             label = format(get(statlist), nsmall = 0) # = value columns
                                         )
                              ) +
      ggplot2::geom_text(size = 3.0, hjust=.5, vjust=.5, angle=0, show.legend = F) +
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
  class(ggB) <- c(class(ggB), "ggsurvfit")
  return(ggB)
}
