#' @title Add risk tables to visR plots through an S3 method
#'
#' @description S3 method for adding risk tables to visR plots.
#'     No default method is available at the moment.
#'
#' @seealso \code{\link[cowplot]{plot_grid}}
#'
#' @param gg visR plot of class `ggsurvfit`
#' @param min_at_risk \code{numeric} The cutoff for number of subjects to display. Default is 0.
#' @param breaks Single numeric or numeric vector indicating breaks. If a vector, breaks are used as specified. If a numeric, it is interpreted as the number of breaks. If added to a ggplot object this argument will override default breaks taken from the plot.
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
#'   estimate_KM(strata = "TRTP") %>%
#'   vr_plot() %>%
#'   add_risktable( min_at_risk = 3
#'                 ,label = c("Subjects at Risk", "Censored")
#'                 ,statlist = c("n.risk", "n.censor")
#'                 ,group = "statlist"
#'                 )
#'                 
#' ## Display 2 risk tables, 1 per stratum
#' adtte %>%
#'   estimate_KM(strata = "TRTP") %>%
#'   vr_plot() %>%
#'   add_risktable( min_at_risk = 3
#'                 ,label = c("Subjects at Risk", "Censored")
#'                 ,statlist = c("n.risk", "n.censor")
#'                 ,group = "strata"
#'                 )
#'
#' ## Display overall risk table
#' adtte %>%
#'   estimate_KM(strata = "TRTP") %>%
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
                          ,breaks = NULL
                          ,statlist = c("n.risk")
                          ,label = "At risk"
                          ,group = "strata"
                          ,collapse = FALSE){
  UseMethod("add_risktable")
}

#' @rdname add_risktable
#' @method add_risktable ggsurvfit
#' @export

add_risktable.ggsurvfit <- function(
   gg
   ,min_at_risk = 0
   ,breaks = NULL
   ,statlist = c("n.risk")
   ,label = "At risk"
   ,group = "strata"
   ,collapse = FALSE
){


  final <- get_risktable(gg
                         ,min_at_risk
                         ,breaks
                         ,statlist
                         ,label
                         ,group
                         ,collapse)
  
  time_ticks <- attributes(final)$time_ticks
  times <- as.numeric(unique(final$time))
  statlist <- attributes(final)$statlist
  title <- attributes(final)$title
  
  
  #### Plot all requested tables below => use list approach with map function ####

  tbls <-  base::Map(function(statlist, title = NA) {
    ggrisk <- ggplot2::ggplot(final, ggplot2::aes(x = time,
                                         y = stats::reorder(y_values, dplyr::desc(y_values)),
                                             label = format(get(statlist), nsmall = 0) # = value columns
                                         )
                              ) +
      ggplot2::geom_text(size = 3.0, hjust=.5, vjust=.5, angle=0, show.legend = F) +
      ggplot2::theme_bw() +
      ggplot2::scale_x_continuous(breaks = times,
                                  limits = c(min(time_ticks), max(time_ticks))) +
      ggplot2::theme(axis.title.x = ggplot2::element_text(size = 8,
                                                 vjust = 1,
                                                 hjust = 1),
                     panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     panel.border = ggplot2::element_blank(),
                     axis.line = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_text(size=8, colour = "black", face = "plain"),
                     plot.margin = ggplot2::unit(c(1,0,0,0), "lines"),
                     plot.title = ggplot2::element_text(hjust = 0, vjust = 0)
                    ) +
      ggplot2::xlab(NULL) +
      ggplot2::ylab(NULL)

      if (!is.na(title) && !is.null(title)){
        ggrisk <- ggrisk +
          ggplot2::ggtitle(title) +
          ggplot2::theme(plot.title = ggplot2::element_text(size = 10))
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
