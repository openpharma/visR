#' @title Add risk tables to visR plots through an S3 method
#'
#' @description S3 method for adding risk tables to visR plots. The function has following workflow:
#'     \itemize{
#'       \item{
#'       The risktables are calculated using \code{\link[visR]{get_risktable}}
#'       }
#'       \item{
#'       The risktables are placed underneath visR plots using \code{\link[cowplot]{plot_grid}}
#'       }
#'       \item{
#'       Both the initial visR plot as the individual risktables are stored as attribute `component`
#'        in the final object to allow post-modification of the individual plots if desired
#'        }
#'     }
#'
#' @param gg visR plot of class `ggsurvfit`
#' @param ... other arguments passed on to the method add_risktable
#' 
#' @rdname add_risktable
#' 
#' @export

add_risktable <- function(gg, ...){
  UseMethod("add_risktable", gg)
}

#' @inheritParams get_risktable
#' 
#' @seealso \code{\link[cowplot]{plot_grid}}
#' 
#' @examples
#'
#' ## Display 2 risk tables, 1 per statlist
#' adtte %>%
#'   visR::estimate_KM(strata = "TRTP") %>%
#'   visR::visr() %>%
#'   visR::add_risktable( label = c("Subjects at Risk", "Censored")
#'                       ,statlist = c("n.risk", "n.censor")
#'                       ,group = "statlist"
#'                      )
#'
#' ## Display overall risk table at selected times
#' adtte %>%
#'   visR::estimate_KM(strata = "TRTP") %>%
#'   visR::visr() %>%
#'   visR::add_risktable( label = c("Subjects at Risk", "Censored")
#'                       ,statlist = c("n.risk", "n.censor")
#'                       ,collapse = TRUE
#'                       ,times = c(0,20,40,60)
#'                      )
#'                      
#' ## Add risk set as specified times
#' adtte %>%
#'   visR::estimate_KM(strata = "TRTP") %>%
#'   visR::visr() %>%
#'   visR::add_risktable(times = c(0, 20, 40, 100,111, 200))
#'
#' @return Object of class \code{ggplot} with added risk table.
#'
#' @rdname add_risktable
#'
#' @export

add_risktable.ggsurvfit <- function(
    gg
   ,times = NULL
   ,statlist = c("n.risk")
   ,label = "At risk"
   ,group = "strata"
   ,collapse = FALSE
   ,...
){

# Obtain the relevant table -----------------------------------------------
  
  tidy_object <- gg$data
  
  call <- as.character(gg$data$call[[1]])
  
  survfit_object <- eval(gg$data$call[[1]])
  
  ggbld <- ggplot2::ggplot_build(gg)
  
  graphtimes <- as.numeric(ggbld$layout$panel_params[[1]]$x$get_labels())
  
  if (is.null(times)) times <- graphtimes

  final <- get_risktable( 
              survfit_object
             ,times
             ,statlist
             ,label
             ,group
             ,collapse)

  times <- as.numeric(unique(final$time))
  statlist <- attributes(final)$statlist
  title <- attributes(final)$title
  
  attr(final, "time_ticks") <- NULL
  attr(final, "statlist") <- NULL
  attr(final, "title") <- NULL

# Plot requested tables below using list approach with map function -------

  tbls <-  base::Map(function(statlist, title = NA) {
    ggrisk <- ggplot2::ggplot(final,
                              ggplot2::aes(
                                 x = time,
                                 y = stats::reorder(y_values, dplyr::desc(y_values)),
                                 label = format(get(statlist), nsmall = 0) # = value columns
                              )
                            ) +
      ggplot2::geom_text(size = 3.0, hjust = 0.5, vjust = 0.5, angle = 0, show.legend = FALSE) +
      ggplot2::theme_bw() +
      ggplot2::scale_x_continuous(breaks = graphtimes,
                                  limits = c(min(graphtimes), max(graphtimes))) +

      ggplot2::theme(axis.title.x = ggplot2::element_text(size = 8,
                                                 vjust = 1,
                                                 hjust = 1),
                     panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     panel.border = ggplot2::element_blank(),
                     axis.line = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_text(size = 8, colour = "black", face = "plain"),
                     plot.margin = ggplot2::unit(c(1, 0, 0, 0), "lines"),
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

# Align plot and table by adjusting width ---------------------------------

  gglist <- list(gg) %>%
    base::append(tbls)
  
  ggA <-  gglist %>%
    AlignPlots()

# Create plot and add class -----------------------------------------------

  ## cowplot allows to align according to an axis (+left) and change the heigth
  ggB <- cowplot::plot_grid(plotlist = ggA,
                            align = "none",
                            nrow = length(ggA),
                            rel_heights = c(1-(8/50 * (length(ggA)-1)), rep(8/50, length(ggA)-1))
                          )
  
  class(ggB) <- c(class(ggB), "ggsurvfit")

# Add individual components -----------------------------------------------
  
  components <- append(list(gg), tbls)
  names(components) = c("visR_plot", title)
  ggB[["components"]] <- components
  
  return(ggB)
}
