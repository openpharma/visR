#' Step ribbon statistic
#'
#' Provides stairstep values for ribbon plots
#'
#' @name stat_stepribbon
#' @inheritParams ggplot2::geom_ribbon
#' @param geom which geom to use; defaults to "`ribbon`"
#' @param direction `hv` for horizontal-vertical steps, `vh` for
#'   vertical-horizontal steps
#' @references [https://groups.google.com/forum/?fromgroups=#!topic/ggplot2/9cFWHaH1CPs]()
#' @return a ggplot
#' @examples
#' library(ggplot2)
#'
#' survival::survfit(survival::Surv(time, status) ~ 1, data = survival::lung) %>%
#'   survival::survfit0() %>%
#'   broom::tidy() %>%
#'   ggplot(aes(x = time, y = estimate, ymin = conf.low, ymax = conf.high)) +
#'   geom_step() +
#'   geom_ribbon(stat = "stepribbon", alpha = 0.2)
NULL

#' @rdname stat_stepribbon
#' @export
stat_stepribbon <- function(mapping = NULL, data = NULL, geom = "ribbon",
                            position = "identity",
                            na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                            direction = "hv", ...) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatStepribbon,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      direction = direction,
      ...
    )
  )
}

#' @rdname stat_stepribbon
#' @export
StatStepribbon <-
  ggplot2::ggproto(
    "StatStepRibbon", ggplot2::Stat,
    required_aes = c("x", "ymin", "ymax"),
    compute_group = function(data, scales, direction = "hv",
                             yvars = c("ymin", "ymax"), ...) {
      stairstepn(data = data, direction = direction, yvars = yvars)
    }
  )

stairstepn <- function(data, direction = "hv", yvars = "y") {
  direction <- match.arg(direction, c("hv", "vh"))

  data <- as.data.frame(data)[order(data$x), ]

  n <- nrow(data)

  if (direction == "vh") {
    xs <- rep(1:n, each = 2)[-2 * n]
    ys <- c(1, rep(2:n, each = 2))
  } else {
    ys <- rep(1:n, each = 2)[-2 * n]
    xs <- c(1, rep(2:n, each = 2))
  }

  data.frame(
    x = data$x[xs],
    data[ys, yvars, drop = FALSE],
    data[xs, setdiff(names(data), c("x", yvars)), drop = FALSE]
  )
}
