#' @title Plot a supported S3 object
#'
#' @description S3 method for creating plots directly from objects using `ggplot2`,
#'   similar to the base R `plot()` function.
#'
#' @seealso \code{\link[ggplot2]{ggplot}}
#'
#' @param x Object of class `survfit`, `attritiontable`, or `tidycuminc`
#' @param x_label \code{character} Label for the x-axis. When not specified,
#'   the function will look for `"PARAM"` or `"PARAMCD"` information in the original data set (CDISC standards).
#'   If no `"PARAM"`/`"PARAMCD"` information is available, the default x-axis label is `"Time"`.
#' @param y_label \code{character} Label for the y-axis. When not specified,
#'   the default will do a proposal, depending on the `fun` argument.
#' @param x_units Unit to be added to the x_label (x_label (x_unit)).
#'   Default is NULL.
#' @param x_ticks Ticks for the x-axis. When not specified, the default will
#'   do a proposal.
#' @param y_ticks Ticks for the y-axis. When not specified,
#'   the default will do a proposal based on the `fun` argument.
#' @param fun Function that represents the scale of the estimate.
#'   The current options are:
#'   \itemize{
#'   \item{`surv`}{ is the survival probability. This is the default.}
#'   \item{`log`}{ is log of the survival probability}
#'   \item{`event`}{ is the failure probability}
#'   \item{`cloglog`}{ is log(-log(survival probability))}
#'   \item{`pct`}{ is survival as a percentage}
#'   \item{`logpct`}{ is log survival as a percentage}
#'   \item{`cumhaz`}{ is the cumulative hazard}
#'   }
#' @param legend_position Specifies the legend position in the plot.
#'   Character values allowed are "top" "left" "bottom" "right".
#'   Numeric coordinates are also allowed.
#'   Default is "right".
#' @param description_column_name \code{character} Name of the column containing
#'   the inclusion descriptions
#' @param value_column_name \code{character} Name of the column containing the
#'   remaining sample counts
#' @param complement_column_name \code{character} Optional: Name of the column
#'   containing the exclusion descriptions
#' @param box_width \code{character} The box width for each box in the flow
#'   chart
#' @param font_size \code{character} The fontsize in pt
#' @param fill The color (string or hexcode) to use to fill the boxes in the
#'   flowchart
#' @param border The color (string or hexcode) to use for the borders of the
#'   boxes in the flowchart
#' @param ... other arguments passed on to the method
#'
#' @return Object of class \code{ggplot} and \code{ggsurvplot} for `survfit` objects.
#'
#' @rdname visr
#'
#' @export

visr <- function(x, ...) {
  UseMethod("visr", x)
}

#' @rdname visr
#' @method visr default
#' @export

visr.default <- function(x, ...) {
  graphics::plot(x)
}

#' @examples
#'
#' # fit KM
#' km_fit <- survival::survfit(survival::Surv(AVAL, 1 - CNSR) ~ TRTP, data = adtte)
#'
#' # plot curves using survival plot function
#' plot(km_fit)
#'
#' # plot same curves using visR::visr plotting function
#' visR::visr(km_fit)
#'
#' # estimate KM using visR wrapper
#' survfit_object <- visR::estimate_KM(data = adtte, strata = "TRTP")
#'
#' # Plot survival probability
#' visR::visr(survfit_object, fun = "surv")
#'
#' # Plot survival percentage
#' visR::visr(survfit_object, fun = "pct")
#'
#' # Plot cumulative hazard
#' visR::visr(survfit_object, fun = "cloglog")
#'
#' @rdname visr
#' @method visr survfit
#' @export

visr.survfit <- function(x = NULL,
                         x_label = NULL,
                         y_label = NULL,
                         x_units = NULL,
                         x_ticks = NULL,
                         y_ticks = NULL,
                         fun = "surv",
                         legend_position = "right",
                         ...) {

  # Minimal input validation  ----------------------------------------------------

  if (!(is.null(x_label) | is.character(x_label) | is.expression(x_label))) {
    stop("Invalid `x_label` argument, must be either `character` or `expression`.")
  }

  if (!(is.null(y_label) | is.character(y_label) | is.expression(y_label))) {
    stop("Invalid `y_label` argument, must be either `character` or `expression`.")
  }

  if (!(is.null(x_units) | is.character(x_units))) {
    stop("Invalid `x_units` argument, must be `character`.")
  }

  if (!(is.null(x_ticks) | is.numeric(x_ticks))) {
    stop("Invalid `x_ticks` argument, must be `numeric`.")
  }

  if (!(is.null(y_ticks) | is.numeric(y_ticks))) {
    stop("Invalid `y_ticks` argument, must be `numeric`.")
  }

  if (is.character(legend_position) &&
    !legend_position %in% c("top", "bottom", "right", "left", "none")) {
    stop(
      "Invalid legend position given. Must either be [\"top\", \"bottom\", \"right\", \"left\", \"none\"] or a vector with two numbers indicating the position relative to the axis. For example c(0.5, 0.5) to place the legend in the center of the plot."
    )
  } else if (is.numeric(legend_position) &&
    length(legend_position) != 2) {
    stop(
      "Invalid legend position given. Must either be [\"top\", \"bottom\", \"right\", \"left\", \"none\"] or a vector with two numbers indicating the position relative to the axis. For example c(0.5, 0.5) to place the legend in the center of the plot."
    )
  }

  valid_funs <- c("surv", "log", "event", "cloglog", "pct", "logpct", "cumhaz")

  if (is.character(fun)) {
    if (!(fun %in% valid_funs)) {
      stop(
        "Unrecognized `fun` argument, must be one of [\"surv\", \"log\", \"event\", \"cloglog\", \"pct\", \"logpct\", \"cumhaz\"] or a user-defined function."
      )
    }
  }

  # Y-label ----------------------------------------------------------------------

  if (is.null(y_label) & is.character(fun)) {
    y_label <- base::switch(fun,
      surv = "survival probability",
      log = "log(survival probability)",
      event = "failure probability",
      cloglog = "log(-log(survival probability))",
      pct = "percentage survival",
      logpct = "log(percentage survival)",
      cumhaz = "cumulative hazard",
      stop("Unrecognized fun argument")
    )
  } else if (is.null(y_label) & is.function(fun)) {
    stop("No Y label defined. No default label is available when `fun` is a function.")
  }

  if (is.character(fun)) {
    .transfun <- base::switch(fun,
      surv = function(y) y,
      log = function(y) log(y),
      event = function(y) 1 - y,
      cloglog = function(y) log(-log(y)),
      pct = function(y) y * 100,
      logpct = function(y) log(y * 100),
      # survfit object contains an estimate for Cumhaz and SE based on Nelson-Aalen with or without correction for ties
      # However, no CI is calculated automatically. For plotting, the MLE estimator is used for convenience.
      cumhaz = function(y) -log(y)
    )
  } else if (is.function(fun)) {
    .transfun <- function(y) fun(y)
  } else {
    stop("fun should be a character or a user-defined function.")
  }

  # Extended tidy of survfit class + transformation + remove NA after transfo ----

  correctme <- NULL
  tidy_object <- tidyme(x)

  if ("estimate" %in% colnames(tidy_object)) {
    tidy_object[["est"]] <- .transfun(tidy_object[["estimate"]])
    correctme <- c(correctme, "est")
  }

  if (all(c("conf.high", "conf.low") %in% colnames(tidy_object))) {
    tidy_object[["est.upper"]] <- .transfun(tidy_object[["conf.high"]])
    tidy_object[["est.lower"]] <- .transfun(tidy_object[["conf.low"]])
    correctme <- c(correctme, "est.lower", "est.upper")
  }

  # Adjust -Inf to minimal value -------------------------------------------------

  if (nrow(tidy_object[tidy_object[["est"]] == "-Inf", ]) > 0) {
    warning("NAs introduced by y-axis transformation.")
  }

  tidy_object[, correctme] <- sapply(tidy_object[, correctme],
    FUN = function(x) {
      x[which(x == -Inf)] <- min(x[which(x != -Inf)], na.rm = TRUE)
      return(x)
    }
  )

  ymin <- min(sapply(tidy_object[, correctme], function(x) min(x[which(x != -Inf)], na.rm = TRUE)), na.rm = TRUE)
  ymax <- max(sapply(tidy_object[, correctme], function(x) max(x[which(x != -Inf)], na.rm = TRUE)), na.rm = TRUE)

  # Obtain X-asis label ----------------------------------------------------------

  if (is.null(x_label)) {
    if ("PARAM" %in% names(x)) {
      if (length(unique(x[["PARAM"]])) == 1) {
        x_label <- as.character(x[["PARAM"]][[1]])
      } else {
        warning("More than one unique entry in 'PARAM'.")
      }
    } else if ("PARAMCD" %in% names(x)) {
      if (length(unique(x[["PARAMCD"]])) == 1) {
        x_label <- as.character(x[["PARAMCD"]][[1]])
      } else {
        warning("More than one unique entry in 'PARAMCD'.")
      }
    } else {
      x_label <- "Time"
    }

    if (!is.null(x_units)) {
      x_label <- paste0(x_label, " (", x_units, ")")
    }
  } else {
    if (!is.null(x_units)) {
      x_label <- paste0(x_label, " (", x_units, ")")
    }
  }

  if (is.null(x_ticks)) x_ticks <- pretty(x$time, 10)

  # Obtain Y-asis label ----------------------------------------------------------

  if (is.null(y_ticks) & is.character(fun)) {
    y_ticks <- switch(fun,
      surv = pretty(c(0, 1), 5),
      log = pretty(c(ymin, ymax), 5),
      event = pretty(c(0, 1), 5),
      cloglog = pretty(c(ymin, ymax), 5),
      pct = pretty(c(0, 100), 5),
      logpct = pretty(c(0, 5), 5),
      cumhaz = pretty(c(ymin, ymax), 5),
      stop("Unrecognized fun argument")
    )
  } else if (is.null(y_ticks) & is.function(fun)) {
    y_ticks <- pretty(c(ymin, ymax), 5)
  }

  # Plotit -----------------------------------------------------

  yscaleFUN <- function(x) sprintf("%.2f", x)

  gg <- ggplot2::ggplot(tidy_object, ggplot2::aes(
    x = time,
    group = strata,
    fill = strata
  )) +
    ggplot2::geom_step(ggplot2::aes(y = est, col = strata)) +
    ggplot2::scale_x_continuous(
      breaks = x_ticks
    ) +
    ggplot2::xlab(x_label) +
    ggplot2::scale_y_continuous(
      breaks = y_ticks,
      labels = yscaleFUN
    ) +
    ggplot2::coord_cartesian(
      xlim = c(min(x_ticks), max(x_ticks)),
      ylim = c(min(y_ticks), max(y_ticks))
    ) +
    ggplot2::ylab(y_label) +
    ggplot2::labs(color = .construct_strata_label(x)) +
    ggplot2::theme(legend.position = legend_position) +
    ggplot2::theme(legend.key = ggplot2::element_blank()) +
    NULL

  # Save applied function so that we don't have to guess later on
  if (is.character(fun)) {
    attr(gg, "fun") <- .transfun
  } else {
    fun_call <- utils::capture.output(fun)
    if (grepl("rimitive", fun_call[1])) {
      fun_call_fun <- regmatches(fun_call, regexpr("\".*\"", fun_call))
      fun_call_fun <- gsub("\"", "", fun_call_fun)
      fun_call_fun <- paste0("function(x) ", fun_call_fun, "(x)")
      attr(gg, "fun") <- eval(parse(text = fun_call_fun))
    } else if (is.function(fun)) {
      attr(gg, "fun") <- fun
    }
  }

  class(gg) <- c("ggsurvfit", class(gg))

  return(gg)
}

#' @description
#' `r lifecycle::badge("questioning")`
#' S3 function to draw a Consort flow diagram chart.
#'
#' @examples
#'
#' ## Create attrition
#' attrition <- visR::get_attrition(adtte,
#'   criteria_descriptions = c(
#'     "1. Not in Placebo Group",
#'     "2. Be 75 years of age or older.",
#'     "3. White",
#'     "4. Female"
#'   ),
#'   criteria_conditions = c(
#'     "TRTP != 'Placebo'",
#'     "AGE >= 75",
#'     "RACE=='WHITE'",
#'     "SEX=='F'"
#'   ),
#'   subject_column_name = "USUBJID"
#' )
#'
#' ## Draw a CONSORT attrition chart without specifying extra text for the complement
#' attrition %>%
#'   visr("Criteria", "Remaining N")
#'
#' ## Add detailed complement descriptions to the "exclusion" part of the CONSORT diagram
#' # Step 1. Add new column to attrition dataframe
#' attrition$Complement <- c(
#'   "NA",
#'   "Placebo Group",
#'   "Younger than 75 years",
#'   "Non-White",
#'   "Male"
#' )
#'
#' # Step 2. Define the name of the column in the call to the plotting function
#' attrition %>%
#'   visr("Criteria", "Remaining N", "Complement")
#'
#' ## Styling the CONSORT flowchart
#' # Change the fill and outline of the boxes in the flowchart
#' attrition %>%
#'   visr("Criteria", "Remaining N", "Complement", fill = "lightblue", border = "grey")
#'
#' ## Adjust the font size in the boxes
#' attrition %>%
#'   visr("Criteria", "Remaining N", font_size = 10)
#'
#' @rdname visr
#' @method visr attrition
#' @export

visr.attrition <- function(x,
                           description_column_name = "Criteria",
                           value_column_name = "Remaining N",
                           complement_column_name = "",
                           box_width = 50,
                           font_size = 12,
                           fill = "white",
                           border = "black",
                           ...) {
  if (!description_column_name %in% names(x)) {
    stop(paste0(
      "Column \"", description_column_name, "\" cannot be found in the input data. ",
      "Please provide the column name as string in the input ",
      "data containing the inclusion descriptions."
    ))
  }

  if (!value_column_name %in% names(x)) {
    stop(paste0(
      "Column \"", value_column_name, "\" cannot be found in the input data. ",
      "Please provide the column name as string in the input data containing",
      "the sample size after applying inclusion criteria."
    ))
  }

  if (complement_column_name != "" & !complement_column_name %in% names(x)) {
    stop(paste0(
      "Column \"", complement_column_name, "\" cannot be found in the input data. ",
      "Please provide a valid column name as string in the input data containing",
      "complement description or omit this argument for default labels."
    ))
  }

  if (!is.numeric(box_width)) {
    warning("An invalid input was given for `box_width`, must be `numeric` value. Setting it to 50.")
    box_width <- 50
  }

  if (!is.numeric(font_size)) {
    warning("An invalid input was given for `font_size`, must be `numeric` value. Setting it to 12.")
    font_size <- 12
  }

  if (!is.character(fill)) {
    warning("An invalid input was given for `fill`, must be `character` string. Setting it to \"white\".")
    fill <- "white"
  }

  if (!is.character(border)) {
    warning("An invalid input was given for `border`, must be `character` string. Setting it to \"black\".")
    border <- "black"
  }

  label <- complement_label <- NULL
  y <- down_ystart <- down_yend <- side_xstart <- side_xend <- side_y <- NULL
  cx <- cy <- NULL

  # split up space into evenly sized chunks
  field_height <- 100 / nrow(x)

  # allow for some spacing between boxes by reducing the size of the chunk
  box_height <- 0.75 * field_height

  # assign coordinates to each row in the attrition table
  plotting_data <- x %>%
    .get_labels(description_column_name, value_column_name, complement_column_name, wrap_width = box_width) %>%
    .get_labelsizes(label, complement_label) %>%
    .get_coordinates(box_width, box_height, field_height)

  # draw plot
  gg <- plotting_data %>%
    ggplot2::ggplot() +
    # boxes
    ggplot2::geom_tile(
      data = plotting_data, ggplot2::aes(
        x = x,
        y = y,
        width = box_width,
        height = box_height
      ),
      color = border, fill = fill
    ) +
    # text in boxes
    ggplot2::geom_text(
      data = plotting_data, ggplot2::aes(
        x = x,
        y = y,
        label = label
      ),
      size = font_size / ggplot2::.pt
    ) +
    # down arrow
    ggplot2::geom_segment(
      data = plotting_data, ggplot2::aes(
        x = x,
        xend = x,
        y = down_ystart,
        yend = down_yend
      ),
      arrow = ggplot2::arrow(length = 0.5 * ggplot2::unit(font_size, "pt")),
      size = .2,
      na.rm = TRUE
    ) +
    # side arrow
    ggplot2::geom_segment(
      data = plotting_data, ggplot2::aes(
        x = side_xstart,
        xend = side_xend,
        y = side_y,
        yend = side_y
      ),
      arrow = ggplot2::arrow(length = 0.5 * ggplot2::unit(font_size, "pt")),
      size = .2,
      na.rm = TRUE
    ) +
    # complement box
    ggplot2::geom_tile(
      data = plotting_data, ggplot2::aes(
        x = cx,
        y = cy,
        width = box_width,
        height = box_height
      ),
      color = border, fill = fill,
      na.rm = TRUE
    ) +
    # text in complement box
    ggplot2::geom_text(
      data = plotting_data, ggplot2::aes(
        x = cx,
        y = cy,
        label = complement_label
      ),
      size = font_size / ggplot2::.pt,
      na.rm = TRUE
    ) +
    # remove all plot elements
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none")

  return(gg)
}

#' @rdname visr
#' @method visr tidycuminc
#' @export
visr.tidycuminc <- function(x = NULL,
                            x_label = "Time",
                            y_label = "Cumulative Incidence",
                            x_units = NULL,
                            x_ticks = pretty(x$tidy$time, 10),
                            y_ticks = pretty(c(0, 1), 5),
                            legend_position = "right",
                            ...) {
  # check for installation of tidycmprsk package
  rlang::check_installed("tidycmprsk", version = "0.1.1")

  if (!is.null(x_units)) {
    x_label <- paste0(x_label, " (", x_units, ")")
  }

  # Plotit -----------------------------------------------------
  yscaleFUN <- function(x) sprintf("%.2f", x)

  gg <-
    visr_tidy_tidycuminc(x) %>%
    ggplot2::ggplot(ggplot2::aes(
      x = time,
      group = strata,
      fill = strata
    )) +
    ggplot2::geom_step(ggplot2::aes(y = est, col = strata)) +
    ggplot2::scale_x_continuous(
      breaks = x_ticks
    ) +
    ggplot2::xlab(x_label) +
    ggplot2::scale_y_continuous(
      breaks = y_ticks,
      labels = yscaleFUN
    ) +
    ggplot2::coord_cartesian(
      xlim = c(min(x_ticks), max(x_ticks)),
      ylim = c(min(y_ticks), max(y_ticks))
    ) +
    ggplot2::ylab(y_label) +
    ggplot2::labs(color = .construct_strata_label(x)) +
    ggplot2::theme(legend.position = legend_position) +
    ggplot2::theme(legend.key = ggplot2::element_blank()) +
    NULL

  class(gg) <- c("ggtidycuminc", class(gg))
  attr(gg, "tidycuminc") <- x

  gg
}
