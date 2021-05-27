#' @title Wrapper around quantile methods
#'
#' @description S3 method for extracting quantiles. 
#'   No default method is available at the moment.
#' 
#' @seealso \code{\link[survival]{quantile.survfit}}
#' 
#' @param x S3 object
#' @param ... other arguments passed on to the method
#' 
#' @examples
#' 
#' ## Kaplan-Meier estimates
#' survfit_object <- visR::estimate_KM(data = adtte, strata = c("TRTP"))
#' 
#' ## visR quantiles
#' visR::get_quantile(survfit_object)
#' 
#' ## survival quantiles
#' quantile(survfit_object)
#' 
#' @return A data frame with quantiles of the object
#'  
#' @rdname get_quantile
#' 
#' @export
 
get_quantile <- function(x, ...){
  UseMethod("get_quantile", x)
} 

#' @param x An object of class \code{survfit}
#' @param ... other arguments passed on to the method
#' @param probs  probabilies Default = c(0.25,0.50,0.75)
#' @inheritParams survival::quantile.survfit
#'
#' @rdname get_quantile
#' @method get_quantile survfit
#' @export

get_quantile.survfit <- function(x,
                                 ...,
                                 probs = c(0.25, 0.50, 0.75),
                                 conf.int = TRUE,
                                 tolerance = sqrt(.Machine$double.eps)
                                ) {
  

# User input validation ---------------------------------------------------

  if (conf.int == TRUE & !base::all(c("lower", "upper") %in% names(x)))
    stop("Confidence limits were not part of original estimation.")
  
  if (!base::all(is.numeric(probs) == TRUE) | (!base::all(probs < 1)))
    stop("probs should be a numeric vector.")
  
  if (!is.numeric(tolerance))
    stop("tolerance should be numeric")
    
# Extract quantiles -------------------------------------------------------
  
  q <- quantile( x
                ,probs = probs
                ,conf.int = conf.int
                ,tolerance = tolerance
                ,type = 3
               )
  
  qdf <- do.call(rbind.data.frame, q)
  
  strata <- as.character(unlist(lapply(q, rownames)))
  quantity <- unlist(lapply(strsplit(rownames(qdf), "\\.", fixed = FALSE), `[[`, 1))
  
  final <- data.frame(
    cbind(strata, quantity, qdf)
   ,row.names = NULL
   ,check.names = FALSE
  )
  
  final <- final[ order( final[, "strata"], final[, "quantity"] ),  ]

  # ## focus on fun = surv, but we could do this for more funs
  # ## get maximal 1-S(t) and max time
  # 
  # cutoff <- tidyme(x) %>%
  #   dplyr::rename(quantile = surv) %>%
  #   dplyr::arrange(strata, time) %>%
  #   dplyr::select(strata, time, quantile, lower, upper) %>%
  #   reshape2::melt(id.vars = c("strata", "time"), value.name = "value") %>%
  #   dplyr::group_by(strata, variable) %>%
  #   dplyr::filter(!is.na(value)) %>%
  #   dplyr::filter(value == min(value)) %>%
  #   dplyr::summarize(
  #     tmax = max(time),
  #     tmin = min(time),
  #     vmin = min(value),
  #     qmax = 1 - (min(value))
  #   ) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::mutate(
  #     quantity = as.character(variable),
  #     variable = NULL,
  #     strata = unlist(lapply(strsplit(strata, "\\.", fixed =
  #                                       FALSE), function(x) {
  #                                         x[[length(x)]]
  #                                       }))
  #   )
  # 
  # 
  # ## merge info
  # correct <- qdf %>%
  #   dplyr::left_join(cutoff, by = base::intersect(colnames(cutoff), colnames(.))) %>%
  #   dplyr::mutate(
  #     variable = as.numeric(as.character(variable)),
  #     diff = abs(variable - qmax * 100),
  #     value = ifelse(
  #       as.numeric(variable) > 100 * qmax |
  #         value > tmax |
  #         (abs(as.numeric(variable) - 100 * qmax) <= tolerance &
  #            tmin != tmax)
  #       ,
  #       NA_real_
  #       ,
  #       value
  #     )
  #   ) %>%
  #   dplyr::arrange(strata, quantity, variable) %>%
  #   dplyr::select(-diff,-qmax,-vmin,-tmin,-tmax) %>%
 
  # final <- tidyr::pivot_wider(
  #   data = qdf,
  #   id_cols = c(strata, quantity),
  #   names_from = variable,
  #   values_from = value
  # ) %>%
  #   data.frame(check.names = FALSE)
  
  return(final)
}
