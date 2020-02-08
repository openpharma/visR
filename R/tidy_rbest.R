library(RBesT)
library(tidyverse)
library(ggplot2)
library(bayesplot)
# Default settings for bayesplot
color_scheme_set("blue")
theme_set(theme_default(base_size=12))
# Load example gMAP object
example(crohn)
print(map_crohn)


str(map_crohn)
forest_plot(map_crohn)


###
# adjust the base font size
theme_set(theme_default(base_size=16))
forest_plot(map_crohn, model="both", est="MAP", size=1) + legend_move("right") +
  labs(title="Forest plot", subtitle="Results of Meta-Analytic-Predictive (MAP) analysis", 
       caption="Plot shows point estimates (posterior medians) with 95% intervals")

### what is the response to be -125?



#' Returns a tidied tibble of a RBesT gMAP object
#'
#' @param x gMAP object 
#'
#' @return td tidied tibble
#' @export
#'
#' @examples
tidy_gMAP <- function(x){
  assert_that(inherits(x, "gMAP"))
  td <- tibble::tibble(
    study = 1,
    type = 1,
    estimate = 1,
    lower = 1,
    upper = 1
  )
  return(td)
}

tidy_gMAP(map_crohn)

summary(map_crohn)
x <- map_crohn

y <- 
  
  summary(
  x, 
  probs=c(0.5, 0.02, 0.98), 
  type="response")[
    c("theta.pred", "theta")
    ]

as.data.frame(do.call(y))

pred_est <- as.data.frame(
  do.call(
    rbind, 
    summary(
      x, 
      probs=c(0.5, low, up), 
      type="response")
    [
      c("theta.pred", "theta")
      ]
    )
  )
