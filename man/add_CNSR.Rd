% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_CNSR.R
\name{add_CNSR}
\alias{add_CNSR}
\alias{add_CNSR.ggsurvfit}
\title{Add censoring symbols to a visR ggplot through an S3 method}
\usage{
add_CNSR(gg, ...)

\method{add_CNSR}{ggsurvfit}(gg, shape = 3, size = 2, ...)
}
\arguments{
\item{gg}{A ggplot created with visR}

\item{...}{other arguments passed on to the method to modify \code{\link[ggplot2]{geom_point}}}

\item{shape}{aesthetic of ggplot2 \code{\link[ggplot2]{geom_point}}. Default is 3.}

\item{size}{aesthetic of ggplot2 \code{\link[ggplot2]{geom_point}}. Default is 2.}
}
\value{
Censoring symbols overlayed on a visR ggplot
}
\description{
S3 method for adding censoring symbols to visR ggplot.
No default method is available at the moment.
}
\examples{
library(survival)
library(dplyr)
library(tidyr)
library(ggplot2)

survfit_object <- survival::survfit(data = adtte, Surv(AVAL, 1-CNSR) ~ TRTP)
vr_plot(survfit_object) \%>\%
  add_CI(alpha = 0.1, style = "step", linetype = 3)
 
}
\author{
Steven Haesendonckx
}