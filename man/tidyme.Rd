% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidyme.R
\name{tidyme}
\alias{tidyme}
\alias{tidyme.default}
\alias{tidyme.survfit}
\title{Extended tidy cleaning of selected objects using S3 method}
\usage{
tidyme(x, ...)

\method{tidyme}{default}(x, ...)

\method{tidyme}{survfit}(x, ...)
}
\arguments{
\item{x}{S3 object}

\item{...}{other arguments passed on to the method}
}
\value{
Tibble containing all list elements of the S3 object as columns
}
\description{
S3 method for extended tidying of selected model outputs.
The default method relies on \code{broom::tidy} to return a tidied object
}
\examples{

## Extended tidying for a survfit object
surv_object <- visR::estimate_KM(data = adtte, strata = "TRTA")
tidied <- visR::tidyme(surv_object)

## Tidyme for non-included classes
data <- cars
lm_object <- stats::lm(data = cars, speed ~ dist)
lm_tidied <- visR::tidyme(lm_object)
lm_tidied

}
\seealso{
\code{\link[broom]{tidy}}
}
