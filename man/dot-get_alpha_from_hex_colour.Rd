% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_visr.R
\name{.get_alpha_from_hex_colour}
\alias{.get_alpha_from_hex_colour}
\title{Extract the numerical alpha representation of #RRGGBBAA colour}
\usage{
.get_alpha_from_hex_colour(hex_colour = NULL)
}
\arguments{
\item{hex_colour}{A string in the format '#RRGGBBAA'}
}
\value{
The numeric representation of the colors' alpha value, rounded to 2 digits.
}
\description{
RGB colours can be encoded as hexadecimal values, as for example internally used by \code{ggplot2}.
For this, the numerical RGB values are mapped from their 0-255 value range to two-character hex-values.
This yields a string in the form of '#RRGGBB'. Additionally, a fourth optional block can be present encoding
the alpha transparency of the colour. This extends the string to '#RRGGBBAA'.
This function takes such a string as input for \code{hex_colour}, extracts the 'AA' part and returns the
numerical representation if it.
}
\keyword{internal}
