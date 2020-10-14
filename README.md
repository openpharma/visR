
<!-- README.md is generated from README.Rmd. Please edit that file -->

# visR <img src='man/figures/logo.png' align="right" height="131.5" />

The goal of visR is to enable fit-for-purpose, reusable clinical and
medical research focused visualizations and tables with sensible
defaults and based on sound [graphical
principles](https://graphicsprinciples.github.io/).

[Package documentation](https://openpharma.github.io/visR/)

## Motivation

By using a common package for visualising data analysis results in the
clinical development process, we want to have a **positive influence**
on

  - **choice of visualisation** by making it easy explore different
    visualisation and to use impactful visualisations fit-for-purpose
  - effective visual communication by making it easy to **implement best
    practices**

We are not judging on what visualisation you chose for your research
question, but want facilitate and support good practice.

You can read more about the philosophy and architecture in the [repo
wiki](https://github.com/openpharma/visR/wiki).

## Lifecycle and status

The package is still experimental and under active development with a
current focus on developing a stable API.

<!-- badges: start -->

| Badge                                                                                                                                                                           | Description                                                |
| ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------- |
| [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)                                 |                                                            |
| ![R CMD Check develop](https://github.com/openpharma/visR/workflows/R-CMD-check-master/badge.svg?branch=develop)                                                                | Develop                                                    |
| ![R CMD Check master](https://github.com/openpharma/visR/workflows/R-CMD-check-master/badge.svg?branch=master)                                                                  | Master                                                     |
| [![R build status last active branch (exclude dev and master)](https://github.com/openpharma/visR/workflows/R-CMD-check/badge.svg)](https://github.com/openpharma/visR/actions) | R build status last active branch (exclude dev and master) |

<!-- badges: end -->

## Installation

Install the development version from [GitHub](https://github.com/) with:

``` r
devtools::install_github("openpharma/visR")
```

## Example

This is a basic example to demonstrate how the API can be used to add
layers to a visualisation. In this example a time to event analysis. The
example calculates stratified Kaplan-Meier by treatment and then plots.
Additional functions can be used to add uncertainty intervals, censoring
information and a risk table.

``` r
library(visR)
library(survival)
library(dplyr)
library(tidyr) 
library(ggplot2)

adtte %>%
  vr_KM_est(strata = "TRTP", conf.int = 0.90) %>%
  vr_plot(legend_position = "right", x_unit = "Days") %>%
  add_CI(style = "ribbon",
         linetype = 3) %>%
  add_CNSR(shape = 3, size = 1) %>%
  add_risktable(
    min_at_risk = 3,
    statlist = c("n.risk", "n.event", "n.censor"),
    label = c("At risk", "Event", "Censor"),
    collapse = F
  )
```

<img src="man/figures/README-example-1.png" width="100%" />

## Contribution

Please note that the ‘visR’ project is released with a [Contributor Code
of Conduct](CODE_OF_CONDUCT.md). By contributing to this project, you
agree to abide by its terms.

## Code contributors

<a href="https://github.com/openpharma/visR/graphs/contributors">
<img src="https://contributors-img.web.app/image?repo=openpharma/visR" />
</a>

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />
