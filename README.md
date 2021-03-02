
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

| Badge                                                                                                                                                                                         | Description                                                                  |
| --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------- |
| [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)                                               | Development stage                                                            |
| [![Codecov test coverage](https://codecov.io/gh/openpharma/visR/branch/develop/graph/badge.svg)](https://codecov.io/gh/openpharma/visR?branch=develop)                                        | Unit testing coverage - currently points at `develop`                        |
| [![R-CMD-check](https://github.com/openpharma/visR/actions/workflows/check-standard.yaml/badge.svg?branch=develop)](https://github.com/openpharma/visR/actions/workflows/check-standard.yaml) | `develop` branch                                                             |
| [![R-CMD-check](https://github.com/openpharma/visR/actions/workflows/check-standard.yaml/badge.svg?branch=master)](https://github.com/openpharma/visR/actions/workflows/check-standard.yaml)  | `master` branch                                                              |
| [![pkgdown](https://github.com/openpharma/visR/actions/workflows/makedocs.yml/badge.svg)](https://github.com/openpharma/visR/actions/workflows/makedocs.yml)                                  | Documentation building to [Github pages](https://openpharma.github.io/visR/) |

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

# Need to add working example!

# adtte %>%
#   vr_KM_est(strata = "TRTP", conf.int = 0.90) %>%
#   vr_plot(legend_position = "right", x_unit = "Days") %>%
#   add_CI(style = "ribbon",
#          linetype = 3) %>%
#   add_CNSR(shape = 3, size = 1) %>%
#   add_risktable(
#     min_at_risk = 3,
#     statlist = c("n.risk", "n.event", "n.censor"),
#     label = c("At risk", "Event", "Censor"),
#     collapse = F
#   )
```

## Contribution

Please note that the ‘visR’ project is released with a [Contributor Code
of Conduct](CODE_OF_CONDUCT.md). By contributing to this project, you
agree to abide by its terms.

## Code contributors

<a href="https://github.com/openpharma/visR/graphs/contributors">
<img src="https://contributors-img.web.app/image?repo=openpharma/visR" />
</a>

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

## Code coverage

Last time readme built.

``` r
covr::package_coverage()
#> visR Coverage: 3.97%
#> R/add_annotation.R: 0.00%
#> R/add_CI.R: 0.00%
#> R/add_CNSR.R: 0.00%
#> R/add_risktable.R: 0.00%
#> R/get_COX_HR.R: 0.00%
#> R/get_pvalue.R: 0.00%
#> R/get_quantile.R: 0.00%
#> R/get_risktable.R: 0.00%
#> R/get_summary.R: 0.00%
#> R/get_tableone.R: 0.00%
#> R/plot.R: 0.00%
#> R/render.R: 0.00%
#> R/style_visR.R: 0.00%
#> R/tableone.R: 0.00%
#> R/tidyme.R: 0.00%
#> R/utils.R: 0.00%
#> R/vr_alluvial_plot.R: 0.00%
#> R/vr_alluvial_wrangling.R: 0.00%
#> R/vr_attrition_table.R: 0.00%
#> R/vr_attrition.R: 0.00%
#> R/vr_cross_tab.R: 0.00%
#> R/vr_plotly.R: 0.00%
#> R/vr_plt_forest.R: 0.00%
#> R/vr_stacked_barchart.R: 0.00%
#> R/vr_tidy_rbest.R: 0.00%
#> R/utilities.R: 8.82%
#> R/estimate_KM.R: 96.08%
```
