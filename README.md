
<!-- README.md is generated from README.Rmd as part of the CICD process. -->
<!-- Please edit README.Rmd - but you do not need to build it! -->

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

-   **choice of visualisation** by making it easy explore different
    visualisation and to use impactful visualisations fit-for-purpose
-   effective visual communication by making it easy to **implement best
    practices**

We are not judging on what visualisation you chose for your research
question, but want to facilitate and support good practice.

You can read more about the philosophy and architecture in the [repo
wiki](https://github.com/openpharma/visR/wiki).

## Lifecycle and status

The package is still experimental and under active development with a
current focus on developing a stable API.

<!-- badges: start -->

| Badge                                                                                                                                                                                         | Description                                                                  |
|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|------------------------------------------------------------------------------|
| [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)                                               | Development stage                                                            |
| [![Codecov test coverage](https://codecov.io/gh/openpharma/visR/branch/develop/graph/badge.svg)](https://codecov.io/gh/openpharma/visR?branch=main)                                           | Unit testing coverage - on `main`                                            |
| [![Codecov test coverage](https://codecov.io/gh/openpharma/visR/branch/develop/graph/badge.svg)](https://codecov.io/gh/openpharma/visR?branch=develop)                                        | Unit testing coverage - on `develop`                                         |
| [![R-CMD-check](https://github.com/openpharma/visR/actions/workflows/check-standard.yaml/badge.svg?branch=develop)](https://github.com/openpharma/visR/actions/workflows/check-standard.yaml) | `develop` branch                                                             |
| [![R-CMD-check](https://github.com/openpharma/visR/actions/workflows/check-standard.yaml/badge.svg?branch=main)](https://github.com/openpharma/visR/actions/workflows/check-standard.yaml)    | `main` branch                                                                |
| [![pkgdown](https://github.com/openpharma/visR/actions/workflows/makedocs.yml/badge.svg)](https://github.com/openpharma/visR/actions/workflows/makedocs.yml)                                  | Documentation building to [Github pages](https://openpharma.github.io/visR/) |

<!-- badges: end -->

## Installation

Install the *development* version from [GitHub](https://github.com/)
with:

``` r
devtools::install_github("openpharma/visR") # defaults to develop branch
```

Install the *latest stable* version from [GitHub](https://github.com/)
with:

``` r
devtools::install_github("openpharma/visR", ref = "main")
```

The package is not on CRAN yet.

## Example

This is a basic example to demonstrate how the API can be used to add
layers to a visualisation. In this example a time to event analysis. The
example calculates stratified Kaplan-Meier by treatment and then plots.
Additional functions can be used to add uncertainty intervals, censoring
information and a risk table.

``` r
library(ggplot2)
library(visR)

theme_set(theme_minimal())


adtte %>%
  estimate_KM(strata = "TRTP", conf.int = 0.90) %>%
  visr(legend_position = "right", x_unit = "Days") %>%
  add_CI(style = "ribbon") %>%
  add_CNSR(shape = 3, size = 1) %>%
  add_risktable(
    statlist = c("n.risk", "n.event", "n.censor"),
    label = c("At risk", "Event", "Censor")
  )
```

<img src="man/figures/README-example-1.png" width="100%" />

## Contribution

Please note that the ‘visR’ project is released with a [Contributor Code
of Conduct](CODE_OF_CONDUCT.md). By contributing to this project, you
agree to abide by its terms.

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

    #> The following name changes were identified (41 in total):
    #> * R/{pvr_cross_tab.R => vr_cross_tab.R}
    #> * R/{pvr_attrition.R => vr_plt_attrition.R}
    #> * R/{pvr_stacked_barchart.R => vr_stacked_barchart.R}
    #> * R/{vr_plt_attrition.R => vr_attrition.R}
    #> * R/{vr_kaplan_meier.R => vr_est_kaplan_meier.R}
    #> * man/{pvr_attrition.Rd => vr_attrition.Rd}
    #> * R/{vr_plot_forest.R => vr_plt_forest.R}
    #> * man/{vr_plot_forest.Rd => vr_plt_forest.Rd}
    #> * R/{vr_KM_risktable.R => add_KM_risktable.R}
    #> * R/{add_KM_CI.R => add_CI.R}
    #> * R/{add_KM_risktable.R => add_risktable.R}
    #> * R/{vr_KM_plot.R => vr_plot.R}
    #> * {R => examples}/example.R
    #> * {R => examples}/example_api.R
    #> * {R => examples}/example_vignette_recreation.R
    #> * R/{add_COX_HR.R => get_COX_HR.R}
    #> * examples/CDISC SDTM ADaM Pilot Project m5toc.pdf => CDISC SDTM ADaM Pilot Project m5toc.pdf
    #> * .github/workflows/{r_cmd_check-MasterDevelop.yml => R-CMD-check.yml}
    #> * .github/workflows/{CI_CD.yaml => CI-CD.yaml}
    #> * docs/articles/Example_analysis_files/figure-html/{unnamed-chunk-4-1.png => unnamed-chunk-5-1.png}
    #> * R/{vr_KM_est.R => estimate_KM.R}
    #> * man/{vr_KM_est.Rd => estimate_KM.Rd}
    #> * R/{vr_create_risktable.R => get_risktable.R}
    #> * R/{vr_render_table.R => render_table.R}
    #> * R/{render_table.R => render.R}
    #> * R/{vr_plot.R => plot.R}
    #> * R/{vr_utils.R => utils.R}
    #> * R/{vr_create_tableone.R => create_tableone.R}
    #> * R/{vr_table_one.R => table_one.R}
    #> * R/{create_tableone.R => get_tableone.R}
    #> * R/{table_one.R => tableone.R}
    #> * vignettes/{Example_analysis.Rmd => Example_analysis2.Rmd}
    #> * vignettes/{Example_analysis2.Rmd => Example_analysis.Rmd}
    #> * .github/workflows/{docs => makedocs.yml}
    #> * R/{utilities.R => utils_plot.R}
    #> * R/{utils.R => utils_table.R}
    #> * R/{vr_attrition_table.R => get_attrition.R}
    #> * R/{vr_attrition.R => plot_attrition.R}
    #> * vignettes/{CDISC ADaM.Rmd => CDISC_ADaM.Rmd}
    #> * R/{plot.R => visr_plot.R}
    #> * R/{utils_plot.R => utils_visr.R}

## Code coverage

Last time readme built.

``` r
#covr::package_coverage()
```
