
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

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/openpharma/visR/branch/develop/graph/badge.svg)](https://codecov.io/gh/openpharma/visR?branch=main)
[![R-CMD-check](https://github.com/openpharma/visR/workflows/R-CMD-check/badge.svg)](https://github.com/openpharma/visR/actions)
[![pkgdown](https://github.com/openpharma/visR/actions/workflows/makedocs.yml/badge.svg)](https://github.com/openpharma/visR/actions/workflows/makedocs.yml)
[![CRAN
status](https://www.r-pkg.org/badges/version/visR)](https://CRAN.R-project.org/package=visR)
<a href=https://github.com/pharmaR/riskmetric><img src=https://img.shields.io/badge/riskmetric--1.24-green></img></a>
<!-- badges: end -->

## Installation

The easiest way to get `visR` is to install from CRAN:

``` r
install.packages("visR")
```

Install the *development* version from [GitHub](https://github.com/)
with:

``` r
# defaults to develop branch
devtools::install_github("openpharma/visR") 
```

Install the *latest stable* version from [GitHub](https://github.com/)
with:

``` r
devtools::install_github("openpharma/visR", ref = "main")
```

## Example

### Visualization

This is a basic example to demonstrate how the API can be used to add
layers to a visualization. This example demonstrates a time-to-event
analysis. The example calculates and then plots stratified Kaplan-Meier
by treatment. It is possible to add uncertainty intervals, censoring
information, and a risk table using additional functions.

``` r
library(visR)
library(ggplot2)

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

### Summary Table

The `tableone` calculates and presents summary statistics in a table
format. There are a few more customization options available, such as
footnote and summary functions.

``` r
library(visR)
library(ggplot2)

theme_set(theme_minimal())

## table by treatment - without overall and render with GT
adtte %>%
  dplyr::select(AGE, SEX, TRTA) %>%
  visR::tableone(
     strata = "TRTA",
     overall = TRUE,
     title = "Cohort Summary",
     datasource = "ADaM Interim Dataset for Time-to-Event Analysis",
     engine = "gt"
  )
```

<div id="xhujzwvhpr" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#xhujzwvhpr .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#xhujzwvhpr .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#xhujzwvhpr .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#xhujzwvhpr .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#xhujzwvhpr .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#xhujzwvhpr .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#xhujzwvhpr .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#xhujzwvhpr .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#xhujzwvhpr .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#xhujzwvhpr .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#xhujzwvhpr .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#xhujzwvhpr .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#xhujzwvhpr .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#xhujzwvhpr .gt_from_md > :first-child {
  margin-top: 0;
}

#xhujzwvhpr .gt_from_md > :last-child {
  margin-bottom: 0;
}

#xhujzwvhpr .gt_row {
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#xhujzwvhpr .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#xhujzwvhpr .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#xhujzwvhpr .gt_row_group_first td {
  border-top-width: 2px;
}

#xhujzwvhpr .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#xhujzwvhpr .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#xhujzwvhpr .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#xhujzwvhpr .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#xhujzwvhpr .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#xhujzwvhpr .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#xhujzwvhpr .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#xhujzwvhpr .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#xhujzwvhpr .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#xhujzwvhpr .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#xhujzwvhpr .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#xhujzwvhpr .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#xhujzwvhpr .gt_left {
  text-align: left;
}

#xhujzwvhpr .gt_center {
  text-align: center;
}

#xhujzwvhpr .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#xhujzwvhpr .gt_font_normal {
  font-weight: normal;
}

#xhujzwvhpr .gt_font_bold {
  font-weight: bold;
}

#xhujzwvhpr .gt_font_italic {
  font-style: italic;
}

#xhujzwvhpr .gt_super {
  font-size: 65%;
}

#xhujzwvhpr .gt_two_val_uncert {
  display: inline-block;
  line-height: 1em;
  text-align: right;
  font-size: 60%;
  vertical-align: -0.25em;
  margin-left: 0.1em;
}

#xhujzwvhpr .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#xhujzwvhpr .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#xhujzwvhpr .gt_slash_mark {
  font-size: 0.7em;
  line-height: 0.7em;
  vertical-align: 0.15em;
}

#xhujzwvhpr .gt_fraction_numerator {
  font-size: 0.6em;
  line-height: 0.6em;
  vertical-align: 0.45em;
}

#xhujzwvhpr .gt_fraction_denominator {
  font-size: 0.6em;
  line-height: 0.6em;
  vertical-align: -0.05em;
}
</style>
<table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="5" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Cohort Summary</th>
    </tr>
    
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1"></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Total (N=254)</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Placebo (N=86)</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Xanomeline High Dose (N=84)</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Xanomeline Low Dose (N=84)</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr class="gt_group_heading_row">
      <td colspan="5" class="gt_group_heading">AGE</td>
    </tr>
    <tr class="gt_row_group_first"><td class="gt_row gt_right gt_stub">Mean (SD)</td>
<td class="gt_row gt_left">75.1 (8.25)</td>
<td class="gt_row gt_left">75.2 (8.59)</td>
<td class="gt_row gt_left">74.4 (7.89)</td>
<td class="gt_row gt_left">75.7 (8.29)</td></tr>
    <tr><td class="gt_row gt_right gt_stub">Median (IQR)</td>
<td class="gt_row gt_left">77 (70-81)</td>
<td class="gt_row gt_left">76 (69.2-81.8)</td>
<td class="gt_row gt_left">76 (70.8-80)</td>
<td class="gt_row gt_left">77.5 (71-82)</td></tr>
    <tr><td class="gt_row gt_right gt_stub">Min-max</td>
<td class="gt_row gt_left">51-89</td>
<td class="gt_row gt_left">52-89</td>
<td class="gt_row gt_left">56-88</td>
<td class="gt_row gt_left">51-88</td></tr>
    <tr><td class="gt_row gt_right gt_stub">Missing</td>
<td class="gt_row gt_left">0 (0%)</td>
<td class="gt_row gt_left">0 (0%)</td>
<td class="gt_row gt_left">0 (0%)</td>
<td class="gt_row gt_left">0 (0%)</td></tr>
    <tr class="gt_group_heading_row">
      <td colspan="5" class="gt_group_heading">SEX</td>
    </tr>
    <tr class="gt_row_group_first"><td class="gt_row gt_right gt_stub">F</td>
<td class="gt_row gt_left">143 (56.3%)</td>
<td class="gt_row gt_left">53 (61.6%)</td>
<td class="gt_row gt_left">40 (47.6%)</td>
<td class="gt_row gt_left">50 (59.5%)</td></tr>
    <tr><td class="gt_row gt_right gt_stub">M</td>
<td class="gt_row gt_left">111 (43.7%)</td>
<td class="gt_row gt_left">33 (38.4%)</td>
<td class="gt_row gt_left">44 (52.4%)</td>
<td class="gt_row gt_left">34 (40.5%)</td></tr>
    <tr class="gt_group_heading_row">
      <td colspan="5" class="gt_group_heading">TRTA</td>
    </tr>
    <tr class="gt_row_group_first"><td class="gt_row gt_right gt_stub">Placebo</td>
<td class="gt_row gt_left">86 (33.9%)</td>
<td class="gt_row gt_left">NA</td>
<td class="gt_row gt_left">NA</td>
<td class="gt_row gt_left">NA</td></tr>
    <tr><td class="gt_row gt_right gt_stub">Xanomeline High Dose</td>
<td class="gt_row gt_left">84 (33.1%)</td>
<td class="gt_row gt_left">NA</td>
<td class="gt_row gt_left">NA</td>
<td class="gt_row gt_left">NA</td></tr>
    <tr><td class="gt_row gt_right gt_stub">Xanomeline Low Dose</td>
<td class="gt_row gt_left">84 (33.1%)</td>
<td class="gt_row gt_left">NA</td>
<td class="gt_row gt_left">NA</td>
<td class="gt_row gt_left">NA</td></tr>
  </tbody>
  <tfoot class="gt_sourcenotes">
    <tr>
      <td class="gt_sourcenote" colspan="5">Data Source: ADaM Interim Dataset for Time-to-Event Analysis</td>
    </tr>
    <tr>
      <td class="gt_sourcenote" colspan="5"></td>
    </tr>
  </tfoot>
  
</table>
</div>

## Cite visR

``` text
> citation("visR")
```

## Contributing

Please note that the `visR` project is released with a [Contributor Code
of Conduct](CODE_OF_CONDUCT.md). By contributing to this project, you
agree to abide by its terms. Thank you to all contributors:

[@AlexandraP-21](https://github.com/AlexandraP-21),
[@ardeeshany](https://github.com/ardeeshany),
[@bailliem](https://github.com/bailliem),
[@ddsjoberg](https://github.com/ddsjoberg),
[@epijim](https://github.com/epijim),
[@gdario](https://github.com/gdario),
[@joanacmbarros](https://github.com/joanacmbarros),
[@lcomm](https://github.com/lcomm),
[@prabhushanmup](https://github.com/prabhushanmup),
[@rebecca-albrecht](https://github.com/rebecca-albrecht),
[@SHAESEN2](https://github.com/SHAESEN2),
[@timtreis](https://github.com/timtreis),
[@cschaerfe](https://github.com/cschaerfe),
[@AlexandraP-21](https://github.com/AlexandraP-21)
