# visR 0.4.1

# visR 0.4.0

* Functions `estimate_KM()` and `visr.survfit()` have been deprecated in favor of `ggsurvfit::ggsurvfit()`.

# visR 0.3.1

* We now zoom in on a figure (e.g. Kaplan-Meier figure) with `ggplot2::coord_cartesian()` instead of using `scale_x_continuous(limits=)` and `scale_y_continuous(limits=)`. The latter first removes data outside the limits, then constructs the line. Zooming constructs the full line, then zooms into the limits. This is useful because the risktable often reports estimates near the end of a KM figure, but the line is cutoff and not shown at the last time point. (#402)

* README update to contributor listing. (#435)

# visR 0.3.0

### New functions

* Highlight specific strata in survival plots using `add_highlight()`.

* Indicate quantiles in survival plots using `add_quantiles()`.

* Estimation of cumulative incidence in the presence of competing risks is now possible with `estimate_cuminc()`. The risk estimates can be plotted similarly to estimates from `estimate_KM()` using the `visr()` function.

* Function `Surv_CNSR()` used the CDISC ADTTE conventions for the default values of the time and status indicators for time-to-event analyses. Of note, the status variable must be coded as 0/1 with 1 indicating an observation has been censored. (#391)

### Breaking Changes

* `AlignPlots()` has been renamed to `align_plots()`.

### Bug Fixes

* Fix in `get_pvalue()` for `estimate_KM()` objects when data piped in with modifications (e.g. `dplyr::filter()`, `dplyr::mutate()`).

* Fix in `add_risktable()` with estimates being assigned the incorrect label.

* Review unit testing for `estimate_KM(formula=)` argument. (#399)

* Fixed bug in `define_theme(strata=)`. (#388)

* Fix a discrepancy issue when saving `survfit()` call in `estimate_KM()`. (#365)

* Added check for no `class(x)== `. (#358)

* Fix the documentation of the S3 method `visr()`. (#301)


### Other Updates

* Stratifying variable names have been removed from legend in `visr.survfit()` figures, and the legend title now describes the stratifying variable(s). (#343)

* The `estimate_KM()` function gains an experimental `formula=` argument. When the argument is used, the AVAL, CNSR, and strata arguments typically used to construct the formula are ignored. (#379)

* `add_risktable` has a rowgutter argument to allow spacing between plot and risktables

* The strata variable is now removed from the body of `tableone()` results. (#254) 

* The `visr.survfit()` function no longer warns about x-axis label when `PARAM` column not found in original data set. (#378)

* The call saved in the `estimate_KM()` object has been updated to a quosure--ensuring the original function call can always be recalled.

* `define_theme()`updated to ensure that only the strata present in the theme are displayed. (#388)

* Removed the external dependency with the `easyalluvial` package. (#383)

* Reduced the number of exported functions. (#381)

* Improved documentation for `visr()` and other generic functions. (#301) (#357)

* Unit testing updated where relevant to have 100% code coverage and function requirements tested.

* The `README` page has been updated with additional examples. (#425)

* `lifecycle` badges have been added at the function level to indicate functions that are still in `experimental` or `questioning` phases. (#398) 

# visR 0.2.0

* Initial CRAN release. 
