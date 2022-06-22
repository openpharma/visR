# visR (development version)

### New functions

* Highlight specific strata in survival plots using `add_highlight()`.

* Indicate quantiles in survival plots using `add_quantiles()`.

* Estimation of cumulative incidence in the presence of competing risks is now possible with `estimate_cuminc()`. The risk estimates can be plotted similarly to estimates from `estimate_KM()` using the `visr()` function.

* Function `Surv_CNSR()` used the CDISC ADTTE conventions for the default values of the time and status indicators for time-to-event analyses. Of note, the status variable must be coded as 0/1 with 1 indicating an observation has been censored. (#391)

### Breaking Changes

* `AlignPlots()` has been renamed to `align_plots()`.

### Bug Fixes
TODO: fill in this section
* Fix in `get_pvalue()` for `estimate_KM()` objects when data piped in with modifications (e.g. `dplyr::filter()`, `dplyr::mutate()`).
* Fix in `add_risktable()` with estimates being assigned the incorrect label.

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

* TODO: Add something on code coverage and unit testing?

* The `README` page has been updated with additional examples. ()

* DRAFT: Place `lifecycle` badges at the function level to indicate functions that are still in experimental or questioning phases. 

# visR 0.2.0

* Initial CRAN release. 
