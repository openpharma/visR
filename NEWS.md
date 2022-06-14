# visR (development version)

### New functions
* Highlight specific strata in survival plots using `add_highlight()`.
* Indicate quantiles in survival plots using `add_quantiles()`.
* Estimation and Plotting of cumulative competing risks via `estimate_cuminc()` and `visr()`. (#293)
* `estimate_cox` added to fit a Cox Proportional Hazards Regression model. 

### Utility functions
* `AlignPlots()` has been renamed to `align_plots()`.

### Other Updates
* Stratifying variable names have been removed from legend in `visr.survfit()` figures, and the legend title now describes the stratifying variable(s). (#343)
* The `estimate_KM()` function gains an experimental `formula=` argument. When the argument is used, the AVAL, CNSR, and strata arguments typically used to construct the formula are ignored. (#379)
* `add_risktable` has a rowgutter argument to allow spacing between plot and risktables
* The strata variable is now removed from the body of `tableone()` results. (#254) 
* The `visr.survfit()` function no longer warns about x-axis label when `PARAM` column not found in original data set. (#378)
* The call saved in the `estimate_KM()` object has been updated to a quosure--ensuring the original function call can always be recalled.

* Improved documentation for `visr()` and other generic functions. (#301)

* The `README` page has been updated with additional examples. ()

# visR 0.2.0

* Initial CRAN release. 

* Changed API for survival outputs. Experimenting with a pipe like interface to start to layer components of reports for example uncertainty intervals, annotations, risk tables, model summaries, etc. 


# visR 0.1.0

* Added a `NEWS.md` file to track changes to the package.
