# visR (development version)

### New functions
* Highlight specific strata in survival plots using `add_highlight()`.
* Indicate quantiles in survival plots using `add_quantiles()`.
* Estimation and Plotting of cumulative competing risks via `estimate_cuminc()` and `visr()`. (#293)

### Utility functions
* `AlignPlots()` has been renamed to `align_plots()`.

### Other Updates
* Stratifying variable names have been removed from legend in `visr.survfit()` figures, and the legend title now describes the stratifying variable(s). (#343)
* `add_risktable` has a rowgutter argument to allow spacing between plot and risktables

* Improved documentation for `visr()` and other generic functions. (#301)

* The `README` page has been updated with additional examples. ()

# visR 0.2.0

* Initial CRAN release. 

* Changed API for survival outputs. Experimenting with a pipe like interface to start to layer components of reports for example uncertainty intervals, annotations, risk tables, model summaries, etc. 


# visR 0.1.0

* Added a `NEWS.md` file to track changes to the package.
