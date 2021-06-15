## Test environments
* local R installation, R 4.1.0
* ubuntu 16.04 (on travis-ci), R 4.1.0
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Resubmission 1

* Removed "A Package to" from title field in the DESCRIPTION file.
* Remove ""The goal of visR is to"" from description field in the DESCRIPTION file.
* removed as.person tag and replaced with person tag per author. 
* Add reference to papers that motivate the package design  Vandemeulebroecke et al. (2018) <doi:10.1002/pst.1912>, and Vandemeulebroecke et al. (2019) <doi:10.1002/psp4.12455>, and Morris et al. (2019) <doi:10.1136/bmjopen-2019-030215>.
* updated lifecycle badge in documentation based on current advice https://github.com/r-lib/lifecycle/issues/39 to clear note: "Package has help file(s) containing install/render-stage \Sexpr{} expressions but no prebuilt PDF manual."
* The following NOTE is due to the addition of the additional references. "Possibly mis-spelled words in DESCRIPTION:Vandemeulebroecke (8:4, 9:4) al (8:25, 9:25, 10:14) et (8:22, 9:22, 10:11)"

## Resubmission 2

* shortened description title to: Clinical Graphs and Tables Adhering to Graphical Principles
* replaced T and F with TRUE and FALSE
* removed all instances of `\dontrun{}` 
* replaced `options(scipen = 999)` in function pvalueformat with a function that formats directly
* captured user settings in two vignettes prior to calling `options(digits = 3)` then reinstate after analysis. 
* replaced `installed.packages("data.table")` with `find.package("data.table")` within three tests. 
* ensure all function documentation includes return value. 
