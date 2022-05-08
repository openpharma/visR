# Contributing to visR

For more detailed info about contributing, please see the
[**development contributing guide**](https://rstd.io/tidy-contrib). 

## Fixing typos

You can fix typos, spelling mistakes, or grammatical errors in the documentation directly using the GitHub web interface, as long as the changes are made in the _source_ file. 
This generally means you'll need to edit [roxygen2 comments](https://roxygen2.r-lib.org/articles/roxygen2.html) in an `.R`, not a `.Rd` file. 
You can find the `.R` file that generates the `.Rd` by reading the comment in the first line.

## Bigger changes

If you want to make a bigger contribution, it's a good idea to first file an issue and make sure someone from the team agrees that it’s needed. 
If you’ve found a bug, please file an issue that illustrates the bug with a minimal 
[reprex](https://www.tidyverse.org/help/#reprex) (this will also help you write a unit test, if needed).

### Pull request process

* Fork the package and clone onto your computer. If you haven't done this before, we recommend using `usethis::create_from_github("openpharma/visR", fork = TRUE)`.

* Install all development dependencies with `devtools::install_dev_deps()`, and then make sure the package passes R CMD check by running `devtools::check()`. If R CMD check doesn't pass cleanly, it's a good idea to ask for help before continuing. 
    
* Create a Git branch for your pull request (PR). We recommend using `usethis::pr_init("brief-description-of-change")`.

* Make your changes, commit to git, and then create a PR by running `usethis::pr_push()`, and following the prompts in your browser.
  The title of your PR should briefly describe the change.
  The body of your PR should contain `Fixes #issue-number`.

* For user-facing changes, add a bullet to the top of `NEWS.md` (i.e. just below the first header). Follow the style described in <https://style.tidyverse.org/news.html>.

## Code style

### General coding principles

* All new functions should preferably be built using the widely adapted tidyverse (please see namespace for allowed package dependencies).
Dependencies should be kept to a minimum. Please don't restyle code that has nothing to do with your PR.  

* Use [roxygen2](https://cran.r-project.org/package=roxygen2), with [Markdown syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/rd-formatting.html), for updating and creating documentation.  

* Use [testthat](https://cran.r-project.org/package=testthat) for unit tests. 
  Contributions should be accompanied by extensive user acceptance testing. Please reach out to the team if you need help. 
   
* CRAN requires us to use TRUE/FALSE, so don't use T/F.

* Subset using '[[' rather than '$' to avoid surprises with partial matching

### Data Manipulation

* Data is expected to be present in tidy form (one row per observation) unless otherwise noted. Dataframes, rather than tibbles should be returned by functions.

* The magrittr pipe (%>%) can be used for consecutive operations on the the data.

* For models the broom package easily converts key information about the models into tidy data tibbles for subsequent wrangling and visualisation. Note that the implemented methods are lazy and only extract key information, used in most common downstream processing. visR has custom tidiers that extract the informtation from model objects into a dataframe so users have all the information required for any downstream processing.

### Figures

* Plotting should be implemented using ggplot2 unless this is not possible.

* Interactivity may be provided using ggplotly or - if needed plotly or other packages.

### Tables

* Tables should always be also available as raw dataframes.

## Testing

* Write tests as soon as the function is in a somewhat usable state. Improving is easier than starting.

* Add a new test for every issue/bug that is identified.

* Use vdiffr for comparison of anything plotted against a library of manually curated plots.

* Educate over break: Rather than just stopping if parameters have been provided that don't make sense, like shape = NULL, warn the user that this a required argument and set it to a reasonable default.

## Package maintenance
We have integrated several "watchdogs" into our testing routine to ensure adherence to certain standards that we've set. Those routines scan our codebase for possible style violations:

### CRAN watchdog
For a successful submission to CRAN, certain rules have been set in place by the CRAN team, as for example the usage of TRUE/FALSE over T/F. To continuously ensure compliance with those, our CRAN watchdog scans the respective files for violations of those.

### Validation watchdog (in PR)
We want to make it easy to use our package in a more stringent environment that might require all packages to be validated. Based on discussions and similar projects we decided to implement traceability into our testing files. Therefore, both the tests as well as the last change to those, are automatically written into the files for potential scraping. The details on the last change are gathered by usage of git log and then table of contents for the unit test specifications are generated based off of the strings in the actual tests.

## Code of Conduct

Please note that visR project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project you agree to abide by its terms.
