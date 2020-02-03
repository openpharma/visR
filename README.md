# visR

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

A repository for fit-for-purpose, reusable Pharma visualizations and tables 
with sensible defaults and based on sound graphical principles.

## Motivation
By using a common package for visualising data analysis results in the clinical development process, we want to have a **positive influence** on 

* **choice of visualisation** by making it easy explore different visualisation and to use impactful visualisations fit-for-purpose
* effective visual communication by making it easy to **implement best practices** 

We are not judging on what visualisation you chose for your research question, but want facilitate to make you do your work as you need it!

You can read more about the philosophy and architecture in the repo wiki.

# Installing from github

If not already installed, install devtools:

```{r}
install.packages("devtools")
```

Install visR from github:

```{r}
library(devtools)
install_github("https://github.com/openpharma/visR.git")
```
