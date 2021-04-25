#' @title Specifications add_annotation
#' @section Last updated by:
#' Steven Haesendonckx
#' @section Last update date:
#' 25-APR-2021

# Specifications ----------------------------------------------------------

#' T1. The function adds annotations to an object of class `ggplot`
#' ...
#' T2. The function accepts a label of class `character`, `data.frame` or customized objects of class `gtable`
#' T2.1 No error when the label specified is of class `character`
#' ... NULL ...
#' 
#' T3. The annotation are representations of the actual label
#' T3.1 The actual object passed to label is not affected by the transformation to an annotation
#' 
#' T4. The annotation can be placed on the plot by specifying the coordinates 
#' T4.1 An error when one of the coordinates is not numeric
#' T4.2 The annotation can be moved on the plot by specifying the x coordinates
#' T4.3 The annotation can be moved on the plot by specifying the y coordinates
#' 
#' T5. The annotation has bold columnheaders when the passed object is of class `data.frame`
#' 
#' T5. The layout of the annotation can be modified to a certain extend
#' T5.1 The annotation has bold columnheaders when the passed object is of class `data.frame`
#' T5.2 What with string bolding?
#' T5.3 The font family can be chosen between "sans", "serif" and "mono"
#' T5.4 
#' 
#' T6. The output object has an additional attribute `components`
#' T6.1 The attribute components[['visR_plot']] contains the plot used as input
#' T6.2 The attribute components contains the annotation
#' T6.3 The output has the same class as the original ggplot


# Requirement T1 ----------------------------------------------------------

context("add_annotation - T1. The function adds annotations to an object of class `ggplot`")

testthat::test_that("T1.1 No error when a `ggplot` object is passed to the function",{

  # visR_plot <- visR::estimate_KM(data = adtte, strata = "TRTA") %>% visR::plot()
  # 
  # testthat::expect_error(visR::add_risktable(visR_plot), NA)

})


# END OF CODE ----------------------------------------------------------

