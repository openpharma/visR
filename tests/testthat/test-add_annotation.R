#' @title Specifications add_annotation
#' @section Last updated by:
#' Steven Haesendonckx
#' @section Last update date:
#' 25-APR-2021

# Specifications ----------------------------------------------------------
#' T1. The function adds annotations to an object of class `ggplot`
#' T1.1 No error when a `ggplot` object is passed to the function in the presence of a label
#' T1.2 An error when a non-`ggplot` object is passed to the function in the presence of a label
#' T1.3 An error when NULL is passed to the function in the presence of a label
#'
#' T2. The function accepts a label of class `character`, `data.frame` or customized objects of class `gtable`
#' T2.1 An error when a `ggplot` object is passed to the function in the absence of a label
#' T2.2 No error when label is of class `character`
#' T2.3 No error when label is of class `data.frame`
#' T2.4 No error when label is of class `gtable`
#' 
#' T3. The annotation are representations of the actual label
#' T3.1 An object of type `character` passed to label is not affected by the transformation to an annotation
#' T3.2 A `data.frame` passed to label is not affected by the transformation to an annotation
#' T3.3 A `gtable` passed to label is not affected by the transformation to an annotation
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

testthat::test_that("T1.1 No error when a `ggplot` object is passed to the function in the presence of a label",{

  visR_plot <- visR::estimate_KM(data = adtte, strata = "TRTA") %>% visR::plot()

  testthat::expect_error(visR::add_annotation(visR_plot, label = "blah"), NA)

})

testthat::test_that("T1.2 An error when a non-`ggplot` object is passed to the function in the presence of a label",{
  
  visR_plot <- "blah"

  testthat::expect_error(visR::add_annotation(gg = visR_plot, label = "blah"))

})

testthat::test_that("T1.3 An error when NULL is passed to the function in the presence of a label",{

  testthat::expect_error(visR::add_annotation(gg = NULL, label = "blah"))

})


# Requirement T2 ----------------------------------------------------------

context("add_annotation - T2. The function accepts a label of class `character`, `data.frame` or customized objects of class `gtable`")

testthat::test_that("T2.1 An error when a `ggplot` object is passed to the function in the absence of a label",{

  visR_plot <- visR::estimate_KM(data = adtte, strata = "TRTA") %>% visR::plot()

  testthat::expect_error(visR::add_annotation(visR_plot, label = NULL), NA)

})

testthat::test_that("T2.2 No error when label is of class `character`",{

  visR_plot <- visR::estimate_KM(data = adtte, strata = "TRTA") %>% visR::plot()

  testthat::expect_error(visR::add_annotation(visR_plot, label = "blah"), NA)

})

testthat::test_that("T2.3 No error when label is of class `data.frame`",{

  visR_plot <- visR::estimate_KM(data = adtte, strata = "TRTA") %>% visR::plot()

  testthat::expect_error(visR::add_annotation(visR_plot, label = adtte[1:5,]), NA)

})

testthat::test_that("T2.3 No error when label is of class `gtable`",{

  visR_plot <- visR::estimate_KM(data = adtte, strata = "TRTA") %>% visR::plot()
  
  lbl <- gridExtra::tableGrob(adtte[1:5,])

  testthat::expect_error(visR::add_annotation(visR_plot, label = lbl), NA)

})

# Requirement T3 ----------------------------------------------------------

context("add_annotation - T3. The annotation are representations of the actual label")

testthat::test_that("T3.1 An object of type `character` passed to label is not affected by the transformation to an annotation",{
  
  lbl <- "blah"
  
  visR_plot <- visR::estimate_KM(data = adtte, strata = "TRTA") %>% visR::plot() %>% 
    visR::add_annotation(label = lbl)
  
  testthat::expect_equal(base::as.character(visR_plot$components$annotation[[1]]$label), lbl)
  
})

### This doe snot work, pushed for Tim
testthat::test_that("T3.2 A `data.frame` passed to label is not affected by the transformation to an annotation",{
  
  lbl <- adtte[1:5,]
  
  visR_plot <- visR::estimate_KM(data = adtte, strata = "TRTA") %>% visR::plot() %>% 
    visR::add_annotation(label = lbl)
  
  result <- base::unlist(base::lapply(visR_plot$components$annotation, function(x){
    as.character(x$label)
  }))
  
  unname(c(names(lbl), unlist(lbl)))
  
  
  
  testthat::expect_equal(visR_plot$components$annotation[[1]], lbl)
  
  
})

### This doe snot work, pushed for Tim
testthat::test_that("T3.3 A `gtable` passed to label is not affected by the transformation to an annotation",{
  
  visR_plot <- visR::estimate_KM(data = adtte, strata = "TRTA") %>% visR::plot()
  
  lbl <- gridExtra::tableGrob(adtte[1:5,])
  
  visR_plot <- visR_plot %>%  visR::add_annotation(label = lbl)
  
  t1 = visR_plot$components$annotation[[1]]
  t2 = lbl$grobs[[1]]
  
  #t1==t2
  
  #t1$
  testthat::expect_equal(t1, t2)
  
  
})


# END OF CODE ----------------------------------------------------------

