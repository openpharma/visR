#' @title Specifications test-add_annotation.R
#' @section Last updated by: Tim Treis (tim.treis@@outlook.de)
#' @section Last update date: 2022-02-09T15:22:32
#'
#' @section List of tested specifications
#' T1. The function adds annotations to an object of class `ggplot`
#' T1.1 No error when a `ggplot` object is passed to the function in the presence of a label
#' T1.2 An error when a non-`ggplot` object is passed to the function in the presence of a label
#' T1.3 An error when NULL is passed to the function in the presence of a label
#' T2. The function accepts a label of class `character`, `data.frame` or customized objects of class `gtable`
#' T2.1 An error when a `ggplot` object is passed to the function in the absence of a label
#' T2.2 No error when label is of class `character`
#' T2.3 No error when label is of class `data.frame`
#' T2.3 No error when label is of class `gtable`
#' T3. The annotation are representations of the actual label
#' T3.1 An object of type `character` passed to label is not affected by the transformation to an annotation
#' T3.2 The content of a `data.frame` passed to label is not affected by the transformation to an annotation
#' T3.3 The content of a `gtable` passed to label is not affected by the transformation to an annotation
#' T4. The annotation can be placed on the plot by specifying the coordinates
#' T4.1 An error when one of the coordinates is not numeric
#' T4.2 The annotation can be moved on the plot by specifying the x coordinates
#' T4.3 The annotation can be moved on the plot by specifying the y coordinates
#' T5. The layout of the annotation can be modified to a certain extend
#' T5.1 The annotation has bold columnheaders when the passed object is of class `data.frame`
#' T5.2 The font size can be changed
#' T5.3 The font family can be chosen between 'sans', 'serif' and 'mono'
#' T6. The output object has an additional attribute `components`
#' T6.1 The attribute components[['visR_plot']] contains the plot used as input
#' T6.2 The attribute components contains the annotation
#' T6.3 The output has the same class as the original ggplot

# Requirement T1 ----------------------------------------------------------

testthat::context("add_annotation - T1. The function adds annotations to an object of class `ggplot`")

testthat::test_that("T1.1 No error when a `ggplot` object is passed to the function in the presence of a label", {
  visR_KM_plot <- adtte %>%
    visR::estimate_KM(strata = "TRTA") %>%
    visR::visr()

  data <- tidycmprsk::trial
  visR_cuminc_plot <- visR::estimate_cuminc(data = data, CNSR = "death_cr", AVAL = "ttdeath") %>%
    visR::visr()

  testthat::expect_error(visR_KM_plot %>% visR::add_annotation(label = "blah"), NA)
  testthat::expect_error(visR_cuminc_plot %>% visR::add_annotation(label = "blah"), NA)
})

testthat::test_that("T1.2 An error when a non-`ggplot` object is passed to the function in the presence of a label", {
  visR_plot <- "blah"

  testthat::expect_error(visR::add_annotation(gg = visR_plot, label = "blah"))
})

testthat::test_that("T1.3 An error when NULL is passed to the function in the presence of a label", {
  testthat::expect_error(visR::add_annotation(gg = NULL, label = "blah"))
})


# Requirement T2 ---------------------------------------------------------------

testthat::context("add_annotation - T2. The function accepts a label of class `character`, `data.frame` or customized objects of class `gtable`")

testthat::test_that("T2.1 An error when a `ggplot` object is passed to the function in the absence of a label", {
  visR_KM_plot <- visR::estimate_KM(data = adtte, strata = "TRTA") %>% visR::visr()

  data <- tidycmprsk::trial
  visR_cuminc_plot <- visR::estimate_cuminc(data = data, CNSR = "death_cr", AVAL = "ttdeath") %>%
    visR::visr()

  testthat::expect_error(visR::add_annotation(visR_KM_plot, label = NULL))
  testthat::expect_error(visR::add_annotation(visR_cuminc_plot, label = NULL))
})

testthat::test_that("T2.2 No error when label is of class `character`", {
  visR_KM_plot <- visR::estimate_KM(data = adtte, strata = "TRTA") %>% visR::visr()

  data <- tidycmprsk::trial
  visR_cuminc_plot <- visR::estimate_cuminc(data = data, CNSR = "death_cr", AVAL = "ttdeath") %>%
    visR::visr()

  testthat::expect_error(visR::add_annotation(visR_KM_plot, label = "blah"), NA)
  testthat::expect_error(visR::add_annotation(visR_cuminc_plot, label = "blah"), NA)
})

testthat::test_that("T2.3 No error when label is of class `data.frame`", {
  visR_KM_plot <- visR::estimate_KM(data = adtte, strata = "TRTA") %>% visR::visr()

  data <- tidycmprsk::trial
  visR_cuminc_plot <- visR::estimate_cuminc(data = data, CNSR = "death_cr", AVAL = "ttdeath") %>%
    visR::visr()

  testthat::expect_error(visR::add_annotation(visR_KM_plot, label = adtte[1:5, ]), NA)
  testthat::expect_error(visR::add_annotation(visR_cuminc_plot, label = adtte[1:5, ]), NA)
})

testthat::test_that("T2.3 No error when label is of class `gtable`", {
  visR_KM_plot <- visR::estimate_KM(data = adtte, strata = "TRTA") %>% visR::visr()

  data <- tidycmprsk::trial
  visR_cuminc_plot <- visR::estimate_cuminc(data = data, CNSR = "death_cr", AVAL = "ttdeath") %>%
    visR::visr()

  lbl <- gridExtra::tableGrob(adtte[1:5, ])

  testthat::expect_error(visR::add_annotation(visR_KM_plot, label = lbl), NA)
  testthat::expect_error(visR::add_annotation(visR_cuminc_plot, label = lbl), NA)
})

# Requirement T3 ---------------------------------------------------------------

testthat::context("add_annotation - T3. The annotation are representations of the actual label")

testthat::test_that("T3.1 An object of type `character` passed to label is not affected by the transformation to an annotation", {
  lbl <- "blah"

  visR_KM_plot <- visR::estimate_KM(data = adtte, strata = "TRTA") %>%
    visR::visr() %>%
    visR::add_annotation(label = lbl)

  data <- tidycmprsk::trial
  visR_cuminc_plot <- visR::estimate_cuminc(data = data, CNSR = "death_cr", AVAL = "ttdeath") %>%
    visR::visr() %>%
    visR::add_annotation(label = lbl)

  testthat::expect_equal(visR_KM_plot$components$grobs[[1]]$label, lbl)
  testthat::expect_equal(visR_cuminc_plot$components$grobs[[1]]$label, lbl)
})

testthat::test_that("T3.2 The content of a `data.frame` passed to label is not affected by the transformation to an annotation", {
  anno <- adtte[1:6, 1:5]

  visR_KM_plot <- visR::estimate_KM(data = adtte, strata = "TRTA") %>%
    visR::visr() %>%
    visR::add_annotation(label = anno)

  data <- tidycmprsk::trial
  visR_cuminc_plot <- visR::estimate_cuminc(data = data, CNSR = "death_cr", AVAL = "ttdeath") %>%
    visR::visr() %>%
    visR::add_annotation(label = anno)

  extracted_lbl_KM <- unlist(lapply(visR_KM_plot$components$grobs, function(x) {
    z <- x$label
    z <- gsub("bold(", "", z, fixed = TRUE)
    z <- gsub(")", "", z, fixed = TRUE)
    z <- gsub('\"', "", z, fixed = TRUE)
    z <- gsub(" - ", "-", z, fixed = TRUE)
    z
  }))

  extracted_lbl_cuminc <- unlist(lapply(visR_cuminc_plot$components$grobs, function(x) {
    z <- x$label
    z <- gsub("bold(", "", z, fixed = TRUE)
    z <- gsub(")", "", z, fixed = TRUE)
    z <- gsub('\"', "", z, fixed = TRUE)
    z <- gsub(" - ", "-", z, fixed = TRUE)
    z
  }))

  cN_KM <- extracted_lbl_KM[1:length(colnames(anno))]
  cN_cuminc <- extracted_lbl_cuminc[1:length(colnames(anno))]

  bD_KM <- extracted_lbl_KM[(length(cN_KM) + 1):length(extracted_lbl_KM)]
  bD_cuminc <- extracted_lbl_cuminc[(length(cN_cuminc) + 1):length(extracted_lbl_cuminc)]

  d_KM <- as.data.frame(matrix(bD_KM, ncol = length(cN_KM), byrow = FALSE),
    stringsAsFactors = FALSE
  )
  d_cuminc <- as.data.frame(matrix(bD_cuminc, ncol = length(cN_cuminc), byrow = FALSE),
    stringsAsFactors = FALSE
  )

  colnames(d_KM) <- cN_KM
  colnames(d_cuminc) <- cN_cuminc

  lbl <- data.frame(lapply(anno, as.character), stringsAsFactors = FALSE)

  testthat::expect_equal(d_KM, lbl, check.attributes = FALSE)
  testthat::expect_equal(d_cuminc, lbl, check.attributes = FALSE)
})

testthat::test_that("T3.3 The content of a `gtable` passed to label is not affected by the transformation to an annotation", {
  visR_KM_plot <- visR::estimate_KM(data = adtte, strata = "TRTA") %>% visR::visr()

  data <- tidycmprsk::trial
  visR_cuminc_plot <- visR::estimate_cuminc(data = data, CNSR = "death_cr", AVAL = "ttdeath") %>%
    visR::visr()

  anno <- gridExtra::tableGrob(adtte[1:6, ])

  visR_KM_plot <- visR_KM_plot %>% visR::add_annotation(label = anno)
  visR_KM_plot$components[[1]] <- NULL

  visR_cuminc_plot <- visR_cuminc_plot %>% visR::add_annotation(label = anno)
  visR_cuminc_plot$components[[1]] <- NULL

  gtab <- append(anno, NULL) # Mimic gtable addition to structure

  testthat::expect_equal(gtab, visR_KM_plot$components)
  testthat::expect_equal(gtab, visR_cuminc_plot$components)
})

# Requirement T4 ---------------------------------------------------------------

testthat::context("add_annotation - T4. The annotation can be placed on the plot by specifying the coordinates")

testthat::test_that("T4.1 An error when one of the coordinates is not numeric", {
  lbl <- "blah"

  visR_KM_plot <- visR::estimate_KM(data = adtte, strata = "TRTA") %>% visR::visr()

  data <- tidycmprsk::trial
  visR_cuminc_plot <- visR::estimate_cuminc(data = data, CNSR = "death_cr", AVAL = "ttdeath") %>%
    visR::visr()

  testthat::expect_error(visR_KM_plot %>% visR::add_annotation(label = lbl, xmin = "blah"))
  testthat::expect_error(visR_cuminc_plot %>% visR::add_annotation(label = lbl, xmin = "blah"))
})

testthat::test_that("T4.2 The annotation can be moved on the plot by specifying the x coordinates", {
  lbl <- "blah"

  visR_KM_plot <- visR::estimate_KM(data = adtte, strata = "TRTA") %>%
    visR::visr() %>%
    visR::add_annotation(label = lbl, xmin = 0, xmax = 99)

  visR_KM_plot2 <- visR::estimate_KM(data = adtte, strata = "TRTA") %>%
    visR::visr() %>%
    visR::add_annotation(label = lbl)

  data <- tidycmprsk::trial
  visR_cuminc_plot <- visR::estimate_cuminc(data = data, CNSR = "death_cr", AVAL = "ttdeath") %>%
    visR::visr() %>%
    visR::add_annotation(label = lbl, xmin = 0, xmax = 99)

  visR_cuminc_plot2 <- visR::estimate_cuminc(data = data, CNSR = "death_cr", AVAL = "ttdeath") %>%
    visR::visr() %>%
    visR::add_annotation(label = lbl)

  testthat::expect_false(isTRUE(all.equal(visR_KM_plot$layers, visR_KM_plot2$layers)))
  testthat::expect_false(isTRUE(all.equal(visR_cuminc_plot$layers, visR_cuminc_plot2$layers)))
})

testthat::test_that("T4.3 The annotation can be moved on the plot by specifying the y coordinates", {
  lbl <- "blah"

  visR_KM_plot <- visR::estimate_KM(data = adtte, strata = "TRTA") %>%
    visR::visr() %>%
    visR::add_annotation(label = lbl, ymin = 1, ymax = 1)

  visR_KM_plot2 <- visR::estimate_KM(data = adtte, strata = "TRTA") %>%
    visR::visr() %>%
    visR::add_annotation(label = lbl)

  data <- tidycmprsk::trial
  visR_cuminc_plot <- visR::estimate_cuminc(data = data, CNSR = "death_cr", AVAL = "ttdeath") %>%
    visR::visr() %>%
    visR::add_annotation(label = lbl, ymin = 1, ymax = 1)

  visR_cuminc_plot2 <- visR::estimate_cuminc(data = data, CNSR = "death_cr", AVAL = "ttdeath") %>%
    visR::visr() %>%
    visR::add_annotation(label = lbl)

  testthat::expect_false(isTRUE(all.equal(visR_KM_plot$layers, visR_KM_plot2$layers)))
  testthat::expect_false(isTRUE(all.equal(visR_cuminc_plot$layers, visR_cuminc_plot2$layers)))
})

# Requirement T5 ---------------------------------------------------------------

testthat::context("add_annotation - T5. The layout of the annotation can be modified to a certain extend")

testthat::test_that("T5.1 The annotation has bold columnheaders when the passed object is of class `data.frame`", {
  anno <- adtte[1:6, 1:5]

  visR_KM_plot <- visR::estimate_KM(data = adtte, strata = "TRTA") %>%
    visR::visr() %>%
    visR::add_annotation(label = anno)

  data <- tidycmprsk::trial
  visR_cuminc_plot <- visR::estimate_cuminc(data = data, CNSR = "death_cr", AVAL = "ttdeath") %>%
    visR::visr() %>%
    visR::add_annotation(label = anno)

  extracted_lbl_KM <- unlist(base::lapply(visR_KM_plot$components$grobs, function(x) {
    x$label
  }))

  extracted_lbl_cuminc <- unlist(base::lapply(visR_cuminc_plot$components$grobs, function(x) {
    x$label
  }))

  cN_KM <- extracted_lbl_KM[1:length(colnames(anno))]
  cN_cuminc <- extracted_lbl_cuminc[1:length(colnames(anno))]

  testthat::expect_match(unique(sub('\".*\"', "", cN_KM)), "bold()")
  testthat::expect_match(unique(sub('\".*\"', "", cN_cuminc)), "bold()")
})

testthat::test_that("T5.2 The font size can be changed", {
  lbl <- "blah"

  visR_plot <- visR::estimate_KM(data = adtte, strata = "TRTA") %>%
    visR::visr() %>%
    visR::add_annotation(label = lbl, base_size = 12)

  visR_plot2 <- visR::estimate_KM(data = adtte, strata = "TRTA") %>%
    visR::visr() %>%
    visR::add_annotation(label = lbl, base_size = 5)

  data <- tidycmprsk::trial
  visR_cuminc_plot <- visR::estimate_cuminc(data = data, CNSR = "death_cr", AVAL = "ttdeath") %>%
    visR::visr() %>%
    visR::add_annotation(label = lbl, base_size = 12)

  visR_cuminc_plot2 <- visR::estimate_cuminc(data = data, CNSR = "death_cr", AVAL = "ttdeath") %>%
    visR::visr() %>%
    visR::add_annotation(label = lbl, base_size = 5)

  testthat::expect_false(isTRUE(all.equal(visR_plot$layers, visR_plot2$layers)))
  testthat::expect_false(isTRUE(all.equal(visR_cuminc_plot$layers, visR_cuminc_plot2$layers)))
})

testthat::test_that("T5.3 The font family can be chosen between 'sans', 'serif' and 'mono'", {
  anno <- "blah"

  visR_plot <- visR::estimate_KM(data = adtte, strata = "TRTA") %>% visR::visr()

  data <- tidycmprsk::trial
  visR_cuminc_plot <- visR::estimate_cuminc(data = data, CNSR = "death_cr", AVAL = "ttdeath") %>%
    visR::visr()

  testthat::expect_error(visR_plot %>% visR::add_annotation(
    label = anno,
    base_family = "sans"
  ), NA)
  testthat::expect_error(visR_cuminc_plot %>% visR::add_annotation(
    label = anno,
    base_family = "sans"
  ), NA)

  testthat::expect_error(visR_plot %>% visR::add_annotation(
    label = anno,
    base_family = "serif"
  ), NA)
  testthat::expect_error(visR_cuminc_plot %>% visR::add_annotation(
    label = anno,
    base_family = "serif"
  ), NA)

  testthat::expect_error(visR_plot %>% visR::add_annotation(
    label = anno,
    base_family = "mono"
  ), NA)
  testthat::expect_error(visR_cuminc_plot %>% visR::add_annotation(
    label = anno,
    base_family = "mono"
  ), NA)

  testthat::expect_error(visR_plot %>% visR::add_annotation(
    label = anno,
    base_family = "blah"
  ))
  testthat::expect_error(visR_cuminc_plot %>% visR::add_annotation(
    label = anno,
    base_family = "blah"
  ))
})

# Requirement T6 ---------------------------------------------------------------

testthat::context("add_annotation - T6. The output object has an additional attribute `components`")

testthat::test_that("T6.1 The attribute components[['visR_plot']] contains the plot used as input", {
  anno <- "blah"

  visR_plot <- visR::estimate_KM(data = adtte, strata = "TRTA") %>% visR::visr()
  visR_plot_anno <- visR_plot %>% visR::add_annotation(label = anno)

  data <- tidycmprsk::trial
  visR_cuminc_plot <- visR::estimate_cuminc(data = data, CNSR = "death_cr", AVAL = "ttdeath") %>%
    visR::visr()
  visR_cuminc_plot_anno <- visR_cuminc_plot %>% visR::add_annotation(label = anno)

  testthat::expect_equal(visR_plot, visR_plot_anno$components$visR_plot)
  testthat::expect_equal(visR_cuminc_plot, visR_cuminc_plot_anno$components$visR_plot)
})

testthat::test_that("T6.2 The attribute components contains the annotation", {
  anno <- adtte[1:6, 1:5]

  visR_plot <- visR::estimate_KM(data = adtte, strata = "TRTA") %>%
    visR::visr() %>%
    visR::add_annotation(label = anno)

  data <- tidycmprsk::trial
  visR_cuminc_plot <- visR::estimate_cuminc(data = data, CNSR = "death_cr", AVAL = "ttdeath") %>%
    visR::visr() %>%
    visR::add_annotation(label = anno)

  testthat::expect_equal(names(visR_plot$components)[[2]], "grobs")
  testthat::expect_equal(names(visR_cuminc_plot$components)[[2]], "grobs")
})

testthat::test_that("T6.3 The output has the same class as the original ggplot", {
  anno <- adtte[1:6, 1:5]

  visR_plot <- visR::estimate_KM(data = adtte, strata = "TRTA") %>%
    visR::visr()

  visR_plot2 <- visR_plot %>%
    visR::add_annotation(label = anno)

  data <- tidycmprsk::trial
  visR_cuminc_plot <- visR::estimate_cuminc(data = data, CNSR = "death_cr", AVAL = "ttdeath") %>%
    visR::visr()

  visR_cuminc_plot2 <- visR_cuminc_plot %>%
    visR::add_annotation(label = anno)

  testthat::expect_equal(class(visR_plot), class(visR_plot2))
  testthat::expect_equal(class(visR_cuminc_plot), class(visR_cuminc_plot2))
})

# END OF CODE -------------------------------------------------------------
