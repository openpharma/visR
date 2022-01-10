test_that("estimate_cuminc() works", {
  expect_error(
    estimate_cuminc(tidycmprsk::trial, CNSR = "death_cr", AVAL = "ttdeath") %>%
      visr() %>%
      add_CI() %>%
      add_CNSR() %>%
      add_risktable(),
    NA
  )

  expect_error(
    cuminc1 <-
      estimate_cuminc(tidycmprsk::trial, strata = "trt", CNSR = "death_cr", AVAL = "ttdeath"),
    NA
  )

  expect_equal(
    cuminc1[c("failcode", "cmprsk", "conf.level", "tidy")],
    tidycmprsk::cuminc(tidycmprsk::Surv(ttdeath, death_cr) ~ trt, tidycmprsk::trial)[c("failcode", "cmprsk", "conf.level", "tidy")]
  )

  expect_error(cuminc1_visr <- visr(cuminc1), NA)

  expect_error(
    cuminc1_visr %>%
      add_CI() %>%
      add_CNSR() %>%
      add_risktable(),
    NA
  )

  expect_error(
    cuminc1_visr %>%
      add_risktable(statlist = c("n.risk", "n.event")),
    NA
  )

  expect_error(
    cuminc1_visr %>%
      add_risktable(statlist = c("n.risk", "n.event"),
                    group = "statlist"),
    NA
  )

  expect_error(
    cuminc1_visr %>%
      add_risktable(statlist = c("n.risk", "n.event"),
                    label = c("No. at Risk", "No. Events")),
    NA
  )

  expect_error(
    cuminc1_visr %>%
      add_risktable(statlist = c("n.risk", "n.event"),
                    label = c("No. at Risk", "No. Events"),
                    group = "statlist"),
    NA
  )

  expect_error(
    cuminc1_visr %>%
      add_risktable(statlist = c("n.risk", "n.event"),
                    label = c("No. at Risk", "No. Events"),
                    group = "statlist",
                    collapse = TRUE),
    NA
  )

  expect_error(
    cuminc1_visr %>%
      add_risktable(statlist = c("n.risk", "n.event"),
                    label = c("No. at Risk", "No. Events"),
                    collapse = TRUE),
    NA
  )
})
