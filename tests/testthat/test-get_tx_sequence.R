test_that("Check for some input errors", {
  lots1 <- data.frame(
    id = c('aIDJFGH','aIDJFGH'),
    num = c(1,1),
    reg = c('Chemo 1', 'TT 2')
  )

  lots2 <- data.frame(
    id = c(1,1,2,2),
    num = c(2,3,1,2),
    reg = rep('A', 4)
  )

  suppressMessages(
    testthat::expect_error(
      visR::get_tx_sequence(lots1,
                            id = 'id',
                            label = 'reg',
                            line='num'),
      "data must contain a maximum of 1 patient per line"
    )
  )
  suppressMessages(
    testthat::expect_error(
      visR::get_tx_sequence(lots2,
                            id='id',
                            label = 'reg',
                            line = 'num'),
      "requires each patient to start on the same first line as others"
    )
  )

})

test_that("Check correct outputs", {
  lots3 <- data.frame(
    id = c('aIDJFGH','aIDJFGH','aIDJFGH',
           'ajfgFGH','ajfgFGH'),
    num = c(1L,2L,3L,1L,2L),
    reg = c('Chemo 1', 'TT 2','Chemo 3',
            'Chemo 4', 'CPI 2')
  )

  tx <- visR::get_tx_sequence(lots3,
                        id='id',
                        label='reg',
                        line='num')

  testthat::expect_equal(class(tx)[1],'tx_sequence')
  testthat::expect_equal(class(tx)[2],'data.frame')
  testthat::expect_true("label_1" %in% names(attributes(tx)))
  testthat::expect_true("label_2" %in% names(attributes(tx)))
})
