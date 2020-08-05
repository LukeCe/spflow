test_that("trace_lookup_template: => correct output", {

  # check for correct number of coefs and no nan values
  trinom_val <- 3
  actual <- trace_lookup_template(aprox_order = trinom_val)
  nb_coefs <- sum_trinom_coefs(trinom_val)
  expect_equal(nrow(actual),nb_coefs)
  expect_false(actual$c_trinom %>% is.nan() %>% any())

  trinom_val <- 20
  actual <- trace_lookup_template(aprox_order = trinom_val)
  nb_coefs <- sum_trinom_coefs(trinom_val)
  expect_equal(nrow(actual),nb_coefs)
  expect_false(actual$c_trinom %>% is.nan() %>% any())
})



test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
