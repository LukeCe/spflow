load(here::here("tests/testthat/test_case_1.rda"))


test_that("spflow_s2sls: model 9 => correct output", {

  s2sls_moments <- test_case_1$moments$Y9
  s2sls_moments$TSS <- s2sls_moments$TSS[1,1]

  actual <- do.call(spflow_s2sls,args = s2sls_moments)
  expect_is(actual  ,"spflow_model")

  expected_parameters <- test_case_1$parmeters$Y9$s2sls
  actual_parameters <- actual$results$est
  expect_equal(expected_parameters,actual_parameters)

  expected_sd_error <- test_case_1$sd_error$Y9$s2sls
  actual_sd_error <- actual$sd
  expect_equal(expected_sd_error,actual_sd_error)

})

