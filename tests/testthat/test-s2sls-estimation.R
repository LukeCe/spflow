load(file.path(rprojroot::find_testthat_root_file(),
               "test_case_1_symmetric.rda"))


test_that("spflow_s2sls: model 9 => correct output", {

  required_moments <- c("N","HH","ZZ","ZY","HY","TSS")
  s2sls_moments <- test_case_1_symmetric$model_moments$M9
  s2sls_moments$TSS <- s2sls_moments$TSS[1,1]

  actual <- do.call(spflow_s2sls,args = s2sls_moments[required_moments])
  expect_is(actual  ,"spflow_model")

  expected_parameters <- test_case_1_symmetric$results$M9$s2sls$params
  actual_parameters <- actual$results$est
  expect_equal(expected_parameters,actual_parameters,
               check.attributes = FALSE)

  expected_sd_param <- test_case_1_symmetric$results$M9$s2sls$sd_params
  actual_sd_param <- actual$results$sd
  expect_equal(expected_sd_param,actual_sd_param,
               check.attributes = FALSE)

})

