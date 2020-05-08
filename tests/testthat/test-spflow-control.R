test_that("Abusive Inputs ==> ERROR", {
  expect_error(spflow_control(estimation_method = 1),
               "The estimation method must be one of*")
  #expect_error(control(estimation_method = "mle",hessian_method = 1))
  expect_error(spflow_control(sdm_variables = 1),
               "*must either be declared as a formula or as a string*")
  expect_error(spflow_control(instrumental_variables = 1),
               "*must either be declared as a formula or as a string*")
})

test_that("No Inputs ==> Default", {

  default_control <- spflow_control()
  expect_is(default_control,"list")

})
