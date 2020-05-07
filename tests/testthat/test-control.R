test_that("Abusive Inputs ==> ERROR", {
  expect_error(control(estimation_method = 1),
               "The estimation method must be one of*")
  #expect_error(control(estimation_method = "mle",hessian_method = 1))
  expect_error(control(sdm_variables = 1),
               "*must either be declared as a formula or as a string*")
  expect_error(control(instrumental_variables = 1),
               "*must either be declared as a formula or as a string*")
})

test_that("No Inputs ==> Default", {

  default_control <- control()
  expect_is(default_control,"list")

})
