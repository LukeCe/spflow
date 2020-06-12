load("tests/testthat/test_case_1.rda")


test_that("spflow_s2sls: model 9 => correct output", {


  s2sls_moments <- test_case_1$moments$Y9
  s2sls_moments$TSS <- s2sls_moments$TSS[1,1]

  actual <- do.call(spflow_s2sls,args = s2sls_moments)

  expect_equal(actual, expected)
})

