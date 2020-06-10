load(here::here("tests/testthat/test_case_1.rda"))

test_that("spflow_model_moments_mat: s2sls => correct output", {

  mockery::stub(where = spflow_model_moments_mat,
                what = "identify_instrumental_variables",
                how = unlist(instrumental_variables,use.names = FALSE))
  actual <- spflow_model_moments_mat(model_matrices = test_input,
                                     estimator = "s2sls")
  expected <- reference_moments

  ## sample size
  expect_equal(actual$N, expected$N)

  ## total sum of squares
  # (one dimensional for the s2sls estimator)
  expect_equal(actual$TSS, expected$TSS[1,1], check.attributes = FALSE)

  ## Variance moment (including instruments)
  expect_equal(actual$HH, expected$HH, check.attributes = FALSE)

  ## Covariance moment (including instruments)
  expect_equal(actual$HY, expected$HY, check.attributes = FALSE)

  ## Variance moment (excluding instruments)
  expect_equal(actual$ZZ, expected$ZZ, check.attributes = FALSE)

  ## Covariance moment (excluding instruments)
  expect_equal(actual$ZY, expected$ZY, check.attributes = FALSE)
})

