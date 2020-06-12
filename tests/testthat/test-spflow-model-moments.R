load(here::here("tests/testthat/test_case_1.rda"))

test_that("spflow_model_moments_mat: s2sls => correct output", {

  mockery::stub(
    where = spflow_model_moments_mat,
    what = "identify_instrumental_variables",
    how = unlist(test_case_1$which_instruments, use.names = FALSE))

  # the test data has multiple versions of the flow matrix
  # we have to extract one for the test
  test_model_matrix <- test_case_1$data
  test_model_matrix$Y <- test_model_matrix[["Y9"]]
  expected <- test_case_1$moments[["Y9"]]
  actual <- spflow_model_moments_mat(model_matrices = test_model_matrix,
                                     estimator = "s2sls")

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

