load(here::here("tests/testthat/test_case_1_symmetric.rda"))

test_that("spflow_model_moments_mat: s2sls => correct output", {

  mockery::stub(
    where = spflow_model_moments_mat,
    what = "identify_instrumental_variables",
    how = unlist(test_case_1_symmetric$which_instruments, use.names = FALSE))

  # the test data has multiple versions of the flow matrix
  # we have to extract one for the test
  model_matrix_sym <-
    c(test_case_1_symmetric$relational_model_matrices,
      list("Y" = test_case_1_symmetric$relational_model_matrices$Y9))

  actual <- spflow_model_moments_mat(model_matrices = model_matrix_sym,
                                     estimator = "s2sls")
  expected <- test_case_1_symmetric$model_moments

  ## sample size
  expect_equal(actual$N, expected$N)

  ## total sum of squares
  # (one dimensional for the s2sls estimator)
  # TSS is also model dependent
  expect_equal(actual$TSS, expected$TSS9[1,1], check.attributes = FALSE)

  ## Variance moment (including instruments)
  expect_equal(actual$HH, expected$HH, check.attributes = FALSE)

  ## Covariance moment (including instruments)
  expect_equal(actual$HY, expected$HY9, check.attributes = FALSE)

  ## Variance moment (excluding instruments)
  expect_equal(actual$ZZ, expected$ZZ, check.attributes = FALSE)

  ## Covariance moment (excluding instruments)
  expect_equal(actual$ZY, expected$ZY9, check.attributes = FALSE)
})

