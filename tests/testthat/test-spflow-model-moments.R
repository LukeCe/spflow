load(file.path(rprojroot::find_testthat_root_file(),
               "test_case_1_symmetric.rda"))

test_that("spflow_model_moments_mat: s2sls => correct output", {

  # stub out the identification of instruments is difficult to test
  # because it is based on attributes
  which_instruments <- test_case_1_symmetric$which_instruments
  mockery::stub(
    where = spflow_model_moments_mat,
    what = "identify_instrumental_variables",
    how = unlist(which_instruments, use.names = FALSE))

  # the test data has multiple versions of the flow matrix
  # we have to extract one for the test
  model_matrix_sym <-
    c(test_case_1_symmetric$relational_model_matrices,
      list("Y" = test_case_1_symmetric$relational_model_matrices$Y9))

  actual <- spflow_model_moments_mat(model_matrices = model_matrix_sym,
                                     estimator = "s2sls",
                                     flow_type = "within")
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

test_that("spflow_model_moments_mat: mle => correct output", {

  # the test data has multiple versions of the flow matrix
  # we have to extract one for the test
  model_matrix_sym <-
    c(test_case_1_symmetric$relational_model_matrices,
      list("Y" = test_case_1_symmetric$relational_model_matrices$Y9))

  # drop instrumental variables which are only relevant for s2sls estmation
  which_instruments <- test_case_1_symmetric$which_instruments
  model_matrix_sym$G[which_instruments$G] <- NULL
  model_matrix_sym$const_intra[which_instruments$intra_const] <- NULL
  model_matrix_sym$DX <- model_matrix_sym$DX[,1:2]
  model_matrix_sym$OX <- model_matrix_sym$OX[,1:2]
  model_matrix_sym$IX <- model_matrix_sym$IX[,1:2]

  nb_non_instruments <- sum(!unlist(which_instruments))
  mockery::stub(
    where = spflow_model_moments_mat,
    what = "identify_instrumental_variables",
    how = rep(FALSE,nb_non_instruments))


  actual <- spflow_model_moments_mat(model_matrices = model_matrix_sym,
                                     estimator = "mle",
                                     flow_type = "within")
  expected <- test_case_1_symmetric$model_moments

  ## sample size
  expect_equal(actual$N, expected$N)

  ## total sum of squares
  # (one dimensional for the s2sls estimator)
  # TSS is also model dependent
  expect_equal(actual$TSS, expected$TSS9, check.attributes = FALSE)

  ## Variance moment
  expect_equal(actual$ZZ, expected$ZZ, check.attributes = FALSE)

  ## Covariance moment
  expect_equal(actual$ZY, expected$ZY9, check.attributes = FALSE)

  ## Traces
  expect_equal(actual$OW_traces, expected$W_traces, check.attributes = FALSE)

})
