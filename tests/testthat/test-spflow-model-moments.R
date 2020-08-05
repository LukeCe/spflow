load(file.path(rprojroot::find_testthat_root_file(),
               "test_case_1_symmetric.rda"))

test_that("spflow_model_moments_mat: M9 s2sls => correct output", {

  # stub out the identification of instruments is difficult to test
  # because it is based on attributes
  which_instruments <- test_case_1_symmetric$which_instruments
  mockery::stub(
    where = spflow_model_moments_mat,
    what = "identify_instrumental_variables",
    how = unlist(which_instruments, use.names = FALSE))

  #
  input_matrix <- test_case_1_symmetric$relational_model_matrices$M9
  actual <- spflow_model_moments_mat(model_matrices = input_matrix,
                                     estimator = "s2sls",
                                     flow_type = "within")
  expected <- test_case_1_symmetric$model_moments$M9

  # s2sls only uses the first entry of the TSS
  expected$TSS <- expected$TSS[1,1, drop = FALSE]

  compare_s2sls_moments <- c("N","TSS","HH","HY","ZZ","ZY")
  lapply(compare_s2sls_moments,
         function(.m) expect_equal(actual[[.m]], expected[[.m]],
                                   check.attributes = FALSE))
})

test_that("spflow_model_moments_mat: mle => correct output", {

  # no intra for model 2
  # no instruments for mle
  which_instruments <- test_case_1_symmetric$which_instruments
  which_instruments$intra_const <- NULL
  which_instruments$X <- which_instruments$X[1:8]
  nb_non_instruments <- sum(!unlist(which_instruments))

  # stub out the identification of instruments is difficult to test
  # because it is based on attributes
  mockery::stub(
    where = spflow_model_moments_mat,
    what = "identify_instrumental_variables",
    how = rep(FALSE,nb_non_instruments))


  # drop instrumental variables which are only relevant for s2sls estmation
  input_matrix <- test_case_1_symmetric$relational_model_matrices$M2

  which_instruments <- test_case_1_symmetric$which_instruments
  input_matrix$G[which_instruments$G] <- NULL
  input_matrix$const_intra <- NULL
  input_matrix$DX <- input_matrix$DX[,1:2]
  input_matrix$OX <- input_matrix$OX[,1:2]
  input_matrix$IX <- NULL
  actual <- spflow_model_moments_mat(
    model_matrices = input_matrix,
    estimator = "mle",
    flow_type = "within")
  expected <- test_case_1_symmetric$model_moments$M2
  compare_mle_moments <- c("N","n_o","n_d","TSS","ZZ","ZY","DW_traces")

  test_moment <- "N"
  expect_equal(actual[[test_moment]], expected[[test_moment]],
               check.attributes = FALSE)
  test_moment <- "n_o"
  expect_equal(actual[[test_moment]], expected[[test_moment]],
               check.attributes = FALSE)
  test_moment <- "n_d"
  expect_equal(actual[[test_moment]], expected[[test_moment]],
               check.attributes = FALSE)
  test_moment <- "TSS"
  expect_equal(actual[[test_moment]], expected[[test_moment]],
               check.attributes = FALSE)
  test_moment <- "ZZ"
  expect_equal(actual[[test_moment]], expected[[test_moment]],
               check.attributes = FALSE)
  test_moment <- "ZY"
  expect_equal(actual[[test_moment]], expected[[test_moment]],
               check.attributes = FALSE)
  test_moment <- "DW_traces"
  expect_equal(actual[[test_moment]], expected[[test_moment]],
               check.attributes = FALSE)

})
