# # ==== OLD =========t===========================================================
# load(file.path(rprojroot::find_testthat_root_file(),
#                "test_case_1_symmetric.rda"))
#
# # pull out test data for the symmetric case
# test_data <- c("X","G","const_intra","HH","H_index")
#
# # test model 9
# test_M9_sym <- named_list(test_data)
# test_M9_sym$"X" <-
#   test_case_1_symmetric$relational_model_matrices$M9[c("DX","OX","IX")]
# test_M9_sym$"G" <-
#   test_case_1_symmetric$relational_model_matrices$M9$G
# test_M9_sym$"const_intra" <-
#   test_case_1_symmetric$relational_model_matrices$M9$const_intra
# test_M9_sym$"HH" <-
#   test_case_1_symmetric$model_moments$M9$HH
# test_M9_sym$"H_index" <-
#   test_case_1_symmetric$model_moments$M9$H_index
#
#
# # ---- variance moments (diag blocks) -----------------------------------------
#
# test_that("var_moment_block_alpha: => correct output", {
#
#   actual_null <- var_moment_block_alpha(NULL)
#   expect_equal(object = actual_null, expected = NULL)
#
#   actual <- var_moment_block_alpha(10)
#   expected <- matrix(10)
#   expect_equal(actual, expected)
# })
#
# test_that("var_moment_block_alpha_I: => correct output", {
#
#   # non intra model
#   actual_null <- var_moment_block_alpha_I(NULL)
#   expect_null(actual_null)
#
#   # without instruments
#   index <- test_M9_sym$H_index$const_intra[1]
#   input <- test_M9_sym$const_intra[1]
#   actual <- var_moment_block_alpha_I(input)
#   expected <- test_M9_sym$HH[index,index, drop = FALSE]
#   expect_equal(actual,expected, check.attributes = FALSE)
#
#   # with all instruments
#   index <- test_M9_sym$H_index$const_intra
#   input <- test_M9_sym$const_intra
#   actual <- var_moment_block_alpha_I(input)
#   expected <- test_M9_sym$HH[index,index, drop = FALSE]
#   expect_equal(actual,expected, check.attributes = FALSE)
# })
#
# test_that("var_moment_block_beta: => correct output", {
#
#   # no attributes
#   actual_null <- var_moment_block_beta(NULL)
#   expect_null(actual_null)
#
#   # with all X: intra, orig, dest
#   index <- test_M9_sym$H_index$X
#   input <- test_M9_sym$X
#   actual <- var_moment_block_beta(input)
#   expected <- test_M9_sym$HH[index,index, drop = FALSE]
#   expect_equal(actual,expected, check.attributes = FALSE)
#
#   # drop X intra
#   input$IX <- NULL
#   index <- test_M9_sym$H_index$X[1:8]
#   actual <- var_moment_block_beta(input)
#   expected <- test_M9_sym$HH[index,index]
#   expect_equal(actual,expected, check.attributes = FALSE)
# })
#
# test_that("var_moment_block_gamma: => correct output", {
#
#   # no pair attributes
#   actual_null <- var_moment_block_gamma(NULL)
#   expect_null(actual_null)
#
#   # standard case
#   index <- test_M9_sym$H_index$G
#   input <- test_M9_sym$G
#   actual <- var_moment_block_gamma(input)
#   expected <- test_M9_sym$HH[index,index, drop = FALSE]
#   expect_equal(actual,expected, check.attributes = FALSE)
# })
#
# # ---- variance moments (off-diag blocks) -------------------------------------
#
# test_that("var_moment_block_alpha_alpha_I: => correct output", {
#
#   # no intra case
#   actual_null <- var_moment_block_alpha_alpha_I(NULL)
#   expect_null(actual_null)
#
#   # standard case
#   row <- test_M9_sym$H_index$const
#   col <- test_M9_sym$H_index$const_intra
#   input <- test_M9_sym$const_intra
#   actual <- var_moment_block_alpha_alpha_I(input)
#   expected <- test_M9_sym$HH[row,col, drop = FALSE]
#   expect_equal(actual,expected, check.attributes = FALSE)
# })
#
# test_that("var_moment_block_alpha_beta: => correct output", {
#
#   # no region attributes
#   actual_null <- var_moment_block_alpha_beta(NULL)
#   expect_null(actual_null)
#
#   # standard case
#   row <- test_M9_sym$H_index$const
#   col <- test_M9_sym$H_index$X
#   input <- test_M9_sym$X
#   actual <- var_moment_block_alpha_beta(input)
#   expected <- test_M9_sym$HH[row,col, drop = FALSE]
#   expect_equal(actual,expected, check.attributes = FALSE)
# })
#
# test_that("var_moment_block_alpha_gamma: => correct output", {
#
#   # no pair attributes
#   actual_null <- var_moment_block_alpha_gamma(NULL)
#   expect_null(actual_null)
#
#   # standard case
#   row <- test_M9_sym$H_index$const
#   col <- test_M9_sym$H_index$G
#   input <- test_M9_sym$G
#   actual <- var_moment_block_alpha_gamma(input)
#   expected <- test_M9_sym$HH[row,col, drop = FALSE]
#   expect_equal(actual,expected, check.attributes = FALSE)
# })
#
# test_that("var_moment_block_alpha_I_beta: => correct output", {
#
#   # no intra no X
#   actual_null <- var_moment_block_alpha_I_beta(NULL,NULL)
#   expect_null(actual_null)
#
#   # no X
#   input_X <- test_M9_sym$X
#   actual_null <- var_moment_block_alpha_I_beta(NULL, input_X)
#   expect_null(actual_null)
#
#   # no intra
#   input_intra <- test_M9_sym$const_intra
#   actual_null <- var_moment_block_alpha_I_beta(input_intra, NULL)
#   expect_null(actual_null)
#
#   # standard case
#   row <- test_M9_sym$H_index$const_intra
#   col <- test_M9_sym$H_index$X
#   actual <- var_moment_block_alpha_I_beta(input_intra, input_X)
#   expected <- test_M9_sym$HH[row,col]
#   expect_equal(actual, expected, check.attributes = FALSE)
# })
#
# test_that("var_moment_block_alpha_I_gamma: => correct output", {
#
#   # no intra no pairs
#   actual_null <- var_moment_block_alpha_I_gamma(NULL,NULL)
#   expect_null(actual_null)
#
#   # no intra
#   input_G <- test_M9_sym$G
#   actual_null <- var_moment_block_alpha_I_gamma(NULL, input_G)
#   expect_null(actual_null)
#
#   # no pairs
#   input_intra <- test_M9_sym$const_intra
#   actual_null <- var_moment_block_alpha_I_gamma(input_intra, NULL)
#   expect_null(actual_null)
#
#   # standard
#   actual <- var_moment_block_alpha_I_gamma(input_intra, input_G)
#   row <- test_M9_sym$H_index$const_intra
#   col <- test_M9_sym$H_index$G
#   expected <- test_M9_sym$HH[row,col]
#   expect_equal(actual, expected, check.attributes = FALSE)
#
# })
#
# test_that("var_moment_block_beta_gamma: => correct output", {
#
#   # no pairs no X
#   actual_null <- var_moment_block_beta_gamma(NULL,NULL)
#   expect_null(actual_null)
#
#   # no pairs
#   input_X <- test_M9_sym$X
#   actual_null <- var_moment_block_beta_gamma(input_X, NULL)
#   expect_null(actual_null)
#
#   # no X
#   input_G <- test_M9_sym$G
#   actual_null <- var_moment_block_beta_gamma(NULL, input_G)
#   expect_null(actual_null)
#
#   # with intra
#   row <- test_M9_sym$H_index$X
#   col <- test_M9_sym$H_index$G
#   actual <- var_moment_block_beta_gamma(input_X,input_G)
#   expected <- test_M9_sym$HH[row,col]
#   expect_equal(actual, expected, check.attributes = FALSE)
#
#   # without intra
#   input_X$IX <- NULL
#   row <- test_M9_sym$H_index$X[1:8]
#   col <- test_M9_sym$H_index$G
#   actual <- var_moment_block_beta_gamma(input_X,input_G)
#   expected <- test_M9_sym$HH[row,col]
#   expect_equal(actual, expected, check.attributes = FALSE)
# })
#
# # ---- covariance moments (blocks) --------------------------------------------
#
# # as single columns of HY and a single matrix Y is enough for the test
# test_M9_sym$"HY" <-
#   test_case_1_symmetric$model_moments$M9$HY[,1]
# test_M9_sym$"Y" <-
#   test_case_1_symmetric$relational_model_matrices$M9$Y[[1]]
#
# test_that("cov_moment_block_alpha: => correct output", {
#
#   # no constant
#   actual_null <- cov_moment_block_alpha(NULL)
#   expect_null(actual_null)
#
#   # with constant
#   index <- test_M9_sym$H_index$const
#   actual <- cov_moment_block_alpha(test_M9_sym$Y)
#   expected <- test_M9_sym$HY[index]
#   expect_equal(actual, expected, check.attributes = FALSE)
# })
#
# test_that("cov_moment_block_alpha_I: => correct output", {
#
#   # no intra
#   input_Y <- test_M9_sym$Y
#   actual_null <- cov_moment_block_alpha_I(input_Y, NULL)
#   expect_null(actual_null)
#
#   # with instruments
#   input_intra <- test_M9_sym$const_intra
#   actual <- cov_moment_block_alpha_I(input_Y, input_intra)
#   index <- test_M9_sym$H_index$const_intra
#   expected <- test_M9_sym$HY[index]
#   expect_equal(actual, expected, check.attributes = FALSE)
#
#   # without instruments
#   input_intra <- test_M9_sym$const_intra[1]
#   actual <- cov_moment_block_alpha_I(input_Y, input_intra)
#   index <- test_M9_sym$H_index$const_intra[1]
#   expected <- test_M9_sym$HY[index]
#   expect_equal(actual, expected, check.attributes = FALSE)
# })
#
# test_that("cov_moment_block_beta: => correct output", {
#
#   # no region attributes
#   input_Y <- test_M9_sym$Y
#   actual_null <- cov_moment_block_beta(input_Y, NULL)
#   expect_null(actual_null)
#
#   # with intra
#   input_X <- test_M9_sym$X
#   actual <- cov_moment_block_beta(input_Y, input_X)
#   index <- test_M9_sym$H_index$X
#   expected <- test_M9_sym$HY[index]
#   expect_equal(actual, expected, check.attributes = FALSE)
#
#   # without intra
#   input_X$IX <- NULL
#   actual <- cov_moment_block_beta(input_Y, input_X)
#   index <- test_M9_sym$H_index$X[1:8]
#   expected <- test_M9_sym$HY[index]
#   expect_equal(actual, expected, check.attributes = FALSE)
# })
#
# test_that("cov_moment_block_gamma: => correct output", {
#
#   # no pairs
#   input_Y <- test_M9_sym$Y
#   actual_null <- cov_moment_block_gamma(input_Y, NULL)
#   expect_null(actual_null)
#
#   # standard
#   input_G <- test_M9_sym$G
#   actual <- cov_moment_block_gamma(input_Y, input_G)
#   index <- test_M9_sym$H_index$G
#   expected <- test_M9_sym$HY[index]
#   expect_equal(actual, expected, check.attributes = FALSE)
# })
#
# # ---- full moment matrices (var + cov) -------------------------------------------------
#
# test_that("moment_empirical_var: => correct output", {
#
#   # model 2 without intra
#   input <- test_case_1_symmetric$relational_model_matrices$M2
#   actual <- moment_empirical_var(input)
#   expected <- test_case_1_symmetric$model_moments$M2$HH
#   expect_equal(actual,expected, check.attributes = FALSE)
#
#   # model 9 with intra
#   input <- test_case_1_symmetric$relational_model_matrices$M9
#   actual <- moment_empirical_var(input)
#   expected <- test_case_1_symmetric$model_moments$M9$HH
#   expect_equal(actual,expected, check.attributes = FALSE)
# })
#
# test_that("moment_empirical_covar: => correct output", {
#
#   # a test based on a single flow matrix is enough
#   one_flow <- 1
#
#   # model 2 without intra
#   input_matrix <- test_case_1_symmetric$relational_model_matrices$M2
#   input_Y <- input_matrix$Y[[one_flow]]
#   actual <- moment_empirical_covar(input_Y,input_matrix)
#   expected <- test_case_1_symmetric$model_moments$M2$HY[,one_flow]
#   expect_equal(actual,expected, check.attributes = FALSE)
#
#   # model 9 with intra
#   input_matrix <- test_case_1_symmetric$relational_model_matrices$M9
#   input_Y <- input_matrix$Y[[one_flow]]
#   actual <- moment_empirical_covar(input_Y,input_matrix)
#   expected <- test_case_1_symmetric$model_moments$M9$HY[,one_flow]
#   expect_equal(actual,expected, check.attributes = FALSE)
#
# })

# ---- OLDER ------------------------------------------------------------------

# load(file.path(rprojroot::find_testthat_root_file(),
#                "test_case_1_symmetric.rda"))
#
# test_that("spflow_model_moments_mat: M9 s2sls => correct output", {
#
#   # stub out the identification of instruments is difficult to test
#   # because it is based on attributes
#   which_instruments <- test_case_1_symmetric$which_instruments
#   mockery::stub(
#     where = spflow_model_moments_mat,
#     what = "identify_instrumental_variables",
#     how = unlist(which_instruments, use.names = FALSE))
#
#   #
#   input_matrix <- test_case_1_symmetric$relational_model_matrices$M9
#   actual <- spflow_model_moments_mat(model_matrices = input_matrix,
#                                      estimator = "s2sls",
#                                      flow_type = "within")
#   expected <- test_case_1_symmetric$model_moments$M9
#
#   # s2sls only uses the first entry of the TSS
#   expected$TSS <- expected$TSS[1,1, drop = FALSE]
#
#   compare_s2sls_moments <- c("N","TSS","HH","HY","ZZ","ZY")
#   lapply(compare_s2sls_moments,
#          function(.m) expect_equal(actual[[.m]], expected[[.m]],
#                                    check.attributes = FALSE))
# })
#
# test_that("spflow_model_moments_mat: mle => correct output", {
#
#   # no intra for model 2
#   # no instruments for mle
#   which_instruments <- test_case_1_symmetric$which_instruments
#   which_instruments$intra_const <- NULL
#   which_instruments$X <- which_instruments$X[1:8]
#   nb_non_instruments <- sum(!unlist(which_instruments))
#
#   # stub out the identification of instruments is difficult to test
#   # because it is based on attributes
#   mockery::stub(
#     where = spflow_model_moments_mat,
#     what = "identify_instrumental_variables",
#     how = rep(FALSE,nb_non_instruments))
#
#
#   # drop instrumental variables which are only relevant for s2sls estmation
#   input_matrix <- test_case_1_symmetric$relational_model_matrices$M2
#
#   which_instruments <- test_case_1_symmetric$which_instruments
#   input_matrix$G[which_instruments$G] <- NULL
#   input_matrix$const_intra <- NULL
#   input_matrix$DX <- input_matrix$DX[,1:2]
#   input_matrix$OX <- input_matrix$OX[,1:2]
#   input_matrix$IX <- NULL
#   actual <- spflow_model_moments_mat(
#     model_matrices = input_matrix,
#     estimator = "mle",
#     flow_type = "within")
#   expected <- test_case_1_symmetric$model_moments$M2
#   compare_mle_moments <- c("N","n_o","n_d","TSS","ZZ","ZY","DW_traces")
#
#   test_moment <- "N"
#   expect_equal(actual[[test_moment]], expected[[test_moment]],
#                check.attributes = FALSE)
#   test_moment <- "n_o"
#   expect_equal(actual[[test_moment]], expected[[test_moment]],
#                check.attributes = FALSE)
#   test_moment <- "n_d"
#   expect_equal(actual[[test_moment]], expected[[test_moment]],
#                check.attributes = FALSE)
#   test_moment <- "TSS"
#   expect_equal(actual[[test_moment]], expected[[test_moment]],
#                check.attributes = FALSE)
#   test_moment <- "ZZ"
#   expect_equal(actual[[test_moment]], expected[[test_moment]],
#                check.attributes = FALSE)
#   test_moment <- "ZY"
#   expect_equal(actual[[test_moment]], expected[[test_moment]],
#                check.attributes = FALSE)
#   test_moment <- "DW_traces"
#   expect_equal(actual[[test_moment]], expected[[test_moment]],
#                check.attributes = FALSE)
#
# })

