# ---- setup ------------------------------------------------------------------
load(file.path(rprojroot::find_testthat_root_file(),
               "test_case_1_symmetric.rda"))

# pull out test data for the symmetric case
test_X_sym <-
  test_case_1_symmetric$relational_model_matrices[c("DX","OX","IX")]
test_G_sym <-
  test_case_1_symmetric$relational_model_matrices$G
const_intra_sym <- test_case_1_symmetric$relational_model_matrices$const_intra
HH_sym <- test_case_1_symmetric$model_moments$HH
H_index_sym <- test_case_1_symmetric$model_moments$H_index

# ---- variance moments (diag blocks) -----------------------------------------

test_that("var_moment_block_alpha: => correct output", {

  actual_null <- var_moment_block_alpha(NULL)
  expect_equal(object = actual_null, expected = NULL)

  actual <- var_moment_block_alpha(10)
  expected <- matrix(10)
  expect_equal(actual, expected)
})

test_that("var_moment_block_alpha_I: => correct output", {

  actual_null <- var_moment_block_alpha_I(NULL)
  expect_null(actual_null)

  const_intra <- test_case_1_symmetric$relational_model_matrices$const_intra
  # without instruments
  actual_without_instruments <- var_moment_block_alpha_I(const_intra[1])
  n <- nrow(const_intra[[1]])
  expected <- matrix(n)
  expect_equal(actual_without_instruments,expected)

  actual_with_instruments <- var_moment_block_alpha_I(const_intra)
  expected <- hadamarad_sum_matrix(const_intra)
  expect_equal(actual_with_instruments,expected)
})

test_that("var_moment_block_beta: => correct output", {

  actual_null <- var_moment_block_beta(NULL)
  expect_null(actual_null)

  actual <- var_moment_block_beta(test_X_sym)
  index <- H_index_sym$X
  expected <- HH_sym[index,index]
  expect_equal(actual,expected, check.attributes = FALSE)

  # drop intra
  test_X_sym$IX <- NULL
  actual <- var_moment_block_beta(test_X_sym)
  keep_DX_OX <- 1:8
  index <- H_index_sym$X[keep_DX_OX]
  expected <- HH_sym[index,index]
  expect_equal(actual,expected, check.attributes = FALSE)
})

test_that("var_moment_block_gamma: => correct output", {

  actual_null <- var_moment_block_gamma(NULL)
  expect_null(actual_null)

  actual <-
    var_moment_block_gamma(test_case_1_symmetric$relational_model_matrices$G)
  index <- H_index_sym$G
  expected <- HH_sym[index,index]
  expect_equal(actual, expected, check.attributes = FALSE)
})

# ---- variance moments (off-diag blocks) -------------------------------------

test_that("var_moment_block_alpha_alpha_I: => correct output", {

  actual_null <- var_moment_block_alpha_alpha_I(NULL)
  expect_null(actual_null)

  const_intra <- test_case_1_symmetric$relational_model_matrices$const_intra
  actual <- var_moment_block_alpha_alpha_I(const_intra)
  row <- H_index_sym$const
  col <- H_index_sym$const_intra
  expected <- HH_sym[row,col]
  expect_equal(actual, expected, check.attributes = FALSE)
})

test_that("var_moment_block_alpha_beta: => correct output", {
  actual_null <- var_moment_block_alpha_beta(NULL)
  expect_null(actual_null)

  actual <- var_moment_block_alpha_beta(test_X_sym)
  row <- H_index_sym$const
  col <- H_index_sym$X
  expected <- HH_sym[row,col]
  expect_equal(actual, expected, check.attributes = FALSE)
})

test_that("var_moment_block_alpha_gamma: => correct output", {
  actual_null <- var_moment_block_alpha_gamma(NULL)
  expect_null(actual_null)
})

test_that("var_moment_block_alpha_I_beta: => correct output", {

  actual_null <- var_moment_block_alpha_I_beta(NULL,NULL)
  expect_null(actual_null)

  actual_null <- var_moment_block_alpha_I_beta(NULL, test_X_sym)
  expect_null(actual_null)

  const_intra <- test_case_1_symmetric$relational_model_matrices$const_intra
  actual_null <- var_moment_block_alpha_I_beta(const_intra, NULL)
  expect_null(actual_null)

  actual <- var_moment_block_alpha_I_beta(const_intra, test_X_sym)
  row <- H_index_sym$const_intra
  col <- H_index_sym$X
  expected <- HH_sym[row,col]
  expect_equal(actual, expected, check.attributes = FALSE)
})

test_that("var_moment_block_alpha_I_gamma: => correct output", {

  actual_null <- var_moment_block_alpha_I_gamma(NULL,NULL)
  expect_null(actual_null)

  actual_null <- var_moment_block_alpha_I_gamma(NULL, test_G_sym)
  expect_null(actual_null)

  actual_null <- var_moment_block_alpha_I_gamma(const_intra_sym, NULL)
  expect_null(actual_null)

  actual <- var_moment_block_alpha_I_gamma(const_intra_sym, test_G_sym)
  row <- H_index_sym$const_intra
  col <- H_index_sym$G
  expected <- HH_sym[row,col]
  expect_equal(actual, expected, check.attributes = FALSE)

})

test_that("var_moment_block_beta_gamma: => correct output", {

  actual_null <- var_moment_block_beta_gamma(NULL,NULL)
  expect_null(actual_null)

  actual_null <- var_moment_block_beta_gamma(test_X_sym, NULL)
  expect_null(actual_null)

  actual_null <- var_moment_block_beta_gamma(NULL, test_G_sym)
  expect_null(actual_null)

  actual <- var_moment_block_beta_gamma(test_X_sym,test_G_sym)
  row <- H_index_sym$X
  col <- H_index_sym$G
  expected <- HH_sym[row,col]
  expect_equal(actual, expected, check.attributes = FALSE)
})

# ---- covariance moments (blocks) --------------------------------------------

# pull the convariance moment and the correspnding flow matrix
Y9_sym <- test_case_1_symmetric$relational_model_matrices$Y9[[1]]
HY_sym <- test_case_1_symmetric$model_moments$HY9[,1]


test_that("cov_moment_block_alpha: => correct output", {

  actual_null <- cov_moment_block_alpha(NULL)
  expect_null(actual_null)

  actual <- cov_moment_block_alpha(Y9_sym)
  expected <- sum(Y9_sym)
  expect_equal(actual, expected)
})

test_that("cov_moment_block_alpha_I: => correct output", {

  actual_null <- cov_moment_block_alpha_I(Y9_sym, NULL)
  expect_null(actual_null)

  actual <- cov_moment_block_alpha_I(Y9_sym, const_intra_sym)
  index <- H_index_sym$const_intra
  expected <- HY_sym[index]
  expect_equal(actual, expected, check.attributes = FALSE)
})

test_that("cov_moment_block_beta: => correct output", {

  actual_null <- cov_moment_block_beta(test_input$Y, NULL)
  expect_null(actual_null)

  actual <- cov_moment_block_beta(Y9_sym, test_X_sym)
  index <- H_index_sym$X
  expected <- HY_sym[index]
  expect_equal(actual, expected, check.attributes = FALSE)
})

test_that("cov_moment_block_gamma: => correct output", {

  actual_null <- cov_moment_block_gamma(Y9, NULL)
  expect_null(actual_null)

  actual <- cov_moment_block_gamma(Y9_sym, test_G_sym)
  index <- H_index_sym$G
  expected <- HY_sym[index]
  expect_equal(actual, expected, check.attributes = FALSE)
})

# ---- full moment matrices (var + cov) -------------------------------------------------
# the test data has multiple versions of the flow matrix
# we have to extract one for the test
model_matrix_sym <-
  c(test_case_1_symmetric$relational_model_matrices,
    list("Y" = test_case_1_symmetric$relational_model_matrices$Y9))

test_that("moment_empirical_var: => correct output", {

  actual <- moment_empirical_var(model_matrix_sym)
  expected <- HH_sym
  expect_equal(actual,expected, check.attributes = FALSE)
})

test_that("moment_empirical_covar: => correct output", {

  actual <- moment_empirical_covar(Y9_sym, model_matrix_sym)
  expected <- HY_sym
  expect_equal(actual, expected, check.attributes = FALSE)
})
