# ---- setup ------------------------------------------------------------------
# For this series of test we create all required inputs to generate the blocks
# of the moment matrix.
# The constructed blocks are then tested against their references obtained as
# the inner product of the vectorized version of the data.

test_object_names <- c("X","W","const_intra","N","G","Y")
test_input <- named_list(test_object_names)
data("germany_grid")

test_input[["W"]] <-
  germany_grid %>%
  spdep::poly2nb() %>%
  spdep::nb2listw() %>%
  spdep::listw2mat() %>%
  Matrix::Matrix()

test_input[["X"]] <- cbind("X" = germany_grid$X,
                           "X_lag" = test_input$W %*% germany_grid$X)
test_input[["const_intra"]] <-
  intra_regional_constant(test_input$W, use_instruments = TRUE)

n_sites <- nrow(test_input$X)
test_input[["N"]] <- n_sites^2

dummy_pair_data <- rnorm(test_input$N)
test_input[["G"]] <-
  replicate(3,matrix(dummy_pair_data, nrow = n_sites, ncol = n_sites),
            simplify = FALSE)
test_input[["Y"]] <-
  matrix(dummy_pair_data, nrow = n_sites, ncol = n_sites)

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

  test_W <- test_input$W
  test_const_intra <- test_input$const_intra
  actual_without_instruments <- var_moment_block_alpha_I(test_const_intra[1])
  expected <- matrix(nrow(test_W))
  expect_equal(actual_without_instruments,expected)

  actual_with_instruments <- var_moment_block_alpha_I(test_const_intra)
  expected <- hadamarad_sum_matrix(test_const_intra)
  expect_equal(actual_with_instruments,expected)
})

test_that("var_moment_block_beta: => correct output", {

  actual_null <- var_moment_block_beta(NULL)
  expect_null(actual_null)

  test_X <- test_input$X %>% expand_O_D_I_mem()
  actual <- var_moment_block_beta(test_X)
  expected <- test_X %>% vec_reference_O_D_I_mem() %>% crossprod_mem()
  expect_equal(actual,expected, check.attributes = FALSE)

  # drop intra
  test_X$IX <- NULL
  actual <- var_moment_block_beta(test_X)
  expected <- test_X %>% vec_reference_O_D_I_mem() %>% crossprod_mem()
  expect_equal(actual,expected, check.attributes = FALSE)
})

test_that("var_moment_block_gamma: => correct output", {

  actual_null <- var_moment_block_gamma(NULL)
  expect_null(actual_null)

  test_G <- test_input$G
  actual <- var_moment_block_gamma(test_G)
  expected <- test_G %>% vec_reference_matrix_mem() %>% crossprod_mem()
  expect_equal(actual, expected, check.attributes = FALSE)
})

# ---- variance moments (off-diag blocks) -------------------------------------

test_that("var_moment_block_alpha_alpha_I: => correct output", {
  actual_null <- var_moment_block_alpha_alpha_I(NULL)
  expect_null(actual_null)

  test_const_intra <- test_input$const_intra
  actual <- var_moment_block_alpha_alpha_I(test_const_intra)
  expected <- test_const_intra %>%
    vec_reference_matrix_mem() %>%
    colSums()
  expect_equal(actual, expected, check.attributes = FALSE)
})

test_that("var_moment_block_alpha_beta: => correct output", {
  actual_null <- var_moment_block_alpha_beta(NULL)
  expect_null(actual_null)

  test_X <- test_input$X %>% expand_O_D_I_mem()
  actual <- var_moment_block_alpha_beta(test_X)
  expected <-
    test_X %>%
    vec_reference_O_D_I_mem() %>%
    col_sums()
  expect_equal(actual, expected, check.attributes = FALSE)
})

test_that("var_moment_block_alpha_gamma: => correct output", {
  actual_null <- var_moment_block_alpha_gamma(NULL)
  expect_null(actual_null)
})

test_that("var_moment_block_alpha_I_beta: => correct output", {

  actual_null <- var_moment_block_alpha_I_beta(NULL,NULL)
  expect_null(actual_null)

  test_X <- test_input$X %>% expand_O_D_I_mem()
  actual_null <- var_moment_block_alpha_I_beta(NULL, test_X)
  expect_null(actual_null)

  test_const_intra <- test_input$const_intra
  actual_null <- var_moment_block_alpha_I_beta(test_const_intra, NULL)
  expect_null(actual_null)

  actual <- var_moment_block_alpha_I_beta(test_const_intra, test_X)
  expected <- crossprod(
    x = test_const_intra %>% vec_reference_matrix_mem(),
    y = test_X %>% vec_reference_O_D_I_mem()
  )
  expect_equal(actual, expected, check.attributes = FALSE)
})

test_that("var_moment_block_alpha_I_gamma: => correct output", {

  actual_null <- var_moment_block_alpha_I_gamma(NULL,NULL)
  expect_null(actual_null)

  test_G <- test_input$G
  actual_null <- var_moment_block_alpha_I_gamma(NULL, test_G)
  expect_null(actual_null)

  test_const_intra <- test_input$const_intra
  actual_null <- var_moment_block_alpha_I_gamma(test_const_intra, NULL)
  expect_null(actual_null)

  # equaltiy of vector and matrix solutions
  actual <- var_moment_block_alpha_I_gamma(test_const_intra, test_G)
  expected <- crossprod(
    x = test_const_intra %>% vec_reference_matrix_mem(),
    y = test_G %>% vec_reference_matrix_mem()
  )
  expect_equal(actual, expected, check.attributes = FALSE)

})

test_that("var_moment_block_beta_gamma: => correct output", {

  actual_null <- var_moment_block_beta_gamma(NULL,NULL)
  expect_null(actual_null)

  test_X <- test_input$X %>% expand_O_D_I_mem()
  actual_null <- var_moment_block_beta_gamma(test_X, NULL)
  expect_null(actual_null)

  test_G <- test_input$G
  actual_null <- var_moment_block_beta_gamma(NULL, test_G)
  expect_null(actual_null)

  actual <- var_moment_block_beta_gamma(test_X,test_G)
  expected <- crossprod(
    x = test_X %>% vec_reference_O_D_I_mem(),
    y = test_G %>% vec_reference_matrix_mem()
  )
  expect_equal(actual, expected, check.attributes = FALSE)
})

# ---- covariance moments (blocks) --------------------------------------------

test_that("cov_moment_block_alpha: => correct output", {

  actual_null <- cov_moment_block_alpha(NULL)
  expect_null(actual_null)

  actual <- cov_moment_block_alpha(test_input$Y)
  expected <- sum(test_input$Y)
  expect_equal(actual, expected)
})

test_that("cov_moment_block_alpha_I: => correct output", {

  actual_null <- cov_moment_block_alpha_I(test_input$Y, NULL)
  expect_null(actual_null)

  actual <- cov_moment_block_alpha_I(test_input$Y, test_input$const_intra)
  expected <- test_input$const_intra %>%
    vec_reference_matrix_mem() %>%
    crossprod(as.vector(test_input$Y), .)
  expect_equal(actual, expected, check.attributes = FALSE)
})

test_that("cov_moment_block_beta: => correct output", {

  actual_null <- cov_moment_block_beta(test_input$Y, NULL)
  expect_null(actual_null)

  test_X <- test_input$X %>% expand_O_D_I_mem()
  actual <- cov_moment_block_beta(test_input$Y, test_X)
  expected <- crossprod(
    as.vector(test_input$Y),
    vec_reference_O_D_I_mem(test_X))
  expect_equal(actual, expected, check.attributes = FALSE)
})

test_that("cov_moment_block_gamma: => correct output", {

  actual_null <- cov_moment_block_gamma(test_input$Y, NULL)
  expect_null(actual_null)

  actual <- cov_moment_block_gamma(test_input$Y, test_input$G)
  expected <- crossprod(
    as.vector(test_input$Y),
    vec_reference_matrix_mem(test_input$G))
  expect_equal(actual, expected, check.attributes = FALSE)
})

# ---- full moment matrices (var + cov) -------------------------------------------------
test_model_matrices <- c(
  list("const" = 1),
  test_input["const_intra"],
  test_input$X %>% expand_O_D_I_mem(),
  test_input["G"],
  test_input["N"],
  list("Y" = test_input["Y"])
)

test_model_matrices_vec_ref <- cbind(
  test_model_matrices$const,
  test_model_matrices$const_intra %>% vec_reference_matrix_mem(),
  test_input$X %>% expand_O_D_I_mem() %>% vec_reference_O_D_I_mem(),
  test_model_matrices$G %>% vec_reference_matrix_mem())

test_that("moment_empirical_var: => correct output", {
  actual <- moment_empirical_var(test_model_matrices)
  expected <-
    crossprod(test_model_matrices_vec_ref) %>%
    Matrix::Matrix()
  expect_equal(actual,expected, check.attributes = FALSE)
})

test_that("moment_empirical_covar: => correct output", {

  actual <- moment_empirical_covar(test_input$Y, test_model_matrices)
  expected <- crossprod(as.vector(test_input$Y),
                        test_model_matrices_vec_ref)
  expect_equal(actual, expected, check.attributes = FALSE)
})
