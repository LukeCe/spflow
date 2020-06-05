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
context("variance moment : diagonal blocks")

test_that("alpha: => correct output", {
  actual_null <- moments$var$alpha(NULL)
  expect_equal(object = actual_null, expected = NULL)

  actual <- moments$var$alpha(10)
  expected <- matrix(10)
  expect_equal(actual, expected)
})

test_that("alpha_I: => correct output", {

  actual_null <- moments$var$alpha_I(NULL)
  expect_null(actual_null)

  test_W <- test_input$W
  test_const_intra <- test_input$const_intra
  actual_without_instruments <- moments$var$alpha_I(test_const_intra[1])
  expected <- matrix(nrow(test_W))
  expect_equal(actual_without_instruments,expected)

  actual_with_instruments <- moments$var$alpha_I(test_const_intra)
  expected <- hadamarad_sum_matrix(test_const_intra)
  expect_equal(actual_with_instruments,expected)
})

test_that("beta: => correct output", {

  actual_null <- moments$var$beta(NULL)
  expect_null(actual_null)

  test_X <- test_input$X %>% expand_O_D_I_mem()
  actual <- moments$var$beta(test_X)
  expected <- test_X %>% vec_reference_O_D_I_mem() %>% crossprod_mem()
  expect_equal(actual,expected, check.attributes = FALSE)

  # drop intra
  test_X$IX <- NULL
  actual <- moments$var$beta(test_X)
  expected <- test_X %>% vec_reference_O_D_I_mem() %>% crossprod_mem()
  expect_equal(actual,expected, check.attributes = FALSE)
})

test_that("gamma: => correct output", {

  actual_null <- moments$var$gamma(NULL)
  expect_null(actual_null)

  test_G <- test_input$G
  actual <- moments$var$gamma(test_G)
  expected <- test_G %>% vec_reference_matrix_mem() %>% crossprod_mem()
  expect_equal(actual, expected, check.attributes = FALSE)
})

# ---- variance moments (off-diag blocks) -------------------------------------
context("variance moment : off-diagonal blocks")

test_that("alpha_alpha_I: => correct output", {
  actual_null <- moments$var$alpha_alpha_I(NULL)
  expect_null(actual_null)

  test_const_intra <- test_input$const_intra
  actual <- moments$var$alpha_alpha_I(test_const_intra)
  expected <- test_const_intra %>%
    vec_reference_matrix_mem() %>%
    colSums()
  expect_equal(actual, expected, check.attributes = FALSE)
})

test_that("alpha_beta: => correct output", {
  actual_null <- moments$var$alpha_beta(NULL)
  expect_null(actual_null)

  test_X <- test_input$X %>% expand_O_D_I_mem()
  actual <- moments$var$alpha_beta(test_X)
  expected <-
    test_X %>%
    vec_reference_O_D_I_mem() %>%
    col_sums()
  expect_equal(actual, expected, check.attributes = FALSE)
})

test_that("alpha_gamma: => correct output", {
  actual_null <- moments$var$alpha_gamma(NULL)
  expect_null(actual_null)
})

test_that("alpha_I_beta: => correct output", {

  actual_null <- moments$var$alpha_I_beta(NULL,NULL)
  expect_null(actual_null)

  test_X <- test_input$X %>% expand_O_D_I_mem()
  actual_null <- moments$var$alpha_I_beta(NULL, test_X)
  expect_null(actual_null)

  test_const_intra <- test_input$const_intra
  actual_null <- moments$var$alpha_I_beta(test_const_intra, NULL)
  expect_null(actual_null)

  actual <- moments$var$alpha_I_beta(test_const_intra, test_X)
  expected <- crossprod(
    x = test_const_intra %>% vec_reference_matrix_mem(),
    y = test_X %>% vec_reference_O_D_I_mem()
  )
  expect_equal(actual, expected, check.attributes = FALSE)
})

test_that("alpha_I_gamma: => correct output", {

  actual_null <- moments$var$alpha_I_gamma(NULL,NULL)
  expect_null(actual_null)

  test_G <- test_input$G
  actual_null <- moments$var$alpha_I_gamma(NULL, test_G)
  expect_null(actual_null)

  test_const_intra <- test_input$const_intra
  actual_null <- moments$var$alpha_I_gamma(test_const_intra, NULL)
  expect_null(actual_null)

  # equaltiy of vector and matrix solutions
  actual <- moments$var$alpha_I_gamma(test_const_intra, test_G)
  expected <- crossprod(
    x = test_const_intra %>% vec_reference_matrix_mem(),
    y = test_G %>% vec_reference_matrix_mem()
  )
  expect_equal(actual, expected, check.attributes = FALSE)

})

test_that("beta_gamma: => correct output", {

  actual_null <- moments$var$beta_gamma(NULL,NULL)
  expect_null(actual_null)

  test_X <- test_input$X %>% expand_O_D_I_mem()
  actual_null <- moments$var$beta_gamma(test_X, NULL)
  expect_null(actual_null)

  test_G <- test_input$G
  actual_null <- moments$var$beta_gamma(NULL, test_G)
  expect_null(actual_null)

  actual <- moments$var$beta_gamma(test_X,test_G)
  expected <- crossprod(
    x = test_X %>% vec_reference_O_D_I_mem(),
    y = test_G %>% vec_reference_matrix_mem()
  )
  expect_equal(actual, expected, check.attributes = FALSE)
})

# ---- covariance moments (blocks) --------------------------------------------
context("covariance moment - blocks")

test_that("alpha: => correct output", {

  actual_null <- moments$cov$alpha(NULL)
  expect_null(actual_null)

  actual <- moments$cov$alpha(test_input$Y)
  expected <- sum(test_input$Y)
  expect_equal(actual, expected)
})

test_that("alpha_I: => correct output", {

  actual_null <- moments$cov$alpha_I(test_input$Y, NULL)
  expect_null(actual_null)

  actual <- moments$cov$alpha_I(test_input$Y, test_input$const_intra)
  expected <- test_input$const_intra %>%
    vec_reference_matrix_mem() %>%
    crossprod(as.vector(test_input$Y), .)
  expect_equal(actual, expected, check.attributes = FALSE)
})

test_that("beta: => correct output", {

  actual_null <- moments$cov$beta(test_input$Y, NULL)
  expect_null(actual_null)

  test_X <- test_input$X %>% expand_O_D_I_mem()
  actual <- moments$cov$beta(test_input$Y, test_X)
  expected <- crossprod(
    as.vector(test_input$Y),
    vec_reference_O_D_I_mem(test_X))
  expect_equal(actual, expected, check.attributes = FALSE)
})

test_that("gamma: => correct output", {

  actual_null <- moments$cov$gamma(test_input$Y, NULL)
  expect_null(actual_null)

  actual <- moments$cov$gamma(test_input$Y, test_input$G)
  expected <- crossprod(
    as.vector(test_input$Y),
    vec_reference_matrix_mem(test_input$G))
  expect_equal(actual, expected, check.attributes = FALSE)
})

# ---- full moment matrices (var + cov) -------------------------------------------------
context("complete moment matrices")
test_model_matrices <- c(
  list("const" = 1),
  test_input["const_intra"],
  list("X" = test_input$X %>% expand_O_D_I_mem()),
  test_input["G"],
  test_input["N"]
)

test_model_matrices_vec_ref <- cbind(
  test_model_matrices$const,
  test_model_matrices$const_intra %>% vec_reference_matrix_mem(),
  test_input$X %>% expand_O_D_I_mem() %>% vec_reference_O_D_I_mem(),
  test_model_matrices$G %>% vec_reference_matrix_mem())

test_that("empirical_var: => correct output", {
  actual <- moments$empirical_var(test_model_matrices)
  expected <-
    crossprod(test_model_matrices_vec_ref) %>%
    Matrix::Matrix()
  expect_equal(actual,expected, check.attributes = FALSE)
})

test_that("emprical_covar: => correct output", {

  actual <- moments$empirical_covar(test_input$Y, test_model_matrices)
  expected <- crossprod(as.vector(test_input$Y),
                        test_model_matrices_vec_ref)
  expect_equal(actual, expected, check.attributes = FALSE)
})
