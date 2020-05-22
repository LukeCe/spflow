# ---- SETUP ------------------------------------------------------------------
data("germany_net")

# create test input values and vector references to test against
test_object_names <- c("X","W","const_intra","N","G","Y")
test_input <- named_list(test_object_names)

test_input[["X"]] <- germany_net@node_data[,!"id", with = FALSE]
test_input[["W"]] <- germany_net@node_neighborhood
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

# ---- Variance Moments (Diag Blocks) -----------------------------------------
context("Variance Moment : Diag Blocks")

test_that("Diag Block - alpha => Correct output", {
  actual_null <- moments$var$alpha(NULL)
  expect_equal(object = actual_null, expected = NULL, )

  actual <- moments$var$alpha(10)
  expected <- matrix(10)
  expect_equal(actual, expected)
})

test_that("Diag Block - alpha_I => Correct output", {

  actual_null <- moments$var$alpha_I(NULL)
  expect_equal(actual_null, expected = NULL)

  test_W <- test_input$W
  test_const_intra <- test_input$const_intra
  actual_without_instruments <- moments$var$alpha_I(test_const_intra[1])
  expected <- matrix(nrow(test_W))
  expect_equal(actual_without_instruments,expected)

  actual_with_instruments <- moments$var$alpha_I(test_const_intra)
  expected <- hadamarad_sum_matrix(test_const_intra)
  expect_equal(actual_with_instruments,expected)
})

test_that("Diag Block - beta => Correct output", {

  actual_null <- moments$var$beta(NULL)
  expect_equal(actual_null, expected = NULL)

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

test_that("Diag Block - gamma => Correct output", {

  actual_null <- moments$var$gamma(NULL)
  expect_equal(actual_null, expected = NULL)

  test_G <- test_input$G
  actual <- moments$var$gamma(test_G)
  expected <- test_G %>% vec_reference_matrix_mem() %>% crossprod_mem()
  expect_equal(actual, expected, check.attributes = FALSE)
})

# ---- Variance Moments (Off-diag Blocks) -------------------------------------
context("Variance Moment : Off-diag Blocks")

test_that("Off-diag Block - alpha:alpha_I => Correct output", {
  actual_null <- moments$var$alpha_alpha_I(NULL)
  expect_equal(actual_null, expected = NULL)

  test_const_intra <- test_input$const_intra
  actual <- moments$var$alpha_alpha_I(test_const_intra)
  expected <- test_const_intra %>%
    vec_reference_matrix_mem() %>%
    colSums()
  expect_equal(actual, expected, check.attributes = FALSE)
})

test_that("Off-diag Block - alpha:beta => Correct output", {
  actual_null <- moments$var$alpha_beta(NULL)
  expect_equal(actual_null, expected = NULL)

  test_X <- test_input$X %>% expand_O_D_I_mem()
  actual <- moments$var$alpha_beta(test_X)
  expected <-
    test_X %>%
    vec_reference_O_D_I_mem() %>%
    col_sums()
  expect_equal(actual, expected, check.attributes = FALSE)
})

test_that("Off-diag Block - alpha:gamma => Correct output", {
  actual_null <- moments$var$alpha_gamma(NULL)
  expect_equal(actual_null, expected = NULL)
})

test_that("Off-diag Block - alpha_I:beta => Correct output", {

  actual_null <- moments$var$alpha_I_beta(NULL,NULL)
  expect_equal(actual_null, expected = NULL)

  test_X <- test_input$X %>% expand_O_D_I_mem()
  actual_null <- moments$var$alpha_I_beta(NULL, test_X)
  expect_equal(actual_null, expected = NULL)

  test_const_intra <- test_input$const_intra
  actual_null <- moments$var$alpha_I_beta(test_const_intra, NULL)
  expect_equal(actual_null, expected = NULL)

  actual <- moments$var$alpha_I_beta(test_const_intra, test_X)
  expected <- crossprod(
    x = test_const_intra %>% vec_reference_matrix_mem(),
    y = test_X %>% vec_reference_O_D_I_mem()
  )
  expect_equal(actual, expected, check.attributes = FALSE)
})

test_that("Off-diag Block - alpha_I:gamma => Correct output", {

  actual_null <- moments$var$alpha_I_gamma(NULL,NULL)
  expect_equal(actual_null, expected = NULL)

  test_G <- test_input$G
  actual_null <- moments$var$alpha_I_gamma(NULL, test_G)
  expect_equal(actual_null, expected = NULL)

  test_const_intra <- test_input$const_intra
  actual_null <- moments$var$alpha_I_gamma(test_const_intra, NULL)
  expect_equal(actual_null, expected = NULL)

  # equaltiy of vector and matrix solutions
  actual <- moments$var$alpha_I_gamma(test_const_intra, test_G)
  expected <- crossprod(
    x = test_const_intra %>% vec_reference_matrix_mem(),
    y = test_G %>% vec_reference_matrix_mem()
  )
  expect_equal(actual, expected, check.attributes = FALSE)

})

test_that("Off-diag Block - beta:gamma => Correct output", {

  actual_null <- moments$var$beta_gamma(NULL,NULL)
  expect_equal(actual_null, expected = NULL)

  test_X <- test_input$X %>% expand_O_D_I_mem()
  actual_null <- moments$var$beta_gamma(test_X, NULL)
  expect_equal(actual_null, expected = NULL)

  test_G <- test_input$G
  actual_null <- moments$var$beta_gamma(NULL, test_G)
  expect_equal(actual_null, expected = NULL)

  actual <- moments$var$beta_gamma(test_X,test_G)
  expected <- crossprod(
    x = test_X %>% vec_reference_O_D_I_mem(),
    y = test_G %>% vec_reference_matrix_mem()
  )
  expect_equal(actual, expected, check.attributes = FALSE)
})
# ---- Variance Moment (Full) -------------------------------------------------
context("Variance Moment : Complete")

test_that("Variance Moment => Correct Output", {
  test_model_matrices <- c(
    list("const" = 1),
    test_input["const_intra"],
    list("X" = test_input$X %>% expand_O_D_I_mem()),
    test_input["G"],
    test_input["N"]
  )

  actual <- moments$empirical_var(test_model_matrices)
  expected <- cbind(
    test_model_matrices$const,
    test_model_matrices$const_intra %>% vec_reference_matrix_mem(),
    test_input$X %>% expand_O_D_I_mem() %>% vec_reference_O_D_I_mem(),
    test_model_matrices$G %>% vec_reference_matrix_mem()) %>%
    crossprod() %>%
    Matrix::Matrix()
  expect_equal(actual,expected, check.attributes = FALSE)
})

# ---- Covariance Moments (Blocks) --------------------------------------------
context("Covariance Moment Blocks")

test_that("Cov Block - alpha", {
  expect_error(object = moments$cov$alpha())

  test_Y <- as.matrix(cars)
  expect_equal(object = moments$cov$alpha(test_Y),
               expected = sum(cars))
})

test_that("Cov Block - alpha_I", {

  W <- neighborhood(germany_net)
  n_obs <- nrow(W)
  test_Y <- rnorm(n_obs^2) %>% matrix(n_obs,n_obs)

  expect_equal(object = moments$cov$alpha_I(test_Y, NULL),
               expected = NULL)


  test_const_intra <- intra_regional_constant(W,use_instruments = TRUE)
  test_const_intra_vec <- lapply(test_const_intra, as.vector) %>% reduce(cbind)

  expect_equivalent(
    object = moments$cov$alpha_I(Y = test_Y,const_intra = test_const_intra),
    expected = crossprod(as.vector(test_Y),test_const_intra_vec))
})

test_that("Cov Block - beta", {
  n_obs <- count(germany_net)
  test_Y <- rnorm(n_obs^2) %>% matrix(n_obs,n_obs)

  expect_equal(object = moments$cov$beta(test_Y, NULL),
               expected = NULL)

  # equality of vector and matrix approach
  # X
  X_germany <- dat(germany_net)[,!"id", with = FALSE]
  eg_X <- named_list(c("OX", "DX", "IX"),X_germany) %>% lapply(as.matrix)
  # equaltiy of vector and matrix solutions
  eg_X_vec <- cbind(eg_X$DX %x% rep(1,nrow(eg_X$OX)),
                    rep(1,nrow(eg_X$DX)) %x% eg_X$DX)
  n_obs <- nrow(eg_X$IX)
  eg_X_vec <- cbind(eg_X_vec, eg_X_vec[,1:2] * as.vector(diag(n_obs)))

  expect_equivalent(object = moments$cov$beta(test_Y,eg_X),
                    expected = crossprod(as.vector(test_Y),eg_X_vec))
})

test_that("Cov Block - gamma", {
  n_obs <- count(germany_net)
  test_Y <- rnorm(n_obs^2,n_obs,n_obs)

  expect_equal(object = moments$cov$gamma(test_Y, NULL),
               expected = NULL)

  # equality of vector and matrix approach
  eg_G <- replicate(3, test_Y + rnorm(1),simplify = FALSE)
  eg_G_vec <- lapply(eg_G, as.vector) %>% reduce(cbind)

  expect_equivalent(object = moments$cov$gamma(test_Y,eg_G) %>% as.matrix(),
                    expected = crossprod(as.vector(test_Y), eg_G_vec))

})

# ---- Covariance Moment (Full) -----------------------------------------------
context("Covariance Moment : Complete")
