# ---- Variance Moments (Diag) ------------------------------------------------
context("Variance Moment Block")
data("germany_net")

test_that("Diag Block - alpha", {
  expect_equal(object = moments$var$alpha(10),
               expected = 10)
  expect_equal(object = moments$var$alpha(NULL),
               expected = NULL, )
})

test_that("Diag Block - alpha_I", {
  expect_equal(object = moments$var$alpha_I(NULL),
               expected = NULL)

  W <- neighborhood(germany_net)
  expect_equal(object =
                 intra_regional_constant(W,use_instruments = FALSE) %>%
                 moments$var$alpha_I(.),
               expected = matrix(nrow(W)))

  example_intra_const <- intra_regional_constant(W,use_instruments = TRUE)
  example_alpha_I <- moments$var$alpha_I(example_intra_const)
  expect_equal(object = example_alpha_I,
               expected = example_intra_const %>% hadamarad_sum_matrix())
})

test_that("Diag Block - beta", {

  expect_equal(object = moments$var$beta(NULL),
               expected = NULL)

  # equality of vector and matrix approach
  eg_X <- list("OX" = cars, "DX" = cars) %>% lapply(as.matrix)
  eg_X_vec <- cbind(eg_X$DX %x% rep(1,nrow(eg_X$OX)),
                    rep(1,nrow(eg_X$DX)) %x% eg_X$DX)

  expect_equivalent(object = moments$var$beta(eg_X) %>% as.matrix(),
                    expected = crossprod(eg_X_vec))

  # include intra
  eg_IX <- c(eg_X,list("IX" = as.matrix(cars)))
  n_obs <- nrow(eg_IX$IX)

  eg_IX_vec <- cbind(eg_X_vec,
                     eg_X_vec[,1:2] * as.vector(diag(n_obs)))
  expect_equivalent(object = moments$var$beta(eg_IX) %>% as.matrix(),
                    expected = crossprod(eg_IX_vec))
})

test_that("Diag Block - gamma", {

  expect_equal(object = moments$var$gamma(NULL),
               expected = NULL)

  # equality of vector and matrix approach
  eg_G <- replicate(3, matrix(rnorm(100),10,10),simplify = FALSE)
  eg_G_vec <- lapply(eg_G, as.vector) %>% reduce(cbind)

  expect_equivalent(object = moments$var$gamma(eg_G) %>% as.matrix(),
                    expected = crossprod(eg_G_vec))

})

# ---- Variance Moments (Off-diag) --------------------------------------------
test_that("Off-diag Block - alpha:alpha_I", {
  expect_equal(object = moments$var$alpha_alpha_I(NULL),
               expected = NULL)

  test_list <- list(1,2,3) %>% lapply(rep,5)
  expect_equal(object = moments$var$alpha_alpha_I(test_list),
               expected = c(1,2,3)*5)
})

test_that("Off-diag Block - alpha:beta", {

  expect_equal(object = moments$var$alpha_beta(NULL),
               expected = NULL)

  eg_X <- list("OX" = cars, "DX" = cars, "IX" = cars) %>% lapply(as.matrix)
  eg_X_vec <- cbind(eg_X$DX %x% rep(1,nrow(eg_X$OX)),
                    rep(1,nrow(eg_X$DX)) %x% eg_X$DX)

  n_obs <- nrow(eg_X$IX)
  eg_X_vec <- cbind(eg_X_vec, eg_X_vec[,1:2] * as.vector(diag(n_obs)))

  expect_equivalent(object = moments$var$alpha_beta(eg_X),
                    expected = colSums(eg_X_vec))

})

test_that("Off-diag Block - alpha:gamma", {
  expect_equal(object = moments$var$alpha_gamma(NULL),
               expected = NULL)

})

test_that("Off-diag Block - alpha_I:beta", {

  # X
  X_germany <- dat(germany_net)[,!"id", with = FALSE]
  eg_X <- named_list(c("OX", "DX", "IX"),X_germany) %>% lapply(as.matrix)

  # I
  W <- neighborhood(germany_net)
  eg_const_I <- intra_regional_constant(W,use_instruments = FALSE)

  expect_equal(
    object = moments$var$alpha_I_beta(X = NULL, const_intra = eg_const_I),
    expected = NULL)

  expect_equal(
    object = moments$var$alpha_I_beta(X = eg_X, const_intra = NULL),
    expected = NULL)

  # equality of vector and matrix moment
  # without instruments
  eg_X_vec <- cbind(eg_X$DX %x% rep(1,nrow(eg_X$OX)),
                    rep(1,nrow(eg_X$DX)) %x% eg_X$DX)
  n_obs <- nrow(eg_X$IX)
  eg_X_vec <- cbind(eg_X_vec, eg_X_vec[,1:2] * as.vector(diag(n_obs)))
  eg_const_vec <-
    eg_const_I %>%
    lapply(as.vector) %>%
    reduce(cbind)

  expect_equivalent(
    object = moments$var$alpha_I_beta(X = eg_X, const_intra =  eg_const_I),
    expected = crossprod(eg_const_vec, eg_X_vec))

  # with instruments
  eg_const_I <- intra_regional_constant(W,use_instruments = TRUE)
  eg_const_vec <- eg_const_I %>% lapply(as.vector) %>% reduce(cbind)

  expect_equivalent(
    object = moments$var$alpha_I_beta(X = eg_X, const_intra =  eg_const_I),
    expected = crossprod(eg_const_vec, eg_X_vec))

})

test_that("Off-diag Block - alpha_I:gamma", {
  W <- neighborhood(germany_net)
  eg_const_I <- intra_regional_constant(W,use_instruments = TRUE)

  n_obs <- nrow(W)
  eg_G <- replicate(3, matrix(rnorm(n_obs^2),nrow = n_obs,ncol = n_obs),
                    simplify = FALSE)

  expect_equal(
    object = moments$var$alpha_I_gamma(const_intra = NULL,G = eg_G),
    expected = NULL)

  expect_equal(
    object = moments$var$alpha_I_gamma(const_intra = NULL,G = eg_G),
    expected = NULL)

  # equaltiy of vector and matrix solutions
  eg_G_vec <- lapply(eg_G, as.vector) %>% reduce(cbind)
  eg_const_I_vec <- lapply(eg_const_I, as.vector) %>% reduce(cbind)

  expect_equivalent(
    object = {
      moments$var$alpha_I_gamma(const_intra = eg_const_I, G = eg_G) %>%
        as.matrix()},
    expected = crossprod(eg_const_I_vec,eg_G_vec))

})

test_that("Off-diag Block - beta:gamma", {
  # X
  X_germany <- dat(germany_net)[,!"id", with = FALSE]
  eg_X <- named_list(c("OX", "DX", "IX"),X_germany) %>% lapply(as.matrix)
  # G
  n_obs <- nrow(X_germany)
  eg_G <- replicate(3, matrix(rnorm(n_obs^2),nrow = n_obs,ncol = n_obs),
                    simplify = FALSE)

  expect_equal(
    object = moments$var$beta_gamma(X = NULL,G = eg_G),
    expected = NULL)

  expect_equal(
    object = moments$var$beta_gamma(X = eg_X,G = NULL),
    expected = NULL)

  # equaltiy of vector and matrix solutions
  eg_X_vec <- cbind(eg_X$DX %x% rep(1,nrow(eg_X$OX)),
                    rep(1,nrow(eg_X$DX)) %x% eg_X$DX)
  n_obs <- nrow(eg_X$IX)
  eg_X_vec <- cbind(eg_X_vec, eg_X_vec[,1:2] * as.vector(diag(n_obs)))

  eg_G_vec <- lapply(eg_G, as.vector) %>% reduce(cbind)

  expect_equivalent(
    object = moments$var$beta_gamma(X = eg_X,G = eg_G),
    expected = crossprod(eg_X_vec,eg_G_vec))

})

# ---- Covariance Moments -----------------------------------------------------
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


  test_intra_const <- intra_regional_constant(W,use_instruments = TRUE)
  test_intra_const_vec <- lapply(test_intra_const, as.vector) %>% reduce(cbind)

  expect_equivalent(
    object = moments$cov$alpha_I(Y = test_Y,const_intra = test_intra_const),
    expected = crossprod(as.vector(test_Y),test_intra_const_vec))
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
