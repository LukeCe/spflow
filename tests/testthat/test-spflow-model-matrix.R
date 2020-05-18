# spflow_model_matrix ---------------------------------------------------------
context("Test spflow_model_matrix")

example_matrices <- spflow_model_matrix(
  sp_multi_network = multi_net_usa_ge,
  network_pair_id = "ge_ge",
  flow_formula = y9 ~ G_(log(distance + 1)) + X,
  flow_control = spflow_control())

test_that("Output has the correct structure", {

  required_matrices <- define_matrix_keys() %>% as.vector()

  expect_true(all(names(example_matrices) %in% required_matrices))

})

test_that("Output elements are correct", {

  W_ge <- neighborhoods(multi_net_usa_ge,"ge")$ge

  # check the weight matrices
  expect_equivalent(object = example_matrices$OW, expected = W_ge)
  expect_equivalent(object = example_matrices$DW, expected = W_ge)


  XX <- dat(multi_net_usa_ge,"ge")[,2:3] %>% as.matrix()
  XX <- cbind(XX, W_ge %*% W_ge %*% XX)
  # check the attributes of [origin], [destination], [intra]
  expect_equivalent(object = example_matrices$DX, expected = XX)
  expect_equivalent(object = example_matrices$OX, expected = XX)
  expect_equivalent(object = example_matrices$IX, expected = XX)

  # check the pair attributes
  n_ge <- 16
  pair_data <- dat(multi_net_usa_ge, network_pair_id = "ge_ge")
  dist <- log(pair_data$distance + 1) %>% Matrix(nrow = n_ge)
  dist.lag1 <- W_ge %*% tcrossprod(dist,W_ge)
  dist.lag2 <- W_ge %*% tcrossprod(dist.lag1,W_ge)

  expect_equivalent(object = example_matrices$G$`log(distance + 1)`,
                    expected = dist)
  expect_equivalent(object = example_matrices$G$`log(distance + 1).lag1`,
                    expected = dist.lag1)
  expect_equivalent(object = example_matrices$G$`log(distance + 1).lag2`,
                    expected = dist.lag2)

  # check the flows
  flows <- pair_data$y9 %>% Matrix(nrow = n_ge)
  flows.o <- W_ge %*% flows
  flows.d <- tcrossprod(flows,W_ge)
  flows.w <- W_ge %*% flows.d

  expect_equivalent(object = example_matrices$Y$y9,
                    expected = flows)
  expect_equivalent(object = example_matrices$Y$y9.o,
                    expected = flows.o)
  expect_equivalent(object = example_matrices$Y$y9.d,
                    expected = flows.d)
  expect_equivalent(object = example_matrices$Y$y9.w,
                    expected = flows.w)

  # check the constant
  expect_true(example_matrices$const == 1)
  expect_equivalent(object = example_matrices$const_intra,
                    expected = intra_regional_constant(W_ge, TRUE))

})
