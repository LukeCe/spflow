# spflow_model_matrix ---------------------------------------------------------
context("Test spflow_model_matrix")

example_matrices <- spflow_model_matrix(
  sp_multi_network = multi_net_examples,
  network_pair_id = "ge_ge",
  flow_formula = y9 ~ .,
  flow_control = spflow_control())

test_that("Output has the correct structure", {
  expect_length(example_matrices,99)
})
