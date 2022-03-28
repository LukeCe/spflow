# ---- compute_signal ---------------------------------------------------------
expect_equal({
  n_o <- 5
  n_d <- 5
  res_all <- matrix(4,n_d,n_o)
  res_dg <- diag(2, n_d,n_o)
  res_all + res_dg

  }, {
  n_o <- 5
  n_d <- 5
  params <- c("const", "const_intra", "D_", "O_", "I_", "G")
  params <- lookup(1, params)
  mats <- list(
    "Y_" = list(matrix(1,n_d,n_o)),
    "const" = 1,
    "const_intra" = list("In" = diag(1, n_d,n_o)),
    "D_" = matrix(1,nrow = n_d),
    "O_" = matrix(1,nrow = n_o),
    "I_" = matrix(1,nrow = n_d),
    "G_" = list(matrix(1,n_d,n_o)))
  as.matrix(compute_signal(mats,delta = params))
}, check.attributes = FALSE)




# ---- compute_expectation ----------------------------------------------------
expect_equal({

  # test the iterative formula based on powers of 2
  signal <- matrix(1,2,2)
  W <- diag(1,2,2)
  spflow:::compute_expectation(
    signal,
    model = "model_2",
    DW =  W, OW = W,
    rho = -1,
    max_it = 10,
    flow_indicator = NULL)
},
{
  # at each iteration we multiply the previous result by 2 and in the end
  # everything is summed
  power_sum <- sum(2^(seq(0,10)))
  matrix(power_sum,2,2)
},
info = "test expectation approximation (cartesian case)")


expect_equal({

  # test the iterative formula based on powers of 2
  signal <- diag(1,2,2)
  W <- diag(1,2,2)
  spflow:::compute_expectation(
    signal,
    model = "model_2",
    DW =  W, OW = W,
    rho = -1,
    max_it = 10,
    flow_indicator = NULL)
},
{
  # at each iteration we multiply the previous result by 2 and in the end
  # everything is summed
  power_sum <- sum(2^(seq(0,10)))
  diag(power_sum,2,2)
},
info = "test expectation approximation (sparse case)")
