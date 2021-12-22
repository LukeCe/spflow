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
    Y_indicator = NULL)
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
    Y_indicator = NULL)
},
{
  # at each iteration we multiply the previous result by 2 and in the end
  # everything is summed
  power_sum <- sum(2^(seq(0,10)))
  diag(power_sum,2,2)
},
info = "test expectation approximation (sparse case)")
