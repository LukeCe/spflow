# ---- derive_log_det_calculator ----------------------------------------------
expect_equal({
  W_ge <- germany_net@node_neighborhood
  W_ge_traces <- spflow:::trace_sequence(W_ge,max_power = 5)
  log_det_fun <- spflow:::derive_log_det_calculator(
    W_ge_traces, W_ge_traces, 16, 16, "model_9")
  log_det_fun(c(.5,.4,-.1))
  },
  {
    W_ge <- germany_net@node_neighborhood
    In <- diag(16)
    W_F_ge <- .5 * (In %x% W_ge) + .4 * (W_ge %x% In) + -.1 * (W_ge %x% W_ge)
    -sum(spflow:::trace_sequence(W_F_ge,max_power = 5)/seq(5))
  },
  info = "model_9")

expect_equal({
  W_ge <- germany_net@node_neighborhood
  W_ge_traces <- spflow:::trace_sequence(W_ge,max_power = 5)
  log_det_fun <- spflow:::derive_log_det_calculator(
    W_ge_traces, W_ge_traces, 16, 16, "model_8")
  log_det_fun(c(.4,.6))
  },
  {
    W_ge <- germany_net@node_neighborhood
    In <- diag(16)
    # There is no equality for the successive filtering...
    # W_F_ge <- .4 * (In %x% W_ge) + .6 * (W_ge %x% In) - .24 * (W_ge %x% W_ge)
    # -sum(spflow:::trace_sequence(W_F_ge,max_power = 5)/seq(5))

    -sum(spflow:::trace_sequence(.4 * (In %x% W_ge),max_power = 5)/seq(5) +
           spflow:::trace_sequence(.6 * (W_ge %x% In),max_power = 5)/seq(5))
  },
  info = "model_8")

expect_equal({
  W_ge_traces <- spflow:::trace_sequence(W_ge,max_power = 5)
  log_det_fun <- spflow:::derive_log_det_calculator(
    W_ge_traces, W_ge_traces, 16, 16, "model_7")
  log_det_fun(c(.4,.6))},
  {
    W_ge <- germany_net@node_neighborhood
    In <- diag(16)
    W_F_ge <- .4 * (In %x% W_ge) + .6 * (W_ge %x% In)
    -sum(spflow:::trace_sequence(W_F_ge,max_power = 5)/seq(5))
  },
  info = "model_7")

expect_equal({
  W_ge_traces <- spflow:::trace_sequence(W_ge,max_power = 5)
  log_det_fun <- spflow:::derive_log_det_calculator(
    W_ge_traces, W_ge_traces, 16, 16, "model_5")
  log_det_fun(c(.4))},
  {
    W_ge <- germany_net@node_neighborhood
    In <- diag(16)
    W_F_ge <- .2 * (In %x% W_ge) + .2 * (W_ge %x% In)
    -sum(spflow:::trace_sequence(W_F_ge,max_power = 5)/seq(5))
  },
  info = "model_5")

expect_equal({
  W_ge_traces <- spflow:::trace_sequence(W_ge,max_power = 5)
  log_det_fun <- spflow:::derive_log_det_calculator(
    W_ge_traces, W_ge_traces, 16, 16, "model_6")
  log_det_fun(c(.6))},
  {
    W_ge <- germany_net@node_neighborhood
    In <- diag(16)
    W_F_ge <- .2 * (In %x% W_ge) + .2 * (W_ge %x% In) + .2 * (W_ge %x% W_ge)
    -sum(spflow:::trace_sequence(W_F_ge,max_power = 5)/seq(5))
  },
  info = "model_6")

expect_equal({
  W_ge_traces <- spflow:::trace_sequence(W_ge,max_power = 5)
  log_det_fun <- spflow:::derive_log_det_calculator(
    W_ge_traces, W_ge_traces, 16, 16, "model_4")
  log_det_fun(c(.5))},
  {
    W_ge <- germany_net@node_neighborhood
    In <- diag(16)
    W_F_ge <- .5 * (W_ge %x% W_ge)
    -sum(spflow:::trace_sequence(W_F_ge,max_power = 5)/seq(5))
  },
  info = "model_4")

expect_equal({
  W_ge_traces <- spflow:::trace_sequence(W_ge,max_power = 5)
  log_det_fun <- spflow:::derive_log_det_calculator(
    W_ge_traces, W_ge_traces, 16, 16, "model_3")
  log_det_fun(c(.5))},
  {
    W_ge <- germany_net@node_neighborhood
    In <- diag(16)
    W_F_ge <- .5 * (W_ge %x% In)
    -sum(spflow:::trace_sequence(W_F_ge,max_power = 5)/seq(5))
  },
  info = "model_3")

expect_equal({
  W_ge_traces <- spflow:::trace_sequence(W_ge,max_power = 5)
  log_det_fun <- spflow:::derive_log_det_calculator(
    W_ge_traces, W_ge_traces, 16, 16, "model_2")
  log_det_fun(c(.5))},
  {
    W_ge <- germany_net@node_neighborhood
    In <- diag(16)
    W_F_ge <- .5 * (In %x% W_ge)
    -sum(spflow:::trace_sequence(W_F_ge,max_power = 5)/seq(5))
  },
  info = "model_2")
