# ---- tracevals2params_c -----------------------------------------------------
expect_equal({
  tvals <- tracevals2params_c(
    OW = diag(c(2,2)),
    DW = diag(c(3,3)),
    n_o = 2,
    n_d = 2,
    model = "model_9",
    approx_order = 2)
  tvals1 <- tvals[order(tvals$rho_d,tvals$rho_o,tvals$rho_w),]
  row.names(tvals1) <- NULL
  tvals1
  },
  {
    DW_trace <- sum(c(3,3))
    DW_trace2 <- sum(c(3,3)^2)
    OW_trace <- sum(c(2,2))
    OW_trace2 <- sum(c(2,2)^2)
    .t <- .5 # divide by order of powers
    .c <- 2  # multiply by multinomial coefficient
    n_o <- n_d <- 2
    tvals <- data.frame(Reduce("rbind", list(
      "o" = c(OW_trace * n_d, 0, 1, 0),
      "d" = c(DW_trace * n_o, 1, 0, 0),
      "w" = c(OW_trace * DW_trace, 0, 0, 1),
      "oo" = c(.5 * OW_trace2 * n_d, 0, 2, 0),
      "dd" = c(.5 * DW_trace2 * n_o, 2, 0, 0),
      "ww" = c(.5 * DW_trace2 * OW_trace2, 0, 0, 2),
      "od" = c(.5 * .c * OW_trace * DW_trace, 1, 1, 0),
      "ow" = c(.5 * .c * OW_trace2 * DW_trace, 0, 1, 1),
      "dw" = c(.5 * .c * OW_trace * DW_trace2, 1, 0, 1))),row.names = NULL)
    p_names <- paste0("rho_", c("d","o","w"))
    colnames(tvals) <- c("TRACE_VAL", p_names)
    tvals2 <- tvals[order(tvals$rho_d,tvals$rho_o,tvals$rho_w),]
    row.names(tvals2) <- NULL
    tvals2
  }, info = "Parameter powers and trace values for model_9 (cartesian).")

expect_equal({
  tvals <- tracevals2params_c(
    OW = diag(c(2,2)),
    DW = diag(c(3,3)),
    n_o = 2,
    n_d = 2,
    model = "model_7",
    approx_order = 2)
  tvals1 <- tvals[order(tvals$rho_d,tvals$rho_o),]
  row.names(tvals1) <- NULL
  tvals1
},
{
  DW_trace <- sum(c(3,3))
  DW_trace2 <- sum(c(3,3)^2)
  OW_trace <- sum(c(2,2))
  OW_trace2 <- sum(c(2,2)^2)
  .t <- .5 # divide by order of powers
  .c <- 2  # multiply by multinomial coefficient
  n_o <- n_d <- 2
  tvals <- data.frame(Reduce("rbind", list(
    "o" = c(OW_trace * n_d, 0, 1),
    "d" = c(DW_trace * n_o, 1, 0),
    "oo" = .5 * c(OW_trace2 * n_d, 0, 4),
    "dd" = .5 * c(DW_trace2 * n_o, 4, 0),
    "od" = .5 * .c * c(OW_trace * DW_trace, 1, 1))),row.names = NULL)
  p_names <- paste0("rho_", c("d","o"))
  colnames(tvals) <- c("TRACE_VAL", p_names)
  tvals2 <- tvals[order(tvals$rho_d,tvals$rho_o),]
  row.names(tvals2) <- NULL
  tvals2
}, info = "Parameter powers and trace values for model_7 (cartesian).")

expect_equal({
  tvals <- tracevals2params_c(
    OW = diag(c(2,2)),
    DW = diag(c(3,3)),
    n_o = 2,
    n_d = 2,
    model = "model_6",
    approx_order = 2)
  tvals1 <- tvals[order(tvals$rho_odw),]
  row.names(tvals1) <- NULL
  tvals1
},
{
  DW_trace <- sum(c(3,3))
  DW_trace2 <- sum(c(3,3)^2)
  OW_trace <- sum(c(2,2))
  OW_trace2 <- sum(c(2,2)^2)
  .t <- .5 # divide by order of powers
  .c <- 2  # multiply by multinomial coefficient
  n_o <- n_d <- 2
  tvals <- data.frame(Reduce("rbind", list(
    "x" = c(OW_trace * n_d
            + DW_trace * n_o
            + OW_trace * DW_trace, 3)/3,
    "xx" = c(.t * OW_trace2 * n_d
             + .t * DW_trace2 * n_o
             + .t * DW_trace2 * OW_trace2
             + .t * .c * OW_trace * DW_trace
             + .t * .c * OW_trace2 * DW_trace
             + .t * .c * OW_trace * DW_trace2, 18)/9)),row.names = NULL)
  p_names <- paste0("rho_", c("odw"))
  colnames(tvals) <- c("TRACE_VAL", p_names)
  tvals2 <- tvals[order(tvals$rho_odw),]
  row.names(tvals2) <- NULL
  tvals2
}, info = "Parameter powers and trace values for model_6 (cartesian).")

expect_equal({
  tvals <- tracevals2params_c(
    OW = diag(c(2,2)),
    DW = diag(c(3,3)),
    n_o = 2,
    n_d = 2,
    model = "model_5",
    approx_order = 2)
  tvals1 <- tvals[order(tvals$rho_od),]
  row.names(tvals1) <- NULL
  tvals1
},
{
  DW_trace <- sum(c(3,3))
  DW_trace2 <- sum(c(3,3)^2)
  OW_trace <- sum(c(2,2))
  OW_trace2 <- sum(c(2,2)^2)
  .t <- .5 # divide by order of powers
  .c <- 2  # multiply by multinomial coefficient
  n_o <- n_d <- 2
  tvals <- data.frame(Reduce("rbind", list(
    "x" = c(OW_trace * n_d
            + DW_trace * n_o,2)/2,
    "xx" = c(.t * OW_trace2 * n_d
             + .t * DW_trace2 * n_o
             + .t * .c * OW_trace * DW_trace, 8)/4)),row.names = NULL)
  p_names <- paste0("rho_", c("od"))
  colnames(tvals) <- c("TRACE_VAL", p_names)
  tvals2 <- tvals[order(tvals$rho_od),]
  row.names(tvals2) <- NULL
  tvals2
}, info = "Parameter powers and trace values for model_5 (cartesian).")


# ---- tracevals2params_nc -----------------------------------------------------
expect_equal({
  tvals <- tracevals2params_nc(
    Wo = diag(c(2,2)),
    Wd = diag(c(3,3)),
    Ww = diag(c(4,4)),
    model = "model_9",
    approx_order = 2)
  tvals1 <- tvals[order(tvals$rho_d,tvals$rho_o,tvals$rho_w, decreasing = T),]
  row.names(tvals1) <- NULL
  tvals1
},
{
  data.frame(
    "rho_d" = c(2,0,0),
    "rho_o" = c(0,2,0),
    "rho_w" = c(0,0,2),
    "TRACE_VAL" = c(sum(c(3,3)^2), # Wd
                    sum(c(2,2)^2), # Wo
                    sum(c(4,4)^2))/ 2) # Ww
}, info = "Parameter powers and trace values for model_9 (non-cartesian).")

expect_equal({
  tvals <- tracevals2params_nc(
    Wo = diag(c(2,2)),
    Wd = diag(c(3,3)),
    Ww = diag(c(4,4)),
    model = "model_8",
    approx_order = 2)
  tvals1 <- tvals[order(tvals$rho_d,tvals$rho_o, decreasing = TRUE),]
  row.names(tvals1) <- NULL
  tvals1
},
{
  data.frame(
    "rho_d" = c(2,2,0),
    "rho_o" = c(2,0,2),
    "TRACE_VAL" = c(sum(c(4,4)^2), # Ww
                    sum(c(3,3)^2), # Wd
                    sum(c(2,2)^2))/ 2) # Wo
}, info = "Parameter powers and trace values for model_8 (non-cartesian).")

expect_equal({
  tvals <- tracevals2params_nc(
    Wo = diag(c(2,2)),
    Wd = diag(c(3,3)),
    model = "model_7",
    approx_order = 2)
  tvals1 <- tvals[order(tvals$rho_d,tvals$rho_o,decreasing = T),]
  row.names(tvals1) <- NULL
  tvals1
},
{
  data.frame(
    "rho_d" = c(2,0),
    "rho_o" = c(0,2),
    "TRACE_VAL" = c(sum(c(3,3)^2),     # Wd
                    sum(c(2,2)^2))/ 2) # Wo
}, info = "Parameter powers and trace values for model_7 (non-cartesian).")

# ---- generate_approxldet_cartesian ------------------------------------------
In <- diag(16)
W_ge <- germany_net@node_neighborhood
Wd <- In %x% W_ge
Wo <- W_ge %x% In
Ww <- W_ge %x% W_ge

expect_equal({
  log_det_fun <- spflow:::generate_approxldet_cartesian(
    OW = W_ge,
    DW = W_ge,
    n_o = nrow(W_ge),
    n_d = nrow(W_ge),
    model = "model_9",
    approx_order = 5)
  log_det_fun(c(.5,.4,-.1))
},
{
  W_F_ge <- .5 * Wd + .4 * Wo + -.1 * Ww
  - sum(spflow:::trace_sequence(W_F_ge,max_power = 5)/seq(5))
},
info = "cartesian model_9")

expect_equal({
  log_det_fun <- spflow:::generate_approxldet_cartesian(
    OW = W_ge,
    DW = W_ge,
    n_o = nrow(W_ge),
    n_d = nrow(W_ge),
    model = "model_8",
    approx_order = 5)
  log_det_fun(c(.4,.6))
},
{
  # There is no equality for the successive filtering...
  # W_F_ge <- .4 * Wd + .6 * Wo - .24 * Ww
  # - sum(spflow:::trace_sequence(W_F_ge,max_power = 5)/seq(5))
  - sum(spflow:::trace_sequence(.4 * Wd,max_power = 5)/seq(5) +
         spflow:::trace_sequence(.6 * Wo,max_power = 5)/seq(5))
},
info = "cartesian model_8")

expect_equal({
  log_det_fun <- spflow:::generate_approxldet_cartesian(
    OW = W_ge,
    DW = W_ge,
    n_o = nrow(W_ge),
    n_d = nrow(W_ge),
    model = "model_7",
    approx_order = 5)
  log_det_fun(c(.4,.6))
},
{
  W_F_ge <- .4 * Wd + .6 * Wo
  - sum(spflow:::trace_sequence(W_F_ge,max_power = 5)/seq(5))
},
info = "cartesian model_7")

expect_equal({
  log_det_fun <- spflow:::generate_approxldet_cartesian(
    OW = W_ge,
    DW = W_ge,
    n_o = nrow(W_ge),
    n_d = nrow(W_ge),
    model =  "model_6",
    approx_order = 5)
  log_det_fun(c(.6))},
  {
    W_F_ge <- .2 * Wd + .2 * Wo + .2 * Ww
    - sum(spflow:::trace_sequence(W_F_ge,max_power = 5)/seq(5))
  },
  info = "cartesian model_6")

expect_equal({
  log_det_fun <- spflow:::generate_approxldet_cartesian(
    OW = W_ge,
    DW = W_ge,
    n_o = nrow(W_ge),
    n_d = nrow(W_ge),
    model =  "model_5",
    approx_order = 5)
  log_det_fun(c(.4))},
  {
    W_F_ge <- .2 * Wd + .2 * Wo
    - sum(spflow:::trace_sequence(W_F_ge,max_power = 5)/seq(5))
  },
  info = "cartesian model_5")

expect_equal({
  log_det_fun <- spflow:::generate_approxldet_cartesian(
    OW = W_ge,
    DW = W_ge,
    n_o = nrow(W_ge),
    n_d = nrow(W_ge),
    model =  "model_4",
    approx_order = 5)
  log_det_fun(c(.5))},
  {
    W_F_ge <- .5 * Ww
    - sum(spflow:::trace_sequence(W_F_ge,max_power = 5)/seq(5))
  },
  info = "cartesian model_4")

expect_equal({
  log_det_fun <- spflow:::generate_approxldet_cartesian(
    OW = W_ge,
    DW = W_ge,
    n_o = nrow(W_ge),
    n_d = nrow(W_ge),
    model =  "model_3",
    approx_order = 5)
  log_det_fun(c(.5))},
  {
    W_F_ge <- .5 * Wo
    - sum(spflow:::trace_sequence(W_F_ge,max_power = 5)/seq(5))
  },
  info = "cartesian model_3")

expect_equal({
  log_det_fun <- spflow:::generate_approxldet_cartesian(
    OW = W_ge,
    DW = W_ge,
    n_o = nrow(W_ge),
    n_d = nrow(W_ge),
    model =  "model_2",
    approx_order = 5)
  log_det_fun(c(.5))},
  {
    W_F_ge <- .5 * Wd
    - sum(spflow:::trace_sequence(W_F_ge,max_power = 5)/seq(5))
  },
  info = "cartesian model_2")

# ---- generate_approxldet_noncartesian ---------------------------------------
expect_equal({
  log_det_fun <- spflow:::generate_approxldet_noncartesian(
    Wd = Wd,
    Wo = Wo,
    Ww = Ww,
    model = "model_9",
    approx_order = 4)
  log_det_fun(c(.5,.4,-.1))
},
{
  W_F_ge <- .5 * Wd + .4 * Wo + -.1 * Ww
  - sum(spflow:::trace_sequence(W_F_ge,max_power = 4)/seq(4))
},
info = "noncartesian model_9")

expect_equal({
  log_det_fun <- spflow:::generate_approxldet_noncartesian(
    Wd = Wd,
    Wo = Wo,
    Ww = Ww,
    model = "model_8",
    approx_order = 4)
  log_det_fun(c(.4,.3))
},
{
  W_F_ge <- .4 * Wd + .3 * Wo - .12 * Ww
  - sum(spflow:::trace_sequence(W_F_ge,max_power = 4)/seq(4))
},
info = "noncartesian model_8")

expect_equal({
  log_det_fun <- spflow:::generate_approxldet_noncartesian(
    Wd = Wd,
    Wo = Wo,
    Ww = Ww,
    model = "model_7",
    approx_order = 4)
  log_det_fun(c(.4,.5))
},
{
  W_F_ge <- .4 * Wd + .5 * Wo
  - sum(spflow:::trace_sequence(W_F_ge, max_power = 4) / seq(4))
},
info = "noncartesian model_7")

expect_equal({
  log_det_fun <- spflow:::generate_approxldet_noncartesian(
    Wd = Wd,
    Wo = Wo,
    Ww = Ww,
    model = "model_6",
    approx_order = 4)
  log_det_fun(c(.6))},
  {
    W_F_ge <- .2 * Wd + .2 * Wo + .2 * Ww
    - sum(spflow:::trace_sequence(W_F_ge,max_power = 4)/seq(4))
  },
  info = "noncartesian model_6")

expect_equal({
  log_det_fun <- spflow:::generate_approxldet_noncartesian(
    Wd = Wd,
    Wo = Wo,
    Ww = Ww,
    model = "model_5",
    approx_order = 4)
  log_det_fun(c(.4))},
  {
    W_F_ge <- .2 * Wd + .2 * Wo
    - sum(spflow:::trace_sequence(W_F_ge,max_power = 4)/seq(4))
  },
  info = "noncartesian model_5")

expect_equal({
  log_det_fun <- spflow:::generate_approxldet_noncartesian(
    Wd = Wd,
    Wo = Wo,
    Ww = Ww,
    model = "model_4",
    approx_order = 4)
  log_det_fun(c(.5))},
  {
    W_F_ge <- .5 * Ww
    - sum(spflow:::trace_sequence(W_F_ge,max_power = 4)/seq(4))
  },
  info = "noncartesian model_4")

expect_equal({
  log_det_fun <- spflow:::generate_approxldet_noncartesian(
    Wd = Wd,
    Wo = Wo,
    Ww = Ww,
    model = "model_3",
    approx_order = 4)
  log_det_fun(c(.5))},
  {
    W_F_ge <- .5 * Wo
    - sum(spflow:::trace_sequence(W_F_ge,max_power = 4)/seq(4))
  },
  info = "noncartesian model_3")

expect_equal({
  log_det_fun <- spflow:::generate_approxldet_noncartesian(
    Wd = Wd,
    Wo = Wo,
    Ww = Ww,
    model = "model_2",
    approx_order = 4)
  log_det_fun(c(.5))},
  {
    W_F_ge <- .5 * Wd
    - sum(spflow:::trace_sequence(W_F_ge,max_power = 4)/seq(4))
  },
  info = "noncartesian model_2")

