# ---- predict_lags_and_inst_satus --------------------------------------------
expect_equal({
  spflow:::predict_lags_and_inst_satus(list(
    "norm" = c("F1", "E2",     "X4"),
    "sdm"  = c("F1",     "D3",     "X5"),
    "inst" = c("F1", "E2", "D3",         "X6")
  ))
  },
  {
    lapply(list(
      "lag0" = c("F1" = F ,"E2" = F, "X4" = F, "X6" = T),
      "lag1" = c("F1" = F, "D3" = F, "X5" = F, "E2" = T, "X6" = T),
      "lag2" = c("F1" = T ,"E2" = T, "D3" = T),
      "lag3" = c("F1" = T, "D3" = T)
    ),spflow:::sort_names)
  },
  info = "predict the instrument status of variables")

# ---- orthoginolize_instruments ----------------------------------------------
expect_equal({
  set.seed(1)
  n <- 100
  var_a <- rnorm(n) # no inst
  var_b <- rnorm(n) # no inst
  var_c <- rnorm(n) # inst
  var_d <- rnorm(n) # inst

  # matrix with four instruments, two of them are redundant
  mat_with_inst <-
    cbind(var_a, var_b,
          var_c + 10 * var_a, var_c + 10 * var_b,
          var_d + 5 * var_b - 5* var_a, var_d - 5 * var_b + 5* var_a)
  spflow:::attr_inst_status(mat_with_inst) <-
    c(FALSE,FALSE,TRUE,TRUE,TRUE,TRUE)

  dim(spflow:::orthoginolize_instruments(mat_with_inst))
  },c(100, 4),
  info = "detect and remove uninformative instruments")

expect_equal({
  set.seed(1)
  n <- 100
  var_a <- rnorm(n) # no inst
  var_b <- rnorm(n) # no inst
  var_c <- rnorm(n) # inst
  var_d <- rnorm(n) # inst
  mat_without_inst <- cbind(var_a, var_b,var_c,var_d)
  spflow:::attr_inst_status(mat_without_inst) <- c(FALSE,FALSE,FALSE,FALSE)
  spflow:::orthoginolize_instruments(mat_without_inst)
  },
  {
    set.seed(1)
    n <- 100
    var_a <- rnorm(n) # no inst
    var_b <- rnorm(n) # no inst
    var_c <- rnorm(n) # inst
    var_d <- rnorm(n) # inst
    mat_without_inst <- cbind(var_a, var_b,var_c,var_d)
    spflow:::attr_inst_status(mat_without_inst) <- c(FALSE,FALSE,FALSE,FALSE)
    mat_without_inst
  },
  info = "do not tuch explanatory variables")

# ---- derive_pair_instruments ------------------------------------------------
expect_null(spflow:::derive_pair_instruments(G = NULL))

expect_equal({
  G <- diag(2)
  W <- diag(2,2,2)
  spflow:::derive_pair_instruments(G,W,W,full_inst = FALSE)
  },
  {
    # the power is equal the occurrence of w
    lapply(list("G" = 0, "G.wGw" = 2, "G.wwGww" = 4),
           function(.p) diag(2^.p,2,2))
  },
  info = "generate a list of lags (reduced instruments)")

expect_equal({
  G <- diag(2)
  W <- diag(2,2,2)
  spflow:::derive_pair_instruments(G,W,W,full_inst = TRUE)
  },
  {
    # the power is equal the occurrence of w
    lapply(list("G" = 0,
                "G.wG" = 1, "G.wwG" = 2,
                "G.Gw" = 1, "G.Gww" = 2,
                "G.wGw" = 2, "G.wwGw" = 3, "G.wGww" = 3, "G.wwGww" = 4),
           function(.p) diag(2^.p,2,2))
  },
  info = "generate a list of lags (full instruments)")

expect_equal({
  G <- diag(2)
  W <- diag(2,2,2)
  spflow:::derive_pair_instruments(G,W, NULL,full_inst = TRUE)
  },
  {
    # the power is equal the occurrence of w
    lapply(list("G" = 0,
                "G.Gw" = 1, "G.Gww" = 2),
           function(.p) diag(2^.p,2,2))
  },
  info = "generate a list of lags (only orig instruments)")

expect_equal({
  G <- diag(2)
  W <- diag(2,2,2)
  spflow:::derive_pair_instruments(G,NULL, W,full_inst = TRUE)
  },
  {
    # the power is equal the occurrence of w
    lapply(list("G" = 0,
                "G.wG" = 1, "G.wwG" = 2),
           function(.p) diag(2^.p,2,2))
  },
  info = "generate a list of lags (only dest instruments)")

# ---- lag_flow_matrix --------------------------------------------------------
expect_equal({
  Y <- diag(2)
  W <- diag(2,2,2)
  spflow:::lag_flow_matrix(Y, model = "model_9", W, W)
  },
  {
    # the power is equal the occurrence of w
    lapply(list("Y" = 0, "Y.d" = 1, "Y.o" = 1, "Y.w" = 2),
           function(.p) diag(2^.p,2,2))
  },
  info = "generate a list of lags (model_9)")

expect_equal({
  Y <- diag(2)
  W <- diag(2,2,2)
  spflow:::lag_flow_matrix(Y, model = "model_7", W, W)
  },
  {
    # the power is equal the occurrence of w
    lapply(list("Y" = 0, "Y.d" = 1, "Y.o" = 1),
           function(.p) diag(2^.p,2,2))
  },
  info = "generate a list of lags (model_7)")

expect_equal({
  Y <- diag(2)
  W <- diag(2,2,2)
  spflow:::lag_flow_matrix(Y, model = "model_5", W, W)
  },
  {
    # the power is equal the occurrence of w
    lapply(list("Y" = 0, "Y.od" = 1),
           function(.p) diag(2^.p,2,2))
  },
  info = "generate a list of lags (model_5)")

expect_equal({
  Y <- diag(2)
  W <- diag(2,2,2)
  spflow:::lag_flow_matrix(Y, model = "model_4", W, W)
  },
  {
    # the power is equal the occurrence of w
    lapply(list("Y" = 0, "Y.w" = 2),
           function(.p) diag(2^.p,2,2))
  },
  info = "generate a list of lags (model_4)")


# ---- by_role_spatial_lags ---------------------------------------------------
expect_equal({
  mat_sources <- lapply(list(
    pair = data.frame(y1 = rep(1,9), p1 = 1),
    orig = data.frame(o1 = rep(1,3), o2 = 2),
    dest = data.frame(d1 = rep(1,3), d2 = 2)),
    "as.matrix")
  var_roles <- list("Y_" = "y1",
                    "G_" = c("p1"),
                    "O_" = c("o1", "o2"),
                    "D_" = c("d1", "d2"))
  var_roles <- spflow:::translist(list(
    "norm" = var_roles,
    "sdm"  = var_roles[c("O_", "D_")],
    "inst" = var_roles[c("O_", "D_", "G_")]))

  ctrl <- list("mat_format" = function(x) matrix(x,3,3),
               "model" = "model_9",
               "twosls_reduce_pair_instruments" = TRUE)
  results <- spflow:::by_role_spatial_lags(
    mat_sources, var_roles,
    list("OW" = diag(2,3,3), "DW" = diag(2,3,3)),
    ctrl)
  results[c("Y_","O_","G_")]
  },
  {
    list(
      "Y_" = lapply(list("Y" = 0, "Y.d" = 1, "Y.o" = 1, "Y.w" = 2),
                    function(.p) matrix(2^.p,3,3)),
      "O_" = as.matrix(data.frame(
        o1 = rep(1,3), o2 = 2, o1.lag1 = 2, o2.lag1 = 4,
        o1.lag2 =   4, o2.lag2 = 8, o1.lag3 = 8, o2.lag3 = 16)),
      "G_" = lapply(list("p1" = 0, "p1.wGw" = 2, "p1.wwGww" = 4),
                    function(.p) matrix(2^.p,3,3))
    )
  },
  check.attributes = FALSE,
  info = "create lags for each type of input data")

