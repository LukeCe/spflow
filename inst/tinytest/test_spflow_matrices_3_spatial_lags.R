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

# ---- double_lag_matrix ------------------------------------------------------
expect_null(spflow:::double_lag_matrix(M = NULL))

expect_equal({
  G <- diag(2)
  W <- diag(2,2,2)
  spflow:::double_lag_matrix(G,W,W,return_all_lags = FALSE)
  },
  {
    # the power is equal the occurrence of w
    lapply(list("G" = 0, "G.wGw" = 2, "G.wwGww" = 4),
           function(.p) diag(2^.p,2,2))
  },
  info = "generate a list of lags (reduced instruments)",
  check.attributes = FALSE)

expect_equal({
  G <- diag(2)
  W <- diag(2,2,2)
  res <- spflow:::double_lag_matrix(G,W,W,return_all_lags = TRUE)
  spflow:::sort_names(res)
  },
  {
    # the power is equal the occurrence of w
  res_ref <- lapply(list(
    "G" = 0,
    "G.wG" = 1, "G.wwG" = 2,
    "G.Gw" = 1, "G.Gww" = 2,
    "G.wGw" = 2, "G.wwGw" = 3, "G.wGww" = 3, "G.wwGww" = 4),
    function(.p) diag(2 ^ .p, 2, 2))
  spflow:::sort_names(res_ref)
  },
  info = "generate a list of lags (full instruments)",
  check.attributes = FALSE)

expect_equal({
  G <- diag(2)
  W <- diag(2,2,2)
  spflow:::double_lag_matrix(G,W,NULL,return_all_lags = FALSE)
  },
  {
    # the power is equal the occurrence of w
    lapply(list("G" = 0, "G.wG" = 1, "G.wwG" = 2),
           function(.p) diag(2^.p,2,2))
  },
  info = "generate a list of lags (only dest instruments)",
  check.attributes = FALSE)

expect_equal({
  G <- diag(2)
  W <- diag(2,2,2)
  spflow:::double_lag_matrix(G,NULL,W,return_all_lags = FALSE)
  },
  {
    # the power is equal the occurrence of w
    lapply(list("G" = 0, "G.Gw" = 1, "G.Gww" = 2),
           function(.p) diag(2^.p,2,2))
  },
  info = "generate a list of lags (only orig instruments)",
  check.attributes = FALSE)

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

expect_equal({
  Y  <- matrix(c(0,1,1,1),2,2)
  W <- matrix(1:4,2,2)
  Y_lags <- spflow:::lag_flow_matrix(Y, model = "model_9", W, W,M_indicator = Y)
  lapply(Y_lags, "!=", 0)

  },
  {
    # the power is equal the occurrence of w
    indic_pos <- matrix(c(F,T,T,T),2,2)
    res_bin <- list("Y" = indic_pos,
                    "Y.d" = indic_pos,
                    "Y.o" = indic_pos,
                    "Y.w" = indic_pos)
  },
  info = "generate sparse lags (model_9)")

