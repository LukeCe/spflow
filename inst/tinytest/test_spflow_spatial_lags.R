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


# ---- spatial_lag ------------------------------------------------------------
expect_equal({
  W <- Matrix::sparseMatrix(i = c(1,1,2,3,4),j =  c(2,3,3,4,1),x =  c(.5,.5,1,1,1))
  X <- X_na <- matrix(1:4,4)
  X_na[2] <- NA
  X <- cbind(X, X_na)
  obs_X <- 1 - is.na(X)

  cbind(
    "X"    = X,
    "WX_i" = W %*% spflow:::drop_na(X),
    "WX_p" = W %*% X,
    "WX_w" = W %*% spflow:::drop_na(X) * Matrix::rowSums(W) / (W %*% obs_X))

},
{
  W <- Matrix::sparseMatrix(i = c(1,1,2,3,4),j =  c(2,3,3,4,1),x =  c(.5,.5,1,1,1))
  X <- X_na <- matrix(1:4,4)
  X_na[2] <- NA
  X <- cbind(X, X_na)

  cbind(
    "X"    = X,
    "WX_i" = spflow:::spatial_lag(X,W,"ignore"),
    "WX_p" = spflow:::spatial_lag(X,W,"propagate"),
    "WX_w" = spflow:::spatial_lag(X,W,"reweight"))

},
info = "Check NA handling for spatial lags.")

expect_equal({
  W <- Matrix::sparseMatrix(i = c(1,2,3,4),j =  c(2,3,4,1),x =  c(1,1,1,1))
  X <- X_na <- matrix(1:4,4)
  X_na[2] <- NA
  X <- cbind(X, X_na)

  cbind(
    "X"    = X,
    "WX_i" = W %*% spflow:::drop_na(X),
    "WX_p" = W %*% X,
    "WX_w" = W %*% spflow:::drop_na(X) * Matrix::rowSums(W) / (W %*% !is.na(X)))

},
{
  W <- Matrix::sparseMatrix(i = c(1,2,3,4),j =  c(2,3,4,1),x =  c(1,1,1,1))
  X <- X_na <- matrix(1:4,4)
  X_na[2] <- NA
  X <- cbind(X, X_na)

  cbind(
    "X"    = X,
    "WX_i" = spflow:::spatial_lag(X,W,"ignore"),
    "WX_p" = spflow:::spatial_lag(X,W,"propagate"),
    "WX_w" = spflow:::spatial_lag(X,W,"reweight"))

},
info = "Check NA handling for spatial lags, case with non observed neighbour.")

# ---- spatial_do_lag ---------------------------------------------------------
expect_equal({
  W <- Matrix::sparseMatrix(i = c(1,1,2,3,4),j =  c(2,3,3,4,1),x =  c(.5,.5,1,1,1))
  Wo <- W %x% diag(4)
  Wd <- diag(4) %x% W
  Ww <- W %x% W
  Y <- Y_na <- matrix(1:16,4)
  diag(Y_na) <- NA

  y <- as.vector(Y_na)
  obs_y <- !is.na(y)
  as.matrix(cbind(
    "y"     = y,
    # prop
    "Wdy_p" = Wd %*% y,
    "Woy_p" = Wo %*% y,
    "Wwy_p" = Ww %*% y,
    # ignore
    "Wdy_i" = Wd %*% spflow:::drop_na(y),
    "Woy_i" = Wo %*% spflow:::drop_na(y),
    "Wwy_i" = Ww %*% spflow:::drop_na(y),
    # reweight
    "Wdy_w" = Wd %*% spflow:::drop_na(y) * Matrix::rowSums(Wd) / (Wd %*% obs_y),
    "Woy_w" = Wo %*% spflow:::drop_na(y) * Matrix::rowSums(Wo) / (Wo %*% obs_y),
    "Wwy_w" = Ww %*% spflow:::drop_na(y) * Matrix::rowSums(Ww) / (Ww %*% obs_y)
    ))

},
{
  W <- Matrix::sparseMatrix(i = c(1,1,2,3,4),j =  c(2,3,3,4,1),x =  c(.5,.5,1,1,1))
  Y <- Y_na <- Matrix(1:16,4)
  diag(Y_na) <- NA
  d <- function(l) data.frame(lapply(l, as.vector))
  y <- as.vector(Y_na)
  obs_y <- !is.na(y)
  as.matrix(cbind(
    "y"    = y,
    #Wd
    "Wdy_p" = d(spflow:::spatial_do_lag(Y_na,W,W,pair_index = NULL,na_handling =  "propagate")),
    "Wdy_i" = d(spflow:::spatial_do_lag(Y_na,W,W,pair_index = NULL,na_handling =  "ignore")),
    "Wdy_r" = d(spflow:::spatial_do_lag(Y_na,W,W,pair_index = NULL,na_handling =  "reweight"))))
},
info = "Check NA handling for spatial lags of cartesian flow matrix.",
check.attributes = FALSE)


expect_equal({
  W <- Matrix::sparseMatrix(i = c(1,1,2,3,4),j =  c(2,3,3,4,1),x =  c(.5,.5,1,1,1))
  Wo <- W %x% diag(4)
  Wd <- diag(4) %x% W
  Ww <- W %x% W
  Y <- Y_na <- matrix(1:16,4)
  diag(Y_na) <- NA
  pair_index <- which(as.vector(W == 0))
  y <- as.vector(Y_na[pair_index])

  Wo <- Wo[pair_index,pair_index]
  Wd <- Wd[pair_index,pair_index]
  Ww <- Ww[pair_index,pair_index]

  obs_y <- !is.na(y)
  as.matrix(cbind(
    "y"     = y,
    # prop
    "Wdy_p" = Wd %*% y,
    "Woy_p" = Wo %*% y,
    "Wwy_p" = Ww %*% y,
    # ignore
    "Wdy_i" = Wd %*% spflow:::drop_na(y),
    "Woy_i" = Wo %*% spflow:::drop_na(y),
    "Wwy_i" = Ww %*% spflow:::drop_na(y),
    # reweight
    "Wdy_w" = Wd %*% spflow:::drop_na(y) * Matrix::rowSums(Wd) / (Wd %*% obs_y),
    "Woy_w" = Wo %*% spflow:::drop_na(y) * Matrix::rowSums(Wo) / (Wo %*% obs_y),
    "Wwy_w" = Ww %*% spflow:::drop_na(y) * Matrix::rowSums(Ww) / (Ww %*% obs_y)
  ))

},
{
  W <- Matrix::sparseMatrix(i = c(1,1,2,3,4),j =  c(2,3,3,4,1),x =  c(.5,.5,1,1,1))
  Y <- Y_na <- Matrix(1:16,4)
  diag(Y_na) <- NA
  pair_index <- which(as.vector(W == 0))
  y <- as.vector(Y_na[pair_index])
  Y_na[-pair_index] <- 0
  Y_na <- drop0(Y_na)

  d <- function(l) data.frame(lapply(l, function(.l) .l@x))
  as.matrix(cbind(
    "y"    = y,
    #Wd
    "Wdy_p" = d(spflow:::spatial_do_lag(Y_na,W,W,pair_index = pair_index,na_handling =  "propagate")),
    "Wdy_i" = d(spflow:::spatial_do_lag(Y_na,W,W,pair_index = pair_index,na_handling =  "ignore")),
    "Wdy_r" = d(spflow:::spatial_do_lag(Y_na,W,W,pair_index = pair_index,na_handling =  "reweight"))))
},
info = "Check NA handling for spatial lags of non-cartesian flow matrix.",
check.attributes = FALSE)

# ---- spatial_do_lag2 --------------------------------------------------------
expect_equal({
  W <- Matrix::sparseMatrix(i = c(1,1,2,3,4),j =  c(2,3,3,4,1),x =  c(.5,.5,1,1,1))
  Wo <- W %x% diag(4)
  Wd <- diag(4) %x% W
  Ww <- W %x% W
  Y <- Y_na <- matrix(1:16,4)
  diag(Y_na) <- NA

  y <- drop_na(as.vector(Y_na))
  d <- function(l) data.frame(lapply(l, as.vector))
  d(list(
    "d"  = Wd %*% y,
    "o"  = Wo %*% y,
    "w"  = Ww %*% y,
    "dd" = Wd %*% Wd %*% y,
    "do" = Wd %*% Wo %*% y,
    "dw" = Wd %*% Ww %*% y,
    "oo" = Wo %*% Wo %*% y,
    "ow" = Wo %*% Ww %*% y,
    "ww" = Ww %*% Ww %*% y))

},
{
  W <- Matrix::sparseMatrix(i = c(1,1,2,3,4),j =  c(2,3,3,4,1),x =  c(.5,.5,1,1,1))
  Y <- Y_na <- Matrix(1:16,4)
  diag(Y_na) <- NA
  d <- function(l) data.frame(lapply(l, as.vector))
  d(spflow:::spatial_do_lag2(drop0(Y_na),W,W,na_handling = "ignore"))
},
info = "Check NA handling for spatial lags double of cartesian flow matrix.",
check.attributes = FALSE)


expect_equal({
  W <- Matrix::sparseMatrix(i = c(1,1,2,3,4),j =  c(2,3,3,4,1),x =  c(.5,.5,1,1,1))
  Y <- Y_na <- matrix(1:16,4)
  diag(Y_na) <- NA
  pair_index <- which(as.vector(W == 0))

  #
  Wo <- W %x% diag(4)
  Wd <- diag(4) %x% W
  Ww <- W %x% W
  #
  Wo <- Wo[pair_index,pair_index]
  Wd <- Wd[pair_index,pair_index]
  Ww <- Ww[pair_index,pair_index]

  y <- as.vector(Y_na[pair_index])
  obs_y <- !is.na(y)
  y <- drop_na(y)

  yd <- Wd %*% y * Matrix::rowSums(Wd) / (Wd %*% obs_y)
  yo <- Wo %*% y * Matrix::rowSums(Wo) / (Wo %*% obs_y)
  yw <- Ww %*% y * Matrix::rowSums(Ww) / (Ww %*% obs_y)
  d(list(
    "d" = yd,
    "o" = yo,
    "w" = yw,
    "dd" = Wd %*% drop_na(yd) * Matrix::rowSums(Wd) / (Wd %*% is.finite(yd)),
    "do" = Wo %*% drop_na(yd) * Matrix::rowSums(Wo) / (Wo %*% is.finite(yd)),
    "dw" = Ww %*% drop_na(yd) * Matrix::rowSums(Ww) / (Ww %*% is.finite(yd)),
    "od" = Wd %*% drop_na(yo) * Matrix::rowSums(Wd) / (Wd %*% is.finite(yo)),
    "oo" = Wo %*% drop_na(yo) * Matrix::rowSums(Wo) / (Wo %*% is.finite(yo)),
    "ow" = Ww %*% drop_na(yo) * Matrix::rowSums(Ww) / (Ww %*% is.finite(yo)),
    "od" = Wd %*% drop_na(yw) * Matrix::rowSums(Wd) / (Wd %*% is.finite(yw)),
    "oo" = Wo %*% drop_na(yw) * Matrix::rowSums(Wo) / (Wo %*% is.finite(yw)),
    "ww" = Ww %*% drop_na(yw) * Matrix::rowSums(Ww) / (Ww %*% is.finite(yw))))
},
{
  W <- Matrix::sparseMatrix(i = c(1,1,2,3,4),j =  c(2,3,3,4,1),x =  c(.5,.5,1,1,1))
  Y <- Y_na <- Matrix(1:16,4)
  diag(Y_na) <- NA
  pair_index <- which(as.vector(W == 0))
  Y_na[-pair_index] <- 0
  Y_na <- drop0(Y_na)
  d <- function(l) data.frame(lapply(l, "slot", "x"))
  d(spflow:::spatial_do_lag2(Y_na,W,W,na_handling = "reweight", pair_index = pair_index))
},
info = "Check NA handling for spatial lags double of non-cartesian flow matrix.",
check.attributes = FALSE)


# ---- derive_dow_rowsums -----------------------------------------------------
expect_equal({
  W <- Matrix::sparseMatrix(i = c(1,1,2,3,4),j =  c(2,3,3,4,1),x =  c(.5,.5,1,1,1))
  Wo <- W %x% diag(4)
  Wd <- diag(4) %x% W
  Ww <- W %x% W
  Y <- Y_na <- matrix(1:16,4)
  diag(Y_na) <- NA
  pair_index <- which(as.vector(W == 0))
  y <- as.vector(Y_na[pair_index])

  Wo <- Wo[pair_index,pair_index]
  Wd <- Wd[pair_index,pair_index]
  Ww <- Ww[pair_index,pair_index]

  data.frame(
    "d" = rowSums(Wd),
    "o" = rowSums(Wo),
    "w" = rowSums(Ww))

},
{
  W <- Matrix::sparseMatrix(i = c(1,1,2,3,4),j =  c(2,3,3,4,1),x =  c(.5,.5,1,1,1))
  Y <- Y_na <- Matrix(1:16,4)
  diag(Y_na) <- NA
  pair_index <- which(as.vector(W == 0))
  as.data.frame(derive_dow_rowsums(W,W,lag_keys = c("d","o","w"), pair_index = pair_index))
},
info = "Check NA handling for spatial lags of non-cartesian flow matrix.",
check.attributes = FALSE)
