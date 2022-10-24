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
  params <- c("(Intercept)", "(Intra)", "D_", "O_", "I_", "P_")
  params <- spflow:::lookup(1, params)
  mats <- list(
    "Y_" = list(matrix(1,n_d,n_o)),
    "CONST" = list("(Intercept)" = 1, "(Intra)" = Diagonal(n_d)),
    "D_" = matrix(1,nrow = n_d),
    "O_" = matrix(1,nrow = n_o),
    "I_" = matrix(1,nrow = n_d),
    "P_" = list(matrix(1,n_d,n_o)))
  spflow:::compute_signal(mats,delta = params)
}, check.attributes = FALSE)




# ---- compute_expectation ----------------------------------------------------
expect_equal({

  signal <- matrix(1,2,2)
  W <- diag(1,2,2)
  spflow:::compute_expectation(
    signal,
    model = "model_2",
    DW =  W, OW = W,
    rho = 1,
    max_it = 10,
    M_indicator = NULL)
},
{
  matrix((10 + 1),2,2)
},
info = "test expectation approximation (cartesian case)")


expect_equal({

  signal <- matrix(2,2,2)
  W <- diag(1,2,2)
  spflow:::compute_expectation(
    signal,
    model = "model_2",
    DW =  W, OW = W,
    rho = 1,
    max_it = 10,
    M_indicator = diag(1,2,2))
},
{
  diag((10) * 2, 2, 2) + 2
},
info = "test expectation approximation (non-cartesian case)")


# ---- compute_diag_precision_mat  --------------------------------------------
OW <- matrix(c(0,5,6,1,0,3,4,4,0),3,3) / 10
DW <- matrix(c(0,5,6,1,5,0,4,4,5,6,0,1,5,4,4,0),4,4) / 10
rho <- c("rho_d" = .3, "rho_o" = .5, "rho_w" = -.2)


expect_equal({
  spflow:::compute_diag_precision_mat(
    DW = DW,
    OW = OW,
    rho = rho["rho_d"],
    n_o = nrow(OW),
    n_d = nrow(DW),
    M_indicator = NULL)
},
{
  A <- diag(nrow(OW) * nrow(DW)) - diag(nrow(OW)) %x% DW * rho["rho_d"]
  matrix(diag(crossprod(A)),nrow = nrow(DW))

},
info = "test calculation of the diagonal of the precision matrix (cartesian case model 2)")

expect_equal({
  spflow:::compute_diag_precision_mat(
    DW = DW,
    OW = OW,
    rho = rho["rho_o"],
    n_o = nrow(OW),
    n_d = nrow(DW),
    M_indicator = NULL)
},
{
  A <- diag(nrow(OW) * nrow(DW)) - OW %x% diag(nrow(DW)) * rho["rho_o"]
  matrix(diag(crossprod(A)),nrow = nrow(DW))

},
info = "test calculation of the diagonal of the precision matrix (cartesian case model 3)")

expect_equal({
  spflow:::compute_diag_precision_mat(
    DW = DW,
    OW = OW,
    rho = rho["rho_w"],
    n_o = nrow(OW),
    n_d = nrow(DW),
    M_indicator = NULL)
},
{
  A <- diag(nrow(OW) * nrow(DW)) - OW %x% DW * rho["rho_w"]
  matrix(diag(crossprod(A)),nrow = nrow(DW))

},
info = "test calculation of the diagonal of the precision matrix (cartesian case model 4)")

expect_equal({
  spflow:::compute_diag_precision_mat(
    DW = DW,
    OW = OW,
    rho = rho,
    n_o = nrow(OW),
    n_d = nrow(DW),
    M_indicator = NULL)
},
{
  A <- diag(nrow(OW) * nrow(DW)) -
    diag(nrow(OW)) %x% DW * rho["rho_d"] -
    OW %x% diag(nrow(DW)) * rho["rho_o"] -
    OW %x% DW * rho["rho_w"]
  matrix(diag(crossprod(A)),nrow = nrow(DW))

},
info = "test calculation of the diagonal of the precision matrix (cartesian case model 9)")


I_gamma <- matrix(c(1,1,0,0,1,1,1,0,1,1,0,1),4,3)
sg <- as.logical(I_gamma)


expect_equal({
  spflow:::compute_diag_precision_mat(
    DW = DW,
    OW = OW,
    rho = rho["rho_d"],
    n_o = nrow(OW),
    n_d = nrow(DW),
    M_indicator = I_gamma)
},
{
  A <- diag(nrow(OW) * nrow(DW)) - diag(nrow(OW)) %x% DW * rho["rho_d"]
  res <- I_gamma
  res[sg] <- diag(crossprod(A[sg,sg]))
  res

},
info = "test calculation of the diagonal of the precision matrix (non-cartesian case model 2)")

expect_equal({
  spflow:::compute_diag_precision_mat(
    DW = DW,
    OW = OW,
    rho = rho["rho_o"],
    n_o = nrow(OW),
    n_d = nrow(DW),
    M_indicator = I_gamma)
},
{
  A <- diag(nrow(OW) * nrow(DW)) - OW %x% diag(nrow(DW)) * rho["rho_o"]
  res <- I_gamma
  res[sg] <- diag(crossprod(A[sg,sg]))
  res
},
info = "test calculation of the diagonal of the precision matrix (non-cartesian case model 3)")

expect_equal({
  spflow:::compute_diag_precision_mat(
    DW = DW,
    OW = OW,
    rho = rho["rho_w"],
    n_o = nrow(OW),
    n_d = nrow(DW),
    M_indicator = I_gamma)
},
{
  A <- diag(nrow(OW) * nrow(DW)) - OW %x% DW * rho["rho_w"]
  res <- I_gamma
  res[sg] <- diag(crossprod(A[sg,sg]))
  res
},
info = "test calculation of the diagonal of the precision matrix (non-cartesian case model 4)")

expect_equal({
  spflow:::compute_diag_precision_mat(
    DW = DW,
    OW = OW,
    rho = rho,
    n_o = nrow(OW),
    n_d = nrow(DW),
    M_indicator = I_gamma)
},
{
  A <- diag(nrow(OW) * nrow(DW)) -
    diag(nrow(OW)) %x% DW * rho["rho_d"] -
    OW %x% diag(nrow(DW)) * rho["rho_o"] -
    OW %x% DW * rho["rho_w"]
  res <- I_gamma
  res[sg] <- diag(crossprod(A[sg,sg]))
  res
},
info = "test calculation of the diagonal of the precision matrix (non-cartesian case model 9)")


rm(OW,DW,rho,I_gamma, sg)


