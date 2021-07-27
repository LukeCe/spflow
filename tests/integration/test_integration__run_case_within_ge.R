# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Project: spflow - integration test 1
# Author: Lukas Dargel
# = = = = = = = = = = = = = = = = = = =
# Description:
#
# The script tests the integration of the package function based on the
# simulated flows within the stylised states of Germany.
# The test case covers:
# - "model_1" and "model_2" and "model_9"
# - "within" flows (origin == destination)
# - "complete" flows (N = n_o * n_d)
# - estimators: "ols" and "twosls" (exact)
# - estimators: "mle" and "mcmc"   (approximate)
# = = = = = = = = = = = = = = = = = = =
# Date: Mai 2021

# ==== [+++ Setup test cases +++] =============================================
library("spflow")
library("Matrix")
data("multi_net_usa_ge")
data("simulation_params")
# test_dir <- "tests/integration/" # uncomment for interactive check
test_dir <- ""
ge_ge_vec_data <-
  readRDS(paste0(test_dir,"vec_data_usa_ge.Rds"))[["ge_ge"]]
ge_ge_pairnb <-
  readRDS(paste0(test_dir,"pair_neighborhoods_usa_ge.Rds"))[["ge_ge"]]


# ---- target model matrices --------------------------------------------------
W <- neighborhood(multi_net_usa_ge, "ge")
OX <- dat(multi_net_usa_ge, "ge")[,"X", drop = FALSE]
OX[["X_lag.1"]] <- as.vector(W %*% OX$X)
n <- 16

target_matrices <- list(
  "D_" = as.matrix(OX),
  "O_" = as.matrix(OX),
  "I_" = as.matrix(OX),
  "OW" = W,
  "G_"  = list("DISTANCE" = matrix(ge_ge_vec_data[,"DISTANCE"],n,n)),
  "Y1_" = list("y1" = matrix(ge_ge_vec_data[,"y1"],n,n)),
  "Y2_" = list("y2" = matrix(ge_ge_vec_data[,"y2"],n,n),
               "y2.d" = W %*% matrix(ge_ge_vec_data[,"y2"],n,n)),
  "Y9_" = list(
    "y9" = matrix(ge_ge_vec_data[,"y9"],n,n),
    "y9.d" = W %*% matrix(ge_ge_vec_data[,"y9"],n,n),
    "y9.o" = tcrossprod(matrix(ge_ge_vec_data[,"y9"],n,n),W),
    "y9.w" = tcrossprod(W %*% matrix(ge_ge_vec_data[,"y9"],n,n),W))
  )


# ---- target moments ---------------------------------------------------------
dep_vars <- paste0("y", c(9,2,1))
Z <- ge_ge_vec_data[,!colnames(ge_ge_vec_data) %in% dep_vars]

## derive lags
W_o <- W %x% diag(n)
W_d <- diag(n) %x% W
W_w <- W %x% W

# lags of flows
Y_t2 <- ge_ge_vec_data[,"y2", drop = FALSE]
Y_t2 <- cbind(
  Y_t2,
  "y2.d"= as.vector(W_d %*% Y_t2))
Y_t9 <- ge_ge_vec_data[,"y9", drop = FALSE]
Y_t9 <- cbind(
  Y_t9,
  "y9.d" = as.vector(W_d %*% Y_t9),
  "y9.o" = as.vector(W_o %*% Y_t9),
  "y9.w" = as.vector(W_w %*% Y_t9))

# lags as instruments
consts <- c("(Intercept)","(Intra)")
iota_I <- ge_ge_vec_data[,"(Intra)"]
U_alpha <- cbind(
  ge_ge_vec_data[,consts, drop = FALSE],
  "W"   = as.vector(W_d %*% iota_I),
  "W'"  = as.vector(W_o %*% iota_I),
  "WW"  = as.vector(W_d %*% W_d %*% iota_I),
  "WW'" = as.vector(W_o %*% W_o %*% iota_I),
  "V"   = as.vector(W_w %*% iota_I),
  "VV"  = as.vector(W_w %*% W_w %*% iota_I),
  "WV"  = as.vector(W_d %*% W_w %*% iota_I),
  "VW'" = as.vector(W_o %*% W_w %*% iota_I))

lag_names <- c("", paste0(".lag",1:3))
U_beta_d <- ge_ge_vec_data[,c("DEST_X","DEST_X.lag1")]
U_beta_d <- cbind(U_beta_d,W_d %*% W_d %*% U_beta_d)
colnames(U_beta_d) <- paste0("DEST_X", lag_names)
U_beta_o <- ge_ge_vec_data[,c("ORIG_X","ORIG_X.lag1")]
U_beta_o <- cbind(U_beta_o,W_o %*% W_o %*% U_beta_o)
colnames(U_beta_o) <- paste0("ORIG_X", lag_names)
U_beta_I <- U_beta_o * iota_I
colnames(U_beta_I) <- paste0("INTRA_X", lag_names)

U_gamma <- ge_ge_vec_data[,"DISTANCE"]
U_gamma <- cbind(U_gamma, W_w %*% U_gamma, W_w %*% W_w %*% U_gamma)
colnames(U_gamma) <- paste0("DISTANCE", c("", ".wGw", ".wwGww"))
U <- cbind(U_alpha,U_beta_d,U_beta_o,U_beta_I,U_gamma)

target_moments <- list(
  # all models
  "ZZ"   = as.matrix(crossprod(Z)),
  "UU"   = as.matrix(crossprod(U)),
  # model 1
  "ZY1"  = as.matrix(crossprod(Z,ge_ge_vec_data[,"y1", drop = FALSE])),
  "TSS1" = as.matrix(crossprod(ge_ge_vec_data[,"y1", drop = FALSE])),
  # model 2
  "ZY2"  = as.matrix(crossprod(Z,Y_t2)),
  "UY2"  = as.matrix(crossprod(U,Y_t2)),
  "TSS2" = as.matrix(crossprod(Y_t2)),
  # model 9
  "ZY9"  = as.matrix(crossprod(Z,Y_t9)),
  "UY9"  = as.matrix(crossprod(U,Y_t9)),
  "TSS9" = as.matrix(crossprod(Y_t9)))

# ---- target results ---------------------------------------------------------

# ols_results
delta1_ols <- solve(as.matrix(target_moments[["ZZ"]]),
                    as.vector(target_moments[["ZY1"]]))
e1 <- ge_ge_vec_data[,"y1"] - (Z %*% delta1_ols)
sigma1_ols <- as.vector(sqrt(crossprod(e1)/n^2))


# s2sls results for model 2
L2_hat <- U %*% solve(
  target_moments[["UU"]],
  target_moments[["UY2"]][, -1])
colnames(L2_hat) <- "rho_d"
Z2_hat <- cbind(L2_hat,Z)
ZZ2 <- crossprod(Z2_hat)
ZY2_hat <- as.vector(crossprod(Z2_hat,ge_ge_vec_data[,"y2",drop = FALSE]))
mu2_s2sls <- solve(as.matrix(ZZ2),ZY2_hat)

ZY2_tilde <- cbind(Y_t2[,-1], Z)
e2 <- ge_ge_vec_data[,"y2"] - ZY2_tilde %*%  mu2_s2sls
sigma2_s2sls <- crossprod(e2)
sigma2_s2sls <- as.vector(sqrt(sigma2_s2sls)/n)


# s2sls results for model 9
L9_hat <- U %*% solve(
  target_moments[["UU"]],
  target_moments[["UY9"]][, -1])
colnames(L9_hat) <- paste0("rho_",c("d","o","w"))
Z9_hat <- cbind(L9_hat,Z)
ZZ9 <- crossprod(Z9_hat)
ZY9_hat <- as.vector(crossprod(Z9_hat,ge_ge_vec_data[,"y9",drop = FALSE]))
mu9_s2sls <- solve(as.matrix(ZZ9),ZY9_hat)

ZY9_tilde <- cbind(Y_t9[,-1], Z)
e9 <- ge_ge_vec_data[,"y9"] - ZY9_tilde %*%  mu9_s2sls
sigma9_s2sls <- crossprod(e9)
sigma9_s2sls <- as.vector(sqrt(sigma9_s2sls)/n)

# all results
target_results <- list(
  # ols
  "delta1_ols" = delta1_ols,
  "sigma1_ols" = sigma1_ols,
  # s2sls
  "mu2_s2sls" = mu2_s2sls,
  "sigma2_s2sls" = sigma2_s2sls,
  "mu9_s2sls" = mu9_s2sls,
  "sigma9_s2sls" = sigma9_s2sls,
  # ground truth
  "mu2_input" = c(simulation_params$rho["rho_d"], simulation_params$delta),
  "mu9_input" = c(simulation_params$rho, simulation_params$delta),
  "sigma_input" = simulation_params$sd_error
)

# ==== [+++ Run tests +++] ====================================================

# ---- ols - model 1 ----------------------------------------------------------
res_model_1_ols <- spflow(
  y1 ~ . + G_(DISTANCE), multi_net_usa_ge, "ge_ge",
  spflow_control(estimation_method = "ols", model = "model_1"))

# test results
expect_inherits(res_model_1_ols, "spflow_model_ols")
expect_equal(target_results$delta1_ols, coef(res_model_1_ols))
expect_equal(target_results$sigma1_ols, sd_error(res_model_1_ols))

# test moments
actual_moments <- res_model_1_ols@model_moments
expect_equal(target_moments[["ZZ"]], actual_moments[["ZZ"]])
expect_equal(target_moments[["ZY1"]], actual_moments[["ZY"]])
expect_equal(target_moments[["TSS1"]], actual_moments[["TSS"]])

# test model matrices
actual_matrices <- res_model_1_ols@design_matrix
expect_equal(target_matrices[["OW"]], actual_matrices[["OW"]])
expect_equal(target_matrices[["D_"]], actual_matrices[["D_"]],
             check.attributes = FALSE)
expect_equal(target_matrices[["O_"]], actual_matrices[["O_"]],
             check.attributes = FALSE)
expect_equal(target_matrices[["I_"]], actual_matrices[["I_"]],
             check.attributes = FALSE)
expect_equal(target_matrices[["G_"]], actual_matrices[["G_"]],
             check.attributes = FALSE)
expect_equal(target_matrices[["Y1_"]], actual_matrices[["Y_"]],
             check.attributes = FALSE)
rm(res_model_1_ols)

# ---- s2sls - model 2 --------------------------------------------------------
res_model_2_s2sls <- spflow(
  y2 ~ . + G_(DISTANCE), multi_net_usa_ge, "ge_ge",
  spflow_control(estimation_method = "s2sls", model = "model_2"))

# test results
expect_inherits(res_model_2_s2sls, "spflow_model_s2sls")
expect_equal(target_results$mu2_s2sls, coef(res_model_2_s2sls))
expect_equal(target_results$sigma2_s2sls, sd_error(res_model_2_s2sls))

# test moments
actual_moments <- res_model_2_s2sls@model_moments
expect_equal(target_moments[["ZZ"]], actual_moments[["ZZ"]])
expect_equal(target_moments[["UU"]], actual_moments[["UU"]])
expect_equal(target_moments[["ZY2"]], actual_moments[["ZY"]])
expect_equal(target_moments[["UY2"]], actual_moments[["UY"]])
expect_equal(target_moments[["TSS2"]], actual_moments[["TSS"]])

# test model matrices
actual_matrices <- res_model_2_s2sls@design_matrix
expect_equal(target_matrices[["OW"]], actual_matrices[["OW"]])
expect_equal(target_matrices[["D_"]], actual_matrices[["D_"]],
             check.attributes = FALSE)
expect_equal(target_matrices[["O_"]], actual_matrices[["O_"]],
             check.attributes = FALSE)
expect_equal(target_matrices[["I_"]], actual_matrices[["I_"]],
             check.attributes = FALSE)
expect_equal(target_matrices[["G_"]], actual_matrices[["G_"]],
             check.attributes = FALSE)
expect_equal(target_matrices[["Y2_"]], actual_matrices[["Y_"]],
             check.attributes = FALSE)
rm(res_model_2_s2sls)

# ---- s2sls - model 9 --------------------------------------------------------
res_model_9_s2sls <- spflow(
  y9 ~ . + G_(DISTANCE), multi_net_usa_ge, "ge_ge",
  spflow_control(estimation_method = "s2sls", model = "model_9"))

# test results
expect_inherits(res_model_9_s2sls, "spflow_model_s2sls")
expect_equal(target_results$mu9_s2sls, coef(res_model_9_s2sls))
expect_equal(target_results$sigma9_s2sls, sd_error(res_model_9_s2sls))

# test moments
actual_moments <- res_model_9_s2sls@model_moments
expect_equal(target_moments[["ZZ"]], actual_moments[["ZZ"]])
expect_equal(target_moments[["UU"]], actual_moments[["UU"]])
expect_equal(target_moments[["ZY9"]], actual_moments[["ZY"]])
expect_equal(target_moments[["UY9"]], actual_moments[["UY"]])
expect_equal(target_moments[["TSS9"]], actual_moments[["TSS"]])

# test model matrices
actual_matrices <- res_model_9_s2sls@design_matrix
expect_equal(target_matrices[["OW"]], actual_matrices[["OW"]])
expect_equal(target_matrices[["D_"]], actual_matrices[["D_"]],
             check.attributes = FALSE)
expect_equal(target_matrices[["O_"]], actual_matrices[["O_"]],
             check.attributes = FALSE)
expect_equal(target_matrices[["I_"]], actual_matrices[["I_"]],
             check.attributes = FALSE)
expect_equal(target_matrices[["G_"]], actual_matrices[["G_"]],
             check.attributes = FALSE)
expect_equal(target_matrices[["Y9_"]], actual_matrices[["Y_"]],
             check.attributes = FALSE)
rm(res_model_9_s2sls)

# ---- mle - model 2 ----------------------------------------------------------
res_model_2_mle <- spflow(
  y2 ~ . + G_(DISTANCE), multi_net_usa_ge, "ge_ge",
  spflow_control(estimation_method = "mle", model = "model_2"))

# test results
expect_inherits(res_model_2_mle, "spflow_model_mle")
expect_equal(names(target_results$mu2_input),
             names(coef(res_model_2_mle)))
expect_equal(target_results$mu2_input / coef(res_model_2_mle),
             rep(1,10), tolerance = 0.3, check.names = FALSE)
expect_equal(target_results$sigma_input / sd_error(res_model_2_mle),
             rep(1,1), tolerance = 0.1, check.names = FALSE)

# test moments
actual_moments <- res_model_2_mle@model_moments
expect_equal(target_moments[["ZZ"]],   actual_moments[["ZZ"]])
expect_equal(target_moments[["ZY2"]],  actual_moments[["ZY"]])
expect_equal(target_moments[["TSS2"]], actual_moments[["TSS"]])

# test model matrices
actual_matrices <- res_model_2_mle@design_matrix
expect_equal(target_matrices[["OW"]], actual_matrices[["OW"]])
expect_equal(target_matrices[["D_"]], actual_matrices[["D_"]],
             check.attributes = FALSE)
expect_equal(target_matrices[["O_"]], actual_matrices[["O_"]],
             check.attributes = FALSE)
expect_equal(target_matrices[["I_"]], actual_matrices[["I_"]],
             check.attributes = FALSE)
expect_equal(target_matrices[["G_"]], actual_matrices[["G_"]],
             check.attributes = FALSE)
expect_equal(target_matrices[["Y2_"]], actual_matrices[["Y_"]],
             check.attributes = FALSE)
rm(res_model_2_mle)

# ---- mle - model 9 ----------------------------------------------------------
res_model_9_mle <- spflow(
  y9 ~ . + G_(DISTANCE), multi_net_usa_ge, "ge_ge",
  spflow_control(estimation_method = "mle", model = "model_9"))

# test results
expect_inherits(res_model_9_mle, "spflow_model_mle")
expect_equal(names(target_results$mu9_input), names(coef(res_model_9_mle)))
expect_equal(target_results$mu9_input / coef(res_model_9_mle),
             rep(1,12), tolerance = 0.3, check.names = FALSE)
expect_equal(target_results$sigma_input / sd_error(res_model_9_mle),
             rep(1,1), tolerance = 0.1, check.names = FALSE)

# test moments
actual_moments <- res_model_9_mle@model_moments
expect_equal(target_moments[["ZZ"]],   actual_moments[["ZZ"]])
expect_equal(target_moments[["ZY9"]],  actual_moments[["ZY"]])
expect_equal(target_moments[["TSS9"]], actual_moments[["TSS"]])

# test model matrices
actual_matrices <- res_model_9_mle@design_matrix
expect_equal(target_matrices[["OW"]], actual_matrices[["OW"]])
expect_equal(target_matrices[["D_"]], actual_matrices[["D_"]],
             check.attributes = FALSE)
expect_equal(target_matrices[["O_"]], actual_matrices[["O_"]],
             check.attributes = FALSE)
expect_equal(target_matrices[["I_"]], actual_matrices[["I_"]],
             check.attributes = FALSE)
expect_equal(target_matrices[["G_"]], actual_matrices[["G_"]],
             check.attributes = FALSE)
expect_equal(target_matrices[["Y9_"]], actual_matrices[["Y_"]],
             check.attributes = FALSE)
rm(res_model_9_mle)

# ---- mcmc - model 2 ---------------------------------------------------------
res_model_2_mcmc <- spflow(
  y2 ~ . + G_(DISTANCE), multi_net_usa_ge, "ge_ge",
  spflow_control(estimation_method = "mcmc", model = "model_2"))

# test results
expect_inherits(res_model_2_mcmc, "spflow_model_mcmc")
expect_equal(names(target_results$mu2_input), names(coef(res_model_2_mcmc)))
expect_equal(target_results$mu2_input / coef(res_model_2_mcmc),
             rep(1,10), tolerance = 0.3, check.names = FALSE)
expect_equal(target_results$sigma_input / sd_error(res_model_2_mcmc),
             rep(1,1), tolerance = 0.1, check.names = FALSE)

# test moments
actual_moments <- res_model_2_mcmc@model_moments
expect_equal(target_moments[["ZZ"]],   actual_moments[["ZZ"]])
expect_equal(target_moments[["ZY2"]],  actual_moments[["ZY"]])
expect_equal(target_moments[["TSS2"]], actual_moments[["TSS"]])

# test model matrices
actual_matrices <- res_model_2_mcmc@design_matrix
expect_equal(target_matrices[["OW"]], actual_matrices[["OW"]])
expect_equal(target_matrices[["D_"]], actual_matrices[["D_"]],
             check.attributes = FALSE)
expect_equal(target_matrices[["O_"]], actual_matrices[["O_"]],
             check.attributes = FALSE)
expect_equal(target_matrices[["I_"]], actual_matrices[["I_"]],
             check.attributes = FALSE)
expect_equal(target_matrices[["G_"]], actual_matrices[["G_"]],
             check.attributes = FALSE)
expect_equal(target_matrices[["Y2_"]], actual_matrices[["Y_"]],
             check.attributes = FALSE)
rm(res_model_2_mcmc)

# ---- mcmc - model 9 ---------------------------------------------------------
res_model_9_mcmc <- spflow(
  y9 ~ . + G_(DISTANCE), multi_net_usa_ge, "ge_ge",
  spflow_control(estimation_method = "mcmc", model = "model_9"))

# test results
expect_inherits(res_model_9_mcmc, "spflow_model_mcmc")
expect_equal(names(target_results$mu9_input), names(coef(res_model_9_mcmc)))
expect_equal(target_results$mu9_input / coef(res_model_9_mcmc),
             rep(1,12), tolerance = 0.3, check.names = FALSE)
expect_equal(target_results$sigma_input / sd_error(res_model_9_mcmc),
             rep(1,1), tolerance = 0.1, check.names = FALSE)

# test moments
actual_moments <- res_model_9_mcmc@model_moments
expect_equal(target_moments[["ZZ"]],   actual_moments[["ZZ"]])
expect_equal(target_moments[["ZY9"]],  actual_moments[["ZY"]])
expect_equal(target_moments[["TSS9"]], actual_moments[["TSS"]])

# test model matrices
actual_matrices <- res_model_9_mcmc@design_matrix
expect_equal(target_matrices[["OW"]], actual_matrices[["OW"]])
expect_equal(target_matrices[["D_"]], actual_matrices[["D_"]],
             check.attributes = FALSE)
expect_equal(target_matrices[["O_"]], actual_matrices[["O_"]],
             check.attributes = FALSE)
expect_equal(target_matrices[["I_"]], actual_matrices[["I_"]],
             check.attributes = FALSE)
expect_equal(target_matrices[["G_"]], actual_matrices[["G_"]],
             check.attributes = FALSE)
expect_equal(target_matrices[["Y9_"]], actual_matrices[["Y_"]],
             check.attributes = FALSE)
rm(res_model_9_mcmc)
