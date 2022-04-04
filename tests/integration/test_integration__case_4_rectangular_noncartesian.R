# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Project: spflow - integration test case 3
# Author: Lukas Dargel
# = = = = = = = = = = = = = = = = = = =
# Description:
#
# The script tests the integration of the package functions based on the
# simulated flows from the stylized states of the USA to those of Germany.
# The test case covers:
# - "model_1" and "model_2" and "model_9"
# - "non-cartesian" flows: as subset of all possible OD pairs are used
# - "rectangular" flows:  connecting two different networks
# - estimators: "ols" and "twosls" (exact tests)
# - estimators: "mle" and "mcmc"   (approximate tests)
# = = = = = = = = = = = = = = = = = = =
# Date: Feb 2022
opts <- options(warn = 1)


# cran packages
library("spflow")
library("Matrix")

# data
data("multi_net_usa_ge")
data("simulation_params")

test_dir <- ""
# test_dir <- "tests/integration/" # uncomment for interactive check
net_pair <- "usa_ge"
usa_ge_vec_data <-
  readRDS(paste0(test_dir,"vec_data_usa_ge.Rds"))[[net_pair]]
usa_ge_pairnb <-
  readRDS(paste0(test_dir,"pair_neighborhoods_usa_ge.Rds"))[[net_pair]]

# ---- define target objects --------------------------------------------------
# ---- ... model matrices -----------------------------------------------------
OW <- neighborhood(multi_net_usa_ge, "usa")
DW <- neighborhood(multi_net_usa_ge, "ge")
n_o <- nrow(OW)
n_d <- nrow(DW)

OX <- dat(multi_net_usa_ge, "usa")[,"X", drop = FALSE]
OX[["X_lag.1"]] <- as.vector(OW %*% OX$X)
OX_inst <- data.frame(
  "X_lag.2" = as.vector(OW %*% OX$X_lag.1),
  "X_lag.3" = as.vector(OW %*% OW %*% OX$X_lag.1))

DX <- dat(multi_net_usa_ge, "ge")[,"X", drop = FALSE]
DX[["X_lag.1"]] <- as.vector(DW %*% DX$X)
DX_inst <- data.frame(
  "X_lag.2" = as.vector(DW %*% DX$X_lag.1),
  "X_lag.3" = as.vector(DW %*% DW %*% DX$X_lag.1))

od_dat <- dat(multi_net_usa_ge, net_pair)
o_index <- as.integer(od_dat[["ID_ORIG"]])
d_index <- as.integer(od_dat[["ID_DEST"]])
sparse_matrix_form <- function(vec) {
  sparseMatrix(i = d_index,
               j = o_index,
               x = vec,
               dims = c(n_d,n_o))
}
dense_matrix_form <- function(vec) {
  mat <- matrix(0, n_d,n_o)
  mat[cbind(d_index, o_index)] <- vec
  return(mat)
}

flow_indicator <- dense_matrix_form(1)
target_matrices <- list(
  "D_" = as.matrix(DX),
  "O_" = as.matrix(OX),
  "OW" = OW,
  "DW" = DW,
  "G_"  = list(
    "DISTANCE" = dense_matrix_form(usa_ge_vec_data[,"DISTANCE"])),
  "Y1_" = list(
    "y1" = dense_matrix_form(usa_ge_vec_data[,"y1"])),
  "Y2_" = list(
    "y2" = dense_matrix_form(usa_ge_vec_data[,"y2"]),
    "y2.d" = flow_indicator * (DW %*% dense_matrix_form(usa_ge_vec_data[,"y2"]))),
  "Y9_" = list(
    "y9" = dense_matrix_form(usa_ge_vec_data[,"y9"]),
    "y9.d" = flow_indicator * (DW %*% dense_matrix_form(usa_ge_vec_data[,"y9"])),
    "y9.o" = flow_indicator * tcrossprod(dense_matrix_form(usa_ge_vec_data[,"y9"]),OW),
    "y9.w" = flow_indicator * tcrossprod(DW %*% dense_matrix_form(usa_ge_vec_data[,"y9"]),OW)))


# ---- ... moments ------------------------------------------------------------
dep_vars <- paste0("y", c(9,2,1))
Z <- usa_ge_vec_data[,!colnames(usa_ge_vec_data) %in% dep_vars]

## derive lags
od_indicator <- as.logical(as.vector(flow_indicator))
W_o <- OW %x% diag(n_d)
W_o <- W_o[od_indicator,od_indicator]
W_d <- diag(n_o) %x% DW
W_d <- W_d[od_indicator,od_indicator]
W_w <- OW %x% DW
W_w <- W_w[od_indicator,od_indicator]

# lags of flows
Y_t2 <- usa_ge_vec_data[,"y2", drop = FALSE]
Y_t2 <- cbind(
  Y_t2,
  "y2.d"= as.vector(W_d %*% Y_t2))
Y_t9 <- usa_ge_vec_data[,"y9", drop = FALSE]
Y_t9 <- cbind(
  Y_t9,
  "y9.d" = as.vector(W_d %*% Y_t9),
  "y9.o" = as.vector(W_o %*% Y_t9),
  "y9.w" = as.vector(W_w %*% Y_t9))


# lags and instruments
U_alpha <- usa_ge_vec_data[,"(Intercept)", drop = FALSE]

lag_names <- c("", paste0(".lag",1:3))
U_beta_d <- usa_ge_vec_data[,c("DEST_X","DEST_X.lag1")]
U_beta_d <- cbind(U_beta_d,as.matrix(DX_inst[d_index,]))
colnames(U_beta_d) <- paste0("DEST_X", lag_names)

U_beta_o <- usa_ge_vec_data[,c("ORIG_X","ORIG_X.lag1")]
U_beta_o <- cbind(U_beta_o,as.matrix(OX_inst[o_index,]))
colnames(U_beta_o) <- paste0("ORIG_X", lag_names)

U_gamma <- usa_ge_vec_data[,"DISTANCE"]
U_gamma <- cbind(U_gamma, W_w %*% U_gamma, W_w %*% W_w %*% U_gamma)
colnames(U_gamma) <- paste0("DISTANCE", c("", ".wGw", ".wwGww"))
U <- cbind(U_alpha,U_beta_d,U_beta_o,U_gamma)

target_moments <- list(
  # all models
  "ZZ"   = as.matrix(crossprod(Z)),
  "UU"   = as.matrix(crossprod(U)),
  # model 1
  "ZY1"  = as.matrix(crossprod(Z,usa_ge_vec_data[,"y1", drop = FALSE])),
  "TSS1" = as.matrix(crossprod(usa_ge_vec_data[,"y1", drop = FALSE])),
  # model 2
  "ZY2"  = as.matrix(crossprod(Z,Y_t2)),
  "UY2"  = as.matrix(crossprod(U,Y_t2)),
  "TSS2" = as.matrix(crossprod(Y_t2)),
  # model 9
  "ZY9"  = as.matrix(crossprod(Z,Y_t9)),
  "UY9"  = as.matrix(crossprod(U,Y_t9)),
  "TSS9" = as.matrix(crossprod(Y_t9)))

# ---- ... results ------------------------------------------------------------

# ols_results
N_s <- nrow(usa_ge_vec_data)
delta1_ols <- solve(
  as.matrix(target_moments[["ZZ"]]),
  as.vector(target_moments[["ZY1"]]))
e1 <- usa_ge_vec_data[,"y1"] - (Z %*% delta1_ols)
sigma1_ols <- as.vector(sqrt(crossprod(e1)/N_s))


# s2sls results for model 2
L2_hat <- U %*% solve(
  target_moments[["UU"]],
  target_moments[["UY2"]][, -1])
colnames(L2_hat) <- "rho_d"
Z2_hat <- cbind(L2_hat,Z)
ZZ2 <- crossprod(Z2_hat)
ZY2_hat <- as.vector(crossprod(Z2_hat,usa_ge_vec_data[,"y2",drop = FALSE]))
mu2_s2sls <- solve(as.matrix(ZZ2),ZY2_hat)

ZY2_tilde <- cbind(Y_t2[,-1], Z)
e2 <- usa_ge_vec_data[,"y2"] - ZY2_tilde %*%  mu2_s2sls
sigma2_s2sls <- as.vector(sqrt(crossprod(e2)/N_s))


# s2sls results for model 9
L9_hat <- U %*% solve(
  target_moments[["UU"]],
  target_moments[["UY9"]][, -1])
colnames(L9_hat) <- paste0("rho_",c("d","o","w"))
Z9_hat <- cbind(L9_hat,Z)
ZZ9 <- crossprod(Z9_hat)
ZY9_hat <- as.vector(crossprod(Z9_hat,usa_ge_vec_data[,"y9",drop = FALSE]))
mu9_s2sls <- solve(as.matrix(ZZ9),ZY9_hat)

ZY9_tilde <- cbind(Y_t9[,-1], Z)
e9 <- usa_ge_vec_data[,"y9"] - ZY9_tilde %*%  mu9_s2sls
sigma9_s2sls <- as.vector(sqrt(crossprod(e9)/N_s))

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
  "mu2_input" = c(
    simulation_params$rho["rho_d"],
    simulation_params$delta[names(delta1_ols)]),
  "mu9_input" = c(
    simulation_params$rho,
    simulation_params$delta[names(delta1_ols)]),
  "sigma_input" = simulation_params$sd_error
)

# ---- run tests --------------------------------------------------------------
expect_zero_diff <- function(y,x) expect_equal(max(abs(x - y)), 0)

# ---- ... ols - model 1 ------------------------------------------------------
res_model_1_ols <- spflow(
  y1 ~ . + G_(DISTANCE), multi_net_usa_ge, net_pair,
  spflow_control(estimation_method = "ols", model = "model_1"))

# test results
expect_inherits(res_model_1_ols, "spflow_model_ols")
expect_true(res_model_1_ols@R2_corr > 0)
expect_equal(target_results$delta1_ols, coef(res_model_1_ols))
expect_equal(target_results$sigma1_ols, sd_error(res_model_1_ols))


# test moments
actual_moments <- res_model_1_ols@model_moments
expect_zero_diff(target_moments[["ZZ"]], actual_moments[["ZZ"]])
expect_zero_diff(target_moments[["ZY1"]], actual_moments[["ZY"]])
expect_zero_diff(target_moments[["TSS1"]], actual_moments[["TSS"]])

# test model matrices
actual_matrices <- res_model_1_ols@design_matrix
expect_zero_diff(target_matrices[["OW"]], actual_matrices[["OW"]])
expect_zero_diff(target_matrices[["D_"]], actual_matrices[["D_"]])
expect_zero_diff(target_matrices[["O_"]], actual_matrices[["O_"]])
expect_zero_diff(target_matrices[["G_"]][[1]], actual_matrices[["G_"]][[1]])
expect_zero_diff(target_matrices[["Y1_"]][[1]], actual_matrices[["Y_"]][[1]])
rm(res_model_1_ols)

# ---- ... s2sls - model 2 ----------------------------------------------------
res_model_2_s2sls <- spflow(
  y2 ~ . + G_(DISTANCE), multi_net_usa_ge, net_pair,
  spflow_control(estimation_method = "s2sls", model = "model_2"))

# test results
expect_inherits(res_model_2_s2sls, "spflow_model_s2sls")
expect_equal(target_results$mu2_s2sls, coef(res_model_2_s2sls))
expect_equal(target_results$sigma2_s2sls, sd_error(res_model_2_s2sls))

# test moments
actual_moments <- res_model_2_s2sls@model_moments
expect_zero_diff(target_moments[["ZZ"]], actual_moments[["ZZ"]])
expect_zero_diff(target_moments[["UU"]], actual_moments[["UU"]])
expect_zero_diff(target_moments[["ZY2"]], actual_moments[["ZY"]])
expect_zero_diff(target_moments[["UY2"]], actual_moments[["UY"]])
expect_zero_diff(target_moments[["TSS2"]], actual_moments[["TSS"]])

# test model matrices
actual_matrices <- res_model_2_s2sls@design_matrix
expect_zero_diff(target_matrices[["OW"]], actual_matrices[["OW"]])
expect_zero_diff(target_matrices[["D_"]], actual_matrices[["D_"]])
expect_zero_diff(target_matrices[["O_"]], actual_matrices[["O_"]])
expect_zero_diff(target_matrices[["G_"]][[1]], actual_matrices[["G_"]][[1]])
expect_zero_diff(target_matrices[["Y2_"]][[1]], actual_matrices[["Y_"]][[1]])
expect_zero_diff(target_matrices[["Y2_"]][[2]], actual_matrices[["Y_"]][[2]])
rm(res_model_2_s2sls)

# ---- ... s2sls - model 9 ----------------------------------------------------
res_model_9_s2sls <- spflow(
  y9 ~ . + G_(DISTANCE), multi_net_usa_ge, net_pair,
  spflow_control(estimation_method = "s2sls", model = "model_9"))

# test results
expect_inherits(res_model_9_s2sls, "spflow_model_s2sls")
expect_equal(target_results$mu9_s2sls, coef(res_model_9_s2sls))
expect_equal(target_results$sigma9_s2sls, sd_error(res_model_9_s2sls))

# test moments
actual_moments <- res_model_9_s2sls@model_moments
expect_zero_diff(target_moments[["ZZ"]], actual_moments[["ZZ"]])
expect_zero_diff(target_moments[["UU"]], actual_moments[["UU"]])
expect_zero_diff(target_moments[["ZY9"]], actual_moments[["ZY"]])
expect_zero_diff(target_moments[["UY9"]], actual_moments[["UY"]])
expect_zero_diff(target_moments[["TSS9"]], actual_moments[["TSS"]])

# test model matrices
actual_matrices <- res_model_9_s2sls@design_matrix
expect_zero_diff(target_matrices[["OW"]], actual_matrices[["OW"]])
expect_zero_diff(target_matrices[["D_"]], actual_matrices[["D_"]])
expect_zero_diff(target_matrices[["O_"]], actual_matrices[["O_"]])
expect_zero_diff(target_matrices[["G_"]][[1]], actual_matrices[["G_"]][[1]])
expect_zero_diff(target_matrices[["Y9_"]][[1]], actual_matrices[["Y_"]][[1]])
expect_zero_diff(target_matrices[["Y9_"]][[2]], actual_matrices[["Y_"]][[2]])
expect_zero_diff(target_matrices[["Y9_"]][[3]], actual_matrices[["Y_"]][[3]])
expect_zero_diff(target_matrices[["Y9_"]][[4]], actual_matrices[["Y_"]][[4]])
rm(res_model_9_s2sls)

# ---- ... mle - model 2 ------------------------------------------------------
res_model_2_mle <- spflow(
  y2 ~ . + G_(DISTANCE), multi_net_usa_ge, net_pair,
  spflow_control(estimation_method = "mle", model = "model_2"))

# test results
expect_inherits(res_model_2_mle, "spflow_model_mle")
expect_equal(names(target_results$mu2_input),
             names(coef(res_model_2_mle)))
expect_equal(target_results$mu2_input / coef(res_model_2_mle),
             rep(1,7), tolerance = 0.3, check.names = FALSE)
expect_equal(target_results$sigma_input / sd_error(res_model_2_mle),
             rep(1,1), tolerance = 0.2, check.names = FALSE)

# test moments
actual_moments <- res_model_2_mle@model_moments
expect_zero_diff(target_moments[["ZZ"]],   actual_moments[["ZZ"]])
expect_zero_diff(target_moments[["ZY2"]],  actual_moments[["ZY"]])
expect_zero_diff(target_moments[["TSS2"]], actual_moments[["TSS"]])

# test model matrices
actual_matrices <- res_model_2_mle@design_matrix
expect_zero_diff(target_matrices[["OW"]], actual_matrices[["OW"]])
expect_zero_diff(target_matrices[["D_"]], actual_matrices[["D_"]])
expect_zero_diff(target_matrices[["O_"]], actual_matrices[["O_"]])
expect_zero_diff(target_matrices[["G_"]][[1]], actual_matrices[["G_"]][[1]])
expect_zero_diff(target_matrices[["Y2_"]][[1]], actual_matrices[["Y_"]][[1]])
expect_zero_diff(target_matrices[["Y2_"]][[2]], actual_matrices[["Y_"]][[2]])
rm(res_model_2_mle)

# ---- ... mle - model 9 ------------------------------------------------------
res_model_9_mle <- spflow(
  y9 ~ . + G_(DISTANCE), multi_net_usa_ge, net_pair,
  spflow_control(estimation_method = "mle", model = "model_9"))

# test results
expect_inherits(res_model_9_mle, "spflow_model_mle")
expect_equal(names(target_results$mu9_input), names(coef(res_model_9_mle)))
expect_equal(target_results$mu9_input / coef(res_model_9_mle),
             rep(1,9), tolerance = 0.3, check.names = FALSE)
expect_equal(target_results$sigma_input / sd_error(res_model_9_mle),
             rep(1,1), tolerance = 0.1, check.names = FALSE)

# test moments
actual_moments <- res_model_9_mle@model_moments
expect_zero_diff(target_moments[["ZZ"]],   actual_moments[["ZZ"]])
expect_zero_diff(target_moments[["ZY9"]],  actual_moments[["ZY"]])
expect_zero_diff(target_moments[["TSS9"]], actual_moments[["TSS"]])

# test model matrices
actual_matrices <- res_model_9_mle@design_matrix
expect_zero_diff(target_matrices[["OW"]], actual_matrices[["OW"]])
expect_zero_diff(target_matrices[["D_"]], actual_matrices[["D_"]])
expect_zero_diff(target_matrices[["O_"]], actual_matrices[["O_"]])
expect_zero_diff(target_matrices[["G_"]][[1]], actual_matrices[["G_"]][[1]])
expect_zero_diff(target_matrices[["Y9_"]][[1]], actual_matrices[["Y_"]][[1]])
expect_zero_diff(target_matrices[["Y9_"]][[2]], actual_matrices[["Y_"]][[2]])
expect_zero_diff(target_matrices[["Y9_"]][[3]], actual_matrices[["Y_"]][[3]])
expect_zero_diff(target_matrices[["Y9_"]][[4]], actual_matrices[["Y_"]][[4]])
rm(res_model_9_mle)

# ---- ... mcmc - model 2 -----------------------------------------------------
res_model_2_mcmc <- spflow(
  y2 ~ . + G_(DISTANCE), multi_net_usa_ge, net_pair,
  spflow_control(estimation_method = "mcmc", model = "model_2"))

# test results
expect_inherits(res_model_2_mcmc, "spflow_model_mcmc")
expect_equal(names(target_results$mu2_input), names(coef(res_model_2_mcmc)))
expect_equal(target_results$mu2_input / coef(res_model_2_mcmc),
             rep(1,7), tolerance = 0.3, check.names = FALSE)
expect_equal(target_results$sigma_input / sd_error(res_model_2_mcmc),
             rep(1,1), tolerance = 0.1, check.names = FALSE)

# test moments
actual_moments <- res_model_2_mcmc@model_moments
expect_zero_diff(target_moments[["ZZ"]],   actual_moments[["ZZ"]])
expect_zero_diff(target_moments[["ZY2"]],  actual_moments[["ZY"]])
expect_zero_diff(target_moments[["TSS2"]], actual_moments[["TSS"]])

# test model matrices
actual_matrices <- res_model_2_mcmc@design_matrix
expect_zero_diff(target_matrices[["OW"]], actual_matrices[["OW"]])
expect_zero_diff(target_matrices[["D_"]], actual_matrices[["D_"]])
expect_zero_diff(target_matrices[["O_"]], actual_matrices[["O_"]])
expect_zero_diff(target_matrices[["G_"]][[1]], actual_matrices[["G_"]][[1]])
expect_zero_diff(target_matrices[["Y2_"]][[1]], actual_matrices[["Y_"]][[1]])
expect_zero_diff(target_matrices[["Y2_"]][[2]], actual_matrices[["Y_"]][[2]])
rm(res_model_2_mcmc)

# ---- ... mcmc - model 9 -----------------------------------------------------
res_model_9_mcmc <- spflow(
  y9 ~ . + G_(DISTANCE), multi_net_usa_ge, net_pair,
  spflow_control(estimation_method = "mcmc", model = "model_9"))

# test results
expect_inherits(res_model_9_mcmc, "spflow_model_mcmc")
expect_equal(names(target_results$mu9_input), names(coef(res_model_9_mcmc)))
expect_equal(target_results$mu9_input / coef(res_model_9_mcmc),
             rep(1,9), tolerance = 0.3, check.names = FALSE)
expect_equal(target_results$sigma_input / sd_error(res_model_9_mcmc),
             rep(1,1), tolerance = 0.1, check.names = FALSE)

# test moments
actual_moments <- res_model_9_mcmc@model_moments
expect_zero_diff(target_moments[["ZZ"]],   actual_moments[["ZZ"]])
expect_zero_diff(target_moments[["ZY9"]],  actual_moments[["ZY"]])
expect_zero_diff(target_moments[["TSS9"]], actual_moments[["TSS"]])

# test model matrices
actual_matrices <- res_model_9_mcmc@design_matrix
expect_zero_diff(target_matrices[["OW"]], actual_matrices[["OW"]])
expect_zero_diff(target_matrices[["D_"]], actual_matrices[["D_"]])
expect_zero_diff(target_matrices[["O_"]], actual_matrices[["O_"]])
expect_zero_diff(target_matrices[["G_"]][[1]], actual_matrices[["G_"]][[1]])
expect_zero_diff(target_matrices[["Y9_"]][[1]], actual_matrices[["Y_"]][[1]])
expect_zero_diff(target_matrices[["Y9_"]][[2]], actual_matrices[["Y_"]][[2]])
expect_zero_diff(target_matrices[["Y9_"]][[3]], actual_matrices[["Y_"]][[3]])
expect_zero_diff(target_matrices[["Y9_"]][[4]], actual_matrices[["Y_"]][[4]])
rm(res_model_9_mcmc)
options(opts)


# ---- test NA's handling -----------------------------------------------------
multi_net_usa_ge2 <- complete_pairs(
  multi_net_usa_ge,
  network_pair_ids = net_pair,
  make_cartesian = TRUE)


expect_equal(npairs(multi_net_usa_ge2, net_pair), 51*16)
expect_error(spflow(
  y9 ~ . + G_(DISTANCE), multi_net_usa_ge2, net_pair,
  spflow_control(estimation_method = "s2sls", model = "model_9")))


res_model_9_s2sls_narm <- spflow(
  flow_formula = y9 ~ . + G_(DISTANCE), multi_net_usa_ge2,
  network_pair_id =  net_pair,
  flow_control = spflow_control(estimation_method = "s2sls", model = "model_9"),
  na_rm = TRUE)


# test results
expect_inherits(res_model_9_s2sls_narm, "spflow_model")
expect_equal(names(target_results$mu9_input), names(coef(res_model_9_s2sls_narm)))
expect_equal(target_results$mu9_s2sls, coef(res_model_9_s2sls_narm))
expect_equal(target_results$sigma9_s2sls, sd_error(res_model_9_s2sls_narm))

# test moments
actual_moments <- res_model_9_s2sls_narm@model_moments
expect_zero_diff(target_moments[["ZZ"]],   actual_moments[["ZZ"]])
expect_zero_diff(target_moments[["ZY9"]],  actual_moments[["ZY"]])
expect_zero_diff(target_moments[["TSS9"]], actual_moments[["TSS"]])

# test model matrices
actual_matrices <- res_model_9_s2sls_narm@design_matrix
expect_zero_diff(target_matrices[["OW"]], actual_matrices[["OW"]])
expect_zero_diff(target_matrices[["D_"]], actual_matrices[["D_"]])
expect_zero_diff(target_matrices[["O_"]], actual_matrices[["O_"]])
expect_zero_diff(target_matrices[["G_"]][[1]], actual_matrices[["G_"]][[1]])
expect_zero_diff(target_matrices[["Y9_"]][[1]], actual_matrices[["Y_"]][[1]])
expect_zero_diff(target_matrices[["Y9_"]][[2]], actual_matrices[["Y_"]][[2]])
expect_zero_diff(target_matrices[["Y9_"]][[3]], actual_matrices[["Y_"]][[3]])
expect_zero_diff(target_matrices[["Y9_"]][[4]], actual_matrices[["Y_"]][[4]])

expect_equal({
  results(spflow(
    y9 ~ . + G_(DISTANCE), multi_net_usa_ge, net_pair,
    spflow_control(estimation_method = "s2sls", model = "model_9")))
},
{
  results(res_model_9_s2sls_narm)
})

rm(res_model_9_s2sls_narm)


