# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Project: spflow - integration test case 1
# Author: Lukas Dargel
# = = = = = = = = = = = = = = = = = = =
# Description:
#
# The script tests the integration of the package functions based on the
# simulated flows within the stylized states of Germany.
# The test case covers:
# - "model_1" and "model_2" and "model_9"
# - "Cartesian" flows: all possible OD pairs are included in the model
# - "Square" flows:  within the same network
# - estimators: "ols" and "twosls" (exact tests)
# - estimators: "mle" and "mcmc"   (approximate tests)
# = = = = = = = = = = = = = = = = = = =
# Date: Aug 2022

# cran packages
library("spflow")
library("Matrix")

# data
data("multi_net_usa_ge")
data("simulation_params")

test_dir <- ""
test_dir <- "tests/integration/" # uncomment for interactive check
ge_ge_vec_data <-
  readRDS(paste0(test_dir,"vec_data_usa_ge.Rds"))[["ge_ge"]]
ge_ge_pairnb <-
  readRDS(paste0(test_dir,"pair_neighborhoods_usa_ge.Rds"))[["ge_ge"]]


# ---- define target objects --------------------------------------------------
# ---- ... model matrices -----------------------------------------------------
W <- neighborhood(multi_net_usa_ge, "ge")
OX <- dat(multi_net_usa_ge, "ge")[,"X", drop = FALSE]
OX[["X_lag.1"]] <- as.vector(W %*% OX$X)
n <- 16

target_matrices <- list(
  "D_" = as.matrix(OX),
  "O_" = as.matrix(OX),
  "I_" = as.matrix(OX[,1]),
  "OW" = W,
  "P_"  = list("DISTANCE" = matrix(ge_ge_vec_data[,"P_DISTANCE"],n,n)),
  "Y1_" = list("y1" = matrix(ge_ge_vec_data[,"y1"],n,n)),
  "Y2_" = list("y2" = matrix(ge_ge_vec_data[,"y2"],n,n),
               "y2.d" = W %*% matrix(ge_ge_vec_data[,"y2"],n,n)),
  "Y9_" = list(
    "y9" = matrix(ge_ge_vec_data[,"y9"],n,n),
    "y9.d" = W %*% matrix(ge_ge_vec_data[,"y9"],n,n),
    "y9.o" = tcrossprod(matrix(ge_ge_vec_data[,"y9"],n,n),W),
    "y9.w" = tcrossprod(W %*% matrix(ge_ge_vec_data[,"y9"],n,n),W))
  )


# ---- ... moments ------------------------------------------------------------
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
U_alpha <- ge_ge_vec_data[,"(Intercept)", drop = FALSE]
iota_I <- ge_ge_vec_data[,"(Intra)"]
U_alpha_I <- cbind(
  "wI"   = as.vector(W_d %*% iota_I),
  "Iw"   = as.vector(W_o %*% iota_I),
  "wIw"  = as.vector(W_w %*% iota_I),
  "wwI"  = as.vector(W_d %*% W_d %*% iota_I),
  "Iww"  = as.vector(W_o %*% W_o %*% iota_I),
  "wwIw" = as.vector(W_d %*% W_w %*% iota_I),
  "wIww" = as.vector(W_o %*% W_w %*% iota_I),
  "wwIww'"  = as.vector(W_w %*% W_w %*% iota_I))
colnames(U_alpha_I) <- paste0("(Intra).", colnames(U_alpha_I))
U_alpha_I <- cbind("(Intra)" = iota_I, U_alpha_I)

lag_names <- c("", paste0(".lag",1:3))
U_beta_d <- ge_ge_vec_data[,c("D_X","D_X.lag1")]
U_beta_d <- cbind(U_beta_d,W_d %*% W_d %*% U_beta_d)
colnames(U_beta_d) <- paste0("D_X", lag_names)
U_beta_o <- ge_ge_vec_data[,c("O_X","O_X.lag1")]
U_beta_o <- cbind(U_beta_o,W_o %*% W_o %*% U_beta_o)
colnames(U_beta_o) <- paste0("O_X", lag_names)

lag_names <- c("", paste0(".lag",1:2))
U_beta_I <- U_beta_o[,1:3] * iota_I
colnames(U_beta_I) <- paste0("I_X", lag_names)

U_gamma <- ge_ge_vec_data[,"P_DISTANCE"]
U_gamma <- cbind(U_gamma, W_w %*% U_gamma, W_w %*% W_w %*% U_gamma)
colnames(U_gamma) <- paste0("P_DISTANCE", c("", ".wGw", ".wwGww"))
U <- cbind(U_alpha,U_alpha_I, U_beta_d,U_beta_o,U_beta_I,U_gamma)

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

# ---- ... results ------------------------------------------------------------

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

# ---- run tests --------------------------------------------------------------
expect_zero_diff <- function(y,x) expect_equal(max(abs(x - y)), 0)

# ---- ... ols - model 1 ------------------------------------------------------
res_model_1_ols <- spflow(
  spflow_formula = y1 ~ . + P_(DISTANCE),
  spflow_networks =  multi_net_usa_ge,
  id_spflow_pairs =  "ge_ge",
  estimation_control = spflow_control(estimation_method = "ols", model = "model_1"))

# test results
expect_inherits(res_model_1_ols, "spflow_model_ols")
expect_equal(target_results$delta1_ols, coef(res_model_1_ols))
expect_equal(target_results$sigma1_ols, sd_error(res_model_1_ols))

# test moments
actual_moments <- res_model_1_ols@spflow_moments
expect_zero_diff(target_moments[["ZZ"]], actual_moments[["ZZ"]])
expect_zero_diff(target_moments[["ZY1"]], actual_moments[["ZY"]])
expect_zero_diff(target_moments[["TSS1"]], actual_moments[["TSS"]])

# test model matrices
actual_matrices <- res_model_1_ols@spflow_matrices
expect_zero_diff(target_matrices[["D_"]], actual_matrices[["D_"]])
expect_zero_diff(target_matrices[["O_"]], actual_matrices[["O_"]])
expect_zero_diff(target_matrices[["I_"]], actual_matrices[["I_"]])
expect_zero_diff(target_matrices[["P_"]][[1]], actual_matrices[["P_"]][[1]])
expect_zero_diff(target_matrices[["Y1_"]][[1]], actual_matrices[["Y_"]][[1]])

# test residuals and goodness of fit
expectied_signal <- as.vector(Z %*% target_results$delta1_ols)
expect_zero_diff(expectied_signal, fitted(res_model_1_ols))
expect_zero_diff(expectied_signal, predict(res_model_1_ols, return_type = "V"))

# test refit
refit <- spflow_refit(res_model_1_ols, "stepwise")
expect_inherits(refit, "data.frame")
expect_equal(ncol(refit), length(coef(res_model_1_ols)))

refit <- spflow_refit(res_model_1_ols, "ar_family")
expect_inherits(refit, "data.frame")
expect_equal(ncol(refit), 9)

od_dropper <- function(drop_nodes) {
  ff <- function(x) !x[["ID_STATE"]] %in% drop_nodes
  list("orig" = ff, "dest" = ff)
}
wt_funs <- lapply(1:13, function(x) od_dropper(germany_grid$ID_STATE[seq(x)]))
names(wt_funs) <- paste0("drop",1:13)
refit <- spflow_refit(res_model_1_ols, "samples", sample_weights = wt_funs)
expect_inherits(refit, "data.frame")
expect_equal(ncol(refit), 14)

rm(res_model_1_ols)

# ---- ... s2sls - model 2 ----------------------------------------------------
res_model_2_s2sls <- spflow(
  spflow_formula = y2 ~ . + P_(DISTANCE),
  spflow_networks =  multi_net_usa_ge,
  id_spflow_pairs =  "ge_ge",
  estimation_control = spflow_control(estimation_method = "s2sls", model = "model_2"))

# test results
expect_inherits(res_model_2_s2sls, "spflow_model_s2sls")
expect_equal(target_results$mu2_s2sls, coef(res_model_2_s2sls))
expect_equal(target_results$sigma2_s2sls, sd_error(res_model_2_s2sls))

# test moments
actual_moments <- res_model_2_s2sls@spflow_moments
expect_zero_diff(target_moments[["ZZ"]], actual_moments[["ZZ"]])
expect_zero_diff(target_moments[["UU"]], actual_moments[["UU"]])
expect_zero_diff(target_moments[["ZY2"]], actual_moments[["ZY"]])
expect_zero_diff(target_moments[["UY2"]], actual_moments[["UY"]])
expect_zero_diff(target_moments[["TSS2"]], actual_moments[["TSS"]])

# test model matrices
actual_matrices <- res_model_2_s2sls@spflow_matrices
expect_zero_diff(target_matrices[["D_"]], actual_matrices[["D_"]])
expect_zero_diff(target_matrices[["O_"]], actual_matrices[["O_"]])
expect_zero_diff(target_matrices[["I_"]], actual_matrices[["I_"]])
expect_zero_diff(target_matrices[["P_"]][[1]], actual_matrices[["P_"]][[1]])
expect_zero_diff(target_matrices[["Y2_"]][[1]], actual_matrices[["Y_"]][[1]])
expect_zero_diff(target_matrices[["Y2_"]][[2]], actual_matrices[["Y_"]][[2]])

# test predictors
expected_signal <- as.vector(Z %*% target_results$mu2_s2sls[-1])
expect_zero_diff(expected_signal, res_model_2_s2sls@spflow_indicators$SIGNAL)

rd <- target_results$mu2_s2sls[1]
expected_trend <- as.vector(target_matrices[["Y2_"]][[2]]) * rd
expect_zero_diff(expected_signal + expected_trend, predict(res_model_2_s2sls,method = "TS", return_type = "V"))

A2 <- diag(256) - W_d * rd
dg_AA2 <- diag(crossprod(A2))
y2 <- as.vector(target_matrices[["Y2_"]][[1]])
bpi_corr <- crossprod(A2, y2 - expected_trend - expected_signal)
expected_bpi <- y2 - bpi_corr/dg_AA2
expect_zero_diff(expected_bpi, predict(res_model_2_s2sls,method = "BPI", return_type = "V"))

expected_tc <- solve(A2, expected_signal)
expected_tca <- expected_signal + (W_d * rd +  W_d %*% W_d * rd^2 + W_d %*% W_d %*% W_d * rd^3)  %*% expected_signal
expect_zero_diff(expected_tca, predict(res_model_2_s2sls,method = "TC", return_type = "V",expectation_approx_order = 3))
expect_zero_diff(expected_tc, predict(res_model_2_s2sls,method = "TC", return_type = "V",approx_expectation = FALSE))

bpa_corr <- A2 %*% crossprod(A2, y2 - expected_tc)
expected_bpa <- expected_tc - bpa_corr/dg_AA2
expect_zero_diff(expected_bpa, predict(res_model_2_s2sls,method = "BPA", return_type = "V",approx_expectation = FALSE))

bp_corr <- A2 %*% crossprod(A2, y2 - expected_tc)
expected_bp <- expected_tc - solve(crossprod(A2), bp_corr)
expect_zero_diff(expected_bp, predict(res_model_2_s2sls,method = "BP", return_type = "V",approx_expectation = FALSE))


# check singular case
multi_net_usa_ge2 <- multi_net_usa_ge
dat(multi_net_usa_ge2,"ge")[["X2"]] <- dat(multi_net_usa_ge2,"ge")[["X"]] * 10
res_model_2_s2sls_singular <- spflow(
  y2 ~ . + P_(DISTANCE), multi_net_usa_ge2, "ge_ge",
  spflow_control(estimation_method = "s2sls", model = "model_2"))
expect_true(16 > sum(
  predict(res_model_2_s2sls_singular, return_type = "M") -
  predict(res_model_2_s2sls, return_type = "M")))

rm(res_model_2_s2sls, res_model_2_s2sls_singular, rd)
# ---- ... s2sls - model 9 ----------------------------------------------------
res_model_9_s2sls <- spflow(
  y9 ~ . + P_(DISTANCE), multi_net_usa_ge, "ge_ge",
  spflow_control(estimation_method = "s2sls", model = "model_9"))

# test results
expect_inherits(res_model_9_s2sls, "spflow_model_s2sls")
expect_equal(target_results$mu9_s2sls, coef(res_model_9_s2sls))
expect_equal(target_results$sigma9_s2sls, sd_error(res_model_9_s2sls))

# test moments
actual_moments <- res_model_9_s2sls@spflow_moments
expect_zero_diff(target_moments[["ZZ"]], actual_moments[["ZZ"]])
expect_zero_diff(target_moments[["UU"]], actual_moments[["UU"]])
expect_zero_diff(target_moments[["ZY9"]], actual_moments[["ZY"]])
expect_zero_diff(target_moments[["UY9"]], actual_moments[["UY"]])
expect_zero_diff(target_moments[["TSS9"]], actual_moments[["TSS"]])

# test model matrices
actual_matrices <- res_model_9_s2sls@spflow_matrices
expect_zero_diff(target_matrices[["D_"]], actual_matrices[["D_"]])
expect_zero_diff(target_matrices[["O_"]], actual_matrices[["O_"]])
expect_zero_diff(target_matrices[["I_"]], actual_matrices[["I_"]])
expect_zero_diff(target_matrices[["P_"]][[1]], actual_matrices[["P_"]][[1]])
expect_zero_diff(target_matrices[["Y9_"]][[1]], actual_matrices[["Y_"]][[1]])
expect_zero_diff(target_matrices[["Y9_"]][[2]], actual_matrices[["Y_"]][[2]])
expect_zero_diff(target_matrices[["Y9_"]][[3]], actual_matrices[["Y_"]][[3]])
expect_zero_diff(target_matrices[["Y9_"]][[4]], actual_matrices[["Y_"]][[4]])



# test predictors
expected_signal <- as.vector(Z %*% target_results$mu9_s2sls[-(1:3)])
expect_zero_diff(expected_signal, res_model_9_s2sls@spflow_indicators$SIGNAL)

r9 <- target_results$mu9_s2sls[1:3]
expected_trend <- as.vector(Reduce("+", Map("*", target_matrices[["Y9_"]][-1], r9)))
expect_zero_diff(expected_signal + expected_trend, predict(res_model_9_s2sls, method = "TS", return_type = "V"))

WF9 <- W_d * r9[1] + W_o * r9[2] + W_w * r9[3]
A9 <- diag(256) - WF9
dg_AA9 <- diag(crossprod(A9))
y9 <- as.vector(target_matrices[["Y9_"]][[1]])
bpi_corr <- crossprod(A9, y9 - expected_trend - expected_signal)
expected_bpi <- y9 - bpi_corr/dg_AA9
expect_zero_diff(expected_bpi, predict(res_model_9_s2sls,method = "BPI", return_type = "V"))

expected_tc <- solve(A9, expected_signal)
expected_tca <- expected_signal + (WF9 + WF9 %*% WF9 + WF9 %*% WF9 %*% WF9)  %*% expected_signal
expect_zero_diff(expected_tca, predict(res_model_9_s2sls,method = "TC", return_type = "V",expectation_approx_order = 3))
expect_zero_diff(expected_tc, predict(res_model_9_s2sls,method = "TC", return_type = "V",approx_expectation = FALSE))

bpa_corr <- A9 %*% crossprod(A9, y9 - expected_tc)
expected_bpa <- expected_tc - bpa_corr/dg_AA9
expect_zero_diff(expected_bpa, predict(res_model_9_s2sls,method = "BPA", return_type = "V",approx_expectation = FALSE))

bp_corr <- A9 %*% crossprod(A9, y9 - expected_tc)
expected_bp <- expected_tc - solve(crossprod(A9), bp_corr)
expect_zero_diff(expected_bp, predict(res_model_9_s2sls,method = "BP", return_type = "V",approx_expectation = FALSE))

# test refit
refit <- spflow_refit(res_model_9_s2sls, "stepwise")
expect_inherits(refit, "data.frame")
expect_equal(ncol(refit), length(coef(res_model_9_s2sls, "delta")))

refit <- spflow_refit(res_model_9_s2sls, "ar_family")
expect_inherits(refit, "data.frame")
expect_equal(ncol(refit), 9)

od_dropper <- function(drop_nodes) {
  ff <- function(x) !x[["ID_STATE"]] %in% drop_nodes
  list("orig" = ff, "dest" = ff)
}
wt_funs <- lapply(1:11, function(x) od_dropper(germany_grid$ID_STATE[seq(x)]))
refit <- spflow_refit(res_model_9_s2sls, "samples", sample_weights = wt_funs)
expect_inherits(refit, "data.frame")
expect_equal(ncol(refit), 12)
rm(res_model_9_s2sls)

# ---- ... mle - model 2 ------------------------------------------------------
res_model_2_mle <- spflow(
  y2 ~ . + P_(DISTANCE), multi_net_usa_ge, "ge_ge",
  spflow_control(estimation_method = "mle", model = "model_2"))

# test results
expect_inherits(res_model_2_mle, "spflow_model_mle")
expect_equal(names(target_results$mu2_input),
             names(coef(res_model_2_mle)))
expect_equal(target_results$mu2_input / coef(res_model_2_mle),
             rep(1,9), tolerance = 0.5, check.names = FALSE)
expect_equal(target_results$sigma_input / sd_error(res_model_2_mle),
             1, tolerance = 0.1, check.names = FALSE)

# test moments
actual_moments <- res_model_2_mle@spflow_moments
expect_zero_diff(target_moments[["ZZ"]],   actual_moments[["ZZ"]])
expect_zero_diff(target_moments[["ZY2"]],  actual_moments[["ZY"]])
expect_zero_diff(target_moments[["TSS2"]], actual_moments[["TSS"]])

# test model matrices
actual_matrices <- res_model_2_mle@spflow_matrices
expect_zero_diff(target_matrices[["D_"]], actual_matrices[["D_"]])
expect_zero_diff(target_matrices[["O_"]], actual_matrices[["O_"]])
expect_zero_diff(target_matrices[["I_"]], actual_matrices[["I_"]])
expect_zero_diff(target_matrices[["P_"]][[1]], actual_matrices[["P_"]][[1]])
expect_zero_diff(target_matrices[["Y2_"]][[1]], actual_matrices[["Y_"]][[1]])
expect_zero_diff(target_matrices[["Y2_"]][[2]], actual_matrices[["Y_"]][[2]])

# check singular case
multi_net_usa_ge2 <- multi_net_usa_ge
dat(multi_net_usa_ge2,"ge")[["X2"]] <- dat(multi_net_usa_ge2,"ge")[["X"]] * 10
res_model_2_mle_singular <- spflow(
  y2 ~ . + P_(DISTANCE), multi_net_usa_ge2, "ge_ge",
  spflow_control(estimation_method = "mle", model = "model_2"))
expect_true( 16 > sum(
  predict(res_model_2_mle_singular, return_type = "M") -
  predict(res_model_2_mle, return_type = "M")))
rm(res_model_2_mle, res_model_2_mle_singular)



# ---- ... mle - model 9 ------------------------------------------------------
res_model_9_mle <- spflow(
  y9 ~ . + P_(DISTANCE), multi_net_usa_ge, "ge_ge",
  spflow_control(estimation_method = "mle", model = "model_9"))

# test results
expect_inherits(res_model_9_mle, "spflow_model_mle")
expect_equal(names(target_results$mu9_input), names(coef(res_model_9_mle)))
expect_equal(target_results$mu9_input / coef(res_model_9_mle),
             rep(1,11), tolerance = 0.3, check.names = FALSE)
expect_equal(target_results$sigma_input / sd_error(res_model_9_mle),
             1, tolerance = 0.1, check.names = FALSE)
expect_equal(res_model_9_mle@estimation_diagnostics$R2_corr,
             spflow:::mom2Rcorr(spflow:::derive_empric_moments(res_model_9_mle),coef(res_model_9_mle)))

# test moments
actual_moments <- res_model_9_mle@spflow_moments
expect_zero_diff(target_moments[["ZZ"]],   actual_moments[["ZZ"]])
expect_zero_diff(target_moments[["ZY9"]],  actual_moments[["ZY"]])
expect_zero_diff(target_moments[["TSS9"]], actual_moments[["TSS"]])

# test model matrices
actual_matrices <- res_model_9_mle@spflow_matrices
expect_zero_diff(target_matrices[["D_"]], actual_matrices[["D_"]])
expect_zero_diff(target_matrices[["O_"]], actual_matrices[["O_"]])
expect_zero_diff(target_matrices[["I_"]], actual_matrices[["I_"]])
expect_zero_diff(target_matrices[["P_"]][[1]], actual_matrices[["P_"]][[1]])
expect_zero_diff(target_matrices[["Y9_"]][[1]], actual_matrices[["Y_"]][[1]])
expect_zero_diff(target_matrices[["Y9_"]][[2]], actual_matrices[["Y_"]][[2]])
expect_zero_diff(target_matrices[["Y9_"]][[3]], actual_matrices[["Y_"]][[3]])
expect_zero_diff(target_matrices[["Y9_"]][[4]], actual_matrices[["Y_"]][[4]])


# test refit
refit <- spflow_refit(res_model_9_mle, "stepwise")
expect_inherits(refit, "data.frame")
expect_equal(ncol(refit), length(coef(res_model_9_mle, "delta")))

refit <- spflow_refit(res_model_9_mle, "ar_family")
expect_inherits(refit, "data.frame")
expect_equal(ncol(refit), 9)

od_dropper <- function(drop_nodes) {
  ff <- function(x) !x[["ID_STATE"]] %in% drop_nodes
  list("orig" = ff, "dest" = ff)
}
wt_funs <- lapply(1:12, function(x) od_dropper(germany_grid$ID_STATE[seq(x)]))
names(wt_funs) <- paste0("drop",1:12)
refit <- spflow_refit(res_model_9_mle, "samples", sample_weights = wt_funs)
expect_inherits(refit, "data.frame")
expect_equal(ncol(refit), 13)

rm(res_model_9_mle)

# ---- ... mcmc - model 2 -----------------------------------------------------
res_model_2_mcmc <- spflow(
  y2 ~ . + P_(DISTANCE), multi_net_usa_ge, "ge_ge",
  spflow_control(estimation_method = "mcmc", model = "model_2"))

# test results
expect_inherits(res_model_2_mcmc, "spflow_model_mcmc")
expect_equal(names(target_results$mu2_input), names(coef(res_model_2_mcmc)))
expect_equal(target_results$mu2_input / coef(res_model_2_mcmc),
             rep(1,9), tolerance = 0.5, check.names = FALSE)
expect_equal(target_results$sigma_input / sd_error(res_model_2_mcmc),
             1, tolerance = 0.1, check.names = FALSE)

# test moments
actual_moments <- res_model_2_mcmc@spflow_moments
expect_zero_diff(target_moments[["ZZ"]],   actual_moments[["ZZ"]])
expect_zero_diff(target_moments[["ZY2"]],  actual_moments[["ZY"]])
expect_zero_diff(target_moments[["TSS2"]], actual_moments[["TSS"]])

# test model matrices
actual_matrices <- res_model_2_mcmc@spflow_matrices
expect_zero_diff(target_matrices[["D_"]], actual_matrices[["D_"]])
expect_zero_diff(target_matrices[["O_"]], actual_matrices[["O_"]])
expect_zero_diff(target_matrices[["I_"]], actual_matrices[["I_"]])
expect_zero_diff(target_matrices[["P_"]][[1]], actual_matrices[["P_"]][[1]])
expect_zero_diff(target_matrices[["Y2_"]][[1]], actual_matrices[["Y_"]][[1]])
expect_zero_diff(target_matrices[["Y2_"]][[2]], actual_matrices[["Y_"]][[2]])


# check singular case
multi_net_usa_ge2 <- multi_net_usa_ge
dat(multi_net_usa_ge2,"ge")[["X2"]] <- dat(multi_net_usa_ge2,"ge")[["X"]] * 10
res_model_2_mcmc_singular <- spflow(
  y2 ~ . + P_(DISTANCE), multi_net_usa_ge2, "ge_ge",
  spflow_control(estimation_method = "mcmc", model = "model_2"))
expect_true( 16 > sum(
  predict(res_model_2_mcmc_singular, return_type = "M") -
  predict(res_model_2_mcmc, return_type = "M")))
rm(res_model_2_mcmc, res_model_2_mcmc_singular)

# ---- ... mcmc - model 9 -----------------------------------------------------
res_model_9_mcmc <- spflow(
  y9 ~ . + P_(DISTANCE), multi_net_usa_ge, "ge_ge",
  spflow_control(estimation_method = "mcmc", model = "model_9"))

# test results
expect_inherits(res_model_9_mcmc, "spflow_model_mcmc")
expect_equal(names(target_results$mu9_input), names(coef(res_model_9_mcmc)))
expect_equal(target_results$mu9_input / coef(res_model_9_mcmc),
             rep(1,11), tolerance = 0.3, check.names = FALSE)
expect_equal(target_results$sigma_input / sd_error(res_model_9_mcmc),
             1, tolerance = 0.1, check.names = FALSE)

# test moments
actual_moments <- res_model_9_mcmc@spflow_moments
expect_zero_diff(target_moments[["ZZ"]],   actual_moments[["ZZ"]])
expect_zero_diff(target_moments[["ZY9"]],  actual_moments[["ZY"]])
expect_zero_diff(target_moments[["TSS9"]], actual_moments[["TSS"]])

# test model matrices
actual_matrices <- res_model_9_mcmc@spflow_matrices
expect_zero_diff(target_matrices[["D_"]], actual_matrices[["D_"]])
expect_zero_diff(target_matrices[["O_"]], actual_matrices[["O_"]])
expect_zero_diff(target_matrices[["I_"]], actual_matrices[["I_"]])
expect_zero_diff(target_matrices[["P_"]][[1]], actual_matrices[["P_"]][[1]])
expect_zero_diff(target_matrices[["Y9_"]][[1]], actual_matrices[["Y_"]][[1]])
expect_zero_diff(target_matrices[["Y9_"]][[2]], actual_matrices[["Y_"]][[2]])
expect_zero_diff(target_matrices[["Y9_"]][[3]], actual_matrices[["Y_"]][[3]])
expect_zero_diff(target_matrices[["Y9_"]][[4]], actual_matrices[["Y_"]][[4]])



# test refit
refit <- spflow_refit(res_model_9_mcmc, "stepwise")
expect_inherits(refit, "data.frame")
expect_equal(ncol(refit), length(coef(res_model_9_mcmc, "delta")))

refit <- spflow_refit(res_model_9_mcmc, "ar_family")
expect_inherits(refit, "data.frame")
expect_equal(ncol(refit), 9)

od_dropper <- function(drop_nodes) {
  ff <- function(x) !x[["ID_STATE"]] %in% drop_nodes
  list("orig" = ff, "dest" = ff)
}
wt_funs <- lapply(1:12, function(x) od_dropper(germany_grid$ID_STATE[seq(x)]))
names(wt_funs) <- paste0("drop",1:12)
refit <- spflow_refit(res_model_9_mcmc, "samples", sample_weights = wt_funs)
expect_inherits(refit, "data.frame")
expect_equal(ncol(refit), 13)

rm(res_model_9_mcmc)

# ---- ... incomplete models --------------------------------------------------
expect_spflow_model <- function(formula, m = "model_9") expect_inherits(
  spflow(spflow_formula = formula,
         spflow_networks =  multi_net_usa_ge,
         id_spflow_pairs =  "ge_ge",
         estimation_control = list("model" = "model_9")
         ), "spflow_model")

expect_spflow_model(y9 ~ + P_(DISTANCE))
expect_spflow_model(y9 ~ + P_(DISTANCE) - 1)
expect_spflow_model(y9 ~ + P_(DISTANCE) - 1 - I_(-1))
expect_spflow_model(y1 ~ + P_(DISTANCE) - 1 - I_(-1), "model_1")
expect_spflow_model(y9 ~ + D_(X) + O_(X) -1)
expect_spflow_model(y9 ~ + D_(X) + -1)
expect_spflow_model(y9 ~ + O_(X) + -1)
expect_spflow_model(y9 ~ + D_(X) + I_(-1) - 1)
expect_spflow_model(y9 ~ + O_(X) + I_(-1) - 1)
expect_spflow_model(y9 ~ + I_(X) + -1)
expect_spflow_model(y9 ~ + I_(X-1) + -1)
expect_spflow_model(y9 ~ + I_(-1))


# ---- ... weighted models ----------------------------------------------------
res_wt_dist <- spflow(
  spflow_formula = y9 ~ . + P_(DISTANCE),
  spflow_networks =  multi_net_usa_ge,
  id_spflow_pairs = "ge_ge",
  estimation_control = spflow_control(
    use_intra = FALSE,
    weight_functions = list("pair" = function(x) x[["DISTANCE"]] > 0)))

expect_equal(nobs(res_wt_dist, "sample"), n^2-n,
             info = "drop intra using weights.")

res_wt_dist_orig <- spflow(
  spflow_formula = y9 ~ . + P_(DISTANCE),
  spflow_networks =  multi_net_usa_ge,
  id_spflow_pairs = "ge_ge",
  estimation_control = spflow_control(
    use_intra = FALSE,
    weight_functions = list("pair" = function(x) x[["DISTANCE"]] > 0,
                            "orig" = function(x) x[["ID_STATE"]] != "HH")))
expect_equal(nobs(res_wt_dist_orig, "sample"), n^2 - n - n + 1,
             info = "drop and one origin using weights.")


res_wt_dist_orig <- spflow(
  spflow_formula = y9 ~ . + P_(DISTANCE),
  spflow_networks =  multi_net_usa_ge,
  id_spflow_pairs = "ge_ge",
  estimation_control = spflow_control(
    use_intra = FALSE,
    weight_functions = list("pair" = function(x) x[["DISTANCE"]] > 0,
                            "orig" = function(x) x[["ID_STATE"]] != "HH",
                            "dest" = function(x) x[["ID_STATE"]] != "SH")))
expect_equal(nobs(res_wt_dist_orig, "sample"), n^2 - n - n + 1 - n + 2,
             info = "drop and one origin and one destination using weights.")
