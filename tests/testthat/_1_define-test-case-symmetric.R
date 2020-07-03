# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Project: spflow - define test case 1
# Author: Lukas Dargel
# = = = = = = = = = = = = = = = = = = =
# Description:
#
# The script creates a consistent set of input values, intermediate results and
# final results which is used for unit and integration testing.
# For the whole estimation procedure it creates simulated test data.
# It includes the following sets:
# 0. Case definition
# 1. Model inputs   : cleaned input data
# 2. Design matrices: model matrices for the spatial interaction model
#                     two formulations are possible (a = matrix, b = vector)
#                     In terms of mathematics they are identical but the matrix
#                     formulation is computationally more efficient.
# 3. Moments        : The empricial moments
#                     (based on inner products of the design matrices)
# 4. Results
# The same input data diffrent models and both formulations;
# To reduce complexity we do not give design results for
# the MCMC and the MLE estimator.
# Formulations: [matrix, vector]
# Models: [model_1, model_2, model_9]
# - - - - - - - - - - - - - - - - - - -
# Date: june 2020


# ---- 0. case description ----------------------------------------------------
load_all()
data("germany_grid")

models <- c("M1","M2", "M9")
case_description <- named_list(models)

case_description$M1 <-
  "Test data for the symmetric OD flows using simulated data of the " %p%
  "16 german states." %p%
  "The simulated flows make use of SDM variables but do not use the " %p%
  "intra coefficients. " %p%
  "They do not exhibit any spatial auto-correlation (model 1)."

case_description$M2 <-
  "Test data for the symmetric OD flows using simulated data of the " %p%
  "16 german states." %p%
  "The simulated flows make use of SDM variables but do not use the " %p%
  "intra coefficients. " %p%
  "They follow the the auto-correlation pattern of model 2"

case_9_description <-
  "Test data for the symmetric OD flows using simulated data of the " %p%
  "16 german states. The simulated flows use the intra-coefficients, " %p%
  "the SDM variables and follow the auto-correlation pattern of model 9."

# ---- 1. input data ----------------------------------------------------------
# in this section we define the input data which is used to feed the
# sp_network and sp_network_pair objects.
# it is identical for all models
input_data <- named_list(c("node_data",
                           "node_neigborhood",
                           "od_pair_data"))

# Data describing origins and destinations
ge_factor_id <- factor(germany_grid$NOM,levels = germany_grid$NOM)
input_data$node_data <-
  data.table::data.table(X = germany_grid$X,
                         id = ge_factor_id,
                         key = "id")

# A neighbourhood matrix for these locations
input_data$node_neighborhood <-
  germany_grid %>%
  spdep::poly2nb(.) %>%
  spdep::nb2listw(.) %>%
  spdep::listw2mat(.) %>%
  Matrix::Matrix(.)

# Next we create data describing the origin-destination pairs
# Each pair is identified by an origin-id and a destination id.
# Distance and Flows (= Y, to be simulated)
od_ids <- expand.grid("orig_id" = ge_factor_id,
                      "dest_id" = ge_factor_id)
pair_distance <-
  sp::coordinates(germany_grid) %>%
  dist() %>% as.matrix() %>% as.vector()

# create place holders for the flows (Y)
# that are simulated in the next step
input_data$od_pair_data <-
  data.table::data.table(
    od_ids,
    pair_distance,
    "Y1" = 0,
    "Y2" = 0,
    "Y9" = 0,
    key = c("orig_id", "dest_id"))

rm(pair_distance,od_ids)
# ---- 1.1 simulation inputs --------------------------------------------------
# Our test case us based on simulated data which allows to keep track of the
# estimation accuracy.

# we simulate three types of flows according to an SDM model
# each flow has a diffrent autocorrelation structure

# for the simulation we require the matrix exogenous variables [Z]
# the flow neighborhoods and a vector of random deviations
# the enumerated simulation input is model specific
fix_input <- c("Wd","Wo","Ww","error")
model_specific_input <- c("A","A_inv","Z","signal","noise","rho","delta")

simulation_input <-
  c(named_list(fix_input),
    named_list(models,init = named_list(model_specific_input)))


data("simulation_parameters")
set.seed(123)
n <- nrow(germany_grid)
N <- n^2
simulation_input$error <- rnorm(N,sd = sd_error)

# create the exogenous variables for the simulation
# we use a destinct set of variables for intra-regional observations
pair_data <- input_data$od_pair_data

# global and intra constant
Z_const <- cbind(
  "const" = 1,
  "const_intra" = as.integer(pair_data$orig_id == pair_data$dest_id))

# X variables
# For the SDM specification we include a spatial lag of X
W <- input_data$node_neighborhood
X_lagged <- data.frame(
  "X" = as.vector(germany_grid$X),
  "X.lag1" = as.vector(W %*% germany_grid$X))
X_vector <- X_lagged %>%
  expand_O_D_I() %>%
  vec_reference_O_D_I()

simulation_input$M1$Z <- cbind(Z_const,X_vector,log(pair_data$pair_distance + 1)) %>% as.matrix()
simulation_input$M2$Z <- cbind(Z_const[,1],X_vector[,1:4],log(pair_data$pair_distance + 1)) %>% as.matrix()
simulation_input$M9$Z <- cbind(Z_const,X_vector,log(pair_data$pair_distance + 1)) %>% as.matrix()

rm(Z_const,X_vector)

# declare flow neighborhood
# BUG there is a problem when loading sp/spdep and matrix
In <- Matrix::Diagonal(n)
# W <- as.matrix(W)
simulation_input$Wd <- In %x% W
simulation_input$Wo <- W  %x% In
simulation_input$Ww <- W  %x% W

# regressuion coefficients for all models
drop_intra_parameters <- grep(pattern = "intra",names(delta),ignore.case = T)

simulation_input$M1$delta <- delta
simulation_input$M2$delta <- delta[-drop_intra_parameters]
simulation_input$M9$delta <- delta

## simulate according to:
# model 1 (non-spatial)
# model 2 (destination depedence)
# model 9 (orig & dest & o-d -dependence)
simulation_input$M1$rho <- NULL
simulation_input$M2$rho <- rho[c("rho_d")]
simulation_input$M9$rho <- rho[c("rho_d", "rho_o", "rho_w")]

# define and invert the filter matrix
IN <- Matrix::Diagonal(N)
simulation_input$M1$A <- NULL
simulation_input$M1$A_inv <- NULL

simulation_input$M2$A <- IN - (simulation_input$M2$rho * simulation_input$Wd)
simulation_input$M2$A_inv <- solve(simulation_input$M2$A)

simulation_input$M9$A <- IN - (
  mapply("*", simulation_input$M9$rho,
         simulation_input[c("Wd","Wo","Ww")],
         SIMPLIFY = FALSE) %>% Reduce("+",.)
  )
simulation_input$M9$A_inv <- solve(simulation_input$M9$A)

# simulate the flows = signal + noise
simulation_input$M1$noise  <- simulation_input$error
simulation_input$M1$signal <- simulation_input$M1$Z %*% simulation_input$M1$delta

simulation_input$M2$noise  <- simulation_input$M2$A_inv %*% simulation_input$error
simulation_input$M2$signal <- simulation_input$M2$A_inv %*% (simulation_input$M2$Z %*% simulation_input$M2$delta)

simulation_input$M9$noise  <- simulation_input$M9$A_inv %*% simulation_input$error
simulation_input$M9$signal <- simulation_input$M9$A_inv %*% (simulation_input$M9$Z %*% simulation_input$M9$delta)

input_data$od_pair_data$Y9 <- as.vector(simulation_input$M9$signal + simulation_input$M9$noise)
input_data$od_pair_data$Y2 <- as.vector(simulation_input$M2$signal + simulation_input$M2$noise)
input_data$od_pair_data$Y1 <- as.vector(simulation_input$M1$signal + simulation_input$M1$noise)

# TODO finsh restructuring of the test case
# ---- 2a. relational model matrices ------------------------------------------
# Here we define the desired output for the relational design matrices,
# which are used to estimate the model with the more efficient
# matrix formulation.

# The model matrices include addtional spatial lags which are used as
# instruments or represented the lagged flow matrices.

#... the intra regional constant with instruments
temp_intra <- spflow:::named_list(c("V","WV","WW"))
temp_intra$V <- Matrix::tcrossprod(W) # def. V = WW'
temp_intra$WV <- W %*% temp_intra$V
temp_intra$WW <- W %*% W

const_intra <- list(
  "In" = In,
  "W" = W,
  "W'" = t(W),
  "WW" = temp_intra$WW,
  "WW'" = t(temp_intra$WW),
  "V" = temp_intra$V,
  "VV" = Matrix::tcrossprod(temp_intra$WV, W),
  "WV" = temp_intra$WV,
  "VW'" = t(temp_intra$WV)
)
rm(temp_intra)

# ... X with instruments
X_lagged2 <-
  cbind(X_lagged, as.matrix(W %*% W %*% as.matrix(X_lagged))) %>% expand_O_D_I()

# ...G with instruments
G_transformed <- matrix(log(pair_data$pair_distance + 1),n,n)
test_G_lag <- tcrossprod(W %*% G_transformed,W)
test_G_lag2 <- tcrossprod(W %*% test_G_lag,W)
G_lagged <- list(G_transformed,test_G_lag,test_G_lag2) %>%
  lapply(as.matrix)
rm(G_transformed,test_G_lag,test_G_lag2,pair_data)

# ...Y lagged flows
Y1 <- matrix(input_data$od_pair_data$Y1,n,n)
Y1 <- list("Y" = list(Y1))

Y2 <- matrix(input_data$od_pair_data$Y2,n,n)
Y2 <- list("Y" = list(Y2,
                       W %*% Y2))

Y9 <- matrix(input_data$od_pair_data$Y9,n,n)
Y9 <- list("Y" = list(Y9,
                       W %*% Y9,
                       tcrossprod(Y9,W),
                       W %*% tcrossprod(Y9,W)))

relational_model_matrices <- named_list(models)

# no instruments for model 1
X_index_instruments <- 3:4
relational_model_matrices$M1 <-
  c(Y1,
    list("const" = 1),
    list("const_intra" = const_intra[1]),
    lapply(X_lagged2, drop_matrix_columns, X_index_instruments),
    list("G" = G_lagged[1]))

# no intra for model 2
relational_model_matrices$M2 <-
  c(Y2,
    list("const" = 1),
    X_lagged2[c("DX","OX")],
    list("G" = G_lagged),
    list("DW" = W))

# all informations for model 9
relational_model_matrices$M9 <-
  c(Y9,
    list("const" = 1),
    list("const_intra" = const_intra),
    X_lagged2,
    list("G" = G_lagged),
    list("DW" = W, "OW" = W))

# ---- 2b. long form model matrix ---------------------------------------------
# Define the long form model matrix which is used during estimation based on a
# vector formulation.
# Again the exogenous variables are the same for all models and the flows
# differ between them

compact_model_matrix <- named_list(models)

x_matrices <- c("DX", "OX", "IX")
M1_rel <- relational_model_matrices$M1
compact_model_matrix$M1 <-
  list("H" = cbind(M1_rel$const,
                   M1_rel$const_intra %>% vec_reference_matrix(),
                   M1_rel[x_matrices] %>% vec_reference_O_D_I(),
                   M1_rel$G %>% vec_reference_matrix()),
       "Y" = M1_rel$Y %>% vec_reference_matrix())


M2_rel <- relational_model_matrices$M2
compact_model_matrix$M2 <-
  list("H" = cbind(M2_rel$const,
                   M2_rel[x_matrices[1:2]] %>% vec_reference_O_D_I(),
                   M2_rel$G %>% vec_reference_matrix()),
       "Y" = M2_rel$Y %>% vec_reference_matrix())

M9_rel <- relational_model_matrices$M9
compact_model_matrix$M9 <-
  list("H" = cbind(M9_rel$const,
                   M9_rel$const_intra %>% vec_reference_matrix(),
                   M9_rel[x_matrices] %>% vec_reference_O_D_I(),
                   M9_rel$G %>% vec_reference_matrix()),
       "Y" = M9_rel$Y %>% vec_reference_matrix())

rm(M1_rel,M2_rel,M9_rel)

# decalre which variables in H are instruments
instrumental_variables <- list(
  "const" = FALSE,
  "intra_const" = c(FALSE,rep(TRUE,8)),
  "X" = rep(c(FALSE,FALSE,TRUE,TRUE),3),
  "G" = c(FALSE,TRUE,TRUE)
)

# ---- 3. model moments -------------------------------------------------------
# Define the required model moments which are used for the estimation
# procedures.

# TODO the traces should be on the flow neighborhood level to have the same structure for the vector (incomplete) and matric (complete) case
requied_moments <- c(
  "HH", "HY","ZZ", "ZY", "TSS",
  "N","n_d","n_o",
  "DW_traces",
  "OW_traces",
  "H_index")
# H_index declares which input corresponds belongs to which type of data source
# it is helpful to test the moment blocks which are grouped by the data sources

# preassign, fill and verify
model_moments <- named_list(models,named_list(requied_moments))

# Some moments are the same for the models:
# N & n_o & n_d
model_moments$M1$N <- model_moments$M2$N <- model_moments$M9$N <-
  N
nn <- c("n_d","n_o")
model_moments$M1[nn] <- model_moments$M2[nn] <- model_moments$M9[nn] <-
  n

# Some are not ...
M1_comp <- compact_model_matrix$M1
model_moments$M1$ZZ  <- crossprod(simulation_input$M1$Z)
model_moments$M1$ZY  <- crossprod(simulation_input$M1$Z, M1_comp$Y)
model_moments$M1$TSS <- crossprod(M1_comp$Y)

M2_comp <- compact_model_matrix$M2
model_moments$M2$HH <- crossprod(M2_comp$H)
model_moments$M2$HY <- crossprod(M2_comp$H,M2_comp$Y)
model_moments$M2$ZZ <- crossprod(simulation_input$M2$Z)
model_moments$M2$ZY <- crossprod(simulation_input$M2$Z, M2_comp$Y)
model_moments$M2$TSS <- crossprod(M2_comp$Y)
model_moments$M2$DW_traces <- trace_sequence(W)
model_moments$M2$H_index <- list("const" = 1,"X" = 2:9, "G" = 10:12)

M9_comp <- compact_model_matrix$M9
model_moments$M9$HH <- crossprod(M9_comp$H)
model_moments$M9$HY <- crossprod(M9_comp$H,M9_comp$Y)
model_moments$M9$ZZ <- crossprod(simulation_input$M9$Z)
model_moments$M9$ZY <- crossprod(simulation_input$M9$Z, M9_comp$Y)
model_moments$M9$TSS <- crossprod(M9_comp$Y)
model_moments$M9$DW_traces <- trace_sequence(W)
model_moments$M9$OW_traces <- trace_sequence(W)
model_moments$M9$H_index <- list("const" = 1,"const_intra" = 2:10, "X" = 11:22,"G" = 23:25)

rm(M1_comp,M2_comp,M9_comp)
# ---- 4. estimation results --------------------------------------------------
# Define the desired estimation results for each estimation method.
design_results <- named_list(c("params","sd_params"))
results <- named_list(models)

# ..4.1 Model 1 ols reg ----
results$M1$ols <- design_results

MM_1 <- model_moments$M1
mu <- solve(MM_1$ZZ,MM_1$ZY)
ESS <- crossprod(MM_1$ZY,mu)
RSS <- MM_1$TSS - ESS
sigma2 <- as.vector(RSS/N)
varcov <- solve(MM_1$ZZ)

results$M1$ols$params <- mu
results$M1$ols$sd_params <- sqrt(diag(varcov) * sigma2)

cbind("est" = mu, "true" = simulation_input$M1$delta)
c(sqrt(sigma2),sd(simulation_input$M1$noise))

rm(MM_1,ESS,RSS,mu,sigma2,varcov)
# ..4.2 Model 2 s2sls reg ----
# pull out the data and split the moments (J = lagged flows, Y = flows)
M2_comp <- compact_model_matrix$M2
MM_2 <- model_moments$M2

H <- M2_comp$H
Z <- simulation_input$M2$Z

J <- M2_comp$Y[, -1]
HJ <- MM_2$HY[, -1]
ZJ <- MM_2$ZY[, -1]

Y <- M2_comp$Y[,1]
HY <- MM_2$HY[, 1]
ZY <- MM_2$ZY[, 1]

# verify that moments can be constructed without
# fitted values of the first stage (J_hat)
J_hat <- H %*% solve(MM_2$HH,crossprod(H,J))
JJ_hat <- crossprod(J_hat)
JJ_hat2 <- crossprod(HJ,solve(MM_2$HH,HJ))
JJ_hat - JJ_hat2 # no significant diffrence

Jy_hat <- crossprod(HJ,solve(MM_2$HH,HY))
Jy_hat2 <- crossprod(J_hat,Y)
Jy_hat - Jy_hat2 # no significant diffrence

ZZ_hat <- rbind(cbind(JJ_hat2,t(ZJ)),
                cbind(ZJ,MM_2$ZZ))

ZY_hat <- c(Jy_hat2,ZY)

mu <- solve(ZZ_hat,ZY_hat)

# check if ESS can be constructed without fitted values
fitted <- cbind(J,Z) %*% mu
resid <- fitted - Y
ESS <- sum(fitted^2)
RSS <- sum(resid^2)
sigma2 <- RSS/N

TSS <- sum(Y^2)
ESS2 <- crossprod(ZY_hat,mu)
RSS2 <- TSS - ESS2
sigma2_v2 <- as.vector(RSS2/N)
c(sigma2,sigma2_v2,var(simulation_input$error))

varcov <- solve(ZZ_hat) * sigma2_v2

true_param <- c(simulation_input$M2$rho,simulation_input$M2$delta)
cbind(mu,true_param)

results$M2$s2sls <- design_results
results$M2$s2sls$params <- mu
results$M2$s2sls$sd_params <- sqrt(diag(varcov))

rm(MM_2,ESS,RSS,mu,sigma2,varcov)
# ..4.2 Model 2 mle reg ----
# MLE regression is much more complex and might not be tested...
# The s2sls can however serve as a template to test the correct format of
# the MLE and MCMC results.


# ..4.9 Model 9 s2sls reg ----
# pull out the data and split the moments (J = lagged flows, Y = flows)
M9_comp <- compact_model_matrix$M9
MM_9 <- model_moments$M9

H <- M9_comp$H
Z <- simulation_input$M9$Z

J <- M9_comp$Y[, -1]
HJ <- MM_9$HY[, -1]
ZJ <- MM_9$ZY[, -1]

Y <- M9_comp$Y[,1]
HY <- MM_9$HY[, 1]
ZY <- MM_9$ZY[, 1]


# verify that moments can be constructed without
# fitted values of the first stage (J_hat)
J_hat <- H %*% solve(MM_9$HH,crossprod(H,J))
JJ_hat <- crossprod(J_hat)
JJ_hat2 <- crossprod(HJ,solve(MM_9$HH,HJ))
JJ_hat - JJ_hat2 # no significant diffrence

Jy_hat <- crossprod(HJ,solve(MM_9$HH,HY))
Jy_hat2 <- crossprod(J_hat,Y)
Jy_hat - Jy_hat2 # no significant diffrence

ZZ_hat <- rbind(cbind(JJ_hat2,t(ZJ)),
                cbind(ZJ,MM_9$ZZ))

ZY_hat <- c(Jy_hat2,ZY)

mu <- solve(ZZ_hat,ZY_hat)

# check if ESS can be constructed without fitted values
fitted <- cbind(J,Z) %*% mu
resid <- fitted - Y
ESS <- sum(fitted^2)
RSS <- sum(resid^2)
sigma2 <- RSS/N

TSS <- sum(Y^2)
ESS2 <- crossprod(ZY_hat,mu)
RSS2 <- TSS - ESS2
sigma2_v2 <- as.vector(RSS2/N)
c(sigma2,sigma2_v2,var(simulation_input$error))

varcov <- solve(ZZ_hat) * sigma2_v2

true_param <- c(simulation_input$M9$rho,simulation_input$M9$delta)
cbind(mu,true_param)

results$M9$s2sls <- design_results
results$M9$s2sls$params <- mu
results$M9$s2sls$sd_params <- sqrt(diag(varcov))

# ---- 5. Export --------------------------------------------------------------
test_case_1_symmetric <- list(
  "description" = case_description,
  "input_data" = input_data,
  "simulation_input" = simulation_input,
  "relational_model_matrices" = relational_model_matrices,
  "compact_model_matrix" = compact_model_matrix,
  "which_instruments" = instrumental_variables,
  "model_moments" = model_moments,
  "results" = results)

save(test_case_1_symmetric,
     file = "tests/testthat/test_case_1_symmetric.rda")






