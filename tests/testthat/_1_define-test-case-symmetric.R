# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Project: spflow - define test case 1
# Author: Lukas Dargel
# = = = = = = = = = = = = = = = = = = =
# Description:
#
# The script creates a consistent set of input values, intermediate results and
# final which is used for unit and integration testing.
# For the whole estimation procedure it uses a simulated test data.
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
# The same test case is used for diffrent estimatiors and all formulations;
# Estimators: [s2sls, mle, mcmc]
# Formulations: [matrix, vector]
# - - - - - - - - - - - - - - - - - - -
# Date: june 2020



# ---- 0. case description ----------------------------------------------------
load_all()
library("data.table")
data("germany_grid")
case_description <- c(
  "Test data for the symmetric OD flows using simulated data of ",
  "16 german states. The simulated flows use the intra coefficients",
  "and also SDM variables. The test data contains three diffrent auto-",
  "correlation structures with 3 versions of simulated flows.",
  "All exogenous variables are held constant for the three models.")

# ---- 1. input data ----------------------------------------------------------
# in this section we define the input data which is used to feed the
# sp_network and sp_network_pair objects.
input_data <- named_list(c("node_data",
                           "node_neigborhood",
                           "od_pair_data"))

# Data describing origins and destinations
ge_factor_id <- factor(germany_grid$NOM,levels = germany_grid$NOM)
input_data$node_data <-
  data.table(X = germany_grid$X,
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
  data.table(
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
simulation_input <- named_list(c("Wd","Wo","Ww","Z","error","delta",
                                 "A1","A1_inv", "signal1", "noise1","rho1",
                                 "A2","A2_inv", "signal2", "noise2","rho2",
                                 "A9","A9_inv", "signal9", "noise9","rho9"))

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

simulation_input$Z <- cbind(Z_const,X_vector,log(pair_data$pair_distance + 1))
rm(Z_const,X_vector)

# declare flow neighborhood
In <- Matrix::Diagonal(n)
simulation_input$Wd <- In %x% W
simulation_input$Wo <- W  %x% In
simulation_input$Ww <- W  %x% W

# regressuion coefficients are equal for all models
simulation_input$delta <- delta

## simulate according to:
# model 1 (non-spatial)
# model 2 (destination depedence)
# model 9 (orig & dest & o-d -dependence)
simulation_input$rho1 <- NULL
simulation_input$rho2 <- rho[c("rho_d")]
simulation_input$rho9 <- rho[c("rho_d", "rho_o", "rho_w")]

# define and invert the filter matrix
IN <- Matrix::Diagonal(N)
simulation_input$A1 <- NULL
simulation_input$A1_inv <- NULL

simulation_input$A2 <- IN - (simulation_input$rho2 * simulation_input$Wd)
simulation_input$A2_inv <- solve(simulation_input$A2)

simulation_input$A9 <- IN - (
  mapply("*", simulation_input$rho9,
         simulation_input[c("Wd","Wo","Ww")],
         SIMPLIFY = FALSE) %>% Reduce("+",.)
  )
simulation_input$A9_inv <- solve(simulation_input$A9)

# simulate the flows = signal + noise
simulation_input$noise9 <-
  simulation_input$A9_inv %*% simulation_input$error
simulation_input$signal9 <-
  simulation_input$A9_inv %*% (simulation_input$Z %*% delta)
input_data$od_pair_data$Y9 <-
  as.vector(simulation_input$signal9 + simulation_input$noise9)

simulation_input$noise2 <-
  simulation_input$A2_inv %*% simulation_input$error
simulation_input$signal2 <-
  simulation_input$A2_inv %*% (simulation_input$Z %*% delta)
input_data$od_pair_data$Y2 <-
  as.vector(simulation_input$signal2 + simulation_input$noise2)

simulation_input$noise1 <-
  simulation_input$error
simulation_input$signal1 <-
  simulation_input$Z %*% delta
input_data$od_pair_data$Y1 <-
  simulation_input$signal1 + simulation_input$noise1


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
Y1 <- list("Y1" = list(Y1))

Y2 <- matrix(input_data$od_pair_data$Y2,n,n)
Y2 <- list("Y2" = list(Y2,
                       W %*% Y2))

Y9 <- matrix(input_data$od_pair_data$Y9,n,n)
Y9 <- list("Y9" = list(Y9,
                       W %*% Y9,
                       tcrossprod(Y9,W),
                       W %*% tcrossprod(Y9,W)))

relational_model_matrices <-
  c(Y1, Y2, Y9,
    list("const" = 1),
    list("const_intra" = const_intra),
    X_lagged2,
    list("G" = G_lagged),
    list("OW" = W))

# ---- 2b. long form model matrix ---------------------------------------------
# Define the long form model matrix which is used during estimation based on a
# vector formulation.
# Again the exogenous variables are the same for all models and the flows
# differ between them


compact_model_matrix <-
  list(
    "H" = cbind("const" = 1,
                vec_reference_matrix(relational_model_matrices$const_intra),
                X_lagged2 %>% vec_reference_O_D_I(),
                vec_reference_matrix(G_lagged)),
    "Y1" = vec_reference_matrix(relational_model_matrices$Y1),
    "Y2" = vec_reference_matrix(relational_model_matrices$Y2),
    "Y9" = vec_reference_matrix(relational_model_matrices$Y9))

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
requied_moments <- c(
  # general
  "HH","ZZ","N","n_d","n_o",
  "H_index",
  "W_traces",
  # model specific
  "TSS1","TSS2","TSS9",
  "HY1", "HY2", "HY9",
  "ZY1", "ZY2", "ZY9")

# preassign, fill and verify
model_moments <- named_list(c(requied_moments))

model_moments$N <- N
model_moments[c("n_d","n_o")]  <- n
model_moments$HH <- crossprod(compact_model_matrix$H)
model_moments$ZZ <- crossprod(simulation_input$Z)

model_moments$TSS1 <- crossprod(compact_model_matrix$Y1)
model_moments$TSS2 <- crossprod(compact_model_matrix$Y2)
model_moments$TSS9 <- crossprod(compact_model_matrix$Y9)

model_moments$HY1 <- crossprod(compact_model_matrix$H,compact_model_matrix$Y1)
model_moments$HY2 <- crossprod(compact_model_matrix$H,compact_model_matrix$Y2)
model_moments$HY9 <- crossprod(compact_model_matrix$H,compact_model_matrix$Y9)

model_moments$ZY1 <- crossprod(simulation_input$Z,compact_model_matrix$Y1)
model_moments$ZY2 <- crossprod(simulation_input$Z,compact_model_matrix$Y2)
model_moments$ZY9 <- crossprod(simulation_input$Z,compact_model_matrix$Y9)

model_moments$W_traces <- trace_sequence(W)


stopifnot(all(names(model_moments) == requied_moments),
          !any(rapply(model_moments, is.null)))

# declare which input corresponds belongs to which type of data source
model_moments$H_index <- list(
  "const" = 1,
  "const_intra" = 2:10, # derived from neighborhood
  "X" = 11:22,          # nodes
  "G" = 23:25           # pairs
)


# ---- 4. estimation results --------------------------------------------------
# Define the desired estimation results for each estimation method.
design_results <- named_list(c("params","sd_params"))

results <- named_list(c("Y1","Y2","Y9"))


# ..4.1 Model 1 ols reg ----
results$Y1$ols <- design_results

mu <- solve(model_moments$ZZ,model_moments$ZY1)
ESS <- crossprod(model_moments$ZY1,mu)
RSS <- model_moments$TSS1 - ESS
sigma2 <- as.vector(RSS/N)
varcov <- solve(model_moments$ZZ)

results$Y1$ols$params <- mu
results$Y1$ols$sd_params <- sqrt(diag(varcov) * sigma2)

cbind(mu,simulation_input$delta)
c(sqrt(sigma2),sd(simulation_input$noise1))

# ..4.2 Model 2 s2sls reg ----
H <- compact_model_matrix$H
Z <- compact_model_matrix$Z

J <- compact_model_matrix$Y2[, -1]
Y <- compact_model_matrix$Y2[,1]

HJ <- model_moments$HY2[, -1]
HY <- model_moments$HY2[, 1]

ZJ <- model_moments$ZY2[, -1]
ZY <- model_moments$ZY2[, 1]

# verify that moments can be constructed without
# fitted values of the first stage (J_hat)
J_hat <- H %*% solve(model_moments$HH,crossprod(H,J))
JJ_hat <- crossprod(J_hat)
JJ_hat2 <- crossprod(HJ,solve(model_moments$HH,HJ))
JJ_hat - JJ_hat2

Jy_hat <- crossprod(HJ,solve(model_moments$HH,HY))
Jy_hat2 <- crossprod(J_hat,Y)
Jy_hat - Jy_hat2

ZZ_hat <- rbind(cbind(JJ_hat2,t(ZJ)),
                cbind(ZJ,model_moments$ZZ))

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

true_param <- c(simulation_input$rho2,simulation_input$delta)
cbind(mu,true_param)

results$Y2$s2sls <- design_results
results$Y2$s2sls$params <- mu
results$Y2$s2sls$sd_params <- sqrt(diag(varcov))

# ..4.2 Model 2 mle reg ----
calc_log_det_model2 <- function(rho){

  max_power <- 10
  order <- 1:max_power
  wt_sequence <- (rho_d^order/order)

  log_det <- -n * sum(wt_sequence * W_traces)

  return(log_det)
}

calc_loglik_model2 <- function(rho){}



# ..4.9 Model 9 s2sls reg ----
# pull out the used quantities
J <- compact_model_matrix$Y9[, -1]
HJ <- model_moments$HY9[, -1]
H <- compact_model_matrix$H
HY <- model_moments$HY9[, 1]
ZJ <- model_moments$ZY9[, -1]
ZY <- model_moments$ZY9[, 1]


# verify that moments can be constructed without
# fitted values of the first stage (J_hat)
J_hat <- H %*% solve(model_moments$HH,crossprod(H,J))
JJ_hat <- crossprod(J_hat)
JJ_hat2 <- crossprod(HJ,solve(model_moments$HH,HJ))
JJ_hat - JJ_hat2

Jy_hat <- crossprod(HJ,solve(model_moments$HH,HY))
Jy_hat2 <- crossprod(J_hat,compact_model_matrix$Y9[,1])
Jy_hat - Jy_hat2

ZZ_hat <- rbind(cbind(JJ_hat2,t(ZJ)),
                cbind(ZJ,model_moments$ZZ))

ZY_hat <- c(Jy_hat2,model_moments$ZY9[,1])

mu <- solve(ZZ_hat,ZY_hat)

# check if ESS can be constructed without fitted values
fitted <- cbind(compact_model_matrix$Y9[,-1],simulation_input$Z) %*% mu
resid <- fitted - compact_model_matrix$Y9[,1]
ESS <- sum(fitted^2)
RSS <- sum(resid^2)
sigma2 <- RSS/N

TSS <- sum(compact_model_matrix$Y9[,1]^2)
ESS2 <- crossprod(ZY_hat,mu)
RSS2 <- TSS - ESS2
sigma2_v2 <- as.vector(RSS2/N)
c(sigma2,sigma2_v2,var(simulation_input$error))

varcov <- solve(ZZ_hat) * sigma2_v2

true_param <- c(simulation_input$rho9,simulation_input$delta)
cbind(mu,true_param)

results$Y9$s2sls <- design_results
results$Y9$s2sls$params <- mu
results$Y9$s2sls$sd_params <- sqrt(diag(varcov))

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






