# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Project: spflow
# Author: Lukas Dargel
# = = = = = = = = = = = = = = = = = = =
# Description:
#
# The script creates a set of objects which is used as inputs for the matrix
# based calculation of the moments.
# It also provides a vecotr reference which is used as a benchmark for the
# correctness of the more efficient, but also more complex matrix formulation.
# - - - - - - - - - - - - - - - - - - -
# Date: june 2020

# for each object create an example input value
# and a vector reference to test against
case_description <-
  "Test data for the symmetric OD flows using simulated data of " %p%
  "16 german states. The simulated flows use the intra coefficients" %p%
  "and also SDM variables. The test data contains three diffrent auto-" %p%
  "correlation structures with 3 versions of simulated flows." %p%
  "All exogenous variables are held constant for the three models."


# ---- input values -----------------------------------------------------------
load_all()
data("germany_grid")
case1_data <- named_list(c("n","N","W","const","const_intra","X","G",
                           "Y1","Y2","Y9"))
case1_vector_reference <- named_list(c("const","const_intra","X","G",
                                       "Y1","Y2","Y9"))

# number of observations
case1_data$n <- nrow(germany_grid)
case1_data$N <- case1_data$n^2

# the spatial weight matrix
case1_data$W <-
  germany_grid %>%
  spdep::poly2nb(.) %>%
  spdep::nb2listw(.) %>%
  spdep::listw2mat(.) %>%
  Matrix::Matrix(.)

# the global constant
case1_data$const <- 1
case1_vector_reference$const <- rep(1,case1_data$N)

# the intra regional constant
temp_intra <- spflow:::named_list(c("V","WV","WW"))
temp_intra$V <- Matrix::tcrossprod(case1_data$W) # def. V = WW'
temp_intra$WV <- case1_data$W %*% temp_intra$V
temp_intra$WW <- case1_data$W %*% case1_data$W

case1_data$const_intra <- list(
  "In" = Matrix::Diagonal(nrow(case1_data$W)),
  "W" = case1_data$W,
  "W'" = t(case1_data$W),
  "WW" = temp_intra$WW,
  "WW'" = t(temp_intra$WW),
  "V" = temp_intra$V,
  "VV" = Matrix::tcrossprod(temp_intra$WV, case1_data$W),
  "WV" = temp_intra$WV,
  "VW'" = t(temp_intra$WV)
)
case1_vector_reference$const_intra <-
  vec_reference_matrix(case1_data$const_intra)
rm(temp_intra)

# origin, destinatuion and intra-regional attributes
case1_data$X <-
  cbind("X" = germany_grid$X,
        "X_lag" = case1_data$W %*% germany_grid$X) %>%
  expand_O_D_I()

case1_vector_reference$X <-
  vec_reference_O_D_I(case1_data$X)

# the distance matrix
case1_data$G <- as.matrix(log(dist(sp::coordinates(germany_grid)) + 1))
case1_vector_reference$G <- as.vector(case1_data$G)

# ---- simulated flows --------------------------------------------------------
data("simulation_parameters")

# some of the data is constant over all models
case1_data_sim <- named_list(c("Wd","Wo","Ww","Z","error"))

set.seed(123)
case1_data_sim$error <- rnorm(case1_data$N,sd = sd_error)

In <- Matrix::Diagonal(nrow(case1_data$W))
case1_data_sim$Wd <- In %x% case1_data$W
case1_data_sim$Wo <- case1_data$W %x% In
case1_data_sim$Ww <- case1_data$W %x% case1_data$W

case1_data_sim$Z <-
  cbind(
    case1_vector_reference$const,
    case1_vector_reference$const_intra[, 1],
    case1_vector_reference$X,
    case1_vector_reference$G
  ) %>%
  as.matrix()

# other data depends on the chosen model
model_specific_data <- c("which_rho","signal","noise","A_inv","A")

model9_data <- named_list(model_specific_data)
model2_data <- named_list(model_specific_data)
model1_data <- named_list(model_specific_data)

# define the auto-regressive parameters
model9_data$which_rho <- c("rho_d", "rho_o", "rho_w")
model2_data$which_rho <- c("rho_d")
model1_data$which_rho <- NULL

# define and invert the filter matrix
IN <- (In %x% In)
model9_data$A <-
  mapply("*", rho[model9_data$which_rho],
         case1_data_sim[c("Wd","Wo","Ww")],
         SIMPLIFY = FALSE) %>% Reduce("+",.)
model9_data$A <- IN - model9_data$A
model9_data$A_inv <- solve(model9_data$A)

model2_data$A <-
  mapply("*", rho[model2_data$which_rho],
         case1_data_sim[c("Wd")],
         SIMPLIFY = FALSE) %>% Reduce("+",.)
model2_data$A <- IN - model2_data$A
model2_data$A_inv <- solve(model2_data$A)

model1_data$A <- NULL
model1_data$A_inv <- NULL

# simulate the flows
model9_data$noise <- model9_data$A_inv %*% case1_data_sim$error
model9_data$signal <- model9_data$A_inv %*% (case1_data_sim$Z %*% delta)
case1_vector_reference$Y9 <- model9_data$signal + model9_data$noise

model2_data$noise <- model2_data$A_inv %*% case1_data_sim$error
model2_data$signal <- model2_data$A_inv %*% (case1_data_sim$Z %*% delta)
case1_vector_reference$Y2 <- model2_data$signal + model2_data$noise

model1_data$noise <- case1_data_sim$error
model1_data$signal <- case1_data_sim$Z %*% delta
case1_vector_reference$Y1 <- model1_data$signal + model1_data$noise

## convert the flow vector into a flow matrix and apply lags
case1_data$Y9 <- matrix(case1_vector_reference$Y9,nrow = case1_data$n)
case1_data$Y9 <- list(case1_data$Y9,
                      case1_data$W %*% case1_data$Y9,
                      tcrossprod(case1_data$Y9, case1_data$W),
                      case1_data$W %*% tcrossprod(case1_data$Y9, case1_data$W))
case1_vector_reference$Y9 <-  vec_reference_matrix(case1_data$Y9)

case1_data$Y2 <- matrix(case1_vector_reference$Y2,nrow = case1_data$n)
case1_data$Y2 <- list(case1_data$Y2,
                      case1_data$W %*% case1_data$Y2)
case1_vector_reference$Y2 <-  vec_reference_matrix(case1_data$Y2)

case1_data$Y1 <- matrix(case1_vector_reference$Y1,nrow = case1_data$n)

# ---- reference moments ------------------------------------------------------
case1_moments1 <- named_list(c("N","HH","HY","ZZ","ZY","TSS"))
case1_moments2 <- named_list(c("N","HH","HY","ZZ","ZY","TSS"))
case1_moments9 <- named_list(c("N","HH","HY","ZZ","ZY","TSS"))

# X matrices with instruments
test_X_inst <- cbind("X" = germany_grid$X,
                     "X_lag" = case1_data$W %*% germany_grid$X)

test_X_inst <- cbind(test_X_inst,
                     case1_data$W %*% case1_data$W %*% test_X_inst)
case1_data$X <- test_X_inst %>% expand_O_D_I()
case1_vector_reference$X <- case1_data$X %>% vec_reference_O_D_I()

# G matrix with instruments
test_G_lag <- tcrossprod(case1_data$W %*% case1_data$G,case1_data$W)
test_G_lag2 <- tcrossprod(case1_data$W %*% test_G_lag,case1_data$W)
case1_data$G <- list(case1_data$G,test_G_lag,test_G_lag2)
case1_vector_reference$G <- vec_reference_matrix(case1_data$G)
rm(test_X_inst,test_G_lag,test_G_lag2)

# reference H
test_H <- cbind(case1_vector_reference$const,
                case1_vector_reference$const_intra,
                case1_vector_reference$X,
                case1_vector_reference$G)

# decalre which variables in H are instruments
instrumental_variables <- list(
  "const" = FALSE,
  "intra_const" = c(FALSE,rep(TRUE,8)),
  "X" = rep(c(FALSE,FALSE,TRUE,TRUE),3),
  "G" = c(FALSE,TRUE,TRUE)
)

# N, ZZ and HH are equal for all cases ()
case1_moments1$N <- case1_moments2$N <- case1_moments9$N <-
  case1_data$N

case1_moments1$ZZ <- case1_moments2$ZZ <- case1_moments9$ZZ <-
  crossprod(case1_data_sim$Z)

case1_moments2$HH <- case1_moments9$HH <-
  crossprod(test_H)

# all moments that include the flows are diffrent...
case1_moments2$HY <- crossprod(test_H,case1_vector_reference$Y2)
case1_moments9$HY <- crossprod(test_H,case1_vector_reference$Y9)

case1_moments1$ZY <- crossprod(case1_data_sim$Z,case1_vector_reference$Y1)
case1_moments2$ZY <- crossprod(case1_data_sim$Z,case1_vector_reference$Y2)
case1_moments9$ZY <- crossprod(case1_data_sim$Z,case1_vector_reference$Y9)

case1_moments1$TSS <- crossprod(case1_vector_reference$Y1)
case1_moments2$TSS <- crossprod(case1_vector_reference$Y2)
case1_moments9$TSS <- crossprod(case1_vector_reference$Y9)

# experiment ols-estimation ---------------------------------------------------------
beta <- solve(case1_moments1$ZZ,case1_moments1$ZY)
cbind(true = delta, est = beta)

# experiment s2sls-estimation model 9 ----------------------------------------------
J <- case1_vector_reference$Y9[,-1]
HJ <- case1_moments9$HY[,-1]

# verify that moments can be constructed without
# fitted values of the first stage (J_hat)
J_hat <- test_H %*% solve(case1_moments9$HH,crossprod(test_H,J))
JJ_hat <- crossprod(J_hat)
JJ_hat2 <- crossprod(HJ,solve(case1_moments9$HH,HJ))
JJ_hat - JJ_hat2

Jy_hat <- crossprod(HJ,solve(case1_moments9$HH,case1_moments9$HY[,1]))
Jy_hat2 <- crossprod(J_hat,case1_vector_reference$Y9[,1])
Jy_hat - Jy_hat2


ZJ <- case1_moments9$ZY[,-1]
ZZ_hat <- rbind(cbind(JJ_hat2,t(ZJ)),
                cbind(ZJ,case1_moments9$ZZ))

ZY_hat <- c(Jy_hat2,case1_moments9$ZY[,1])

mu <- solve(ZZ_hat,ZY_hat)
cbind(true = c(rho[model9_data$which_rho],delta),
      est = mu)

# check if ESS can be constructed without fitted values
fitted <- cbind(case1_vector_reference$Y9[,-1],case1_data_sim$Z) %*% mu
resid <- fitted - case1_vector_reference$Y9[,1]
ESS <- sum(fitted^2)
RSS <- sum(resid^2)
sd <- sqrt(RSS/case1_data$N)

TSS <- sum(case1_vector_reference$Y9[,1]^2)
ESS2 <- crossprod(ZY_hat,mu)
RSS2 <- TSS - ESS2
sd2 <- sqrt(RSS2/case1_data$N)

# ---- sort-test-case-data ----------------------------------------------------
test_case_1 <- list(
  "description" = case_description,
  "data" = case1_data,
  "vector_reference" = case1_vector_reference,
  "which_instruments" = instrumental_variables,
  "moments" = list("Y1" = case1_moments1,
                   "Y2" = case1_moments2,
                   "Y9" = case1_moments9),
  "simulation_inputs" = case1_data_sim)


save(test_case_1,
     file = "tests/testthat/test_case_1.rda")

