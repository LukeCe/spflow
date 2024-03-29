# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Project: spflow - define a set of parameters to simulate flows
# Author: Lukas Dargel & Thibault Laurent
# = = = = = = = = = = = = = = = = = = =
# Description:
#
# For the two example networks we simulate trade flows for the the four
# possible pairs of networks.
# This script sets the model parameters used for the simulation.
# - - - - - - - - - - - - - - - - - - -
# Date: Mai 2021

# model coefficients
delta <- c(
  "(Intercept)" =  12,
  "(Intra)"     =  10,
  "D_X"         =  1,
  "D_X.lag1"    =  0.6,
  "O_X"         =  -0.8,
  "O_X.lag1"    =  -0.4,
  "I_X"         =  2,
  "P_DISTANCE"  = -3)

# auto-correlation parameters
rho <- c(
  "rho_d"   =  0.45,
  "rho_o"   =  0.32,
  "rho_w"   = -0.21
)

# sd of errors
sd_error <- c("sigma" = 2)

simulation_params <- list("delta" = delta, "rho" = rho, "sd_error" = sd_error)
save(simulation_params, file = "data/simulation_params.rda")
