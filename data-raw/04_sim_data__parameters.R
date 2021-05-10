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
# Date: February 2021

# model coefficients
delta <- c("(Intercept)"    = 12,
           "(Intra)"        = 5,
           "ORIG_X"         = 0.8,
           "ORIG_X.lag1"    = 0.3,
           "DEST_X"         = 1,
           "DEST_X.lag1"    = 0.75,
           "INTRA_X"        = 2,
           "INTRA_X.lag"    = 0.5,
           "DISTANCE"       = -2)


# auto-correlation parameters
rho <- c(rho_d   = 0.45,
         rho_o   = 0.32,
         rho_w   = -0.21,
         rho_od  = 0.41,
         rho_odw = 0.25)

# sd of errors
sd_error <- c(sigma = 2)

simulation_params <- list("delta" = delta, "rho" = rho, "sd_error" = sd_error)

save(simulation_params, file = "data/simulation_params.rda")
