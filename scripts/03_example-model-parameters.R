# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Project: spflow - define a set of parameters to simulate flows
# Author: Lukas Dargel & Thibault Laurent
# = = = = = = = = = = = = = = = = = = =
# Description:
#
# For the two example networks we simulate trade flows for the the four
# possible pairs of networks.
# This script sets the model parameters.
# - - - - - - - - - - - - - - - - - - -
# Date: Mai 2020


#### Model parameters
# model coefficients
delta <- c("(Intercept)"    = 12,
           "(Intra)"        = 5,
           "orig_X"         = 0.8,
           "orig_X_lag"     = 0.3,
           "dest_X"         = 1,
           "dest_X_lag"     = 0.75,
           "intra_X"        = 2,
           "intra_X_lag"    = 0.5,
           "distance"       = -2)


# auto-correlation parameters
rho <- c(rho_d = 0.45,
         rho_o = 0.32,
         rho_w = -0.21,
         rho_od = 0.41,
         rho_odw = 0.25)

# sd of errors
sd_error <- c(sigma = 2)

save(list = ls(),file = "data/simulation_parameters.rda")
