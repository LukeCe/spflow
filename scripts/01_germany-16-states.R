# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Project: spflow - simulate 16 german states as an example network
# Author: Lukas Dargel & Thibault Laurent
# = = = = = = = = = = = = = = = = = = =
# Description:
#
# We create an artificial explanatory variable, as well as a simplifyed spatial
# structre for the states Germany.
# = = = = = = = = = = = = = = = = = = =
# Notes:
#
# Our examples come from https://ialab.it.monash.edu/~dwyer/papers/maptrix.pdf.
# - - - - - - - - - - - - - - - - - - -
# Date: Mai 2020

load_all()
library("sp")
library("spdep")
source("scripts/00_create_grid.R")

# invent data for the 16 states of germany
germany_data <-
  data.frame("state_ids" = c("SH", "HH", "MV", "NW", "HB", "BB", "BE", "RP",
                             "NI", "ST", "SN", "SL", "HE", "TH", "BW", "BY"),
             "X" = c(10, 15, 20,  7, 20, 25, 15, 10,
                     30, 20, 15, 10, 15, 10,  7, 7))

# add stylized geographic information
state_coordinates <- list(
  "x" = c(1, 1, 2, 0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 1, 2),
  "y" = c(5, 4, 4, 3, 3, 3, 3, 2, 2, 2, 2, 1, 1, 1, 0, 0)
)

germany_grid <- SpatialPointsDataFrame(
  coords = Reduce("cbind", state_coordinates),
  data = data.frame(germany_data, row.names = "state_ids")) %>%
  create_grid(.)

germany_contingency <- germany_grid %>%
  poly2nb() %>%
  nb2listw() %>%
  listw2mat()

# add a spatial lag
germany_data <- cbind(germany_data,
                      X_lag = germany_contingency %*% germany_data$X)

germany_net <- sp_network_nodes(
  network_id = "ge",
  node_neighborhood = germany_contingency,
  node_data = germany_data,
  node_id_column = "state_ids")

usethis::use_data(germany_net, overwrite = TRUE)
usethis::use_data(germany_grid, overwrite = TRUE)
