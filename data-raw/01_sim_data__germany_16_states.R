# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Project: spflow - simulate 16 German states as an example network
# Author: Lukas Dargel & Thibault Laurent
# = = = = = = = = = = = = = = = = = = =
# Description:
#
# We create an artificial explanatory variable, as well as a simplified spatial
# structure for the states Germany.
# = = = = = = = = = = = = = = = = = = =
# Notes:
#
# Our examples come from https://ialab.it.monash.edu/~dwyer/papers/maptrix.pdf
# - - - - - - - - - - - - - - - - - - -
# Date: December 2021

library("magrittr")
library("sf")
library("sp")
library("spdep")
library("spflow")
source("data-raw/helpers_sim-data.R")

# generate data for the 16 states of Germany
germany_data <-
  data.frame("ID_STATE" = c("SH", "HH", "MV", "NW", "HB", "BB", "BE", "RP",
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
  data = data.frame(germany_data, row.names = "ID_STATE"))
germany_grid <- create_grid(germany_grid)
germany_grid <- st_as_sf(germany_grid)

germany_grid <- st_as_sf(germany_grid)[c(2,1,3)]
names(germany_grid)[c(1,2)] <- names(germany_data)

germany_contiguity <- poly2nb(germany_grid)
germany_contiguity <- nb2listw(germany_contiguity)
germany_contiguity <- listw2mat(germany_contiguity)

germany_net <- sp_network_nodes(
  network_id = "ge",
  node_neighborhood = germany_contiguity,
  node_data = germany_grid,
  node_key_column = "ID_STATE")

germany_inputs <- list("data" = germany_data,
                       "neighborhood" = germany_contiguity,
                       "key_column" = "ID_STATE",
                       "net_id" = "ge")

save(germany_inputs, file = "tests/integration/germany_inputs.rda", compress = "bzip2")
save(germany_net, file = "data/germany_net.rda", compress = "bzip2")
save(germany_grid, file = "data/germany_grid.rda", compress = "bzip2")
