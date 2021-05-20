# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Project: spflow - simulate 51 states of the USA as an example network
# Author: Lukas Dargel & Thibault Laurent
# = = = = = = = = = = = = = = = = = = =
# Description:
#
# We create an artificial explanatory variable, as well as a simplifyed spatial
# structre for the USA.
# = = = = = = = = = = = = = = = = = = =
# Notes:
#
# Our examples come from https://ialab.it.monash.edu/~dwyer/papers/maptrix.pdf
# - - - - - - - - - - - - - - - - - - -
# Date: February 2021

library("magrittr")
library("sf")
library("sp")
library("spdep")
source("data-raw/helpers_sim-data.R")

# generate data for the 16 states of the USA
usa_data <-
  data.frame(
    "ID_STATE" =
      c("AK", "ME", "WI", "VT", "NH", "WA", "ID", "MT", "ND",
        "MN", "IL", "MI", "NY", "MA", "OR", "NV", "WY", "SD",
        "IA", "IN", "OH", "PA", "NJ", "CT", "RI", "CA", "UT",
        "CO", "NE", "MO", "KY", "WV", "VA", "MD", "DE", "AZ",
        "NM", "KS", "AR", "TN", "NC", "SC", "DC", "OK", "LA",
        "MS", "AL", "GA", "HI", "TX", "FL"),
    "X" =
      c(35, 29, 30, 29, 26, 35, 31, 28, 32, 40, 32, 27, 33, 32,
        31, 32, 25, 35, 32, 31, 35, 32, 38, 29, 35, 31, 27, 29,
        30, 33, 38, 30, 30, 30, 40, 35, 34, 34, 33, 37, 31, 31,
        31, 27, 32, 31, 30, 28, 29, 29, 34))

# add stylized geographic information
# introduce shifts compared to the german example
usa_offset_x <- -20
usa_offset_y <- -5

state_coordinates <- list(
  "x" = c(0, 10, 5, 9, 10, 0, 1, 2, 3, 4, 5, 6, 8, 9, 0:10, 0:9, 1:8, 3:7, 0, 3, 8
  ) + usa_offset_x,
  "y" = c(7, 7, rep(6, 3), rep(5, 9), rep(4, 11), rep(3, 10), rep(2, 8), rep(1, 5), rep(0, 3)
  ) + usa_offset_y
)

usa_grid <- SpatialPointsDataFrame(
  coords = Reduce("cbind", state_coordinates),
  data = data.frame(usa_data, row.names = "ID_STATE")) %>%
  create_grid(.) %>%
  st_as_sf()

usa_grid <- st_as_sf(usa_grid)[c(2,1,3)]
names(usa_grid)[c(1,2)] <- names(usa_data)

usa_4_nearest_neighbours <-
  suppressWarnings(st_centroid(usa_grid)) %>%
  knearneigh(k = 4) %>%
  knn2nb() %>%
  nb2listw() %>%
  listw2mat()

usa_net <- sp_network_nodes(
  network_id = "usa",
  node_neighborhood = usa_4_nearest_neighbours,
  node_data = usa_data,
  node_key_column = "ID_STATE")

save(usa_net, file = "data/usa_net.rda")
save(usa_grid, file = "data/usa_grid.rda")
