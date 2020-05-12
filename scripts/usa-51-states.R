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
# Our examples come from https://ialab.it.monash.edu/~dwyer/papers/maptrix.pdf.
# - - - - - - - - - - - - - - - - - - -
# Date: Mai 2020

library("sp")
library("spdep")
library("spflow")
source("scripts/create_grid.R")

# invent data for the 51 states of the USA
usa_data <-
  data.frame(
    "state_ids" = c("AK", "ME", "WI", "VT", "NH", "WA", "ID", "MT", "ND",
                    "MN", "IL", "MI", "NY", "MA", "OR", "NV", "WY", "SD",
                    "IA", "IN", "OH", "PA", "NJ", "CT", "RI", "CA", "UT",
                    "CO", "NE", "MO", "KY", "WV", "VA", "MD", "DE", "AZ",
                    "NM", "KS", "AR", "TN", "NC", "SC", "DC", "OK", "LA",
                    "MS", "AL", "GA", "HI", "TX", "FL"),
    "invented_gdp" = c(35, 29, 30, 29, 26, 35, 31, 28, 32, 40, 32, 27, 33, 32,
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
  data = data.frame(usa_data, row.names = "state_ids")) %>%
  create_grid(.)

usa_4_nearest_neighbours <- usa_grid %>%
  coordinates() %>%
  knearneigh(k = 4) %>%
  knn2nb() %>%
  nb2listw() %>%
  listw2mat()

usa_net <- sp_network(
  network_id = "usa",
  node_neighborhood = usa_4_nearest_neighbours,
  node_data = usa_data,
  node_id_column = "state_ids")

usethis::use_data(usa_net, overwrite = TRUE)
usethis::use_data(usa_grid, overwrite = TRUE)
