# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Project: spflow - simulate flows based on the example networks
# Author: Lukas Dargel & Thibault Laurent
# = = = = = = = = = = = = = = = = = = =
# Description:
#
# For the two example networks we simulate trade flows for the the four
# possible pairs of networks.
# - - - - - - - - - - - - - - - - - - -
# Date: Mai 2020

library("sp")
library("spflow")
data("germany_grid")
data("germany_net")
data("usa_grid")
data("usa_net")

# create the distances
all_dist <-
  dist(x = rbind(coordinates(germany_grid),
                 coordinates(usa_grid))) %>%
  as.matrix()

n_ge <- count(germany_net)
n_usa <- count(usa_net)

index_ge <- seq_len(n_ge)
index_usa <- seq_len(n_usa) + n_ge

dist_ge_ge   <- spflow:::stack_cols(all_dist[index_ge, index_ge],value = "distance")
dist_ge_usa  <- spflow:::stack_cols(all_dist[index_ge, index_usa],value = "distance")
dist_usa_ge  <- spflow:::stack_cols(all_dist[index_usa,index_ge],value = "distance")
dist_usa_usa <- spflow:::stack_cols(all_dist[index_usa,index_usa],value = "distance")

within_ge_pairs <- sp_network_pair(
  origin_network_id = "ge",
  destination_network_id = "ge",
  node_pair_data = dist_ge_ge,
  origin_key_column = "row",
  destination_key_column = "col"
)

within_usa_pairs <- sp_network_pair(
  origin_network_id = "usa",
  destination_network_id = "usa",
  node_pair_data = dist_usa_usa,
  origin_key_column = "row",
  destination_key_column = "col"
)

ge_to_usa_pairs <- sp_network_pair(
  origin_network_id = "ge",
  destination_network_id = "usa",
  node_pair_data = dist_ge_usa,
  origin_key_column = "row",
  destination_key_column = "col"
)

usa_to_ge_pairs <- sp_network_pair(
  origin_network_id = "usa",
  destination_network_id = "ge",
  node_pair_data = dist_usa_ge,
  origin_key_column = "row",
  destination_key_column = "col"
)
