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

n_ge <- nnodes(germany_net)
n_usa <- nnodes(usa_net)

index_ge <- seq_len(n_ge)
index_usa <- seq_len(n_usa) + n_ge

dist_ge_ge   <- spflow:::stack_cols(all_dist[index_ge, index_ge],value = "distance")
dist_ge_usa  <- spflow:::stack_cols(all_dist[index_ge, index_usa],value = "distance")
dist_usa_ge  <- spflow:::stack_cols(all_dist[index_usa,index_ge],value = "distance")
dist_usa_usa <- spflow:::stack_cols(all_dist[index_usa,index_usa],value = "distance")

within_ge_pairs <- sp_network_pair(
  orig_net_id = "ge",
  dest_net_id = "ge",
  pair_data = dist_ge_ge,
  orig_key_column = "row",
  dest_key_column = "col"
)

within_usa_pairs <- sp_network_pair(
  orig_net_id = "usa",
  dest_net_id = "usa",
  pair_data = dist_usa_usa,
  orig_key_column = "row",
  dest_key_column = "col"
)

ge_to_usa_pairs <- sp_network_pair(
  orig_net_id = "ge",
  dest_net_id = "usa",
  pair_data = dist_ge_usa,
  orig_key_column = "row",
  dest_key_column = "col"
)

usa_to_ge_pairs <- sp_network_pair(
  orig_net_id = "usa",
  dest_net_id = "ge",
  pair_data = dist_usa_ge,
  orig_key_column = "row",
  dest_key_column = "col"
)
