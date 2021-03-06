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

library("sf")
library("sp")
library("spflow")
source("data-raw/01_sim_data__germany_16_states.R")
source("data-raw/02_sim_data__usa_51_states.R")


# create the distances
all_dist <- as.matrix(dist(
  x = rbind(coordinates(as_Spatial(germany_grid)),
            coordinates(as_Spatial(usa_grid[,"ID_STATE"])))
  ))

n_ge <- nnodes(germany_net)
n_usa <- nnodes(usa_net)

index_ge <- seq_len(n_ge)
index_usa <- seq_len(n_usa) + n_ge

stack_od_log_distance <- function(od_dist) {
  spflow:::stack_columns(od_dist,
                         cols = "ID_DEST",
                         rows = "ID_ORIG",
                         value = "DISTANCE")
}

dist_ge_ge <- stack_od_log_distance(all_dist[index_ge, index_ge])
dist_ge_usa  <- stack_od_log_distance(all_dist[index_ge, index_usa])
dist_usa_ge  <- stack_od_log_distance(all_dist[index_usa,index_ge])
dist_usa_usa <- stack_od_log_distance(all_dist[index_usa,index_usa])

within_ge_pairs <- sp_network_pair(
  orig_net_id = "ge",
  dest_net_id = "ge",
  pair_data = dist_ge_ge,
  orig_key_column = "ID_ORIG",
  dest_key_column = "ID_DEST"
)

within_usa_pairs <- sp_network_pair(
  orig_net_id = "usa",
  dest_net_id = "usa",
  pair_data = dist_usa_usa,
  orig_key_column = "ID_ORIG",
  dest_key_column = "ID_DEST"
)

ge_to_usa_pairs <- sp_network_pair(
  orig_net_id = "ge",
  dest_net_id = "usa",
  pair_data = dist_ge_usa,
  orig_key_column = "ID_ORIG",
  dest_key_column = "ID_DEST"
)

usa_to_ge_pairs <- sp_network_pair(
  orig_net_id = "usa",
  dest_net_id = "ge",
  pair_data = dist_usa_ge,
  orig_key_column = "ID_ORIG",
  dest_key_column = "ID_DEST"
)
