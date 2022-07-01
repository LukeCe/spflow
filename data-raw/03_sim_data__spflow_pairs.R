# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Project: spflow - simulate flows based on the example networks
# Author: Lukas Dargel & Thibault Laurent
# = = = = = = = = = = = = = = = = = = =
# Description:
#
# For the two example networks we simulate trade flows for the four
# possible pairs of networks. Each network pair is used to cover one of four
# model types:
# - ge_ge  : square      + cartesian
# - usa_usa: square      + non-cartesian
# - ge_usa : rectangular + cartesian
# - usa_ge : rectangular + non-cartesian
# - - - - - - - - - - - - - - - - - - -
# Date: Jan 2022

library("sf")
library("sp")
library("spflow")
source("data-raw/01_sim_data__germany_16_states.R")
source("data-raw/02_sim_data__usa_51_states.R")
set.seed(1234)

# ---- create the pairwise distances ------------------------------------------
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

# ---- create the network pair objects ----------------------------------------
# for the non-cartesian cases the probability of inclusion is computed as a
# function of the distance
subsample_pairs <- function(pair_df) {

  n <- nrow(pair_df)
  n_keep <- ceiling(n * .3)

  keep_probs <- 1 / (pair_df[["DISTANCE"]]^2 + sqrt(2))
  keep_index <- sort(sample(seq(n),n_keep,prob = keep_probs))

  return(pair_df[keep_index,])
}


within_ge_pairs <- spflow_pairs(
  id_orig_nodes = "ge",
  id_dest_nodes = "ge",
  pair_data = dist_ge_ge,
  orig_key_column = "ID_ORIG",
  dest_key_column = "ID_DEST"
)


within_usa_pairs <- spflow_pairs(
  id_orig_nodes = "usa",
  id_dest_nodes = "usa",
  pair_data = subsample_pairs(dist_usa_usa),
  orig_key_column = "ID_ORIG",
  dest_key_column = "ID_DEST"
)

ge_to_usa_pairs <- spflow_pairs(
  id_orig_nodes = "ge",
  id_dest_nodes = "usa",
  pair_data = dist_ge_usa,
  orig_key_column = "ID_ORIG",
  dest_key_column = "ID_DEST"
)

usa_to_ge_pairs <- spflow_pairs(
  id_orig_nodes = "usa",
  id_dest_nodes = "ge",
  pair_data = subsample_pairs(dist_usa_ge),
  orig_key_column = "ID_ORIG",
  dest_key_column = "ID_DEST"
)
