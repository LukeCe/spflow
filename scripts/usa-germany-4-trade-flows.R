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

dist_ge_ge   <- stack_cols(all_dist[index_ge, index_ge])
dist_ge_usa  <- stack_cols(all_dist[index_ge, index_usa])
dist_usa_ge  <- stack_cols(all_dist[index_usa,index_ge])
dist_usa_usa <- stack_cols(all_dist[index_usa,index_usa])

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

multi_net_usa_ge <- sp_multi_network(
  germany_net,
  usa_net,
  within_ge_pairs,
  within_usa_pairs,
  ge_to_usa_pairs,
  usa_to_ge_pairs
)

usethis::use_data(multi_net_usa_ge,overwrite = TRUE)
