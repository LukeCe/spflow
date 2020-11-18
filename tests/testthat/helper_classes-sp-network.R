# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Project: spflow - test sp_network objects
# Author: Lukas Dargel
# = = = = = = = = = = = = = = = = = = =
# Description:
#
# Create mock classes to test sp_network objects "alphabet dummy networks".
# The objects are meant to test the case of complete flows (all orig and dest)
# "within" and "between" networks.
# - - - - - - - - - - - - - - - - - - -
# Date: November 2020
set.seed(1)

# define two test networks ----------------------------------------------------
# network LETTERS
n_o <- 10  # number of origins
origin_list <- LETTERS[seq_len(n_o)]

net_dat_LETTERS <- data.table::data.table(
  "ID" = origin_list %>% factor_in_order(),
  "XX" = rnorm(n_o),
  key = "ID")

sp_net_LETTERS <- sp_network_nodes(
  network_id = "LETTERS",
  node_id_column = "ID",
  node_neighborhood = Diagonal(n_o) * 2,
  node_data = net_dat_LETTERS)

# network letters
n_o <- 8  # number of origins
origin_list <- letters[seq_len(n_o)]

net_dat_letters <- data.table::data.table(
  "ID" = origin_list %>% factor_in_order(),
  "XX" = rnorm(n_o),
  key = "ID")

sp_net_letters <- sp_network_nodes(
  network_id = "letters",
  node_id_column = "ID",
  node_neighborhood = Diagonal(n_o) * 2,
  node_data = net_dat_letters)

# define the four possible network pairs --------------------------------------

# 1) LETTERS_LETTERS
n_o <- nrow(net_dat_LETTERS)
origin_list <- LETTERS[seq_len(n_o)]
N <- n_o^2
pair_dat_LETTERS <- data.table::data.table(
  "ORIG_ID" = rep(origin_list,n_o) %>% factor_in_order(),
  "DEST_ID" = rep(origin_list, each = n_o) %>% factor_in_order(),
  "YY" = rnorm(N),
  "GG" = rnorm(N),
  key = c("ORIG_ID","DEST_ID"))

sp_pair_LETTERS <- sp_network_pair(
  orig_net_id = "LETTERS", dest_net_id = "LETTERS",
  pair_data = pair_dat_LETTERS,
  orig_key_column = "ORIG_ID", dest_key_column = "DEST_ID")

# 2) letters_letters
n_o <- nrow(net_dat_letters)
origin_list <- letters[seq_len(n_o)]
N <- n_o^2
pair_dat_letters <- data.table::data.table(
  "ORIG_ID" = rep(origin_list,n_o) %>% factor_in_order(),
  "DEST_ID" = rep(origin_list, each = n_o) %>% factor_in_order(),
  "YY" = rnorm(N),
  "GG" = rnorm(N),
  key = c("ORIG_ID","DEST_ID"))

sp_pair_letters <- sp_network_pair(
  orig_net_id = "letters", dest_net_id = "letters",
  pair_data = pair_dat_letters,
  orig_key_column = "ORIG_ID", dest_key_column = "DEST_ID")

# 3) LETTERS_letters
n_o <- nrow(net_dat_LETTERS)
n_d <- nrow(net_dat_letters)
origin_list <- LETTERS[seq_len(n_o)]
destination_list <- letters[seq_len(n_d)]
N <- n_o*n_d
pair_dat_LETters <- data.table::data.table(
  "ORIG_ID" = rep(origin_list,n_d) %>% factor_in_order(),
  "DEST_ID" = rep(destination_list, each = n_o) %>% factor_in_order(),
  "YY" = rnorm(N),
  "GG" = rnorm(N),
  key = c("ORIG_ID","DEST_ID"))

sp_pair_LETters <- sp_network_pair(
  orig_net_id = "LETTERS", dest_net_id = "letters",
  pair_data = pair_dat_LETters,
  orig_key_column = "ORIG_ID", dest_key_column = "DEST_ID")

# 4) letters_LETTERS
n_o <- nrow(net_dat_letters)
n_d <- nrow(net_dat_LETTERS)
origin_list <- letters[seq_len(n_o)]
destination_list <- LETTERS[seq_len(n_d)]
N <- n_o*n_d
pair_dat_letTERS <- data.table::data.table(
  "ORIG_ID" = rep(origin_list,n_d) %>% factor_in_order(),
  "DEST_ID" = rep(destination_list, each = n_o) %>% factor_in_order(),
  "YY" = rnorm(N),
  "GG" = rnorm(N),
  key = c("ORIG_ID","DEST_ID"))

sp_pair_letTERS <- sp_network_pair(
  orig_net_id = "letters", dest_net_id = "LETTERS",
  pair_data = pair_dat_letTERS,
  orig_key_column = "ORIG_ID", dest_key_column = "DEST_ID")

sp_multi_net_alphabet <- sp_multi_network(
  sp_net_letters,sp_net_LETTERS,
  sp_pair_LETTERS,sp_pair_letters,sp_pair_LETters,sp_pair_letTERS)
