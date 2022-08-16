library("spflow")
# ---- constructor ------------------------------------------------------------

expect_inherits({
  test_network_ids <- c("net1","net2")
  test_nodes <- lapply(test_network_ids, "spflow_network_nodes")
  test_pairs <- lapply(test_network_ids,
                       function(.id) spflow_network_pairs(.id, .id))
  spflow_network_multi(test_nodes,test_pairs)
  },
  class = "spflow_network_multi")

expect_warning({
  test_nodes <- spflow_network_nodes("net1")
  spflow_network_multi(test_nodes,data.frame(1))
  },
  info = "warn when dropping unused classes")

expect_inherits({
  suppressWarnings(spflow_network_multi(test_nodes,data.frame(1)))
  },
  class =  "spflow_network_multi")

expect_error({
  test_o_net <-
    spflow_network_nodes("o1",NULL,data.frame("ID" = LETTERS[1:3]),"ID")
  test_pairs_wrong_orig <-
    spflow_network_pairs("o1","d1",data.frame("ID_O" = rep(LETTERS[2:4],times = 3),
                                         "ID_D" = rep(letters[1:3],each = 3)),
                    "ID_O","ID_D")
  spflow_network_multi(test_o_net, test_pairs_wrong_orig)
  },
  info = "ids of origins are diffrent than node ids")

expect_warning({
  test_o_net <- spflow_network_nodes(
    "o1",
    node_neighborhood = NULL,
    node_data = data.frame("ID" = LETTERS[1:3]),
    node_key_column = "ID")

  test_d_net <- spflow_network_nodes(
    "d1",
    node_neighborhood = NULL,
    node_data = data.frame("ID" = letters[1:3]),
    node_key_column = "ID")

  test_pairs_unordered <- spflow_network_pairs(
    id_orig_nodes = "o1",
    id_dest_nodes = "d1",
    pair_data = data.frame(
      "ID_O" = rep(LETTERS[c(2,1,3)], 3),
      "ID_D" = rep(letters[3:1], each = 3)),
    orig_key_column = "ID_O",
    dest_key_column = "ID_D")

  spflow_network_multi(test_o_net, test_d_net, test_pairs_unordered)
  },
  info = "wrong ordering of nodes gives a warning")


expect_warning({
  test_o_net <- spflow_network_nodes(
    "o1",
    node_neighborhood = NULL,
    node_data = data.frame("ID" = LETTERS[1:3]),
    node_key_column = "ID")

  test_d_net <- spflow_network_nodes(
    "d1",
    node_neighborhood = NULL,
    node_data = data.frame("ID" = letters[1:3]),
    node_key_column = "ID")

  test_pairs_unordered <- spflow_network_pairs(
    id_orig_nodes = "o1",
    id_dest_nodes = "d1",
    pair_data = data.frame(
      "ID_O" = rep(LETTERS[c(2,1,3)], 3),
      "ID_D" = rep(letters[3:1], each = 3)),
    orig_key_column = "ID_O",
    dest_key_column = "ID_D")

  test_multinet_ordered <- spflow_network_multi(
    test_o_net, test_d_net, test_pairs_unordered)
}, info = "adjusts wrong ordering of od keys when possible")

expect_equal({
  test_o_net <- spflow_network_nodes(
    "o1",
    node_neighborhood = NULL,
    node_data = data.frame("ID" = LETTERS[1:3]),
    node_key_column = "ID")

  test_d_net <- spflow_network_nodes(
    "d1",
    node_neighborhood = NULL,
    node_data = data.frame("ID" = letters[1:3]),
    node_key_column = "ID")

  test_pairs_unordered <- spflow_network_pairs(
      id_orig_nodes = "o1",
      id_dest_nodes = "d1",
      pair_data = data.frame(
        "ID_O" = rep(LETTERS[c(2,1,3)], 3),
        "ID_D" = rep(letters[3:1], each = 3)),
      orig_key_column = "ID_O",
      dest_key_column = "ID_D")

  suppressWarnings({test_multinet_ordered <-
    spflow_network_multi(test_o_net, test_d_net, test_pairs_unordered)
  })
  test_pairs_ordered <- test_multinet_ordered@pairs$o1_d1@pair_data
  cbind(levels(test_pairs_ordered[["ID_O"]]),
        levels(test_pairs_ordered[["ID_D"]]))
  },
  cbind(LETTERS[1:3],letters[1:3]),
  info = "adjusts wrong ordering of od keys when possible")

# ---- accessing methods ------------------------------------------------------
expect_equal({
  test_network_ids <- c("net1","net2")
  test_nodes <- lapply(test_network_ids, "spflow_network_nodes")
  test_pairs <- lapply(test_network_ids,
                       function(.id) spflow_network_pairs(.id, .id))
  test_multinet <- spflow_network_multi(test_nodes,test_pairs)
  id(test_multinet)
  },
  list("nodes" = c("net1", "net2"),
       "pairs" =  c("net1_net1", "net2_net2")),
  info = "acessing the id works")

expect_equal({
  test_multinet <- spflow_network_multi(spflow_network_nodes("net1"))
  pull_member(test_multinet, "net1")
  },
  spflow_network_nodes("net1"),
  info = "pull existing net")

expect_error({
  test_multinet <- spflow_network_multi(spflow_network_nodes("net1"))
  pull_member(test_multinet, "net2")
  },
  info = "pull non-existing net")

expect_equal({
  test_multinet <- spflow_network_multi(spflow_network_pairs("net1","net1"))
  pull_member(test_multinet, "net1_net1")
  },
  spflow_network_pairs("net1","net1"),
  info = "pull existing pair")

expect_error({
  test_multinet <- spflow_network_multi(spflow_network_pairs("net1","net1"))
  pull_member(test_multinet, "net2_net2")
  },
  info = "pull non-existing pair")


# ---- pair_merge -------------------------------------------------------------
expect_equal({
  test_o_net <- spflow_network_nodes(
    "net1",
    node_neighborhood =  NULL,
    node_data =  data.frame("ID" = c("A", "B"),"VAL" = "OO"),
    node_key_column = "ID")

  test_d_net <- spflow_network_nodes(
    "net2",
    node_neighborhood =  NULL,
    node_data =  data.frame("ID" = c("C","D"), "VAL" = "DD"),
    node_key_column = "ID")

  test_net_pair <- spflow_network_pairs(
    id_orig_nodes = "net1",
    id_dest_nodes = "net2",
    pair_data = data.frame(
      "ID_O" = c("A","A","B","B"),
      "ID_D" = c("C","D","C","D"),
      "DIST" = 1:4),
    orig_key_column = "ID_O",
    dest_key_column =  "ID_D")
  test_multinet <- spflow_network_multi(test_net_pair,test_o_net,test_d_net)
  data.frame(pair_merge(test_multinet, "net1_net2",
                        pair_cols = "DIST",
                        orig_cols = "VAL",
                        dest_cols = "VAL"))
  },
  {
    data.frame("ID_D" = factor(c("C","D","C","D")),
               "ID_O" = factor(c("A","A","B","B")),
               "DIST" = 1:4,
               "D_VAL" = "DD",
               "O_VAL" = "OO")
  },
  info = "merging origin and destination infos to the pairs")

expect_equal({
  # invert order of ids for pairs
  test_o_net <- spflow_network_nodes(
    "net1",
    node_neighborhood =  NULL,
    node_data =  data.frame("ID" = c("B","A"),"VAL" = "OO"),
    node_key_column = "ID")

  test_d_net <- spflow_network_nodes(
    "net2",
    node_neighborhood =  NULL,
    node_data =  data.frame("ID" = c("D", "C"), "VAL" = "DD"),
    node_key_column = "ID")

  test_net_pair <- spflow_network_pairs(
    id_orig_nodes = "net1",
    id_dest_nodes = "net2",
    pair_data = data.frame(
      "ID_O" = c("A", "B"),
      "ID_D" = c("C", "D"),
      "DIST" = c(1,4)),
    orig_key_column = "ID_O",
    dest_key_column =  "ID_D")

  test_multinet <- suppressWarnings(spflow_network_multi(
    test_net_pair, test_o_net, test_d_net))

  data.frame(pair_merge(test_multinet, "net1_net2", make_cartesian = TRUE, pair_cols = "DIST"))
  },
  {
    data.frame(
      "ID_D" = factor(c("D", "C", "D", "C"), levels = c("D","C")),
      "ID_O" = factor(c("B", "B","A", "A"), levels = c("B","A")),
      "DIST" = c(4, NA, NA, 1)
    )
  },
  info = "merging origin and destination infos to the pairs
          test expansion of missing pairs and correct ordering")

expect_equal({
  # invert order of ids for pairs
  test_o_net <- spflow_network_nodes(
    "net1",
    node_neighborhood =  NULL,
    node_data =  data.frame("ID" = c("B","A"),"VAL" = "OO"),
    node_key_column = "ID")

  test_d_net <- spflow_network_nodes(
    "net2",
    node_neighborhood =  NULL,
    node_data =  data.frame("ID" = c("D", "C"), "VAL" = "DD"),
    node_key_column = "ID")

  test_net_pair <- spflow_network_pairs(
    id_orig_nodes = "net1",
    id_dest_nodes = "net2",
    pair_data = data.frame(
      "ID_O" = c("A", "B"),
      "ID_D" = c("C", "D"),
      "DIST" = c(1,4)),
    orig_key_column = "ID_O",
    dest_key_column =  "ID_D")

  test_multinet <- suppressWarnings(spflow_network_multi(
    test_net_pair, test_o_net, test_d_net))

  data.frame(pair_merge(test_multinet, "net1_net2",
                        pair_cols = NULL,
                        orig_cols = "VAL",
                        dest_cols = "VAL",
                         keep_od_keys = FALSE,
                        make_cartesian = TRUE))
},
{
  data.frame(
    "D_VAL" = rep("DD",4),
    "O_VAL" = rep("OO",4))
},
info = "pair_merge with selection")

# ----- check_pair_completeness -----------------------------------------------
expect_equal({
  test_o_net <- spflow_network_nodes(
    "net1",
    node_neighborhood =  NULL,
    node_data = data.frame(
      "ID"  = c("A", "B"),
      "VAL" = "OO"),
    node_key_column = "ID")

  test_d_net <- spflow_network_nodes(
    "net2",
    node_neighborhood =  NULL,
    node_data = data.frame(
      "ID"  = c("C", "D"),
      "VAL" = "DD"),
    node_key_column = "ID")

  test_net_pair <- spflow_network_pairs(
    id_orig_nodes = "net1",
    id_dest_nodes = "net2",
    pair_data = data.frame(
      "ID_O" = c("A", "B"),
      "ID_D" = c("C", "D"),
      "DIST" = c(1, 4)),
    orig_key_column = "ID_O",
    dest_key_column = "ID_D")

  test_multinet <- spflow_network_multi(test_net_pair, test_o_net, test_d_net)
  check_infos <- c("ID_NET_PAIR", "NPAIRS", "COMPLETENESS",
                   "ID_ORIG_NET", "ORIG_NNODES",
                   "ID_DEST_NET", "DEST_NNODES")
  spflow:::check_pair_completeness("net1_net2", test_multinet)[,check_infos]
}, {
  data.frame("ID_NET_PAIR" = "net1_net2",
             "NPAIRS" = 2,
             "COMPLETENESS" = 0.5,
             "ID_ORIG_NET" = "net1",
             "ORIG_NNODES" = 2,
             "ID_DEST_NET" = "net2",
             "DEST_NNODES" = 2,
             row.names = NULL)
}, info = "create completeness info")

# ---- update_dat -------------------------------------------------------------
expect_equal({
  new_dat_ge <- dat(multi_net_usa_ge, "ge")[1:5,1:2]
  new_dat_ge$X <- 1:5
  new_dat_ge_ge <- dat(multi_net_usa_ge, "ge_ge")[1:5,1:3]
  new_dat_ge_ge$DISTANCE <- 1:5
  update_dat(multi_net_usa_ge, list("ge" = new_dat_ge, "ge_ge" = new_dat_ge_ge))
},{
  multi_net_usa_ge2 <- multi_net_usa_ge
  multi_net_usa_ge2@nodes$ge@node_data$X[1:5] <- 1:5
  multi_net_usa_ge2@pairs$ge_ge@pair_data$DISTANCE[1:5] <- 1:5
  multi_net_usa_ge2
})

# ---- show method ------------------------------------------------------------
expect_stdout({
  test_o_net <- spflow_network_nodes(
    "net1",
    node_neighborhood =  NULL,
    node_data = data.frame(
      "ID"  = c("A", "B"),
      "VAL" = "OO"),
    node_key_column = "ID")

  test_d_net <- spflow_network_nodes(
    "net2",
    node_neighborhood =  NULL,
    node_data = data.frame(
      "ID"  = c("C", "D"),
      "VAL" = "DD"),
    node_key_column = "ID")

  test_net_pair <- spflow_network_pairs(
    id_orig_nodes = "net1",
    id_dest_nodes = "net2",
    pair_data = data.frame(
      "ID_O" = c("A", "B"),
      "ID_D" = c("C", "D"),
      "DIST" = c(1, 4)),
    orig_key_column = "ID_O",
    dest_key_column = "ID_D")

  test_multinet <- spflow_network_multi(test_net_pair, test_o_net, test_d_net)
  test_multinet
  },
  info = "show something on print")

