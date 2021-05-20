# ==== [+++ constructor +++] ==================================================
library("spflow")

expect_inherits({
  test_network_ids <- c("net1","net2")
  test_nodes <- lapply(test_network_ids, "sp_network_nodes")
  test_pairs <- lapply(test_network_ids,
                       function(.id) sp_network_pair(.id, .id))
  sp_multi_network(test_nodes,test_pairs)
  },
  class = "sp_multi_network")

expect_warning({
  test_nodes <- sp_network_nodes("net1")
  sp_multi_network(test_nodes,data.frame(1))
  },
  info = "warn when dropping unused classes")

expect_inherits({
  suppressWarnings(sp_multi_network(test_nodes,data.frame(1)))
  },
  class =  "sp_multi_network")

expect_error({
  test_o_net <-
    sp_network_nodes("o1",NULL,data.frame("ID" = LETTERS[1:3]),"ID")
  test_pairs_wrong_orig <-
    sp_network_pair("o1","d1",data.frame("ID_O" = rep(LETTERS[2:4],times = 3),
                                         "ID_D" = rep(letters[1:3],each = 3)),
                    "ID_O","ID_D")
  sp_multi_network(test_o_net, test_pairs_wrong_orig)
  },
  info = "ids of origins are diffrent than node ids")

expect_warning({
  test_o_net <-
    sp_network_nodes("o1",NULL,data.frame("ID" = LETTERS[1:3]),"ID")
  test_d_net <-
    sp_network_nodes("d1",NULL,data.frame("ID" = letters[1:3]),"ID")
  test_pairs_unordered <-
    sp_network_pair("o1","d1",data.frame("ID_O" = rep(LETTERS[c(2,1,3)], 3),
                                         "ID_D" = rep(letters[3:1], each = 3)),
                    "ID_O","ID_D")
  sp_multi_network(test_o_net, test_d_net, test_pairs_unordered)
  },
  info = "wrong ordering of nodes gives a warning")

expect_equal({
  test_o_net <-
    sp_network_nodes("o1",NULL,data.frame("ID" = LETTERS[1:3]),"ID")
  test_d_net <-
    sp_network_nodes("d1",NULL,data.frame("ID" = letters[1:3]),"ID")
  test_pairs_unordered <-
    sp_network_pair("o1","d1",data.frame("ID_O" = rep(LETTERS[c(2,1,3)], 3),
                                         "ID_D" = rep(letters[3:1], each = 3)),
                    "ID_O","ID_D")
  suppressWarnings({test_multi_net_ordered <-
    sp_multi_network(test_o_net,test_d_net, test_pairs_unordered)
  })
  test_pairs_ordered <- test_multi_net_ordered@network_pairs$o1_d1@pair_data
  cbind(levels(test_pairs_ordered[["ID_O"]]),
        levels(test_pairs_ordered[["ID_D"]]))
  },
  cbind(LETTERS[1:3],letters[1:3]),
  info = "adjusts wrong ordering of od keys when possible")

# ---- accessing methods ------------------------------------------------------
expect_equal({
  test_network_ids <- c("net1","net2")
  test_nodes <- lapply(test_network_ids, "sp_network_nodes")
  test_pairs <- lapply(test_network_ids,
                       function(.id) sp_network_pair(.id, .id))
  test_multi_net <- sp_multi_network(test_nodes,test_pairs)
  id(test_multi_net)
  },
  list("networks" = c("net1", "net2"),
       "network_pairs" =  c("net1_net1", "net2_net2")),
  info = "acessing the id works")

expect_equal({
  test_multi_net <- sp_multi_network(sp_network_nodes("net1"))
  pull_member(test_multi_net, "net1")
  },
  sp_network_nodes("net1"),
  info = "pull existing net")

expect_error({
  test_multi_net <- sp_multi_network(sp_network_nodes("net1"))
  pull_member(test_multi_net, "net2")
  },
  info = "pull non-existing net")

expect_equal({
  test_multi_net <- sp_multi_network(sp_network_pair("net1","net1"))
  pull_member(test_multi_net, "net1_net1")
  },
  sp_network_pair("net1","net1"),
  info = "pull existing pair")

expect_error({
  test_multi_net <- sp_multi_network(sp_network_pair("net1","net1"))
  pull_member(test_multi_net, "net2_net2")
  },
  info = "pull non-existing pair")


# ---- pair_merge method ------------------------------------------------------
expect_equal({
  test_o_net <-
    sp_network_nodes("net1", NULL, data.frame("ID" = c("A", "B"),
                                              "VAL" = "OO"),
                     "ID")
  test_d_net <-
    sp_network_nodes("net2", NULL, data.frame("ID" = c("C","D"),
                                              "VAL" = "DD"),
                     "ID")
  test_net_pair <- sp_network_pair(
    "net1","net2",
    data.frame("ID_O" = c("A","A","B","B"),
               "ID_D" = c("C","D","C","D"),
               "DIST" = 1:4),
    "ID_O", "ID_D")
  test_multi_net <- sp_multi_network(test_net_pair,test_o_net,test_d_net)
  pair_merge(test_multi_net, "net1_net2")
  },
  {
    data.frame("ID_O" = factor(c("A","A","B","B")),
               "ID_D" = factor(c("C","D","C","D")),
               "ORIG_VAL" = "OO",
               "DEST_VAL" = "DD",
               "DIST" = 1:4)
  },
  info = "merging origin and destination infos to the pairs")

expect_equal({
  # invert order of ids for pairs
  test_o_net <-
    sp_network_nodes("net1", NULL, data.frame("ID" = c("B","A"),
                                              "VAL" = "OO"),
                     "ID")
  test_d_net <-
    sp_network_nodes("net2", NULL, data.frame("ID" = c("D","C"),
                                              "VAL" = "DD"),
                     "ID")
  test_net_pair <- sp_network_pair(
    "net1",
    "net2",
    data.frame(
      "ID_O" = c("A", "B"),
      "ID_D" = c("C", "D"),
      "DIST" = c(1, 4)
    ),
    "ID_O",
    "ID_D")
  test_multi_net <-
    suppressWarnings(sp_multi_network(test_net_pair, test_o_net, test_d_net))
  pair_merge(test_multi_net, "net1_net2", TRUE)
  },
  {
    data.frame(
      "ID_O" = factor(c("B", "B","A", "A"),levels = c("B","A")),
      "ID_D" = factor(c("D", "C", "D", "C"),levels = c("D","C")),
      "ORIG_VAL" = "OO",
      "DEST_VAL" = "DD",
      "DIST" = c(4, NA, NA, 1)
    )
  },
  info = "merging origin and destination infos to the pairs
          test expansion of missing pairs and correct ordering")

# ---- show method ------------------------------------------------------------
expect_stdout({
  test_o_net <-
    sp_network_nodes("net1", NULL, data.frame("ID" = c("A", "B"),
                                              "VAL" = "OO"),
                     "ID")
  test_d_net <-
    sp_network_nodes("net2", NULL, data.frame("ID" = c("C", "D"),
                                              "VAL" = "DD"),
                     "ID")
  test_net_pair <- sp_network_pair("net1",
                                   "net2",
                                   data.frame(
                                     "ID_O" = c("A", "B"),
                                     "ID_D" = c("C", "D"),
                                     "DIST" = c(1, 4)
                                   ),
                                   "ID_O",
                                   "ID_D")
  test_multi_net <-
    sp_multi_network(test_net_pair, test_o_net, test_d_net)
  test_multi_net
  },
  info = "show something on print")

