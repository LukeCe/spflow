# ---- constructor ------------------------------------------------------------

# correct constructions
test_network_ids <- c("net1","net2")
test_nodes <- lapply(test_network_ids, sp_network_nodes)
test_pair <- lapply(test_network_ids, function(.id) sp_network_pair(.id, .id))
test_multi_net <- sp_multi_network(test_nodes,test_pair)
expect_inherits(test_multi_net, "sp_multi_network")

# warn when dropping unused classes
df_input <- cars
expect_warning(test_multi_net <- sp_multi_network(test_nodes,df_input))
expect_inherits(test_multi_net, "sp_multi_network")

# correct but inconsistent objects are supplied...
# when origin ids do not match -> error
sp_net_letters_altered <- sp_net_letters
dat(sp_net_letters_altered)[, ID := factor(letters[1:8 + 9])] %>% setkey(ID)
expect_error(sp_multi_network(sp_net_letters_altered, sp_pair_letters))

# ... when origin ids are in different order -> warn and reorder (not yet)
# ... for now error instead of warning
dat(sp_net_letters_altered)[, ID := factor(letters[1:8],
                                           letters[8:1])] %>% setkey(ID)
expect_error(sp_multi_network(sp_net_letters_altered, sp_pair_letters))

# ---- assessor methods -------------------------------------------------------

# ids: all
test_multi_net <- sp_multi_network(test_nodes,test_pair)
actual_id <- id(test_multi_net)
expected_id_net <- test_network_ids
expected_id_pairs <- lapply(test_pair, "id")
expect_equal(actual_id$networks, expected_id_net)
expect_equal(actual_id$network_pairs,expected_id_pairs,
             check.attributes	= FALSE)

# ids: specific
expect_equal(id(test_multi_net)[["networks"]], expected_id_net)
expect_equal(id(test_multi_net)[["network_pairs"]], expected_id_pairs,
             check.attributes	= FALSE)

# nodes
expect_equal(pull_nodes(test_multi_net), test_nodes,
             check.attributes	= FALSE)
expect_equal(pull_nodes(test_multi_net)[["net1"]], test_nodes[[1]])
expect_equal(pull_nodes(test_multi_net)[["net2"]], test_nodes[[2]])

# pairs
expect_equal(pull_pairs(test_multi_net), test_pair,
             check.attributes	= FALSE)
expect_equal(pull_pairs(test_multi_net)[["net1_net1"]], test_pair[[1]])
expect_equal(pull_pairs(test_multi_net)[["net2_net2"]], test_pair[[2]])

# ---- pair_merge method ------------------------------------------------------
test_that("sp_network_nodes.pair_merge: case 1 => correct output", {

  # test case 1:
  # flows are complete
  # origin == destination, both are given
  test_multi_net <- sp_multi_network(sp_net_LETTERS,sp_pair_LETTERS)
  actual <- pair_merge(test_multi_net, "LETTERS_LETTERS")

  p_dat <- copy(dat(sp_pair_LETTERS))
  o_dat <- copy(dat(sp_net_LETTERS)) %>% prefix_columns("ORIG_")
  d_dat <- copy(dat(sp_net_LETTERS)) %>% prefix_columns("DEST_")
  expected <- p_dat[d_dat, on = "DEST_ID"][o_dat, on = "ORIG_ID"]
  expect_equivalent(actual,expected)

  intital_dim <- c(100,4)
  dim_after <- dim(dat(test_multi_net,network_pair_id = "LETTERS_LETTERS"))
  expect_equal(intital_dim, dim_after)

  expect_error(pair_merge(test_multi_net, "A_A"),
               "Network pair with id A_A was not found!")
})

# ---- show method ------------------------------------------------------------
test_that("sp_multi_network: correct show-method", {
  test_object <- sp_multi_network(test_nodes,test_pair)
  expect_output(show(test_object))
})

