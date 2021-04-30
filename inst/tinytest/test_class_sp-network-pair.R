# ==== [+++ constructor +++] ==================================================
test_pair_data <- data.frame(o_key = rep(LETTERS[1:3],times = 3),
                             d_key = rep(LETTERS[1:3],each = 3),
                             dist = 1:9)
test_sp_net_pair <- sp_network_pair("net1","net1",test_pair_data,
                                    "o_key","d_key")
expect_inherits(test_sp_net_pair,"sp_network_nodes")

# check that data is not changed apart from factor conversions
test_pair_data_fct <- data.frame(
  o_key = factor_in_order(rep(LETTERS[1:3], times = 3)),
  d_key = factor_in_order(rep(LETTERS[1:3], each = 3)),
  dist = 1:9)
expect_equal(test_pair_data_fct,test_sp_net_pair@pair_data,
             check.attributes = FALSE)

# check that ids are correct and count works
expect_equal(test_sp_net_pair@orig_net_id, "net1")
expect_equal(test_sp_net_pair@dest_net_id, "net1")
expect_equal(test_sp_net_pair@network_pair_id, "net1_net1")
expect_equal(test_sp_net_pair@npairs, nrow(test_pair_data))

# ==== [+++ replacement and accessor methods +++] =============================

# ---- dat --------------------------------------------------------------------
test_pair_data <- data.frame(o_key = rep(LETTERS[1:3],times = 3),
                             d_key = rep(LETTERS[1:3],each = 3),
                             dist = 1:9)
test_sp_net_pair <- sp_network_pair("net1","net1",test_pair_data,
                                    "o_key","d_key")

### error for unidentified origins or destinations
# ... no key columns
not_identfyed_key <- data.frame(
  o_key = rep(LETTERS[1:3], times = 3),
  d_key = rep(LETTERS[1:3], each = 3),
  dist = 1:9)
expect_error(dat(test_sp_net_pair) <- not_identfyed_key,
             "invalid class")
# ... above works when keys are given
identfyed_key <- not_identfyed_key
attr_key_od(identfyed_key) <- c("o_key","d_key")
dat(test_sp_net_pair) <- identfyed_key
expect_true(validObject(test_sp_net_pair))
# ... double origins
not_identfyed_orig <- data.frame(
  o_key = rep(LETTERS[c(1,1,3)], times = 3),
  d_key = rep(LETTERS[1:3], each = 3),
  dist = 1:9)
attr_key_od(not_identfyed_orig) <- c("o_key","d_key")
expect_error(dat(test_sp_net_pair) <- not_identfyed_orig,
             "invalid class")
# ... double destinations
not_identfyed_dest <- data.frame(
  o_key = rep(LETTERS[1:3], times = 3),
  d_key = rep(LETTERS[c(1,1,3)], each = 3),
  dist = 1:9)
attr_key_od(not_identfyed_dest) <- c("o_key","d_key")
expect_error(dat(test_sp_net_pair) <- not_identfyed_dest,
             "invalid class")




