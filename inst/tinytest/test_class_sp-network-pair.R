library("spflow")
# ==== [+++ constructor +++] ==================================================

expect_inherits({
  test_pair_data <- data.frame(o_key = rep(LETTERS[1:3],times = 3),
                               d_key = rep(LETTERS[1:3],each = 3),
                               dist = 1:9)
  sp_network_pair("net1","net1",test_pair_data, "o_key","d_key")
  },
  class = "sp_network_pair")

expect_equal({
  test_pair_data_fct <- data.frame(o_key = rep(LETTERS[1:3],times = 3),
                                   d_key = rep(LETTERS[1:3],each = 3),
                                   dist = 1:9)
  sp_network_pair("net1","net1",test_pair_data, "o_key","d_key")@pair_data
  },
  {
    data.frame(
      o_key = factor_in_order(rep(LETTERS[1:3], times = 3)),
      d_key = factor_in_order(rep(LETTERS[1:3], each = 3)),
      dist = 1:9)
    },
  info = "check that data is not changed apart from factor conversions",
  check.attributes = FALSE)

# ids and node counts
expect_equal({
  sp_network_pair("net1","net1")@orig_net_id
  },"net1")
expect_equal({
  sp_network_pair("net1","net2")@dest_net_id
  }, "net2")
expect_equal({
  sp_network_pair("net1","net2")@network_pair_id
  }, "net1_net2")
expect_equal({
  test_pair_data <- data.frame(o_key = rep(LETTERS[1:3],times = 3),
                               d_key = rep(LETTERS[1:3],each = 3),
                               dist = 1:9)
  sp_network_pair("net1","net1",test_pair_data, "o_key","d_key")@npairs
  }, 9)

# ==== [+++ replacement and accessor methods +++] =============================
# ---- dat --------------------------------------------------------------------
expect_error({
  test_pair_data <- data.frame(o_key = rep(LETTERS[1:3],times = 3),
                               d_key = rep(LETTERS[1:3],each = 3),
                               dist = 1:9)
  test_sp_net_pair <- sp_network_pair("net1","net1",test_pair_data,
                                      "o_key","d_key")
  dat(test_sp_net_pair) <- test_pair_data
  },
  pattern = "invalid class",
  info = "data replacements works only when key columns are defined")

expect_true({
  test_pair_data <- data.frame(o_key = rep(LETTERS[1:3],times = 3),
                               d_key = rep(LETTERS[1:3],each = 3),
                               dist = 1:9)
  test_sp_net_pair <- sp_network_pair("net1","net1",test_pair_data,
                                      "o_key","d_key")
  new_dat <- dat(test_sp_net_pair)
  new_dat$dist <- new_dat$dist + 1
  dat(test_sp_net_pair) <- new_dat
  validObject(test_sp_net_pair)
  },
  info = "data replacements works only when key columns are defined")

expect_error({
  test_pair_data <- data.frame(o_key = rep(LETTERS[1:3],times = 3),
                               d_key = rep(LETTERS[1:3],each = 3),
                               dist = 1:9)
  test_sp_net_pair <- sp_network_pair("net1","net1",test_pair_data,
                                      "o_key","d_key")
  not_identfyed_orig <- dat(test_sp_net_pair)
  not_identfyed_orig$o_key <- rep(LETTERS[c(1,1,3)], times = 3)
  dat(test_sp_net_pair) <- not_identfyed_orig
  },
  pattern = "invalid class",
  info = "data replacements works only when od pairs are unique")
