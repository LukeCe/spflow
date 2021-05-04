# ==== [+++ constructor +++] ==================================================
library("spflow")
library("Matrix")

expect_inherits({
  sp_network_nodes("net1")
  }, class = "sp_network_nodes")

expect_equal({
  test_neighborhood <- matrix(c(0,1,0,.5,0,.5,0,1,0),3,3,TRUE)
  sp_network_nodes("net1",test_neighborhood)@node_neighborhood
  },
  {Matrix(c(0,1,0,.5,0,.5,0,1,0),3,3,TRUE)},
  info = "neighborhood matrix is transformed for efficiency")

expect_equal({
  test_node_data <- data.frame(key = LETTERS[3:1], val = seq(3))
  ids <- dat(sp_network_nodes("net1",NULL,test_node_data,"key"))[["key"]]
  as.numeric(ids)
  }, seq(3),
  info = "order of the origins is set correctly")

expect_error({
  sp_network_nodes(network_id = "id_with_special_characters")
  },
  info = "network id must be alphanumeric")

expect_error({
  test_node_data <- data.frame(key = LETTERS[seq(3)], val = seq(3))
  sp_network_nodes("net1",NULL,test_node_data)
  },
  info = "node key column must exist and identify uniquely")

expect_error({
  test_node_data <- data.frame(key = LETTERS[seq(3)], val = seq(3))
  sp_network_nodes("net1",NULL,test_node_data,"not_a_column")
  },
  info = "node key column must exist and identify uniquely")

expect_error({
  test_node_data <- data.frame(key = LETTERS[c(1,1,3)], val = seq(3))
  sp_network_nodes("net1",NULL,test_node_data,"not_a_column")
  },
  info = "node key column must exist and identify uniquely")

expect_error({
  test_node_data <- data.frame(key = LETTERS[seq(3)], val = seq(3))
  to_large_neighborhood <- matrix(0,4,4)
  sp_network_nodes("net1",to_large_neighborhood, test_node_data,"key")
  },
  info = "dimensions of neighborhood and data must match")

# ==== [+++ replacement and accessor methods +++] =============================

# ---- dat --------------------------------------------------------------------
expect_equal({
  test_node_data <- data.frame(key = factor(LETTERS[seq(3)]), val = seq(3))
  test_neighborhood <- matrix(c(0,1,0,.5,0,.5,0,1,0),3,3,TRUE)
  test_sp_nodes <-
    sp_network_nodes("net1",test_neighborhood,test_node_data,"key")
  dat(test_sp_nodes)
  },
  {data.frame(key = factor(LETTERS[seq(3)]), val = seq(3))
    },
  info = "factor conversion is the only change",
  check.attributes = FALSE)

expect_error({
  valid_node_data <- data.frame(key = factor(LETTERS[seq(3)]), val = seq(3))
  test_sp_nodes <- sp_network_nodes("net1",NULL,valid_node_data,"key")
  dat(test_sp_nodes) <- valid_node_data
  },
  pattern = "invalid class",
  info = "data replacement is rejected for unspecifyed keys")

expect_error({
  valid_node_data <- data.frame(key = factor(LETTERS[seq(3)]), val = seq(3))
  test_sp_nodes <- sp_network_nodes("net1",NULL,valid_node_data,"key")
  invalid_node_data <- dat(test_sp_nodes)
  invalid_node_data[["key"]] <- factor(LETTERS[c(1,1,2)])
  dat(test_sp_nodes) <- invalid_node_data
  },
  pattern = "invalid class",
  info = "data replacement is rejected for duplicated keys")

expect_true({
  valid_node_data <- data.frame(key = factor(LETTERS[seq(3)]), val = seq(3))
  test_sp_nodes <- sp_network_nodes("net1",NULL,valid_node_data,"key")
  valid_node_data <- dat(test_sp_nodes)
  dat(test_sp_nodes)$val <- dat(test_sp_nodes)$val + 5
  validObject(test_sp_nodes)
  },
  info = "data replacement supports column mutations")

expect_error({
  valid_node_data <- data.frame(key = factor(LETTERS[seq(3)]), val = seq(3))
  test_sp_nodes <- sp_network_nodes("net1",NULL,valid_node_data,"key")
  to_small_data <- data.frame(key = factor(LETTERS[seq(2)]), val = seq(2))
  dat(test_sp_nodes) <- to_small_data
  },
  pattern = "invalid class",
  info = "data replacement is rejected for dimension missmatch")

# ---- neighborhood -----------------------------------------------------------

expect_equal({
  test_neighborhood <- matrix(c(0,1,0,.5,0,.5,0,1,0),3,3,TRUE)
  test_sp_nodes <- sp_network_nodes("net1",test_neighborhood)
  neighborhood(test_sp_nodes) <- matrix(c(0,1,0,.5,0,.5,0,1,0),3,3,TRUE)
  neighborhood(test_sp_nodes)
  }, Matrix(c(0,1,0,.5,0,.5,0,1,0),3,3,TRUE),
  info = "replacement method coserves efficiency coversion")

expect_error({
  test_node_data <- data.frame(key = factor(LETTERS[seq(3)]), val = seq(3))
  test_neighborhood <- matrix(c(0,1,0,.5,0,.5,0,1,0),3,3,TRUE)
  test_sp_nodes <-
    sp_network_nodes("net1",test_neighborhood,test_node_data,"key")
  too_small_neighborhood <- matrix(0,2,2)
  neighborhood(test_sp_nodes) <- too_small_neighborhood
  },
  pattern = "invalid class",
  info = "replacement is rejected for wrong dimensions")

# ... invalid because non-zero diagonal
invalid_neighborhhod <- matrix(1,3,3)
expect_error(neighborhood(test_sp_nodes) <- invalid_neighborhhod,
             "invalid class")

