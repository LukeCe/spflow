# ==== [+++ constructor +++] ==================================================
## correct construction and data adjustment ##
test_node_data <- data.frame(key = LETTERS[seq(3)], val = seq(3))
test_neighborhood <- matrix(c(0,1,0,.5,0,.5,0,1,0),3,3,TRUE)
test_sp_nodes <-
  sp_network_nodes("net1",test_neighborhood,test_node_data,"key")
expect_inherits(test_sp_nodes,"sp_network_nodes")

# ... transforms for efficiency
expect_equal(Matrix(test_neighborhood),test_sp_nodes@node_neighborhood)

# ... orders correctly
expect_equal(seq(3),as.numeric(test_sp_nodes@node_data[["key"]]))

# ... orders as in the original data
test_node_data2 <- data.frame(key = LETTERS[rev(seq(3))], val = seq(3))
test_sp_nodes2 <-
  sp_network_nodes("net1",test_neighborhood,test_node_data2,"key")
expect_equal(seq(3),as.numeric(test_sp_nodes2@node_data[["key"]]))

## recognizes erroneous inputs ##
# ... network id must be alphanumeric
expect_error(sp_network_nodes(network_id = "id_with_special_characters"))

# ... node key column must exist and identify uniquely
test_node_data <- data.frame(key = LETTERS[seq(3)], val = seq(3))
expect_error(sp_network_nodes("net1",NULL,test_node_data))
expect_error(sp_network_nodes("net1",NULL,test_node_data,"not_a_column"))

test_node_data <- data.frame(key = LETTERS[c(1,1,3)], val = seq(3))
expect_error(sp_network_nodes("net1",NULL,test_node_data,"key"))

# ... dimensions of neighborhood and data must match
test_node_data <- data.frame(key = LETTERS[seq(3)], val = seq(3))
to_large_neighborhood <- matrix(0,4,4)
expect_error(sp_network_nodes("net1",to_large_neighborhood,
                              test_node_data,"key"))

# ==== [+++ replacement and accessor methods +++] =============================

# ---- dat --------------------------------------------------------------------
test_node_data <- data.frame(key = factor(LETTERS[seq(3)]), val = seq(3))
test_neighborhood <- matrix(c(0,1,0,.5,0,.5,0,1,0),3,3,TRUE)
test_sp_nodes <-
  sp_network_nodes("net1",test_neighborhood,test_node_data,"key")

# ... check that the change to the data is a transformation to factor
expect_equal(test_node_data,dat(test_sp_nodes), check.attributes = FALSE)

# ... check that unsuitable data is rejected
# ... for size wrong size
to_small_data <- data.frame(key = factor(LETTERS[seq(2)]), val = seq(2))
expect_error(dat(test_sp_nodes, "key") <- to_small_data,
             "invalid class")
# ... for missing ids
valid_node_data <- data.frame(key = factor(LETTERS[seq(3)]), val = seq(3))
expect_error(dat(test_sp_nodes) <- valid_node_data,
             "invalid class")
# ... if set id is reused
attr_key_nodes(valid_node_data) <- "key"
expect_equal(dat(test_sp_nodes), valid_node_data)
# ... error when id is not unique
data_without_id <- data.frame(key = factor(LETTERS[c(1,1,3)]), val = seq(3))
expect_error(dat(test_sp_nodes, "key") <- data_without_id,
             "invalid class")

# ---- neighborhood -----------------------------------------------------------
test_node_data <- data.frame(key = factor(LETTERS[seq(3)]), val = seq(3))
test_neighborhood <- matrix(c(0,1,0,.5,0,.5,0,1,0),3,3,TRUE)
test_sp_nodes <-
  sp_network_nodes("net1",test_neighborhood,test_node_data,"key")
expect_equal(Matrix(test_neighborhood),neighborhood(test_sp_nodes))

# ... invalid replacements are rejected
too_small_neighborhood <- matrix(0,2,2)
expect_error(neighborhood(test_sp_nodes) <- too_small_neighborhood,
             "invalid class")
# ... invalid because non-zero diagonal
invalid_neighborhhod <- matrix(1,3,3)
expect_error(neighborhood(test_sp_nodes) <- invalid_neighborhhod,
             "invalid class")

