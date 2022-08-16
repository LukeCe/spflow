library("spflow")
# ---- constructor ------------------------------------------------------------

expect_inherits({
  test_pair_data <- data.frame(
    o_key = rep(LETTERS[1:3],times = 3),
    d_key = rep(LETTERS[1:3],each = 3),
    dist = 1:9)
  spflow_network_pairs(
    id_orig_nodes = "net1",
    id_dest_nodes = "net1",
    pair_data = test_pair_data,
    orig_key_column =  "o_key",
    dest_key_column = "d_key")
  },
  class = "spflow_network_pairs")

expect_equal({
  test_pair_data_fct <- data.frame(
    o_key = rep(LETTERS[1:3],times = 3),
    d_key = rep(LETTERS[1:3],each = 3),
    dist = 1:9)

  spflow_network_pairs(
    id_orig_nodes = "net1",
    id_dest_nodes = "net1",
    pair_data = test_pair_data,
    orig_key_column =  "o_key",
    dest_key_column = "d_key")@pair_data
  },
  {
    data.frame(
      d_key = factor(rep(LETTERS[1:3], times = 3)),
      o_key = factor(rep(LETTERS[1:3], each = 3)),
      dist = c(1,4,7,2,5,8,3,6,9))
    },
  info = "check that data is ordered correctly and that ids are factors",
  check.attributes = FALSE)

# ids and node counts
expect_equal({
  spflow_network_pairs("net1","net1")@id_orig_nodes
  },"net1")

expect_equal({
  spflow_network_pairs("net1","net2")@id_dest_nodes
  }, "net2")

expect_equal({
  spflow_network_pairs("net1","net2")@id_spflow_network_pairs
  }, "net1_net2")

expect_equal({
  test_pair_data <- data.frame(
    o_key = rep(LETTERS[1:3],times = 3),
    d_key = rep(LETTERS[1:3],each = 3),
    dist = 1:9)

  npairs(spflow_network_pairs(
    id_orig_nodes = "net1",
    id_dest_nodes = "net1",
    pair_data = test_pair_data,
    orig_key_column =  "o_key",
    dest_key_column = "d_key"))
  }, 9)

# ---- accessor methods -------------------------------------------------------
# ---- ... dat ----------------------------------------------------------------
expect_error({
  test_pair_data <- data.frame(
    o_key = rep(LETTERS[1:3],times = 3),
    d_key = rep(LETTERS[1:3],each = 3),
    dist = 1:9)

  test_sp_net_pair <-   spflow_network_pairs(
    id_orig_nodes = "net1",
    id_dest_nodes = "net1",
    pair_data = test_pair_data,
    orig_key_column =  "o_key",
    dest_key_column = "d_key")

  dat(test_sp_net_pair) <- test_pair_data
  },
  pattern = "invalid class",
  info = "data replacements works only when key columns are defined")

expect_true({
  test_pair_data <- data.frame(
    o_key = rep(LETTERS[1:3],times = 3),
    d_key = rep(LETTERS[1:3],each = 3),
    dist = 1:9)

  test_sp_net_pair <- spflow_network_pairs(
    id_orig_nodes = "net1",
    id_dest_nodes = "net1",
    pair_data = test_pair_data,
    orig_key_column =  "o_key",
    dest_key_column = "d_key")

  new_dat <- dat(test_sp_net_pair)
  new_dat$dist <- new_dat$dist + 1
  dat(test_sp_net_pair) <- new_dat
  validObject(test_sp_net_pair)
  },
  info = "data replacements works only when key columns are defined")

expect_error({
  test_pair_data <- data.frame(
    o_key = rep(LETTERS[1:3],times = 3),
    d_key = rep(LETTERS[1:3],each = 3),
    dist = 1:9)

  test_sp_net_pair <- spflow_network_pairs(
    id_orig_nodes = "net1",
    id_dest_nodes = "net1",
    pair_data = test_pair_data,
    orig_key_column =  "o_key",
    dest_key_column = "d_key")

  not_identfyed_orig <- dat(test_sp_net_pair)
  not_identfyed_orig$o_key <- rep(LETTERS[c(1,1,3)], times = 3)
  dat(test_sp_net_pair) <- not_identfyed_orig
  },
  pattern = "invalid class",
  info = "data replacements works only when od pairs are unique")

# ---- update_dat -------------------------------------------------------------
test_pdat <- data.frame(
  ID_DEST = LETTERS[rep(1:3,3)],
  ID_ORIG = letters[rep(1:3,each = 3)],
  DIST = 1:9)

test_pnet <- spflow_network_pairs(
  id_orig_nodes = "letters",
  id_dest_nodes = "LETTERS",
  pair_data = test_pdat,
  orig_key_column = "ID_ORIG",
  dest_key_column = "ID_DEST")

expect_equal({
  test_pdat2 <- test_pdat[c(1,5,9),]
  test_pdat2[["DIST"]] <- 0
  update_dat(test_pnet, test_pdat2)
},{
  test_pnet2 <- test_pnet
  dat(test_pnet2)[c(1,5,9),"DIST"] <- 0
  test_pnet2
})

expect_error({
  new_dat <- dat(germany_net)[1:5,2, drop = FALSE]
  new_dat[["X"]] <- 1:5
  update_dat(germany_net, new_dat)
}, "must have the column")

expect_error({
  test_pdat2 <- test_pdat[c(1,5,9),]
  test_pdat2[["DIST2"]] <- 0
  update_dat(test_pnet, test_pdat2)
}, "columns in new_dat must exist")

expect_error({
  test_pdat2 <- test_pdat[c(1,1,3,4,9),]
  update_dat(test_pnet, test_pdat2)
}, "duplicated")

expect_error({
  test_pdat2 <- test_pdat[c(1,5,9),]
  test_pdat2[["ID_DEST"]] <- LETTERS[2:4]
  update_dat(test_pnet, test_pdat2)
}, "do not correpond to observations")

expect_error({
  test_pdat2 <- test_pdat[c(1,5,9),]
  test_pdat2[["ID_ORIG"]] <- letters[2:4]
  update_dat(test_pnet, test_pdat2)
}, "do not correpond to observations")


# ---- matrix_form_control ----------------------------------------------------
expect_equal({
  test_pair_data <- data.frame(
    o_key = rep(LETTERS[1:3],times = 3),
    d_key = rep(LETTERS[1:3],each = 3),
    dist = 1:9)

  test_sp_net_pair <- spflow_network_pairs(
    id_orig_nodes = "net1",
    id_dest_nodes = "net1",
    pair_data = test_pair_data,
    orig_key_column =  "o_key",
    dest_key_column = "d_key")

  mat_cntrl <- spflow:::matrix_form_control(test_sp_net_pair)
  mat_cntrl$mat_format(test_pair_data$dist)
  },
  {
    matrix(1:9,3,3)
  },
  info = "Matrix format: dense case")

expect_equal({
  test_pair_data <- data.frame(
    o_key = LETTERS[c(1,1:3)],
    d_key = LETTERS[c(1:3,3)],
    dist = 1:4)

  test_sp_net_pair <- spflow_network_pairs(
    id_orig_nodes = "net1",
    id_dest_nodes = "net1",
    pair_data = test_pair_data,
    orig_key_column =  "o_key",
    dest_key_column = "d_key")

  mat_cntrl <- spflow:::matrix_form_control(test_sp_net_pair)
  mat_cntrl$mat_format(test_pair_data$dist)
  },
  {
    Matrix::sparseMatrix(i = c(1:3,3),j =  c(1,1:3),x = 1:4)
  },
  info = "Matrix format: sparse case")

expect_equal({
  test_pair_data <- data.frame(
    o_key = rep(LETTERS[1:3],2),
    d_key = rep(LETTERS[1:3],each =2),
    dist = 1:6)

  test_sp_net_pair <- spflow_network_pairs(
    id_orig_nodes = "net1",
    id_dest_nodes = "net1",
    pair_data = test_pair_data,
    orig_key_column =  "o_key",
    dest_key_column = "d_key")

  mat_cntrl <- spflow:::matrix_form_control(test_sp_net_pair)
  mat_cntrl$mat_format(test_sp_net_pair@pair_data$dist)
  },
  {
     cbind(c(1,4,0), c(2,0,5), c(0,3,6))
  },
  info = "Matrix format: incomplete (but dense) case")

