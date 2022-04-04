# ---- pull_neighborhood_data -------------------------------------------------
expect_equal({

  test_o_net <- sp_network_nodes(
    network_id = "net1",
    node_neighborhood = matrix(c(0,1,1,0),2,2),
    node_data = data.frame("ID" = c("A", "B"), "VAL" = "OO"),
    node_key_column = "ID")

  test_d_net <- sp_network_nodes(
    network_id = "net2",
    node_neighborhood = matrix(c(0,1,1,0),2,2),
    node_data = data.frame("ID" = c("C", "D"), "VAL" = "DD"),
    node_key_column = "ID")

  test_net_pair <- sp_network_pair(
    orig_net_id = "net1",
    dest_net_id =  "net2",
    pair_data = data.frame(
      "ID_O" = c("A", "B"),
      "ID_D" = c("C", "D"),
      "DIST" = c(1, 4)),
    orig_key_column = "ID_O",
    dest_key_column =  "ID_D")


  test_mult_net <- sp_multi_network(test_o_net,test_d_net, test_net_pair)
  spflow:::pull_neighborhood_data(test_mult_net,"net1_net2")
  },
  {
    spflow:::named_list(c("OW","DW"),Matrix(c(0,1,1,0),2,2))
  },
  info = "neigborhood matrices of origins and destinations as list")

# ---- define_lags_and_instruments --------------------------------------------
expect_equal({
  data_sources <- list(
    pair = data.frame(y1 = 1, p1 = 1:4, p2 = 5:8),
    orig = data.frame(o1 = 1:4, o2 = 5:8),
    dest = data.frame(d1 = 1:4, d2 = 5:8))
  formula_norm <- list("Y_" = ~ log(y1),
                       "G_" = ~ p1 + p2,
                       "O_" = ~ o1,
                       "D_" = ~ log(d1))
  formula_sdm <- list("O_" = ~ o2,
                      "D_" = ~ log(d2))
  formula_inst <- list("G_" = ~ sqrt(p1),
                       "O_" = ~ sqrt(o1),
                       "D_" = ~ sqrt(d1))


  forumula_parts <- list("norm" = formula_norm,
                         "sdm"  = formula_sdm,
                         "inst" = formula_inst)
  res <- spflow:::define_lags_and_instruments(forumula_parts, data_sources)
  lapply(res, "[", c("norm", "sdm", "inst"))
  },
  {list(
    "D_" = data.frame(
      norm = c(TRUE, FALSE, FALSE),
      sdm = c(FALSE,TRUE, FALSE),
      inst = c(FALSE,FALSE, TRUE),
      row.names = c("log(d1)", "log(d2)", "sqrt(d1)")),
    "O_" = data.frame(
      norm = c(TRUE, FALSE, FALSE),
      sdm = c(FALSE,TRUE, FALSE),
      inst = c(FALSE,FALSE, TRUE),
      row.names = c("o1", "o2", "sqrt(o1)")),
    "G_" = data.frame(
      norm = c(TRUE, TRUE, FALSE),
      sdm = FALSE,
      inst = c(FALSE,FALSE, TRUE),
      row.names = c("p1", "p2", "sqrt(p1)")),
    "Y_" = data.frame(
      norm = TRUE,
      sdm = FALSE,
      inst = FALSE,
      row.names = "log(y1)"))

  },
  info = "collect all variables by role and the way it is used in the model")

