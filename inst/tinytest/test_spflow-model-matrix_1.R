# ---- pull_flow_data ---------------------------------------------------------
expect_equal({
  test_o_net <- sp_network_nodes(
    "net1",
    NULL,
    data.frame("ID" = c("A", "B"),"VAL" = "OO"),
    "ID")

  test_d_net <- sp_network_nodes(
    "net2",
    NULL,
    data.frame("ID" = c("C", "D"), "VAL" = "DD"),
    "ID")

  test_net_pair <- sp_network_pair(
    "net1", "net2",
    data.frame("ID_O" = c("A", "B"),
               "ID_D" = c("C", "D"),
               "DIST" = c(1, 4)),
    "ID_O", "ID_D")
  test_mult_net <- sp_multi_network(test_o_net,test_d_net,test_net_pair)
  spflow:::pull_flow_data(test_mult_net,"net1_net2")
  },
  {
    list("pair" = data.frame("DIST" = c(1, 4)),
         "orig" = data.frame("VAL" = c("OO","OO")),
         "dest" = data.frame("VAL" = c("DD","DD")))

  },
  info = "extracts data and drops key columns",
  check.attributes = FALSE)


expect_equal({
  test_o_net <- sp_network_nodes(
    "net1",
    NULL,
    data.frame("ID" = c("A", "B"),"VAL" = "OO"),
    "ID")

  test_d_net <- sp_network_nodes(
    "net2",
    NULL,
    data.frame("ID" = c("C", "D"), "VAL" = "DD"),
    "ID")

  test_net_pair <- sp_network_pair(
    "net1", "net1",
    data.frame("ID_O" = c("A", "B"),
               "ID_D" = c("A", "B"),
               "DIST" = c(1, 4)),
    "ID_O", "ID_D")
  test_mult_net <- sp_multi_network(test_o_net,test_d_net,test_net_pair)
  spflow:::pull_flow_data(test_mult_net,"net1_net1")
  },
  {
    list("pair" = data.frame("DIST" = c(1, 4)),
         "orig" = data.frame("VAL" = c("OO","OO")))

  },
  info = "extracts ignores dest when orig == dest",
  check.attributes = FALSE)

# ---- pull_neighborhood_data -------------------------------------------------
expect_equal({
  test_o_net <- sp_network_nodes(
    "net1",
    matrix(c(0,1,1,0),2,2),
    data.frame("ID" = c("A", "B"),"VAL" = "OO"),
    "ID")

  test_d_net <- sp_network_nodes(
    "net2",
    matrix(c(0,1,1,0),2,2),
    data.frame("ID" = c("C", "D"), "VAL" = "DD"),
    "ID")
  test_mult_net <- sp_multi_network(test_o_net,test_d_net)
  spflow:::pull_neighborhood_data(test_mult_net,"net1_net2")
  },
  {
    spflow:::named_list(c("OW","DW"),Matrix(c(0,1,1,0),2,2))
  },
  info = "neigborhood matrices of origins and destinations as list")

# ---- define_flow_constants --------------------------------------------------
expect_equal({
  spflow:::define_flow_constants(
    list("global" = TRUE, "intra" = TRUE),
    use_instruments = FALSE,
    OW = Matrix::Matrix(c(0,1,1,0),2))
  },
  {
    list(global = 1, intra = list(In = Matrix::Diagonal(2)))
  },
  info = "define memory efficient constants",
  check.attributes = FALSE)

expect_equal({
  intra_const <- spflow:::define_flow_constants(
    list("global" = TRUE, "intra" = TRUE),
    use_instruments = TRUE,
    OW = Matrix::Matrix(c(0,1,1,0),2))$intra
  lapply(intra_const, "is", "Matrix")
},
  {
    spflow:::named_list(c("(Intra)","W","W'","WW","WW'","V","VV", "WV", "VW'"),
                        TRUE)
  },
  info = "define memory efficient constants")

# ---- define_variable_roles --------------------------------------------------
expect_equal({
  data_sources <- list(
    pair = data.frame(y1 = 1, p1 = 1:4, p2 = 5:8),
    orig = data.frame(o1 = 1:4, o2 = 5:8),
    dest = data.frame(d1 = 1:4, d2 = 5:8))
  formula_roles <- list("Y_" = ~ log(y1),
                        "G_" = ~ p1 + p2,
                        "O_" = ~ o1,
                        "D_" = ~ log(d1))
  formula_roles2 <- list("O_" = ~ o2,
                         "D_" = ~ log(d2))
  formula_roles3 <- list("G_" = ~ sqrt(p1),
                         "O_" = ~ sqrt(o1),
                         "D_" = ~ sqrt(d1))


  forumula_parts <- list("norm" = formula_roles,
                         "sdm"  = formula_roles2,
                         "inst" = formula_roles3)
  spflow:::define_variable_roles(forumula_parts, data_sources)
  },
  {list("Y_" = list("norm" = "log(y1)"),
        "G_" = list("norm" = c("p1","p2"),
                    "inst" = "sqrt(p1)"),
        "O_" = list("norm" = "o1",
                    "sdm"  = "o2",
                    "inst" = "sqrt(o1)"),
        "D_" = list("norm" = "log(d1)",
                    "sdm"  = "log(d2)",
                    "inst" = "sqrt(d1)"))
  },
  info = "collect all variables by role and the way it is used in the model")
