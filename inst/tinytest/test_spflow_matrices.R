# ---- pull_spflow_neighborhood -------------------------------------------------
expect_equal({

  test_o_net <- spflow_network(
    "net1",
    node_neighborhood = matrix(c(0,1,1,0),2,2),
    node_data = data.frame("ID" = c("A", "B"), "VAL" = "OO"),
    node_key_column = "ID")

  test_d_net <- spflow_network(
    "net2",
    node_neighborhood = matrix(c(0,1,1,0),2,2),
    node_data = data.frame("ID" = c("C", "D"), "VAL" = "DD"),
    node_key_column = "ID")

  test_net_pair <- spflow_network_pair(
    id_orig_net = "net1",
    id_dest_net =  "net2",
    pair_data = data.frame(
      "ID_O" = c("A", "B"),
      "ID_D" = c("C", "D"),
      "DIST" = c(1, 4)),
    orig_key_column = "ID_O",
    dest_key_column =  "ID_D")


  test_mult_net <- spflow_network_multi(test_o_net,test_d_net, test_net_pair)
  spflow:::pull_spflow_neighborhood(test_mult_net,"net1_net2")
  },
  {
    spflow:::named_list(c("OW","DW"),Matrix(c(0,1,1,0),2,2))
  },
  check.attributes = FALSE,
  info = "neigborhood matrices of origins and destinations as list")

# ---- flow_conform_model_matrix ----------------------------------------------
expect_equal(
  spflow:::flow_conform_model_matrix(~ . , data.frame("A" = 1:2, "B" = 3:4)),
  model.matrix(~ . , data.frame("A" = 1:2, "B" = 3:4))[,c("A","B")],
  info = "handles intercept removal",
  check.attributes = FALSE)

expect_equal(
  spflow:::flow_conform_model_matrix(~ . - 1 , data.frame("A" = 1:2, "B" = 3:4)),
  model.matrix(~ . - 1, data.frame("A" = 1:2, "B" = 3:4)),
  info = "handles intercept removal",
  check.attributes = FALSE)

expect_equal({
  spflow:::flow_conform_model_matrix(
    ~ . - 1,
    data.frame("A" = 1:2,
               "B" = 3:4,
               "C" = factor(c("D","F")))
  )
},
{
  model.matrix(
    ~ . - 1,
    data.frame("A" = 1:2,
               "B" = 3:4,
               "C" = factor(c("D","F")))
  )[,c("A","B","CF")]
},
info = "handles one factor (dont expand all levels)",
check.attributes = FALSE)

expect_equal({
  spflow:::flow_conform_model_matrix(
    ~ . - 1,
    data.frame("A" = 1:4,
               "B" = 3:6,
               "C" = factor(c("D","F")),
               "H" = factor(c("G","I","K","K")))
  )
},
{
  model.matrix(
    ~ .,
    data.frame("A" = 1:4,
               "B" = 3:6,
               "C" = factor(c("D","F")),
               "H" = factor(c("G","I","K","K")))
  )[,c("A","B","CF","HI","HK")]
},
info = "handles two factor (dont expand all levels)",
check.attributes = FALSE)

# ---- impute_lost_cases ------------------------------------------------------
expect_equal({
  m <- matrix(1:4, nrow = 2, ncol = 2, dimnames = list(NULL, c("A","B")))
  rbind(m[1,,drop = FALSE],0,m[2,,drop = FALSE])
},
spflow:::impute_lost_cases(matrix(1:4, nrow = 2, ncol = 2, dimnames = list(NULL, c("A","B"))),lost_cases = c(F,T,F))
)


