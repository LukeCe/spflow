test_that("drop_columns: => correct output", {

  # test character input
  char_cols <- names(cars)
  net_object <- sp_network(network_id = "dummy_net", node_data = cars)
  actual <- drop_columns(net_object,char_cols)
  expected <- data.table::data.table()
  expect_equal(actual[,id := NULL], expected)


  # test integer input
  char_cols <- 1:2
  net_object <- sp_network(network_id = "dummy_net", node_data = cars)
  actual <- drop_columns(net_object,char_cols)
  expected <- data.table::data.table()
  expect_equal(actual[,id := NULL], expected)
})


