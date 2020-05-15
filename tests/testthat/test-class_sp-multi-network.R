# ---- constructor ------------------------------------------------------------
context("sp_multi_network - constructor")

test_network_ids <- c("net1","net2")
test_multi_net <- sp_multi_network(
  lapply(test_network_ids, sp_network),
  lapply(test_network_ids, function(.id)
    sp_network_pair(.id, .id)))

test_that("Correct construction", {
    expect_s4_class(test_multi_net, "sp_multi_network")
})


# ---- Method: id -------------------------------------------------------------
context("sp_multi_network - Method: id")

test_that("id is read correctly", {

  test_ids <- id(test_multi_net)

  expect_equal(object = test_ids$networks,
               expected = test_network_ids)
})

# ---- Method: dat ------------------------------------------------------------
context("sp_multi_network - Method: dat")

test_that("Data retrieval works correctly", {

  net_id <- "iris"
  test_net <- sp_network(net_id,node_data = iris)

  orig_id <- "carsO"
  dest_id <- "carsD"
  test_pairs <- sp_network_pair("carsO","carsD",
                                node_pair_data = cars,
                                origin_node_count = 5)
  test_multi_net <- sp_multi_network(test_net,test_pairs)

  expect_equal(object = dat(test_multi_net,network_id = net_id),
               expected = dat(test_net))

  expect_equal(
    object = dat(test_multi_net,
                 network_pair_id = orig_id %p% "_" %p% dest_id),
    expected = dat(test_pairs))
})

# ---- Method: pair_merge -----------------------------------------------------
context("sp_multi_network - Method: pair_merge")
data("multi_net_usa_ge")

test_that("Combining neworks works", {

  test_pair_merged <- pair_merge(multi_net_usa_ge,
                                 network_pair_id = "ge_ge")

  nrow_ge <- 16
  ncol_ge <- 1
  ncol_pairs <- 3


  expect_is(test_pair_merged,c("data.table"))
  expect_equal(dim(test_pair_merged),c(nrow_ge^2,ncol_pairs + 2 * ncol_ge))


})
