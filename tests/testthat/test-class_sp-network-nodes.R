# ---- constructor ------------------------------------------------------------
test_that("sp_network_nodes: => correct construction", {

  test_object <- sp_network_nodes(
    network_id = "cars",
    node_neighborhood = diag(1, nrow = nrow(cars), ncol = nrow(cars)),
    node_data = cars)
  expect_s4_class(test_object, "sp_network_nodes")

})

test_that("sp_network_nodes: abusive input => error", {

  expect_error({
    test_object <- sp_network_nodes(
      network_id = "cars",
      node_neighborhood = "cars",
      node_data = "cars")
  }, "^[Object ].*[ must be coercible to a ].*\\!$")
})

test_that("sp_network_nodes: inconsistent input => error", {

  expect_error({
    test_object <- sp_network_nodes(
      network_id = "cars",
      node_neighborhood = diag(1, nrow(cars) + 1),
      node_data = cars)
  }, "Row number of node_data does not match the dimensions of the neighborhood matrix!")

})

# ---- assessor methods -------------------------------------------------------
test_that("sp_network_nodes: => correct assessors", {

    test_object <- sp_network_nodes(
      network_id = "cars",
      node_neighborhood = diag(1, nrow = nrow(cars), ncol = nrow(cars)),
      node_data = cars)

    expect_equal(dat(test_object),test_object@node_data)
    expect_equal(id(test_object),"cars")
    expect_equal(nnodes(test_object),test_object@nnodes)
    expect_equal(neighborhood(test_object),test_object@node_neighborhood)
    expect_equal(variable_names(test_object),test_object@node_data %>% names())
})

# ---- replacement methods ----------------------------------------------------
test_that("sp_network_nodes: => correct replacements", {

    test_object <- sp_network_nodes(
      network_id = "cars",
      node_data = cars)

    dat(test_object) <- rbind(cars, cars)
    # FIXME This should not be equal
    expect_equal(dat(test_object),rbind(cars, cars))
    expect_equal(nnodes(test_object),nrow(cars) * 2)

    id(test_object) <- "cars_double"
    expect_equal(id(test_object),"cars_double")

    nb <- Diagonal(nrow(cars) * 2)
    neighborhood(test_object) <- nb
    expect_equal(neighborhood(test_object),nb)
})
