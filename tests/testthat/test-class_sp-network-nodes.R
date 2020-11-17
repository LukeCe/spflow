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
  }, "The row number of node_data does not match the dimensions ofthe neighborhood matrix!")

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
      node_data =  cars)
    test_object2 <- sp_network_nodes(
      network_id = "iris",
      node_data =  iris,
      node_neighborhood = Diagonal(nrow(iris)))

    # Replacement in consistent order
    id(test_object) <- id(test_object2)
    dat(test_object) <- dat(test_object2)
    neighborhood(test_object) <- neighborhood(test_object2)
    expect_equal(test_object,test_object2)

    # Inconsistencies are captured by NULL replacements
    test_object <- sp_network_nodes(network_id = "cars", node_data =  cars)
    test_dat <- dat(test_object) %>% copy()

    neighborhood(test_object) <- Diagonal(51)
    expect_null(dat(test_object))
    expect_equal(neighborhood(test_object), Diagonal(51))

    dat(test_object) <- test_dat
    expect_null(neighborhood(test_object))
    expect_equal(dat(test_object), test_dat)
})

test_that("sp_network_nodes: inconsistent replacements => error", {

  test_object <- sp_network_nodes(
    network_id = "cars",
    node_data =  cars)

  expect_error(dat(test_object) <- cars)
  expect_error(neighborhood(test_object) <- cars)
  expect_error(id(test_object) <- 10)

})

# ---- show method ------------------------------------------------------------
test_that("sp_network_nodes: correct show-method", {
  test_object <- sp_network_nodes(
    network_id = "cars",
    node_neighborhood = diag(1, nrow = nrow(cars), ncol = nrow(cars)),
    node_data = cars)

  # complete
  expect_output(show(test_object))
  # no neighborhood
  neighborhood(test_object) <- NULL
  expect_output(show(test_object))
  # no data
  dat(test_object) <- NULL
  expect_output(show(test_object))
})
