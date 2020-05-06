test_that("Abusive input ==> ERROR", {

    expect_error({
        test_object <- sp_network(
          network_id = "cars",
          node_neighborhood = "cars",
          node_data = "cars")
    }, "^[Object ].*[ must be coercible to a ].*\\!$")

})

test_that("Inconsisten input ==> ERROR", {

    expect_error({
        test_object <- sp_network(
          network_id = "cars",
          node_neighborhood = diag(1, nrow(cars) + 1),
          node_data = cars)
    }, "Row number of node_data does not match the dimensions of the neighborhood matrix!")


})

test_that("Correct S4 construction ", {

    test_object <- sp_network(
      network_id = "cars",
      node_neighborhood = diag(1, nrow = nrow(cars), ncol = nrow(cars)),
      node_data = cars)

    expect_s4_class(test_object, "sp_network")
})

test_that("Correct S4 accessors", {

    test_object <- sp_network(
      network_id = "cars",
      node_neighborhood = diag(1, nrow = nrow(cars), ncol = nrow(cars)),
      node_data = cars)

    expect_equal(test_object@node_data, dat(test_object))
    expect_equal(test_object@node_data %>% names(), variable_names(test_object))
    expect_equal(test_object@node_count, count(test_object))
    expect_equal(test_object@network_id, id(test_object))
    expect_equal(test_object@node_neighborhood, neighborhood(test_object))

})

test_that("Correct data replacements", {

    test_object <- sp_network(
      network_id = "cars",
      node_neighborhood = diag(1, nrow = nrow(cars), ncol = nrow(cars)),
      node_data = cars)

    dat(test_object) <- rbind(cars, cars)

    expect_equal(rbind(cars, cars), dat(test_object))
    expect_equal(nrow(cars) * 2, count(test_object))
})
