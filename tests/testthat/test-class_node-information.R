context("Test node_information")

test_that("Abusive input ==> ERROR",{

  expect_error({
    test_object <- node_information(network_id = "cars",
                                    neighborhood = "cars",
                                    node_data = "cars")
  },
  "^[Object ].*[ must be coercible to a ].*\\!$")

})

test_that("Inconsisten input ==> ERROR",{

  expect_error({
    test_object <- node_information(network_id = "cars",
                                    neighborhood = diag(1,nrow(cars) + 1),
                                    node_data = cars)
  },
  "Row number of node_data does not match the dimensions of the neighborhood matrix!")


})

test_that("Correct construction",{

  test_object <- node_information(network_id = "cars",
                                  neighborhood = diag(1,nrow = nrow(cars), ncol = nrow(cars)),
                                  node_data = cars)

  expect_s4_class(test_object,"node_information")
})
