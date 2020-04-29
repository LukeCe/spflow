# node_information ------------------------------------------------------------
context("Test node_information")

test_that("Abusive input ==> ERROR",{

  expect_error({
    test_object <- node_information(network_id = "cars",
                                    neighbourhood = "cars",
                                    attributes = "cars")
  },
  "^[Object ].*[ must be coercible to a ].*\\!$")

})

test_that("Inconsisten input ==> ERROR",{

  expect_error({
    test_object <- node_information(network_id = "cars",
                                    neighbourhood = diag(1,nrow(cars) + 1),
                                    attributes = cars)
  },
  "Row number of attributes does not match the dimensions of the neighbourhood matrix!")


})

test_that("Correct construction",{

  test_object <- node_information(network_id = "cars",
                                  neighbourhood = diag(1,nrow = nrow(cars), ncol = nrow(cars)),
                                  attributes = cars)

  expect_s4_class(test_object,"node_information")
})
