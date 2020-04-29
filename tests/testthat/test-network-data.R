# node_information ------------------------------------------------------------
context("Test node_information")

test_that("Abusive input ==> ERROR",{

  expect_error({
    test_object <- node_information(network_id = "cars",
                                    neighborhood = "cars",
                                    attributes = "cars")
  },
  "^[Object ].*[ must be coercible to a ].*\\!$")

})

test_that("Inconsisten input ==> ERROR",{

  expect_error({
    test_object <- node_information(network_id = "cars",
                                    neighborhood = diag(1,nrow(cars) + 1),
                                    attributes = cars)
  },
  "Row number of attributes does not match the dimensions of the neighborhood matrix!")


})

test_that("Correct construction",{

  test_object <- node_information(network_id = "cars",
                                  neighborhood = diag(1,nrow = nrow(cars), ncol = nrow(cars)),
                                  attributes = cars)

  expect_s4_class(test_object,"node_information")
})

# od_pair_information ---------------------------------------------------------
context("Test od_pair_information")

test_that("Abusive input ==> ERROR",{

  expect_error({
    test_object <- od_pair_information(origin_id = "cars",
                                       destination_id = "cars",
                                       pair_attributes = "cars")
  },
  "^[Object ].*[ must be coercible to a ].*\\!$")

  expect_error({
    test_object <- od_pair_information(origin_id = 1,
                                       destination_id = 2,
                                       pair_attributes = cars)
  })
})


test_that("Correct construction",{
  test_object <- od_pair_information(origin_id = "cars",
                                     destination_id = "cars",
                                     pair_attributes = diag(1,50,50))

  expect_s4_class(test_object,"od_pair_information")
})

# network_data ----------------------------------------------------------------
context("Test network_data")

test_that("Correct construction",{
  test_object <- network_data()

  expect_s4_class(test_object,"network_data")
})


