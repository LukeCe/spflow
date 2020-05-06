context("Test sp_network_pair")

test_that("Abusive input ==> ERROR",{

  expect_error({
    test_object <- sp_network_pair(origin_id = "cars",
                                       destination_id = "cars",
                                       pair_data = "cars")
  },
  "^[Object ].*[ must be coercible to a ].*\\!$")

  expect_error({
    test_object <- sp_network_pair(origin_id = 1,
                                       destination_id = 2,
                                       pair_data = cars)
  })
})


test_that("Correct S4 construction",{
  test_object <- sp_network_pair(origin_id = "cars",
                                     destination_id = "cars",
                                     pair_data = diag(1,50,50))

  expect_s4_class(test_object,"sp_network_pair")
})

test_that("Correct S4 construction",{
  test_object <- sp_network_pair(origin_id = "cars",
                                     destination_id = "cars",
                                     pair_data = diag(1,50,50))

  expect_s4_class(test_object,"sp_network_pair")
})

test_that("Correct S4 accessors",{

  test_object <- sp_network_pair(origin_id = "cars",
                                     destination_id = "cars",
                                     pair_data = diag(1,50,50))

  expect_equal(test_object@pair_data,data(test_object))
  expect_equal(test_object@pair_data %>% names ,variable_names(test_object))
  expect_equivalent(c("cars","cars","cars_cars"),id(test_object))
  expect_equivalent(c(50,50,50^2),count(test_object))
})


test_that("Correct data replacements",{

  test_object <- sp_network_pair(origin_id = "cars",
                                     destination_id = "cars",
                                     pair_data = diag(1,50,50))

  data(test_object) <- diag(2,51,51)
    expect_equivalent(c(51,51,51^2),count(test_object))
})
