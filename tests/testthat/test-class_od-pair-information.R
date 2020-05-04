context("Test od_pair_information")

test_that("Abusive input ==> ERROR",{

  expect_error({
    test_object <- od_pair_information(origin_id = "cars",
                                       destination_id = "cars",
                                       pair_data = "cars")
  },
  "^[Object ].*[ must be coercible to a ].*\\!$")

  expect_error({
    test_object <- od_pair_information(origin_id = 1,
                                       destination_id = 2,
                                       pair_data = cars)
  })
})


test_that("Correct construction",{
  test_object <- od_pair_information(origin_id = "cars",
                                     destination_id = "cars",
                                     pair_data = diag(1,50,50))

  expect_s4_class(test_object,"od_pair_information")
})
