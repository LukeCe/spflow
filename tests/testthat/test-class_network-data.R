# network_data ----------------------------------------------------------------
context("Test network_data")

test_that("Correct construction",{
  test_object <- network_data()

  expect_s4_class(test_object,"network_data")
})


