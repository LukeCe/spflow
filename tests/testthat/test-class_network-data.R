# sp_multi_network ----------------------------------------------------------------
context("Test sp_multi_network")

test_that("Correct construction",{
  test_object <- sp_multi_network()

  expect_s4_class(test_object,"sp_multi_network")
})


