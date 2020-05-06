# interpret_flow_formula ------------------------------------------------------
context("Test interpret_flow_formula")



test_that("multiplication works", {

  test_fromula <- y ~ .
  variables <- c(
    "origin" = "X1",
    "destination" = "X1",
    "intra" = "X1",
    "pair" = "G1")




  expect_equal(2 * 2, interpret_flow_formula(test_fromula,variables))
})
