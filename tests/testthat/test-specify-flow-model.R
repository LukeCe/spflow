# specify_flow_model ----------------------------------------------------------
context("Test detailed model specification")


test_that("Sensefull defaults", {

  expect_is(specify_flow_model(),
            "list")
})
