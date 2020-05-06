# spflow ----------------------------------------------------------------------
context("Test spflow user interface")

test_that("Abusive input ==> ERROR", {

  expect_error(spflow(formula = "A"),
               "A valid formula is required!")

  expect_error(spflow(y ~ x + z, sp_multi_network = cars),
               "The data musst be a network data object!")

  expect_error(spflow(y ~ x + z,
                      sp_multi_network = sp_multi_network(),
                      origin_id = 1),
               "The origin and destination id musst be strings!")

  expect_error(spflow(y ~ x + z,
                      sp_multi_network = sp_multi_network(),
                      origin_id = "A",
                      destination_id = "B",
                      model_specifications = "ABC"),
               "If provided, the model specifications must be of specify_flow_model class!")

})


test_that("Inconsisten input ==> ERROR", {

  expect_error(spflow(y ~ x + z,
                      sp_multi_network = sp_multi_network(),
                      origin_id = "A",
                      destination_id = "B",
                      model_specifications),
               "If provided, the model specifications must be of specify_flow_model class!")

})

