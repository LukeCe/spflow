# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Project: spflow - integration test for model matrix creation
# Author: Lukas Dargel
# = = = = = = = = = = = = = = = = = = =
# Description:
#
# Use the alphabet test data to set up test cases for the creation of the model
# matrices.
# - Case 1: symmetric and complete flows
# = = = = = = = = = = = = = = = = = = =
# Date: November 2020


# ---- Case 1: symmetric and complete flows -----------------------------------
test_formula <- exp(YY) ~ I(XX^2) + G_(exp(GG))
test_control <- spflow_control(estimation_method = "s2sls")
# inputs...
part_variables <- c("Y_" = "exp(YY)", "G_" = "exp(GG)",
                    lookup("I(XX^2)", c("O_","D_","I_")))
var_to_form <- function(v) reformulate_string(v) %>% remove_constant()
part_formulas <- part_variables %>% lapply("var_to_form")
part_formulas <- list("norm" = part_formulas,
                      "sdm" = part_formulas[c("O_","D_","I_")],
                      "inst" = part_formulas[c("O_","D_","I_","G_")])
data_sources <- list("orig" = net_dat_letters %>% cols_keep("XX"),
                     "pair" = pair_dat_letters %>% cols_keep(c("YY","GG")))
test_nb <- named_list(c("OW","DW"),neighborhood(sp_net_letters))
flow_dim <- c(nnodes(sp_net_letters),nnodes(sp_net_letters))
# outputs...
model_matrices <- list(
  "orig" = model.matrix( ~ I(XX^2) -1 ,data = net_dat_letters),
  "pair" = model.matrix( ~ exp(YY) + exp(GG) -1 ,data = pair_dat_letters))

lag_requirements <- c(
  list("Y_" = named_list("norm", "exp(YY)"),
       "G_" = named_list(c("norm", "inst"), "exp(GG)")),
  named_list(names = c("O_","D_","I_"),
             init = named_list(c("norm","sdm","inst"),"I(XX^2)")))


# tests of individual steps...
test_that("by_source_model_matrix: => correct output", {

  #... tests
  actual <- by_source_model_matrix(part_formulas, data_sources)
  expect_orig <- model.matrix( ~ I(XX^2) -1 ,data = net_dat_letters)
  expect_pair <- model.matrix( ~ exp(YY) + exp(GG) -1 ,data = pair_dat_letters)
  expect_equal(actual$orig,expect_orig, check.attributes = FALSE)
  expect_equal(actual$pair,expect_pair, check.attributes = FALSE )
})

test_that("def_spatial_lag_requirements: => correct output", {

  actual_null <- def_spatial_lag_requirements(part_formulas,data_sources)
  expect_null(actual_null)

  actual <- def_spatial_lag_requirements(fun_input)
  expected <- reference
  expect_equal(actual, expected)
})

test_that("by_role_spatial_lags: => correct output", {

  actual <- by_role_spatial_lags(model_matrices,
                                 lag_requirements,
                                 test_nb,
                                 flow_dim,
                                 test_control$model)

})

# test of the complete function...
test_that("spflow_model_matrix: => correct output", {

  actual <- spflow_model_matrix(
    sp_multi_network = sp_multi_net_alphabet,
    network_pair_id = "letters_letters",
    flow_formula = test_formula,
    flow_control = test_control
    )

})


