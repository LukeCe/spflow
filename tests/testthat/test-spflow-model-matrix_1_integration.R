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

n <- 8
YY_mat <- matrix(exp(pair_dat_letters$YY),n,n)
GG_mat <- matrix(exp(pair_dat_letters$GG),n,n)
XX_mat <- net_dat_letters$XX^2
spatial_lags <- c(
  list("Y_" = lapply(c(1,2,2,4), "*" , YY_mat) %>%
         plapply(object = . ,
                 Class = c("matrix",rep("dgeMatrix",3)),.f =  "as"),
       "G_" = lapply(c(1,4,16), "*" , GG_mat)),
  named_list(c("D_","O_","I_"),
             lapply((2)^(0:3), "*",XX_mat) %>% lreduce(cbind))
)

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

  actual <- def_spatial_lag_requirements(part_formulas,data_sources)
  expected <- lag_requirements
  expect_equal(actual, expected)
})

test_that("by_role_spatial_lags: => correct output", {

  args <- def_matrix_form_args(sp_multi_net_alphabet, "letters_letters")
  actual <- by_role_spatial_lags(model_matrices,
                                 lag_requirements,
                                 test_nb,
                                 args,
                                 test_control$model)

  expect_equivalent(actual$Y_, spatial_lags$Y_)
  expect_equivalent(actual$G_ %>% lapply(as,"matrix"), spatial_lags$G_)
  expect_equivalent(actual$I_, spatial_lags$I_)
  expect_equivalent(actual$D_, spatial_lags$D_)
  expect_equivalent(actual$O_, spatial_lags$O_)

})

# test of the complete function...
test_that("spflow_model_matrix: => correct output", {

  actual <- spflow_model_matrix(
    sp_multi_network = sp_multi_net_alphabet,
    network_pair_id = "letters_letters",
    flow_formula = test_formula,
    flow_control = test_control
    )

  expected_names <- c("Y_","G_","O_","D_","I_","OW","DW","constants","weights" )
  expect_named(actual,expected_names)

  # Test instruments
  # G
  actual_inst_stat <- actual$G_ %>%
    lapply(get_instrument_status) %>%
    flatten(use.names = FALSE)
  expect_inst <- c(FALSE,TRUE,TRUE)
  expect_equal(actual_inst_stat,expect_inst)
  # D O I
  expect_inst <- c(FALSE,FALSE,TRUE,TRUE)
  expect_equal(actual$D_ %>% get_instrument_status(),expect_inst)
  expect_equal(actual$O_ %>% get_instrument_status(),expect_inst)
  expect_equal(actual$I_ %>% get_instrument_status(),expect_inst)

  # No weights
  expect_null(actual$weights)
})


