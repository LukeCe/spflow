# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Project: spflow - test case based on the paris10km example
# Author: Lukas Dargel
# = = = = = = = = = = = = = = = = = = =
# Description:
#
# Test error free estimation for all possible models (sepcific estimates).
# test different use of variables:
#   - some are used as in: (SAR)
#   - some are used as in: (SDM only lags)
#   - some are used as in: (SDM lags and originals)
# - - - - - - - - - - - - - - - - - - -
# Date: October 2020
devtools::load_all()

# setup the test example
data("paris10km_nodes","paris10km_node_pairs","paris10km_mat_nb")

paris10km_sp_nodes <-
  sp_network_nodes(
    network_id = "paris10km",
    node_neighborhood = paris10km_mat_nb$by_border,
    node_data = paris10km_nodes %>% sf::st_drop_geometry(.),
    node_id_column = "ID")

paris10km_sp_pairs <-
  sp_network_pair(
    origin_network_id = "paris10km",
    destination_network_id = "paris10km",
    node_pair_data = paris10km_node_pairs,
    origin_key_column = "ORIG_ID",
    destination_key_column = "DEST_ID")

paris10km_sp_multi_net <-
  sp_multi_network(paris10km_sp_nodes,paris10km_sp_pairs)


describe("Estimate models with advances use of formulas",{

  it("Allows mixed use of variable usage",{

    # normal variables in: O_() & D_() & G_()
    flow_formula <-
      log(COMMUTE_FLOW + 1) ~
      D_(log(POPULATION + 1) + log(NB_COMPANY + 1) + log(MED_INCOME + 1)) +
      O_(log(POPULATION + 1) + log(NB_COMPANY + 1) + log(MED_INCOME + 1)) +
      G_(log(DISTANCE + 1))

    # sdm variables (lags) in: D_() -> not O_() & I_()
    sdm_formula <- ~
      D_(log(POPULATION + 1) + log(NB_COMPANY + 1) + log(MED_INCOME + 1))
    flow_control <- spflow_control(model = "model_7",
                                   sdm_variables = sdm_formula)

    results <- spflow(flow_formula,
                      paris10km_sp_multi_net,
                      flow_control = flow_control)

    # test 1: correct return value
    expect_s4_class(results,"spflow_model_meta")

    # test 2: correct model was estimated
    transformed_vars <-
      c("log(POPULATION + 1)", "log(NB_COMPANY + 1)","log(MED_INCOME + 1)")

    expected_parameters <- c(
      "rho_d","rho_o",
      "Constant", "Constant_intra",
      "Dest_" %p% transformed_vars, "Dest_" %p% transformed_vars %p% ".lag1",
      "Orig_" %p% transformed_vars,
      "log(DISTANCE + 1)")
    actual_parameters <- results %>% coef() %>% names()
    expect_equal(expected_parameters,actual_parameters)

  })

  it("Allows to use exclusion logic for variables that should not be used.",{

    # normal variables in: O_() & D_() & G_()
    flow_formula_explicit <-
      log(COMMUTE_FLOW + 1) ~
      D_(log(POPULATION + 1) + log(NB_COMPANY + 1) + log(MED_INCOME + 1)) +
      O_(log(POPULATION + 1) + log(NB_COMPANY + 1) + log(MED_INCOME + 1)) +
      G_(log(DISTANCE + 1))

    # implicitly use O_() & D_() by removing I_()
    flow_formula_implicit <-
      log(COMMUTE_FLOW + 1) ~
      log(POPULATION + 1) + log(NB_COMPANY + 1) + log(MED_INCOME + 1) +
      I_() +
      G_(log(DISTANCE + 1))

    # sdm variables (lags) in: D_() & I_() -> not O_()
    sdm_formula_explicit <- ~
      D_(log(POPULATION + 1) + log(NB_COMPANY + 1) + log(MED_INCOME + 1)) +
      I_(log(POPULATION + 1) + log(NB_COMPANY + 1) + log(MED_INCOME + 1))

    # implicitly use D_() & I_() by removing O_()
    sdm_formula_implicit <- ~
      log(POPULATION + 1) + log(NB_COMPANY + 1) + log(MED_INCOME + 1) + O_()

    set.seed(123)
    results_explicit <- spflow(
      flow_formula_explicit,
      paris10km_sp_multi_net,
      flow_control = spflow_control(sdm_variables = sdm_formula_explicit))

    set.seed(123)
    results_implicit <- spflow(
      flow_formula_implicit,
      paris10km_sp_multi_net,
      flow_control = spflow_control(sdm_variables = sdm_formula_implicit))

    expect_equal(results_implicit %>% results(),
                 results_explicit %>% results())

  })


  it("Detects incorrect use of the flow formula and returns nice errors.",{

    # implicitly use O_() & D_() by removing I_()
    flow_formula_implicit <-
      log(COMMUTE_FLOW + 1) ~
      log(POPULATION + 1) + log(NB_COMPANY + 1) + log(MED_INCOME + 1) +
      I_() +
      G_(log(DISTANCE + 1))

    # implicitly use D_() & I_()
    # by removing O_(1) --> error
    sdm_formula_implicit <- ~
      log(POPULATION + 1) + log(NB_COMPANY + 1) + log(MED_INCOME + 1) +
      O_(1)

    expect_error({
      results_implicit <- spflow(
        flow_formula_implicit,
        paris10km_sp_multi_net,
        flow_control = spflow_control(sdm_variables = sdm_formula_implicit))
    },
    regexp = "The specifyed flow_formula can not be interpreted.")

  })
})
