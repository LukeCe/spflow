# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Project: spflow - test case1 end 2 end
# Author: Lukas Dargel
# = = = = = = = = = = = = = = = = = = =
# Description:
#
# End to end test from clean data to results.
# The test is based on test case1.
# (256 flows within the 16 simulated states of germany.)
# - - - - - - - - - - - - - - - - - - -
# Date: June 2020

load(file.path(rprojroot::find_testthat_root_file(),
               "test_case_1_symmetric.rda"))
load_all()

ge_df <- test_case_1_symmetric$input_data$node_data
ge_neigh <- test_case_1_symmetric$input_data$node_neighborhood
network_ge <- sp_network(network_id = "ge",
                         node_neighborhood = ge_neigh,
                         node_data = ge_df,node_id_column = "id" )

# setup the network pair object
ge_pair_df <- test_case_1_symmetric$input_data$od_pair_data

# for the default estimation we do all transformation
# upfront and keep only a single version of the flows
ge_pair_df_default <- data.table::copy(ge_pair_df)
ge_pair_df_default$Y2 <- NULL
ge_pair_df_default$Y1 <- NULL
ge_pair_df_default$pair_distance <- log(ge_pair_df_default$pair_distance + 1)

pairs_ge_ge_default <- sp_network_pair(
  origin_network_id = "ge",
  destination_network_id = "ge",
  node_pair_data = ge_pair_df_default,
  origin_key_column = "orig_id",
  destination_key_column = "dest_id")

pairs_ge_ge_flexible <- sp_network_pair(
  origin_network_id = "ge",
  destination_network_id = "ge",
  node_pair_data = ge_pair_df,
  origin_key_column = "orig_id",
  destination_key_column = "dest_id")

# combine them into a multi-network
multi_net_ge_default  <- sp_multi_network(network_ge, pairs_ge_ge_default)
multi_net_ge_flex  <- sp_multi_network(network_ge, pairs_ge_ge_flexible)

# finish setup ----


describe("Moments can be generated from formula and multinet",{

  it("Is correct for the default s2sls estimation (M9)",{

    model_formulation <- "matrix"
    default_control <- spflow_control()
    defulat_formula <- Y9 ~ .

    actual_matrices <- spflow_model_matrix(
      sp_multi_network = multi_net_ge_default,
      network_pair_id = "ge_ge",
      flow_formula = defulat_formula,
      flow_control = default_control)

    expected_matrices <- test_case_1_symmetric$relational_model_matrices$M9

    expect_equal(actual_matrices$const,expected_matrices$const,
                 check.attributes = FALSE)
    expect_equal(actual_matrices$const_intra,expected_matrices$const_intra,
                 check.attributes = FALSE)
    expect_equal(actual_matrices$DX,expected_matrices$DX,
                 check.attributes = FALSE)
    expect_equal(actual_matrices$OX,expected_matrices$OX,
                 check.attributes = FALSE)
    expect_equal(actual_matrices$IX,expected_matrices$IX,
                 check.attributes = FALSE)
    expect_equal(actual_matrices$G %>% lapply(as.matrix) ,expected_matrices$G,
                 check.attributes = FALSE)


    actual_moments <- spflow_model_moments(
      formulation = model_formulation,
      actual_matrices,
      estimator = default_control$estimation_method)

    expected_moments <- test_case_1_symmetric$model_moments$M9

    expect_equal(actual_moments$N, expected_moments$N)

    expect_equal(actual_moments$HH, expected_moments$HH,
                 check.attributes = FALSE)
    expect_equal(actual_moments$ZZ, expected_moments$ZZ,
                 check.attributes = FALSE)
    expect_equal(actual_moments$HY, expected_moments$HY,
                 check.attributes = FALSE)
    expect_equal(actual_moments$ZY, expected_moments$ZY,
                 check.attributes = FALSE)
    expect_equal(actual_moments$TSS, expected_moments$TSS[[1]],
                 check.attributes = FALSE)


  })

  it("Is correct for the default mle estimation (M9)",{

    model_formulation <- "matrix"
    default_control <- spflow_control(estimation_method = "mle")
    defulat_formula <- Y9 ~ .

    actual_matrices <- spflow_model_matrix(
      sp_multi_network = multi_net_ge_default,
      network_pair_id = "ge_ge",
      flow_formula = defulat_formula,
      flow_control = default_control)

    expected_matrices <- test_case_1_symmetric$relational_model_matrices$M9

    expect_equal(actual_matrices$const,expected_matrices$const,
                 check.attributes = FALSE)
    expect_equal(actual_matrices$const_intra,expected_matrices$const_intra[1],
                 check.attributes = FALSE)

    drop_inst <- -(3:4)
    expect_equal(actual_matrices$DX,expected_matrices$DX[,drop_inst],
                 check.attributes = FALSE)
    expect_equal(actual_matrices$OX,expected_matrices$OX[,drop_inst],
                 check.attributes = FALSE)
    expect_equal(actual_matrices$IX,expected_matrices$IX[,drop_inst],
                 check.attributes = FALSE)

    expect_equal(actual_matrices$G %>% lapply(as.matrix),
                 expected_matrices$G[1],
                 check.attributes = FALSE)


    actual_moments <- spflow_model_moments(
      formulation = model_formulation,
      actual_matrices,
      estimator = default_control$estimation_method,
      flow_type = "within")

    expected_moments <- test_case_1_symmetric$model_moments$M9

    expect_equal(actual_moments$N, expected_moments$N)

    expect_equal(actual_moments$ZZ, expected_moments$ZZ,
                 check.attributes = FALSE)
    expect_equal(actual_moments$ZY, expected_moments$ZY,
                 check.attributes = FALSE)
    expect_equal(actual_moments$TSS, expected_moments$TSS,
                 check.attributes = FALSE)
    expect_equal(actual_moments$OW_traces, expected_moments$OW_traces,
                 check.attributes = FALSE)
    expect_equal(actual_moments$DW_traces, expected_moments$DW_traces,
                 check.attributes = FALSE)
    expect_equal(actual_moments$n_o, expected_moments$n_o,
                 check.attributes = FALSE)
    expect_equal(actual_moments$n_d, expected_moments$n_d,
                 check.attributes = FALSE)
    })
})

describe("Quick start estimstion for minimals user input.",{

  it("Works for the default s2sls estimation (M9 - sym)",{
    default_results <- spflow(
      flow_formula = Y9 ~ . ,
      multi_net_ge_default)

    expect_is(default_results,"spflow_model")

    actual_estimates <- default_results$results$est
    expected_estimates <- test_case_1_symmetric$results$M9$s2sls$params
    expect_equal(actual_estimates,expected_estimates,
                 check.attributes = FALSE)

    actual_uncertainty <- default_results$results$sd
    expected_uncertainty <- test_case_1_symmetric$results$M9$s2sls$sd_params
    expect_equal(actual_uncertainty,expected_uncertainty,
                 check.attributes = FALSE)

    actual_names <- rownames(default_results$results)
    expected_names <- c(
      "rho_d", "rho_o", "rho_w", "Constant","Constant_intra",
      "Dest_X", "Dest_X.lag1", "Orig_X", "Orig_X.lag1",
      "Intra_X", "Intra_X.lag1", "pair_distance")
    expect_equal(actual_names,expected_names)

  })

  it("Works for the mle estimation (M9 - sym)",{

    default_results <- spflow(
      flow_formula = Y9 ~ . ,
      multi_net_ge_default,
      flow_control = spflow_control(estimation_method = "mle"))

    expect_is(default_results,"spflow_model")

    # test length as exact reference values are not available
    actual_estimates <- default_results$results$est
    expected_estimates_len <-
      test_case_1_symmetric$results$M9$s2sls$params %>% length()

    expect_length(actual_estimates,expected_estimates_len)

    expected_uncertainty_len <-
      test_case_1_symmetric$results$M9$s2sls$sd_params %>% length()
    actual_uncertainty <- default_results$results$sd
    expect_length(actual_uncertainty,expected_uncertainty_len)

  })

  it("Works for the mcmc estimation (M9 - sym)",{

    default_results <- spflow(
      flow_formula = Y9 ~ . ,
      multi_net_ge_default,
      flow_control = spflow_control(estimation_method = "mcmc"))

    expect_is(default_results,"spflow_model")

    # test length as exact reference values are not available
    actual_estimates <- default_results$results$est
    expected_estimates_len <-
      test_case_1_symmetric$results$M9$s2sls$params %>% length()

    expect_length(actual_estimates,expected_estimates_len)

    expected_uncertainty_len <-
      test_case_1_symmetric$results$M9$s2sls$sd_params %>% length()
    actual_uncertainty <- default_results$results$sd
    expect_length(actual_uncertainty,expected_uncertainty_len)

  })

})

describe("Estimation via the formula interface without intra model", {

  # different expressions of the same formula are tested for each method
  it("Works for s2sls estimation (M2 - sym)",{

    test_control <- spflow_control(estimation_method = "s2sls",
                                   use_intra = FALSE,
                                   model = "model_2")
    test_results <- spflow(
      flow_formula = Y2 ~ . + G_(log(pair_distance + 1)) ,
      multi_net_ge_flex,
      flow_control = test_control)

    expect_is(test_results,"spflow_model")

    actual_estimates <- test_results$results$est
    expected_estimates <- test_case_1_symmetric$results$M2$s2sls$params
    expect_equal(actual_estimates,expected_estimates,
                 check.attributes = FALSE)

    actual_uncertainty <- test_results$results$sd
    expected_uncertainty <- test_case_1_symmetric$results$M2$s2sls$sd_params
    expect_equal(actual_uncertainty,expected_uncertainty,
                 check.attributes = FALSE)

  })

  it("Works for mle estimation (M2 - sym - mixed hessian)",{

    test_control <- spflow_control(estimation_method = "mle",
                                   use_intra = FALSE,
                                   model = "model_2")

    # should ignore the I(... part) and carry distance to G(... part)
    test_results <- spflow(
      flow_formula = Y2 ~ D_(X) + O_(X) + I_(X)+ log(pair_distance + 1),
      multi_net_ge_flex,
      flow_control = test_control)

    expect_is(test_results,"spflow_model")

    actual_estimates <- test_results$results$est
    expected_estimates <- test_case_1_symmetric$results$M2$s2sls$params
    expect_length(actual_estimates,length(expected_estimates))

    actual_uncertainty <- test_results$results$sd
    expected_uncertainty <- test_case_1_symmetric$results$M2$s2sls$sd_params
    expect_length(actual_uncertainty,length(expected_uncertainty))

  })

  it("Works for mle estimation (M2 - sym - f2 hessian)",{

    test_control <- spflow_control(estimation_method = "mle",
                                   hessian_method = "f2",
                                   use_intra = FALSE,
                                   model = "model_2")

    test_results <- spflow(
      flow_formula = Y2 ~ D_(X) + O_(X) + log(pair_distance + 1),
      multi_net_ge_flex,
      flow_control = test_control)

    expect_is(test_results,"spflow_model")

    actual_estimates <- test_results$results$est
    expected_estimates <- test_case_1_symmetric$results$M2$s2sls$params
    expect_length(actual_estimates,length(expected_estimates))

    actual_uncertainty <- test_results$results$sd
    expected_uncertainty <- test_case_1_symmetric$results$M2$s2sls$sd_params
    expect_length(actual_uncertainty,length(expected_uncertainty))

  })

  it("Works for mle estimation (M2 - sym)",{

    test_control <- spflow_control(estimation_method = "mcmc",
                                   use_intra = FALSE,
                                   model = "model_2")
    test_results <- spflow(
      flow_formula = Y2 ~ D_(X) + X + G_(log(pair_distance + 1)),
      multi_net_ge_flex,
      flow_control = test_control)

    expect_is(test_results,"spflow_model")

    actual_estimates <- test_results$results$est
    expected_estimates <- test_case_1_symmetric$results$M2$s2sls$params
    expect_length(actual_estimates,length(expected_estimates))

    actual_uncertainty <- test_results$results$sd
    expected_uncertainty <- test_case_1_symmetric$results$M2$s2sls$sd_params
    expect_length(actual_uncertainty,length(expected_uncertainty))

  })

})



