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

load(here::here("tests/testthat/test_case_1_symmetric.rda"))
load_all()

# setup the network object
ge_df <- test_case_1_symmetric$input_data$node_data
ge_neigh <- test_case_1_symmetric$input_data$node_neighborhood
network_ge <- sp_network(network_id = "ge",
                         node_neighborhood = ge_neigh,
                         node_data = ge_df,node_id_column = "id" )

# setup the network pair object
ge_pair_df <- test_case_1_symmetric$input_data$od_pair_data

# for the default estimation we do all transformation
# upfront and keep only a single version of the flows
ge_pair_df_default <- ge_pair_df
ge_pair_df_default$Y2 <- NULL
ge_pair_df_default$Y1 <- NULL
ge_pair_df_default$pair_distance <- log(ge_pair_df_default$pair_distance + 1)

pairs_ge_ge_default <- sp_network_pair(origin_network_id = "ge",
                                       destination_network_id = "ge",
                                       node_pair_data = ge_pair_df_default,
                                       origin_key_column = "orig_id",
                                       destination_key_column = "dest_id")

# combine them into a multi-network
multi_net_ge_default  <- sp_multi_network(network_ge, pairs_ge_ge_default)


describe("Moment generation", {

  describe("Moments can be generated from formula and multinet",{

    it("Is correct for the default estimation",{

      model_formulation <- "matrix"
      default_control <- spflow_control()
      defulat_formula <- Y9 ~ .

      actual_matrices <- spflow_model_matrix(
        sp_multi_network = multi_net_ge_default,
        network_pair_id = "ge_ge",
        flow_formula = defulat_formula,
        flow_control = default_control)

      expected_matrices <- test_case_1_symmetric$relational_model_matrices

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

      expect_equal(actual_matrices$G %>% lapply(as.matrix),expected_matrices$G,
                   check.attributes = FALSE)


      actual_moments <- spflow_model_moments(
        formulation = model_formulation,
        actual_matrices,
        estimator = default_control$estimation_method)

      expected_moments <- test_case_1_symmetric$model_moments

      moment_corrspondence <- data.frame(
        act = c("N","HH","ZZ","HY", "ZY", "TSS"),
        exp = c("N","HH","ZZ","HY9","ZY9","TSS9")
      )


      expect_equal(actual_moments$N, expected_moments$N)
      expect_equal(actual_moments$HH, expected_moments$HH,
                   check.attributes = FALSE)
      expect_equal(actual_moments$ZZ, expected_moments$ZZ,
                   check.attributes = FALSE)
      expect_equal(actual_moments$HY, expected_moments$HY9,
                   check.attributes = FALSE)
      expect_equal(actual_moments$ZY, expected_moments$ZY9,
                   check.attributes = FALSE)
      expect_equal(actual_moments$TSS, expected_moments$TSS9[[1]],
                   check.attributes = FALSE)

    })

  })

})


describe("Quick start estimation", {

  describe("Allows sensefull default estimation for minimal user input.",{

    it("Works for the default s2sls estimation",{
      default_results <- spflow(
        flow_formula = Y9 ~ . ,
        multi_net_ge_default)

      expect_is(default_results,"spflow_model")

      actual_estimates <- default_results$results$est
      expected_estimates <- test_case_1_symmetric$results$Y9$s2sls$params
      expect_equal(actual_estimates,expected_estimates,
                   check.attributes = FALSE)

      actual_uncertainty <- default_results$results$sd
      expected_uncertainty <- test_case_1_symmetric$results$Y9$s2sls$sd_params
      expect_equal(actual_uncertainty,expected_uncertainty,
                   check.attributes = FALSE)

    })

  })
})



