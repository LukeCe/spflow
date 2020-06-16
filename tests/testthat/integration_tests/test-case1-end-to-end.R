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

load(here::here("tests/testthat/test_case_1.rda"))
data("germany_grid")

# setup the network object
ge_df <- germany_grid@data
ge_neigh <- test_case_1$data$W
network_ge <- sp_network(network_id = "ge",
                         node_neighborhood = ge_neigh,
                         node_data = ge_df,
                         node_id_column = "NOM")

# setup the network pair object
ge_flow_mat <- test_case_1$data$Y9[[1]]
colnames(ge_flow_mat) <- ge_df$NOM
rownames(ge_flow_mat) <- ge_df$NOM

ge_pair_df <- ge_flow_mat %>%
  stack_cols(.,rows = "orig", cols = "dest", value = "flow") %>%
  cbind(., "dist" = test_case_1$vector_reference$G[,1])
pairs_ge_ge <- sp_network_pair(origin_network_id = "ge",
                               destination_network_id = "ge",
                               node_pair_data = ge_pair_df,
                               origin_key_column = "orig",
                               destination_key_column = "dest")

# combine them into a multi-network
multi_net_ge <- sp_multi_network(network_ge, pairs_ge_ge)


describe("Quick start estimation", {

  describe("Allows sensefull default estimation for minimal user input.",{

    it("Works for the default s2sls estimation",{
      default_results <- spflow(
        flow_formula = flow ~ .,
        multi_net_ge)

      expect_is(default_results,"spflow_model")

      actual_estimates <- default_results$results$est
      expected_estimates <- test_case_1$parmeters$Y9$s2sls
      expect_equal(actual_estimates,expected_estimates)

      actual_sd <- default_results$sd
      expected_sd <- test_case_1$sd_error$Y9$s2sls
      expect_equal(actual_estimates,expected_estimates)

    })

  })
})



