# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Project: spflow - profile based on the acceptence tests
# Author: Lukas Dargel & Thibault Laurent
# = = = = = = = = = = = = = = = = = = =
# Description:
#
# For all four network pairs we simulate two types of models.
# - - - - - - - - - - - - - - - - - - -
# Date: Mai 2020

library("spflow")
library("profvis")
data("multi_net_usa_ge")


profile_method <- "mcmc"
profvis::profvis({
res9_s2sls <-
  spflow(
    y9 ~ X + G_(DISTANCE),
    sp_multi_network = multi_net_usa_ge,
    network_pair_id = "usa_usa",
    flow_control = spflow_control(model = "model_9",
                                  estimation_method = profile_method)
  )
})

profvis::profvis({
  res2_s2sls <-
    spflow(
      y2 ~ X + G_(log(distance + 1)),
      sp_multi_network = multi_net_usa_ge,
      network_pair_id = "usa_usa",
      flow_control = spflow_control(model = "model_2",
                                    estimation_method = "s2sls")
    )
})


