# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Project: spflow - acceptance test
# Author: Lukas Dargel
# = = = = = = = = = = = = = = = = = = =
# Description:
#
# The script tests the user experience of the packages using the scenario of
# commuting flows in pars.
# = = = = = = = = = = = = = = = = = = =
# Date: Mai 2021

library("data.table")
library("spflow")
library("sf")
library("tibble")

# ---- step 1: default estimation ---------------------------------------------
# The first step should be successful creation of the required network objects,
# followed by the default estimate with minimal user input.
# Check that data can be data.frame, data.table and tibble.
data("paris10km_commuteflows")
data("paris10km_municipalities")
data("paris10km_neighborhood")

# data.frames
paris_net <- spflow_network(
  id_net = "paris10km",
  node_neighborhood = paris10km_neighborhood[["by_contiguity"]],
  node_data = st_drop_geometry(paris10km_municipalities),
  node_key_column = "ID_MUN")

paris_pairs <- spflow_network_pair(
  id_orig_net = "paris10km",
  id_dest_net = "paris10km",
  orig_key_column = "ID_ORIG",
  dest_key_column = "ID_DEST",
  pair_data = paris10km_commuteflows)

pairs_multinet <- spflow_network_multi(paris_net, paris_pairs)
default_formula <- log(COMMUTE_FLOW+1) ~ . + P_(log(DISTANCE + 1))
results_default <- spflow(default_formula, pairs_multinet)

expect_inherits(results_default,"spflow_model")

# data.tables
paris_net_DT <- spflow_network(
  id_net = "paris10km",
  node_neighborhood = paris10km_neighborhood[["by_contiguity"]],
  node_data = as.data.table(st_drop_geometry(paris10km_municipalities)),
  node_key_column = "ID_MUN")

paris_pairs_DT <- spflow_network_pair(
  id_orig_net = "paris10km",
  id_dest_net = "paris10km",
  orig_key_column = "ID_ORIG",
  dest_key_column = "ID_DEST",
  pair_data = as.data.table(paris10km_commuteflows))

pairs_multinet_DT <- spflow_network_multi(paris_net_DT, paris_pairs_DT)
default_formula <- log(COMMUTE_FLOW+1) ~ . + P_(log(DISTANCE + 1))
results_default_DT <- spflow(default_formula, pairs_multinet_DT)

expect_inherits(results_default_DT,"spflow_model")
rm(paris_net_DT, paris_pairs_DT, pairs_multinet_DT, results_default_DT)

# tibbles
paris_net_tib <- spflow_network(
  id_net = "paris10km",
  node_neighborhood = paris10km_neighborhood[["by_contiguity"]],
  node_data = as.data.table(st_drop_geometry(paris10km_municipalities)),
  node_key_column = "ID_MUN")

paris_pairs_tib <- spflow_network_pair(
  id_orig_net = "paris10km",
  id_dest_net = "paris10km",
  orig_key_column = "ID_ORIG",
  dest_key_column = "ID_DEST",
  pair_data = as.data.table(paris10km_commuteflows))

pairs_multinet_tib <- spflow_network_multi(paris_net_tib, paris_pairs_tib)
default_formula <- log(COMMUTE_FLOW+1) ~ . + P_(log(DISTANCE + 1))
results_default_tib <- spflow(default_formula, pairs_multinet_tib)

expect_inherits(results_default_tib,"spflow_model")
rm(paris_net_tib, paris_pairs_tib, pairs_multinet_tib, results_default_tib)

# ---- step 2: high level estimation options ----------------------------------
# high level options are "model" and "use_intra" and "estimation_method"
model_options <- paste0("model_",1:9)
for (i in seq_along(model_options)){
  this_control <- spflow_control(model = model_options[i])
  this_model <- spflow(default_formula, pairs_multinet,
                       estimation_control = this_control)

  expect_inherits(this_model,"spflow_model",
                  info = paste0("default estimation for model_",
                                model_options[i]))
}

use_intra <- FALSE
estim_options <- c("ols","mle","mcmc") # "s2sls" is singular
model_types <- c(rep("SDM",3),"OLM")
for (i in seq_along(estim_options)){
  this_control <- spflow_control(estimation_method = estim_options[i],
                                 use_intra = use_intra)
  this_model <- spflow(default_formula, pairs_multinet,
                       estimation_control = this_control)

  expect_inherits(this_model,"spflow_model",
                  info = paste0("default estimation for estimator",
                                estim_options[i]))
  expect_equal(this_model@estimation_control$model_type, model_types[i])
  expect_true(FALSE)
}

# ---- step 4: model comparisons ----------------------------------------------
# decide for the best model based on some criterion
this_model <- spflow(
  log(COMMUTE_FLOW+1) ~ . + P_(log(DISTANCE + 1)),
  pairs_multinet,
  estimation_control = spflow_control(use_intra = TRUE))

this_model_stepwise <- spflow_refit(this_model,refit_type = "stepwise", protected_params = "P_log(DISTANCE + 1)")
expect_inherits(compare_results(this_model_stepwise), "data.frame")

this_model_family <- spflow_refit(this_model)
compare_results(this_model_family)
expect_inherits(compare_results(this_model_family), "data.frame")


# ---- step 5: analyse the residuals ------------------------------------------
# compute the marginal effects and visualize them
stop("not implemented")

# ---- step 6: impact calculations --------------------------------------------
# compute the marginal effects and visualize them
stop("not implemented")

# ---- step 7: predict for new observations -----------------------------------
# compute the marginal effects and visualize them
stop("not implemented")


