# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Project: spflow - simulate spatial interaction for the given examples
# Author: Lukas Dargel & Thibault Laurent
# = = = = = = = = = = = = = = = = = = =
# Description:
#
# For all four sets of od-pairs we simulate two types of models.
# - - - - - - - - - - - - - - - - - - -
# Date: Jul 2022


library("spflow")
source("data-raw/01_sim_data__germany_16_states.R")
source("data-raw/02_sim_data__usa_51_states.R")
source("data-raw/03_sim_data__spflow_network_pairs.R")
source("data-raw/04_sim_data__parameters.R")

# default multi-network
 suppressWarnings({
   multi_net_usa_ge <- spflow_network_multi(
     germany_net,
     usa_net,
     within_ge_pairs,
     within_usa_pairs,
     ge_to_usa_pairs,
     usa_to_ge_pairs
   )
 })

# copy that includes lags...
# ... for Germany
ge_data_with_lag <- dat(germany_net)
W_ge <- neighborhood(germany_net)
ge_data_with_lag$X.lag1 <- as.vector(W_ge %*% ge_data_with_lag$X)

# ... for USA
usa_data_with_lag <- dat(usa_net)
W_usa <- neighborhood(usa_net)
usa_data_with_lag$X.lag1 <- as.vector(W_usa %*% usa_data_with_lag$X)

# ... insert them in the multi-net
multi_net_usa_ge_copy <- multi_net_usa_ge
dat(multi_net_usa_ge_copy@nodes$ge) <- ge_data_with_lag
dat(multi_net_usa_ge_copy@nodes$usa) <- usa_data_with_lag

# extract the exogenous variables and add intraregional ones
pair_ids <- spflow:::lookup(id(multi_net_usa_ge)[["pairs"]])
pair_variables <- lapply(pair_ids, function(.id) {
  .dat <- pair_merge(multi_net_usa_ge_copy,.id, orig_cols = TRUE, dest_cols = TRUE, pair_cols = TRUE)
  .dat[["(Intercept)"]] <- 1
  .dat
  })


pair_variables[1:2] <- lapply(pair_variables[1:2], function(.dat) {
  .dat[["(Intra)"]] <- as.integer(.dat[["ID_ORIG"]] == .dat[["ID_DEST"]])
  .dat[["I_X"]] <- .dat[["(Intra)"]] * .dat[["O_X"]]
  .dat})

model_vars <- names(delta)
pair_variables_mat <- lapply(pair_variables, function(.dat) {
  .dat[["P_DISTANCE"]] <- .dat[["DISTANCE"]]
  as.matrix(.dat[intersect(model_vars, names(.dat))])
})

# compute spatial filter matrices for the simulations
pair_neighborhoods <- lapply(spflow:::lookup(pair_ids), function(.id) {
  net_pair <- pull_member(multi_net_usa_ge_copy, .id)
  net_pair_ids <- id(net_pair)

  spflow:::expand_spflow_neighborhood(
    OW = neighborhood(multi_net_usa_ge_copy, net_pair_ids["orig"]),
    DW = neighborhood(multi_net_usa_ge_copy, net_pair_ids["dest"]),
    flow_indicator = spflow:::get_flow_indicator(net_pair))
  })

model_filters <- lapply(pair_neighborhoods, function(.nbs) {
  rho_ <- function(x) rho[paste0("rho_",x)]
  list("y9" = spflow:::spatial_filter(.nbs, rho_(c("d","o","w"))),
       "y2" = spflow:::spatial_filter(.nbs["Wd"], rho_("d")),
       "y1" = Matrix::Diagonal(nrow(.nbs[["Wd"]])))})

# simulate for all models
set.seed(1234)
flows <- mapply(function(filters,variables,ids) {
  sim_one <- function(.filt) { spflow:::spflow_sim(
    exogenous_variables = variables,
    model_coefficients = delta[colnames(variables)],
    filter_matrix = .filt,
    noise_sd = sd_error)}
  data.frame(lapply(filters, "sim_one"))
  },
  filters = model_filters,
  variables = pair_variables_mat,
  SIMPLIFY = FALSE)

# add the simulated flows to the initial data
y_cols <- c("y9","y2","y1")
for (i in seq_along(multi_net_usa_ge@pairs)) {
  multi_net_usa_ge@pairs[[i]]@pair_data[y_cols] <- flows[[i]]
}

save(multi_net_usa_ge, file = "data/multi_net_usa_ge.rda", compress = "bzip2")



# keep the vectorized data for integration tests
vec_data_usa_ge <- Map("cbind",flows,pair_variables_mat)
vec_data_usa_ge <- lapply(vec_data_usa_ge, "as.matrix")
saveRDS(vec_data_usa_ge, "tests/integration/vec_data_usa_ge.Rds")
saveRDS(pair_neighborhoods, "tests/integration/pair_neighborhoods_usa_ge.Rds")
