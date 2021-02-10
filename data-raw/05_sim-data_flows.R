# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Project: spflow - simulate spatial interaction for the given examples
# Author: Lukas Dargel & Thibault Laurent
# = = = = = = = = = = = = = = = = = = =
# Description:
#
# For all four network pairs we simulate two types of models.
# - - - - - - - - - - - - - - - - - - -
# Date: February 2021


library("spflow")
source("data-raw/01_sim-data_germany-16-states.R")
source("data-raw/02_sim-data_usa-51-states.R")
source("data-raw/03_sim-data_network-pairs.R")
source("data-raw/04_sim-data_parameters.R")

# default multi-network
multi_net_usa_ge <- sp_multi_network(
  germany_net,
  usa_net,
  within_ge_pairs,
  within_usa_pairs,
  ge_to_usa_pairs,
  usa_to_ge_pairs
)

# copy that includes lags...
# ... for Germany
ge_data_with_lag <- germany_net %>% dat() %>% data.table::copy()
W_ge <- germany_net %>% neighborhood() %>% as.matrix()
ge_data_with_lag[, X_lag := W_ge %*% X ]

# ... for USA
usa_data_with_lag <- usa_net %>% dat() %>% data.table::copy()
W_usa <- usa_net %>% neighborhood() %>% as.matrix()
usa_data_with_lag[, X_lag := W_usa %*% X ]

# ... insert them in the multi-net
multi_net_usa_ge_copy <- multi_net_usa_ge %>% data.table::copy()
dat(multi_net_usa_ge_copy@networks$ge) <- ge_data_with_lag
dat(multi_net_usa_ge_copy@networks$usa) <- usa_data_with_lag

# extract the exogenous variables and add intraregional ones
assign_columns <- c("intra_X","intra_X_lag")
pair_ids <- id(multi_net_usa_ge)$network_pairs %>% names

pair_variables <- pair_ids %>%
  lapply(function(.id) pair_merge(multi_net_usa_ge_copy,.id)) %>%
  lapply(function(.dat) .dat[,"(Intercept)" := 1]) %>%
  lapply(function(.dat) .dat[,"distance" := log(distance + 1)])

pair_variables[1:2] <- pair_variables[1:2] %>%
  lapply(function(.dat) .dat[,"(Intra)" := (ORIG_ID == DEST_ID) ]) %>%
  lapply(function(.dat) .dat[,"intra_X" := `(Intra)` * ORIG_X]) %>%
  lapply(function(.dat) .dat[,"intra_X_lag" := `(Intra)` * DEST_X_lag])

model_vars <- names(delta)
pair_variables_mat <- pair_variables %>%
  lapply(function(.dat) .dat[,model_vars[model_vars %in% names(.dat)],
                             with = FALSE]) %>%
  lapply(as.matrix)

# compute spatial filter matrices for the simulations
origin_ids <- spflow:::lookup(pair_ids) %>%
  lapply(function(.id)
    id(multi_net_usa_ge)$network_pairs[[.id]][["orig"]])

destination_ids <- spflow:::lookup(pair_ids) %>%
  lapply(function(.id)
    id(multi_net_usa_ge)$network_pairs[[.id]][["dest"]])


all_regions <- unlist(c(origin_ids,destination_ids)) %>% unique()

sp_neighborhoods <- pull_neighborhood(multi_net_usa_ge,all_regions)

pair_neighborhoods <- mapply(
  function(.o, .d) expand_flow_neighborhood(sp_neighborhoods[[.o]],
                                            sp_neighborhoods[[.d]]),
  .o = origin_ids, .d = destination_ids, SIMPLIFY = FALSE)


invers_model_filters <- list(
  "y9" = pair_neighborhoods %>%
    lapply(spflow:::invert_spatial_filter, rho[paste0("rho_",c("d","o","w"))]),
  "y2" = pair_neighborhoods %>%
    lapply(function(.w) spflow:::invert_spatial_filter(.w[["Wd"]], rho["rho_d"])))


# simulate for all models
spflow_sim_multi <- function(filters) {
  mapply(
    function(filters,variables) {
      spflow_sim(
      exogenous_variables = variables,
      model_coefficients = delta[colnames(variables)],
      inverted_filter = filters,
      noise_sd = sd_error)},
    filters = filters,
    variables = pair_variables_mat,
    SIMPLIFY = FALSE)
}

flows <- invers_model_filters %>%
  lapply(spflow_sim_multi) %>%
  spflow:::translist(.) %>%
  lapply(data.frame)

# add the simulated flows to the initial data
mapply(set_columns,
       multi_net_usa_ge %>% pull_pairs(),
       flows,
       SIMPLIFY = FALSE) %>%
  invisible()

usethis::use_data(multi_net_usa_ge,overwrite = TRUE)
