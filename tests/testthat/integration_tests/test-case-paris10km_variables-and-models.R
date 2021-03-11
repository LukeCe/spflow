# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Project: spflow - test case based on the paris10km example
# Author: Lukas Dargel
# = = = = = = = = = = = = = = = = = = =
# Description:
#
# Test that variables are used in the intended way and that the model type is
# identified correctly.
# - - - - - - - - - - - - - - - - - - -
# Date: February 2021

devtools::load_all()
library("sf")
library("data.table")
library("rrMD")

## Create the sp network objects ----------------------------------------------
data("paris10km_mat_nb")
data("paris10km_nodes")
data("paris10km_node_pairs")

network_choice <- "by_border"
paris10km_sp_nodes <-
  sp_network_nodes(network_id = "paris10km",
                   node_neighborhood = paris10km_mat_nb[[network_choice]],
                   node_data = paris10km_nodes %>% st_drop_geometry(),
                   node_id_column = "ID")

paris10km_sp_pairs <-
  sp_network_pair(orig_net_id = "paris10km",
                  dest_net_id = "paris10km",
                  pair_data = paris10km_node_pairs,
                  orig_key_column = "ORIG_ID",
                  dest_key_column = "DEST_ID")

paris10km_sp_multi_net <- sp_multi_network(paris10km_sp_nodes,
                                           paris10km_sp_pairs)


# Estimate with 4 methods and 5 models ----------------------------------------
est_methods <- c("mle","mcmc","s2sls","ols")

# Model fine tuning...
flow_formula <-
  log(COMMUTE_FLOW + 1) ~
  D_(clog(NB_COMPANY + 1) + clog(MED_INCOME + 1)) +
  O_(clog(MED_INCOME + 1) + clog(POPULATION + 1)) +
  I_(clog(POPULATION + 1) + clog(NB_COMPANY + 1)) +
  G_(log(DISTANCE + 1))

sdm_formula <- ~
  D_(clog(MED_INCOME + 1)) +
  O_(clog(NB_COMPANY + 1))

estim_spflow <- function(.est, form, form_sdm) {
  spflow(form,
         paris10km_sp_multi_net,
         flow_control = spflow_control(estimation_method = .est,
                                       sdm_variables = form_sdm))
}

fitted_models <-c(
  list("ols1" = estim_spflow("ols",flow_formula, "none")),
  list("mle1" = estim_spflow("mle",flow_formula, "none")),
  lookup(est_methods) %>% lapply("estim_spflow", flow_formula, sdm_formula)
)


describe("We estimated models with specific use for specific variables.",{

  it("Recognized the right model types.",{

    expexted_type <- c("ols1" = "OLM", "ols" = "SLM",
                       "mle" = "SDM", "mle1" = "LAG",
                       "s2sls" = "SDM",
                       "mcmc" = "SDM")
    auctual_types <-
      lapply(fitted_models, function(x) x@estimation_control$sp_model_type) %>%
      unlist()
    expect_equal(expexted_type,auctual_types[names(expexted_type)])
  })

  it("Used all variables in the inteded ways.",{

    lm_coefs <- c("Constant", "Constant_intra",
                  "Dest_clog(MED_INCOME + 1)", "Dest_clog(NB_COMPANY + 1)",
                  "Orig_clog(MED_INCOME + 1)", "Orig_clog(POPULATION + 1)",
                  "Intra_clog(POPULATION + 1)", "Intra_clog(NB_COMPANY + 1)",
                  "log(DISTANCE + 1)")
    ar_coefs <- "rho" %p% c("_d","_o","_w")
    lag_coefs <- c("Dest_clog(MED_INCOME + 1).lag1",
                   "Orig_clog(NB_COMPANY + 1).lag1")

    ## OLM
    olm_coefs <- fitted_models$ols1 %>% coef() %>% names()
    expect_true(all(sort(olm_coefs) == sort(lm_coefs)))

    ## SLM
    slm_coefs <- fitted_models$ols %>% coef() %>% names()
    expect_true(all(sort(slm_coefs) == sort(c(lm_coefs,lag_coefs))))

    ## SDM
    # mle
    sdm_coefs <- fitted_models$mle %>% coef() %>% names()
    expect_true(all(sort(sdm_coefs) == sort(c(lm_coefs,lag_coefs,ar_coefs))))

    # mcmc
    sdm_coefs <- fitted_models$mcmc %>% coef() %>% names()
    expect_true(all(sort(sdm_coefs) == sort(c(lm_coefs,lag_coefs,ar_coefs))))

    # s2sls
    sdm_coefs <- fitted_models$s2sls %>% coef() %>% names()
    expect_true(all(sort(sdm_coefs) == sort(c(lm_coefs,lag_coefs,ar_coefs))))

    ## SAR
    #mle
    sar_coefs <- fitted_models$mle1 %>% coef() %>% names()
    expect_true(all(sort(sar_coefs) == sort(c(lm_coefs,ar_coefs))))

  })

})



