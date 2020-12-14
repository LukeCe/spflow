# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Project: spflow - test case based on the paris10km example
# Author: Lukas Dargel
# = = = = = = = = = = = = = = = = = = =
# Description:
#
# Test error free estimation for all possible models (default estimates).
# All estimators: mle s2sls mcmc
# All models: 1 - 9
# All strucutres: SDM - SAR
# - - - - - - - - - - - - - - - - - - -
# Date: June 2020
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
    orig_net_id = "paris10km",
    dest_net_id = "paris10km",
    pair_data = paris10km_node_pairs,
    orig_key_column = "ORIG_ID",
    dest_key_column = "DEST_ID")

paris10km_sp_multi <-
  sp_multi_network(paris10km_sp_nodes,paris10km_sp_pairs)


# test understandible failueres -----------------------------------------------
describe("Returns readable errors for invalid data!",{

  it("Works for infinite values",{

    # there zeros in the distance and in the flows

    log_0_formula <- log(1 + COMMUTE_FLOW) ~ . + G_(log(DISTANCE))
    expect_error({
      spflow(flow_formula = log_0_formula,
             sp_multi_network = paris10km_sp_multi)
    },
    "^The estimation is aborted because the explanatory variables contain " %p%
      "infinite values!*"
    )

    log_0_formula <- log(COMMUTE_FLOW) ~ . + G_(log(1 + DISTANCE))
    expect_error({
      spflow(flow_formula = log_0_formula,
             sp_multi_network = paris10km_sp_multi)
    },
    "^The estimation is aborted because the response variables contain " %p%
      "infinite values!*"
    )


  })

  # NA is already verifyed in the network objects

})

# test results for ols estimator ----------------------------------------------
describe("OLS returns sensible results and is used in the correct case",{

  it("Work for all models in the SDM specification",{

    # generic formula for all models
    ols_formula <- log(1 + COMMUTE_FLOW) ~
      log(POPULATION) + MED_INCOME + log(NB_COMPANY + 1) +
      G_(log(1 + DISTANCE))

    # include special instruments that were not used in estimation
    expect_ols_model <- function(model, est) {
      result <- spflow(
        flow_formula = ols_formula,
        sp_multi_network = paris10km_sp_multi,
        flow_control = spflow_control(
          est, model = model,
          decorrelate_instruments = TRUE,
          instrumental_variables = ~log(POPULATION) + log(AREA) + G_(log(1 + DISTANCE))))

      expect_is(result,class = "spflow_model_ols")
      expect_true(resid(result) %>% sum() < 1)
      return(invisible(result))
    }

    # 5 cases for OLS
    m1_ols <- expect_ols_model("model_1", "ols")
    m9_ols <- expect_ols_model("model_9", "ols")
    m1_mle <- expect_ols_model("model_1", "mle")
    m1_mcmc <- expect_ols_model("model_1", "mcmc")
    m1_s2sls <- expect_ols_model("model_1", "s2sls")


  })
})

# test results for s2sls estimator --------------------------------------------

describe("S2SLS returns sensible results.",{

  it("Work for all models in the SDM specification",{

    # generic formula for all models
    s2sls_formula <- log(1 + COMMUTE_FLOW) ~
      log(POPULATION) + MED_INCOME + log(NB_COMPANY + 1) +
      G_(log(1 + DISTANCE))

    # 9 cases
    # include special instruments that were not used in estimation
    expect_s2sls_model <- function(model) {
      result <- spflow(
        flow_formula = s2sls_formula,
        sp_multi_network = paris10km_sp_multi,
        flow_control = spflow_control(
          "s2sls", model = model,
          decorrelate_instruments = TRUE,
          instrumental_variables = ~log(POPULATION) + log(AREA) + G_(log(1 + DISTANCE))))

      expect_is(result,class = "spflow_model_s2sls")
      expect_true(resid(result) %>% sum() < 1)
      return(invisible(result))
    }


    m2 <- expect_s2sls_model("model_2")
    m3 <- expect_s2sls_model("model_3")
    m4 <- expect_s2sls_model("model_4")
    m5 <- expect_s2sls_model("model_5")
    m6 <- expect_s2sls_model("model_6")
    m7 <- expect_s2sls_model("model_7")
    m8 <- expect_s2sls_model("model_8")
    m9 <- expect_s2sls_model("model_9")

    # test that "pure" instruments dont enter the regression coefficients
    coef_names <-
    has_coef <- function(mod, pattern){
      any(grepl(pattern,coef(mod) %>% names()))
    }

    expect_false(has_coef(m9,"^.*log\\(AREA\\)$"))
    expect_true(has_coef(m9,"^.*log\\(POPULATION\\)$"))

    })

  it("Works for the special case of missing pair variables",{


    # include special instruments that were not used in estimation
    # dont use sdm
    expect_s2sls_model <- function(model, formula) {
      result <- spflow(
        flow_formula = formula,
        sp_multi_network = paris10km_sp_multi,
        flow_control = spflow_control(
          "s2sls", model = model,
          use_sdm = FALSE,
          decorrelate_instruments = TRUE,
          instrumental_variables = ~log(POPULATION) + log(AREA) + G_(log(1 + DISTANCE))))

      expect_is(result,class = "spflow_model_s2sls")
      return(invisible(result))
    }


    # formula without pair variables
    s2sls_formula <- log(1 + COMMUTE_FLOW) ~
      O_(log(POPULATION) + I(MED_INCOME + 1) + log(NB_COMPANY + 1)) +
      D_(log(POPULATION) + MED_INCOME + log(NB_COMPANY + 1)) +
      I_(log(POPULATION) + MED_INCOME + log(NB_COMPANY + 1))

    m9 <- expect_s2sls_model("model_9", s2sls_formula)




  })

})

# test results for mle estimator ----------------------------------------------
describe("MLE returns sensible results.",{

  it("Work for all models and all hessians",{

    # generic formula for all models
    mle_formula <- log(1 + COMMUTE_FLOW) ~
      log(POPULATION) + MED_INCOME + log(NB_COMPANY + 1) +
      G_(log(1 + DISTANCE))

    # 9 cases
    # include special instruments that were not used in estimation
    expect_mle_model <- function(model, hess = "f2") {
      result <- spflow(
        flow_formula = mle_formula,
        sp_multi_network = paris10km_sp_multi,
        flow_control = spflow_control("mle", model = model,
                                      hessian_method = hess))

      expect_is(result,class = "spflow_model_mle")
      expect_true(resid(result) %>% sum() < 1)
      return(invisible(result))
    }


    # TODO f2 hessian is to unstable (test syymetric?)
    # f2 hessian
    #expect_mle_model("model_1")
    # m2 <- expect_mle_model("model_2")
    # m3 <- expect_mle_model("model_3")
    # m4 <- expect_mle_model("model_4")
    # m5 <- expect_mle_model("model_5")
    # m6 <- expect_mle_model("model_6")
    # m7 <- expect_mle_model("model_7")
    # m8 <- expect_mle_model("model_8")
    # m9 <- expect_mle_model("model_9")

    # mixed
    # m1_mix <- expect_mle_model("model_1","mixed")
    m2_mix <- expect_mle_model("model_2","mixed")
    m3_mix <- expect_mle_model("model_3","mixed")
    m4_mix <- expect_mle_model("model_4","mixed")
    m5_mix <- expect_mle_model("model_5","mixed")
    m6_mix <- expect_mle_model("model_6","mixed")
    m7_mix <- expect_mle_model("model_7","mixed")
    m8_mix <- expect_mle_model("model_8","mixed")
    m9_mix <- expect_mle_model("model_9","mixed")

  })
})



# test results for mcmc estimator ---------------------------------------------
describe("MCMC returns sensible results.",{

  it("Work for all models",{

    # generic formula for all models
    mcmc_formula <- log(1 + COMMUTE_FLOW) ~
      log(POPULATION) + MED_INCOME + log(NB_COMPANY + 1) +
      G_(log(1 + DISTANCE))

    expect_mcmc_model <- function(model,hessian = "mixed") {
      result <- spflow(
        flow_formula = mcmc_formula,
        sp_multi_network = paris10km_sp_multi,
        flow_control = spflow_control("mcmc", model = model))

      expect_is(result,class = "spflow_model_mcmc")
      expect_true(resid(result) %>% sum() < 5)
      return(invisible(result))
    }


    #expect_mcmc_model("model_1")
    m2 <- expect_mcmc_model("model_2")
    m3 <- expect_mcmc_model("model_3")
    m4 <- expect_mcmc_model("model_4")
    m5 <- expect_mcmc_model("model_5")
    m6 <- expect_mcmc_model("model_6")
    m7 <- expect_mcmc_model("model_7")
    m8 <- expect_mcmc_model("model_8")
    m9 <- expect_mcmc_model("model_9")

  })
})

