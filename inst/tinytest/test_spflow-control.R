# ---- spflow_control ---------------------------------------------------------

## general options
expect_inherits(spflow_control(),"list")
expect_error(spflow_control(estimation_method = "no_estimator"))
expect_error(spflow_control(model = "no_model"))
expect_error(spflow_control(use_intra = "no_logical"))
expect_error(spflow_control(use_intra = "no_logical"))
expect_error(spflow_control(sdm_variables = "no_sdm_option"))
expect_error(spflow_control(parameter_space = "no_parameter_space_option"))

## specific options
expect_null({
  spflow_control("ols", mle_hessian_method = "mixed")[["mle_hessian_method"]]
  },
  info = "igonore and hide irrelevant options")
expect_inherits({
  spflow_control("ols",
                 twosls_instrumental_variables =
                   "no_instrument_options")
  }, "list",
  info = "igonore and hide irrelevant options")

# mle
expect_error(spflow_control("mle", mle_hessian_method = "no_hessian_method"))

# s2sls
expect_error({
  spflow_control("s2sls",
                 twosls_instrumental_variables =
                   "no_instrument_options")
  })
expect_error({
  spflow_control("s2sls",
                 twosls_decorrelate_instruments =
                   "no_logical")
  })
expect_error({
  spflow_control("s2sls",
                 twosls_reduce_pair_instruments =
                   "no_logical")
})

# mcmc
expect_error(spflow_control("mcmc", mcmc_iterations = -1))
expect_error(spflow_control("mcmc", mcmc_burn_in = -1))
expect_error(spflow_control("mcmc", mcmc_resampling_limit = -1))
expect_error(spflow_control("mcmc", mcmc_iterations = 1, mcmc_burn_in = 2))
