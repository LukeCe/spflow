spflow_model_estimation <- function(
  model_matrices,
  flow_control) {

  estimator <- flow_control$estimation_method

  ## ... derive the model moments
  model_moments <- spflow_model_moments(
    formulation =  flow_control$formulation,
    model_matrices = model_matrices,
    estimator = estimator,
    flow_type = flow_control$flow_type)

  na_error_template <-
    "The estimation is aborted because the %s variables contain " %p%
    "NA values!" %p%
    "\nPlease check that all variables are well defined."

  assert(all(!is.na(model_moments$ZZ)),
         sprintf(na_error_template, "explanatory"))

  assert(all(!is.na(model_moments$ZY)),
         sprintf(na_error_template, "response"))


  estimation_results <- switch(estimator,
    "s2sls" = {
      spflow_s2sls(
        HH  = model_moments$HH,
        HY  = model_moments$HY,
        ZZ  = model_moments$ZZ,
        ZY  = model_moments$ZY,
        TSS = model_moments$TSS,
        N   = model_moments$N,
        flow_control = flow_control
      )},
    "mle" = {
      spflow_mle(
        ZZ    = model_moments$ZZ,
        ZY    = model_moments$ZY,
        TSS   = model_moments$TSS,
        N     = model_moments$N,
        n_d   = model_moments$n_d,
        n_o   = model_moments$n_o,
        OW_traces = model_moments$OW_traces,
        DW_traces = model_moments$DW_traces,
        flow_control = flow_control
      )},
    "mcmc" = {spflow_mcmc(
      ZZ = model_moments$ZZ,
      ZY = model_moments$ZY,
      TSS = model_moments$TSS,
      N = model_moments$N,
      n_d = model_moments$n_d,
      n_o = model_moments$n_o,
      OW_traces = model_moments$OW_traces,
      DW_traces = model_moments$DW_traces,
      flow_control = flow_control
    )}
  )

  estimation_results <- add_details(estimation_results,
                                    model_matrices = model_matrices,
                                    flow_control = flow_control)


  return(estimation_results)

}

