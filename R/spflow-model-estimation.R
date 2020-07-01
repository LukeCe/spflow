spflow_model_estimation <- function(
  model_moments,
  flow_control) {

  estimator <- flow_control$estimation_method

  estimation_results <- switch(estimator,
    "s2sls" = {
      spflow_s2sls(
        HH  = model_moments$HH,
        HY  = model_moments$HY,
        ZZ  = model_moments$ZZ,
        ZY  = model_moments$ZY,
        TSS = model_moments$TSS,
        N   = model_moments$N
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
        model = flow_control$model,
        hessian_method = flow_control$hessian_method
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
      model = flow_control$model
    )}
  )

  return(estimation_results)

}

spflow_model <- function(
  results_df,
  varcov,
  sd_error,
  N,
  method,
  ...) {


    c(list("results" = results_df,
           "varcov" = varcov,
           "sd" = sd_error,
           "N" = N,
           "method" = method,
           "residuals" = NULL,
           "fitted" = NULL,
           "data" = NULL,
           "model" = NULL,
           "auto-corr" = NULL,
           "formulation" = NULL),
      list(...)
      ) %>%
    structure(class = "spflow_model")
}
