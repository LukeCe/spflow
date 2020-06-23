spflow_model_estimation <- function(
  model_moments,
  flow_control) {

  estimator <- flow_control$estimation_method

  estimation_results <- switch(estimator,
    "s2sls" = {
      spflow_s2sls(
        HH  = model_moments$HH,
        HY  = model_moments$HZ,
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
        hessian_method = flow_control$hessian_method)}
  )

  return(estimation_results)

}

spflow_model <- function(
  results_df,
  varcov,
  sd_error,
  N,
  method,
  formulation,
  ...) {


    c(list("results" = results_df,
           "varcov" = varcov,
           "sd" = sd_error,
           "N" = N,
           "method" = method,
           "residuals" = NULL,
           "fitted" = NULL,
           "data" = NULL,
           "formulation" = formulation),
      list(...)
      ) %>%
    structure(class = "spflow_model")
}

