#' @keywords internal
spflow_model_estimation <- function(
  model_moments,
  estim_control) {

  estimation_results <- switch(estim_control$estimation_method,
    "ols" = {
      spflow_ols(
        ZZ  = model_moments[["ZZ"]],
        ZY  = model_moments[["ZY"]],
        TSS = model_moments[["TSS"]],
        N   = model_moments[["N"]],
        flow_control = estim_control
      )},
    "s2sls" = {
      spflow_s2sls(
        UU  = model_moments[["UU"]],
        UY  = model_moments[["UY"]],
        ZZ  = model_moments[["ZZ"]],
        ZY  = model_moments[["ZY"]],
        TSS = model_moments[["TSS"]],
        N   = model_moments[["N"]],
        flow_control = estim_control
      )},
    "mle" = {
      spflow_mle(
        ZZ    = model_moments[["ZZ"]],
        ZY    = model_moments[["ZY"]],
        TSS   = model_moments[["TSS"]],
        N     = model_moments[["N"]],
        n_d   = model_moments[["n_d"]],
        n_o   = model_moments[["n_o"]],
        DW_traces = model_moments[["DW_traces"]],
        OW_traces = model_moments[["OW_traces"]],
        flow_control = estim_control
      )},
    "mcmc" = {spflow_mcmc(
      ZZ  = model_moments[["ZZ"]],
      ZY  = model_moments[["ZY"]],
      TSS = model_moments[["TSS"]],
      N   = model_moments[["N"]],
      n_d = model_moments[["n_d"]],
      n_o = model_moments[["n_o"]],
      DW_traces = model_moments[["DW_traces"]],
      OW_traces = model_moments[["OW_traces"]],
      flow_control = estim_control
    )}
  )

  return(estimation_results)

}

