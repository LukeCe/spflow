#' @keywords internal
spflow_model_estimation <- function(
  model_moments,
  estim_control) {

  na_error_template <-
    "The estimation is aborted because the %s variables contain NA values!" %p%
    "\nPlease ensure that all variables in the network objects are well " %p%
    "defined."
  assert(all(!is.na(model_moments[["ZZ"]])), na_error_template, "explanatory")
  assert(all(!is.na(model_moments[["ZY"]])), na_error_template, "response")

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

