spflow_model_moments <- function(formulation,...) {

  model_moments <- switch(formulation,
    "vector" = NULL, # TODO implement vector formulation
    "matrix" = spflow_model_moments_mat(...)
  )

  ## ... veirfy the model moments
  infinities_error_template <-
    "The estimation is aborted because the %s variables contain " %p%
    "infinite values!" %p%
    "\nPlease check that all variables are well defined and that all " %p%
    "tranformations are valid (e.g avoid logarithms of 0)."

  assert(all(!is.infinite(model_moments$ZZ)),
         sprintf(infinities_error_template, "explanatory"))

  assert(all(!is.infinite(model_moments$ZY)),
         sprintf(infinities_error_template, "response"))


  return(model_moments)

}

spflow_model_moments_mat <- function(
  model_matrices,
  estimator,
  flow_type
) {

  # number of observations
  N <- length(model_matrices$Y[[1]])

  # Full moments including all instrumental variables
  HH <- moment_empirical_var(model_matrices) %>% as.matrix()
  HY <- model_matrices$Y %>%
    lapply(moment_empirical_covar,model_matrices) %>%
    Reduce(cbind, x = .,init = matrix(nrow = nrow(HH),ncol = 0))

  # For the stage two moments is suffices to drop the instrumental variables
  variable_order <- c("const","const_intra","DX","OX","IX","G")
  keep_for_stage2 <-
    !identify_instrumental_variables(model_matrices[variable_order])

  ZZ <- HH[keep_for_stage2, keep_for_stage2]
  ZY <- HY[keep_for_stage2,]

  # total sum of squares is different for GMM and likelihood based estimators
  # because the lagged flows are considered as endogenous regressors and not
  # as additional dependent variable
  is_GMM_estimator <- estimator %in% c("s2sls","ols")
  nb_lhs_vars <- ifelse(is_GMM_estimator,1,ncol(ZY))

  TSS <- hadamard_sum_matrix(model_matrices$Y[seq_len(nb_lhs_vars)])

  model_moments <- list(
    "N" = N,
    "TSS" = TSS,
    "HH" = HH,
    "HY" = HY,
    "ZZ" = ZZ,
    "ZY" = ZY)
  if (is_GMM_estimator)
    return(model_moments)

  # The likelihood estimators do not require the "first stage moments"
  model_moments$HH <- NULL
  model_moments$HY <- NULL


  # The trace sequence is used to approximate the log-determinant term in
  # in the likelihood function
  LL_moments <- named_list(c("DW_traces", "OW_traces","n_d","n_o"))
  LL_moments[c("n_d","n_o")] <- dim(model_matrices$Y[[1]])

  # TODO develop a solution to traces based on Wo, Wd, Ww
  # calculate only once if O=D
  if (flow_type == "within") {
    W <- model_matrices$OW %||% model_matrices$DW %||% NULL
    W_traces <- W %|!|% trace_sequence(W)

    LL_moments$OW_traces <- model_matrices$OW %|!|% W_traces
    LL_moments$DW_traces <- model_matrices$DW %|!|% W_traces
  }

  if (flow_type == "between") {
    W <- model_matrices$OW %||% model_matrices$OD %||% NULL
    W_traces <- W %|!|% trace_sequence(model_matrices$OW)

    LL_moments$DW_traces <-
      model_matrices$DW %|!|% trace_sequence(model_matrices$DW)
    LL_moments$OW_traces <-
      model_matrices$OW %|!|% trace_sequence(model_matrices$OW)
  }
  return(c(model_moments,LL_moments))
}

identify_instrumental_variables <- function(model_matrices) {
  rapply(
    object = model_matrices,
    f = attr,
    which = "is_instrument_var")
}
