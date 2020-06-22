spflow_model_moments <- function(formulation,...) {

  # TODO implement vector formulation
  formulation <- "matrix"

  model_moments <- switch(formulation,
    "vector" = NULL,
    "matrix" = spflow_model_moments_mat(...)
  )

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
    reduce(cbind)

  # For the stage two moments is suffices to drop the instrumental variables
  variable_order <- c("const","const_intra","DX","OX","IX","G")
  keep_for_stage2 <-
    !identify_instrumental_variables(model_matrices[variable_order])

  ZZ <- HH[keep_for_stage2, keep_for_stage2]
  ZY <- HY[keep_for_stage2,]

  # total sum of squares is diffrent for GMM and liklihood based estimators
  # because the lagged flows are considered as endogenous regressors and not
  # as additional dependent variable
  is_GMM_estimator <- estimator == "s2sls"
  nb_lhs_vars <- ifelse(is_GMM_estimator,1,ncol(ZY))

  TSS <- hadamarad_sum_matrix(model_matrices$Y[seq_len(nb_lhs_vars)])

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


  # The trace seqence is used to approximate the log-determinant term in
  # in the likelihood function
  LL_moments <- named_list(c("OW_traces", "OW_traces"))

  LL_moments$OW_traces <- trace_sequence(model_matrices$OW)
  orign_net_equals_destination_net <- (flow_type == "within")

  if (!orign_net_equals_destination_net)
    LL_moments$DW_traces <- trace_sequence(model_matrices$DW)

  return(c(model_moments,LL_moments))
}

identify_instrumental_variables <- function(model_matrices) {
  rapply(
    object = model_matrices,
    f = attr,
    which = "is_instrument_var")
}
