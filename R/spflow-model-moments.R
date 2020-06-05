spflow_model_moments_mat <- function(
  model_matrices,
  estimator
) {

  # number of observations
  N <- length(model_matrices$Y[[1]])

  # Full moments including all instrumental variables
  HH <- moments$empirical_var(model_matrices)
  HY <- model_matrices$Y %>%
    lapply(moments$empirical_covar,model_matrices) %>%
    reduce(cbind)

  # For the stage two moments is suffices to drop the instrumental variables
  variable_order <- c("const","const_intra","DX","OX","IX","G")
  keep_for_stage2 <- !rapply(
    object = model_matrices[variable_order],
    f = attr,
    which = "is_instrument_var")

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

  LL_moments <- list("traces" = stop("implement when we need it"))

  return(c(model_moments,LL_moments))
}


