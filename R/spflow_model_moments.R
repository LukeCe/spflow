spflow_model_moments <- function(
  model_matrices,
  estimator
) {

  switch(estimator,
    "s2sls" = derive_GMM_moments(model_matrices),
    "mle" = derive_likelihood_moments(model_matrices),
    "mcmc" = derive_likelihood_moments(model_matrices),
    "ols" = derive_OLS_moments(model_matrices)
  )
}

derive_GMM_moments <- function(
  model_matrices) {


  # number of obsercations
  N <- length(model_matrices$Y[[1]])

  # sum of squares of the response variable
  TSS <- hadamarad_sum(model_matrices$Y[[1]])

  # empirical variance of all exogenous variable (including instruments)
  HH <- "1"

  # empirical covariance of the exogenous variables (including instruments)
  # with the response and all endogenous variables
  HY <- "2"


  # For the stage two moments is suffices to drop the instrumental variables
  variable_order <- c("const","const_intra","DX","OX","IX","G")
  subset_stage2 <- !rapply(attr,which = "is_instrument_var")

  # empirical variance of all exogenous variable (without instruments)
  ZZ <- "3"

  # empirical covariance of the exogenous variables (without instruments)
  # with the response and all endogenous variables
  ZY <- "4"


  return(list("N" = N,
              "TSS" = TSS,
              "HH" = HH,
              "HY" = HY,
              "ZZ" = ZZ,
              "ZY" = ZY,))

}


derive_likelihood_moments <- function(
  model_matrices) {

  N <- length(model_matrices$Y[[1]])






}
