spflow_model_estimation <- function(
  model_moments,
  estimator) {

  estimator <- "spflow_" %p% estimator
  estimation_results <- do.call(estimator,args = model_moments)

  return(estimation_results)

}

spflow_s2sls <- function(HH,HY,ZZ,ZY,TSS,N) {

  # number of auto-regressive parameters and model coefficients and total
  nb_rho <- nrow(HY) - 1
  nb_delta <- nrow(ZZ)
  size_mu <- nb_rho + nb_delta

  # generate the moments of the second stage
  stage2_ZZ <- matrix(nrow = size_mu, ncol = size_mu)
  stage2_ZY <- vector(length = size_mu)

  index_rho <- seq_len(nb_rho)
  index_delta <- seq_len(nb_delta) + nb_rho

  # fill four blocks of the second stage variance moment
  stage2_ZZ[index_rho,index_rho] <- crossprod(HJ,solve(HH,HJ))
  stage2_ZZ[index_rho,index_delta] <- t(ZY)
  stage2_ZZ[index_delta,index_rho] <- ZY
  stage2_ZZ[index_delta,index_delta] <- ZZ

  # fill two blocks of the second stage covariance moment
  stage2_ZY[index_rho] <- crossprod(HJ,solve(HH,HY))
  stage2_ZY[index_delta] <- ZY

  #parameters
  mu <- solve(stage2_ZZ,stage2_ZY)

  # standard errors
  ESS <- crossprod(stage2_ZY %*% mu)
  RSS <- TSS - ESS
  sigma2 <- sum(RSS)/N

  # variance covariance matrix of the parameters
  varcov <- sigma2*solve(s2_MM_var)
  sd_mu <- sqrt(diag(varcov))

  results_df <- data.frame(
    "est" = mu,
    "sd" = sd_mu,
    "t.stat" = NA,
    "p.value" = NA
  )

  results_df$"t.stat" <- results_df$est / results_df$sd
  results_df$"p.value" <- 1 - pt(abs(results_df$est / results_df$sd), N - nb_delta)

  estimation_results <- structure(
    list(
      "results" = results_df,
      "varcov" = varcov,
      "sd" = sqrt(sigma2),
      "method" = "Spatial 2SLS",
      "N" = N,
      "residuals" = NULL,
      "fitted" = NULL,
      "data" = NULL,
      "formulation" = "matrix"
    ),
    class = "spflow_model")

  return(estimation_results)
}
