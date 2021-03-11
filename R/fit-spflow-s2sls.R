spflow_s2sls <- function(HH,HY,ZZ,ZY,TSS,N,flow_control) {

  # number of auto-regressive parameters and model coefficients and total
  nb_rho <- ncol(HY) - 1
  nb_delta <- ncol(ZZ)
  size_mu <- nb_rho + nb_delta

  # generate the moments of the second stage
  stage2_ZZ <- matrix(nrow = size_mu, ncol = size_mu)
  stage2_ZY <- vector(length = size_mu)

  index_rho <- seq_len(nb_rho)
  index_delta <- seq_len(nb_delta) + nb_rho

  # fill four blocks of the second stage variance moment
  HJ <- HY[,-1]
  ZJ <- ZY[,-1]
  JJ <- crossprod(HJ,solve(HH,HJ))
  stage2_ZZ[index_rho,index_rho] <- JJ
  stage2_ZZ[index_rho,index_delta] <- t(ZJ)
  stage2_ZZ[index_delta,index_rho] <- ZJ
  stage2_ZZ[index_delta,index_delta] <- ZZ

  # fill two blocks of the second stage covariance moment
  stage2_ZY[index_rho] <- crossprod(HJ,solve(HH,HY[,1]))
  stage2_ZY[index_delta] <- ZY[,1]

  # parameters
  mu <- solve(stage2_ZZ,stage2_ZY)

  # standard errors
  ESS <- crossprod(stage2_ZY, mu)
  RSS <- TSS - ESS
  sigma2 <- sum(RSS)/N

  # variance covariance matrix of the parameters
  varcov <- sigma2 * solve(stage2_ZZ)
  sd_mu <- sqrt(diag(varcov))

  results_df <- data.frame(
    "est" = mu,
    "sd" = sd_mu)

  estimation_results <- spflow_model(
    varcov = varcov,
    estimation_results = results_df,
    estimation_control = flow_control,
    sd_error = sqrt(sigma2),
    N = N)

  return(estimation_results)
}



